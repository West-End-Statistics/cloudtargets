#!/usr/bin/env bash
# Container start command for images that do not run sshd themselves.
# Installs openssh-server, rsync and tmux, authorises $PUBLIC_KEY for $SSH_USER,
# and runs sshd in the foreground as the container's main process.
# Works as root, or as a user with passwordless sudo (e.g. devcontainer images).
set -eu
export DEBIAN_FRONTEND=noninteractive
user="${SSH_USER:-root}"

if [ "$(id -u)" = "0" ]; then sudo_cmd=""; else sudo_cmd="sudo -n"; fi

${sudo_cmd} apt-get update -qq
${sudo_cmd} apt-get install -y -qq --no-install-recommends openssh-server rsync tmux ca-certificates curl >/dev/null
${sudo_cmd} mkdir -p /run/sshd
# A locked account ("!") can refuse key logins; "*" has no valid password but is not locked.
${sudo_cmd} usermod -p '*' "${user}" 2>/dev/null || true

home="$(getent passwd "${user}" | cut -d: -f6)"
${sudo_cmd} install -d -m 700 -o "${user}" "${home}/.ssh"
printf '%s\n' "${PUBLIC_KEY}" | ${sudo_cmd} tee "${home}/.ssh/authorized_keys" >/dev/null
${sudo_cmd} chown "${user}" "${home}/.ssh/authorized_keys"
${sudo_cmd} chmod 600 "${home}/.ssh/authorized_keys"

# Keep the container's environment (PATH, R_*, tool locations such as CMDSTAN, ...)
# for SSH sessions, which otherwise start with a minimal one. Secrets and
# per-session variables are left out; the platform injects its own API key.
export -p \
  | grep -vE '^declare -x (RUNPOD_[A-Z_]*|PUBLIC_KEY|SSH_USER|HOME|USER|LOGNAME|SHLVL|PWD|OLDPWD|_|HOSTNAME|TERM|SUDO_[A-Z]*|[A-Z_]*(KEY|SECRET|TOKEN|PASSWORD|PASS)[A-Z_]*)=' \
  | sed 's/^declare -x /export /' \
  | ${sudo_cmd} tee /etc/profile.d/zz-container-env.sh >/dev/null

# The pool runs every worker of a machine over one multiplexed SSH connection, and sshd
# allows only 10 sessions per connection by default. Lift that, and tolerate the burst of
# connections when many workers start at once.
# ClientAlive* makes sshd notice a controller that vanished without closing its connections
# (power loss, dropped network), which the idle check below relies on.
printf 'MaxSessions 512\nMaxStartups 100:30:300\nClientAliveInterval 30\nClientAliveCountMax 4\n' | ${sudo_cmd} tee /etc/ssh/sshd_config.d/10-cloudtargets.conf >/dev/null

# Time limit (CT_MAX_HOURS): the machine must not outlive its controller. After the limit the
# pod terminates ITSELF through RunPod's GraphQL API, using the key RunPod injects into every pod
# (that key is refused by the REST API, 403, but accepted for GraphQL podTerminate). It retries
# every minute in case a call fails. Ending the container instead would not work: RunPod
# restarts a container whose main process exits.
# What was set up, and every attempt's HTTP status, is written to files for debugging:
#   /var/log/cloudtargets-start.log, /var/tmp/cloudtargets-delete.log
secs=""
if [ -n "${CT_MAX_HOURS:-}" ]; then
  secs="$(awk -v h="${CT_MAX_HOURS}" 'BEGIN { printf "%d", h * 3600 }')"
fi
log=/var/log/cloudtargets-start.log
{
  echo "started: $(date -u +%FT%TZ)  CT_MAX_HOURS=${CT_MAX_HOURS:-unset}  limit_seconds=${secs:-none}"
  echo "RUNPOD_POD_ID: $([ -n "${RUNPOD_POD_ID:-}" ] && echo set || echo MISSING)  RUNPOD_API_KEY: $([ -n "${RUNPOD_API_KEY:-}" ] && echo set || echo MISSING)  curl: $(command -v curl || echo MISSING)"
} | ${sudo_cmd} tee "${log}" >/dev/null
# One helper deletes the pod, retrying every minute; the time limit and the idle check both use it.
# The key stays in the environment and reaches curl through stdin; it must not appear on any command line, where `ps` shows it.
cat > /var/tmp/ct-terminate.sh <<'EOS'
n=0
while [ "$n" -lt 120 ]; do
  code=$(printf "header = \"Authorization: Bearer %s\"\n" "$RUNPOD_API_KEY" | curl -s -m 30 -K - -o /var/tmp/cloudtargets-delete.out -w "%{http_code}" -X POST \
    -H "Content-Type: application/json" \
    -d "{\"query\":\"mutation { podTerminate(input: {podId: \\\"$RUNPOD_POD_ID\\\"}) }\"}" \
    https://api.runpod.io/graphql)
  echo "$(date -u +%FT%TZ) attempt $n http $code $(head -c 200 /var/tmp/cloudtargets-delete.out)" >> /var/tmp/cloudtargets-delete.log
  n=$((n + 1))
  sleep 60
done
EOS
if [ -n "${RUNPOD_POD_ID:-}" ] && [ -n "${RUNPOD_API_KEY:-}" ]; then
  if [ -n "${secs}" ]; then
    nohup sh -c 'sleep "$1"; echo "$(date -u +%FT%TZ) time limit reached" >> /var/tmp/cloudtargets-delete.log; exec sh /var/tmp/ct-terminate.sh' sh "${secs}" >/dev/null 2>&1 &
    echo "time limit: pod terminates itself ${secs} s after this start (GraphQL podTerminate, retried each minute)" | ${sudo_cmd} tee -a "${log}" >/dev/null
  else
    echo "time limit: NOT scheduled (CT_MAX_HOURS missing)" | ${sudo_cmd} tee -a "${log}" >/dev/null
  fi
  # Idle check (CT_ORPHAN_MINUTES, pool machines only): if no SSH connection to this machine has
  # existed for that long, the controller is gone, so terminate. Counts established connections
  # to port 22 from /proc, which needs no extra tools.
  orphan_secs="$(awk -v m="${CT_ORPHAN_MINUTES:-0}" 'BEGIN { printf "%d", m * 60 }')"
  if [ "${orphan_secs}" -gt 0 ]; then
    nohup sh -c '
      idle=0
      while :; do
        sleep 30
        n=$(awk "NR > 1 && \$4 == \"01\" && \$2 ~ /:0016\$/" /proc/net/tcp /proc/net/tcp6 2>/dev/null | wc -l)
        if [ "$n" -gt 0 ]; then idle=0; else idle=$((idle + 30)); fi
        if [ "$idle" -ge "$1" ]; then
          echo "$(date -u +%FT%TZ) no SSH connection for ${idle} s; terminating" >> /var/tmp/cloudtargets-delete.log
          exec sh /var/tmp/ct-terminate.sh
        fi
      done' sh "${orphan_secs}" >/dev/null 2>&1 &
    echo "idle check: pod terminates itself after ${orphan_secs} s without any SSH connection" | ${sudo_cmd} tee -a "${log}" >/dev/null
  fi
else
  echo "self-termination: NOT scheduled (needs RUNPOD_POD_ID and RUNPOD_API_KEY)" | ${sudo_cmd} tee -a "${log}" >/dev/null
fi

exec ${sudo_cmd} /usr/sbin/sshd -D -e
