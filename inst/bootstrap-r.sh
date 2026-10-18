#!/usr/bin/env bash
# Make the R packages available on the machine. If R is installed, install only
# the missing packages. Otherwise (fresh Ubuntu 24.04 "noble", as root) install R
# and the packages as r2u binaries. Idempotent per package list.
# Usage: bootstrap-r.sh pkg1 pkg2 ...
set -euo pipefail
export DEBIAN_FRONTEND=noninteractive

packages=("$@")
stamp="/var/lib/cloudtargets/bootstrap-$(printf '%s\n' "${packages[@]}" | sort | sha1sum | cut -c1-12)"
if [[ -f "${stamp}" ]]; then
  echo "bootstrap: already done (${stamp})"
  exit 0
fi

if command -v Rscript >/dev/null 2>&1; then
  # R is already installed (e.g. the project's own image): add only what is missing.
  Rscript -e 'pkgs <- commandArgs(TRUE); miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(miss)) { message("bootstrap: installing ", paste(miss, collapse = ", ")); install.packages(miss, Ncpus = max(1L, parallel::detectCores())) }; bad <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(bad)) stop("failed to install: ", paste(bad, collapse = ", "))' "${packages[@]}"
  mkdir -p /var/lib/cloudtargets 2>/dev/null && touch "${stamp}" 2>/dev/null || true
  echo "bootstrap: done"
  exit 0
fi

apt-get update -qq
apt-get install -y -qq --no-install-recommends wget ca-certificates gnupg rsync tmux >/dev/null

if [[ ! -f /usr/share/keyrings/r2u.gpg ]]; then
  gpg --homedir /tmp --no-default-keyring --keyring /usr/share/keyrings/r2u.gpg \
    --keyserver keyserver.ubuntu.com \
    --recv-keys A1489FE2AB99A21A 67C2D66C4B1D4339 51716619E084DAB9
fi
cat > /etc/apt/sources.list.d/r2u.sources <<EOF
Types: deb
URIs: https://r2u.stat.illinois.edu/ubuntu
Suites: noble
Components: main
Architectures: amd64 arm64
Signed-By: /usr/share/keyrings/r2u.gpg
EOF
cat > /etc/apt/sources.list.d/cran.sources <<EOF
Types: deb
URIs: https://cloud.r-project.org/bin/linux/ubuntu
Suites: noble-cran40/
Components:
Architectures: amd64 arm64
Signed-By: /usr/share/keyrings/r2u.gpg
EOF
printf 'Package: *\nPin: release o=CRAN-Apt Project\nPin: release l=CRAN-Apt Packages\nPin-Priority: 700\n' \
  > /etc/apt/preferences.d/99cranapt

apt-get update -qq
apt-get install -y -qq --no-install-recommends r-base-core >/dev/null

# r2u publishes CRAN packages as r-cran-<lowercased name>. Anything apt does
# not know (not on CRAN, or newer than r2u) falls back to a source install.
missing=()
for pkg in "${packages[@]}"; do
  deb="r-cran-$(printf '%s' "${pkg}" | tr '[:upper:]' '[:lower:]')"
  if apt-get install -y -qq --no-install-recommends "${deb}" >/dev/null 2>&1; then
    echo "bootstrap: ${pkg} (binary)"
  else
    missing+=("${pkg}")
  fi
done
if (( ${#missing[@]} > 0 )); then
  echo "bootstrap: no binary for ${missing[*]}; installing from source"
  Rscript -e 'pkgs <- commandArgs(TRUE); install.packages(pkgs, repos = "https://cloud.r-project.org", Ncpus = max(1L, parallel::detectCores())); bad <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]; if (length(bad)) stop("failed to install: ", paste(bad, collapse = ", "))' "${missing[@]}"
fi

mkdir -p /var/lib/cloudtargets
touch "${stamp}"
echo "bootstrap: done"
