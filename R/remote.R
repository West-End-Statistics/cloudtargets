ct_require_state <- function(config) {
  state <- ct_state_read(config)
  if (is.null(state) || is.null(state$host)) {
    cli::cli_abort(c(
      "No ready machine is recorded in {.file {ct_state_path(config)}}.",
      i = "Run {.fn ct_provision} first."
    ), class = "ct_error_no_machine")
  }
  state
}

ct_local_dir <- function() normalizePath(getwd(), winslash = "/")

ct_remote_dir <- function(config) {
  config$remote_dir %||% ct_local_dir()
}

ct_ssh_prefix <- function(state, config) {
  c(ct_ssh_options(config), "-p", as.character(state$port), paste0(state$user, "@", state$host))
}

# Run a shell command on the machine. The command is base64-wrapped so no
# quoting from the caller can break the remote shell.
ct_ssh_run <- function(state, config, command, echo = FALSE, timeout = 600) {
  # Pick up the container's PATH/R settings, which sshd's minimal environment drops.
  command <- paste("[ -f /etc/profile.d/zz-container-env.sh ] && . /etc/profile.d/zz-container-env.sh", command, sep = "\n")
  wrapped <- sprintf("echo %s | base64 -d | bash -eo pipefail", ct_b64(command))
  processx::run(
    "ssh", c(ct_ssh_prefix(state, config), wrapped),
    error_on_status = FALSE, echo = echo, timeout = timeout
  )
}

ct_b64 <- function(text) {
  # base64_enc wraps lines at 72 characters; the remote `echo | base64 -d`
  # needs a single line.
  gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(enc2utf8(text))))
}

ct_shell_quote <- function(x) paste0("'", gsub("'", "'\\\\''", x), "'")

#' R packages to install on the machine
#'
#' Union of `r_packages` from the configuration, the packages installed by the
#' configured Dockerfile, the Depends/Imports of a
#' `DESCRIPTION` in the project (if `r_packages_from_description` is true), and
#' `targets` and `crew`, minus base packages and `r_packages_exclude`.
#'
#' @param config Configuration list.
#' @param dir Project directory.
#' @return A sorted character vector.
#' @export
ct_r_packages <- function(config = ct_config(), dir = ".") {
  pkgs <- c(unlist(config$r_packages), unlist(config$dockerfile_packages))
  desc <- file.path(dir, "DESCRIPTION")
  if (isTRUE(config$r_packages_from_description) && file.exists(desc)) {
    fields <- read.dcf(desc, fields = c("Depends", "Imports"))
    raw <- unlist(strsplit(paste(fields[!is.na(fields)], collapse = ","), ","))
    raw <- trimws(sub("\\(.*\\)", "", raw))
    pkgs <- c(pkgs, raw[nzchar(raw)])
  }
  pkgs <- unique(c("targets", "crew", pkgs))
  base <- c("R", rownames(utils::installed.packages(priority = "base")))
  sort(setdiff(pkgs, c(base, unlist(config$r_packages_exclude))))
}

#' Install R and the project's packages on the machine
#'
#' Runs `inst/bootstrap-r.sh` remotely: R from CRAN's Ubuntu repository and
#' packages as r2u binaries. Safe to repeat; a finished package list is skipped.
#'
#' @inheritParams ct_provision
#' @param packages Packages to install; defaults to [ct_r_packages()].
#' @param state Machine to act on; defaults to the one recorded by
#'   [ct_provision()]. [ct_crew_controller()] passes each pod of its pool.
#' @return Invisibly `TRUE`.
#' @export
ct_bootstrap <- function(config = ct_config(), packages = ct_r_packages(config),
                         state = ct_require_state(config)) {
  script <- paste(readLines(system.file("bootstrap-r.sh", package = "cloudtargets")), collapse = "\n")
  cli::cli_alert_info("Installing R and {length(packages)} package{?s} on {state$host}")
  command <- sprintf(
    "cat > /tmp/bootstrap-r.sh <<'CT_EOF'\n%s\nCT_EOF\nbash /tmp/bootstrap-r.sh %s",
    script, paste(vapply(packages, ct_shell_quote, ""), collapse = " ")
  )
  res <- ct_ssh_run(state, config, command, echo = TRUE, timeout = 3600)
  if (!identical(res$status, 0L)) {
    cli::cli_abort(c(
      "Bootstrap failed (exit {res$status}).",
      i = "The machine is still running; fix and retry, or call {.fn ct_teardown}."
    ))
  }
  invisible(TRUE)
}

ct_rsync <- function(state, config, from, to, excludes = character(), delete = FALSE) {
  if (!nzchar(Sys.which("rsync"))) {
    cli::cli_abort("{.code rsync} is required locally but was not found on PATH.")
  }
  ssh <- paste(c("ssh", ct_ssh_options(config), "-p", state$port), collapse = " ")
  args <- c("-az", if (delete) "--delete", unlist(lapply(excludes, function(e) c("--exclude", e))), "-e", ssh, from, to)
  res <- processx::run("rsync", args, error_on_status = FALSE, timeout = 3600)
  if (!identical(res$status, 0L)) {
    cli::cli_abort(c("rsync failed (exit {res$status}).", x = trimws(res$stderr)))
  }
  invisible(TRUE)
}

#' Copy the project to the machine
#'
#' Rsyncs the current directory to `remote_dir` (default: the same absolute
#' path as locally, so paths hard-coded in `_targets.yaml` keep working).
#' `sync_exclude` paths are not sent.
#'
#' @inheritParams ct_provision
#' @param state Machine to copy to; defaults to the one recorded by
#'   [ct_provision()].
#' @return Invisibly `TRUE`.
#' @export
ct_sync <- function(config = ct_config(), state = ct_require_state(config)) {
  remote <- ct_remote_dir(config)
  q <- ct_shell_quote(remote)
  res <- ct_ssh_run(state, config, sprintf(
    "mkdir -p %s 2>/dev/null || { sudo -n mkdir -p %s && sudo -n chown \"$(id -u):$(id -g)\" %s; }", q, q, q
  ))
  if (!identical(res$status, 0L)) {
    cli::cli_abort("Could not create {.path {remote}} on the machine: {trimws(res$stderr)}")
  }
  ct_rsync(
    state, config,
    from = paste0(ct_local_dir(), "/"),
    to = sprintf("%s@%s:%s/", state$user, state$host, remote),
    excludes = unlist(config$sync_exclude), delete = TRUE
  )
  cli::cli_alert_success("Synced project to {.path {remote}}")
  invisible(TRUE)
}

# The script the machine runs inside tmux: cd, export env, run, record exit code.
ct_runner_script <- function(remote, command, env = character()) {
  exports <- if (length(env)) {
    paste0("export ", names(env), "=", vapply(env, ct_shell_quote, ""), collapse = "\n")
  } else {
    ""
  }
  paste(
    "#!/usr/bin/env bash",
    paste("cd", ct_shell_quote(remote)),
    "rm -f .cloudtargets-exit",
    exports,
    sprintf("( %s ) > .cloudtargets-run.log 2>&1", command),
    "echo $? > .cloudtargets-exit",
    sep = "\n"
  )
}

#' Start the pipeline on the machine, detached
#'
#' Launches `command` inside a `tmux` session named `cloudtargets`, so the run
#' survives dropped SSH connections and this R session closing. Output goes to
#' `.cloudtargets-run.log` and the exit status to `.cloudtargets-exit` in the
#' remote project directory. Poll with [ct_progress()] or [ct_monitor()].
#'
#' @inheritParams ct_provision
#' @param command Shell command to run; defaults to the `command` setting
#'   (`Rscript -e 'targets::tar_make()'`).
#' @param env Named character vector of environment variables for the run.
#'   `CLOUDTARGETS_VCPUS` (the machine's vCPU count) is always set, so a pipeline
#'   can size its worker pool with `Sys.getenv("CLOUDTARGETS_VCPUS", "8")`.
#' @return Invisibly `TRUE`.
#' @export
ct_launch <- function(config = ct_config(), command = config$command, env = character()) {
  state <- ct_require_state(config)
  remote <- ct_remote_dir(config)
  if (!is.null(state$vcpus)) {
    env <- c(CLOUDTARGETS_VCPUS = as.character(state$vcpus), env[setdiff(names(env), "CLOUDTARGETS_VCPUS")])
  }
  runner <- ct_runner_script(remote, command, env)
  launch <- sprintf(
    paste(
      "if tmux has-session -t cloudtargets 2>/dev/null; then echo 'already running' >&2; exit 3; fi",
      "echo %s | base64 -d > /tmp/ct-run.sh",
      "tmux new-session -d -s cloudtargets 'bash /tmp/ct-run.sh'",
      "sleep 2",
      "if ! tmux has-session -t cloudtargets 2>/dev/null && [ ! -f %s/.cloudtargets-exit ]; then echo 'run did not start' >&2; exit 4; fi",
      sep = "\n"
    ),
    ct_b64(runner), ct_shell_quote(remote)
  )
  res <- ct_ssh_run(state, config, launch)
  if (identical(res$status, 3L)) {
    cli::cli_abort("A run is already in progress on the machine (tmux session {.val cloudtargets}).")
  }
  if (!identical(res$status, 0L)) {
    cli::cli_abort("Could not start the run: {trimws(res$stderr)}")
  }
  cli::cli_alert_success("Started in tmux session {.val cloudtargets}; log at {.path {remote}/.cloudtargets-run.log}")
  invisible(TRUE)
}

#' Report run progress
#'
#' Reads `targets::tar_progress_summary()` from the remote store, whether the
#' run has finished (and its exit code), and the last lines of the log.
#'
#' @inheritParams ct_provision
#' @param log_lines Number of log lines to return.
#' @return A list with `finished`, `exit_code`, `counts` (named integers, or
#'   `NULL` before the store exists), `memory` (see below) and `log`. `memory` is
#'   a list with `total_mb`, `available_mb`, and, when the machine exposes
#'   cgroup v2 accounting, `limit_mb` (`NA` if unlimited), `used_mb` and
#'   `oom_kills`; `NULL` if unavailable. A rising `oom_kills` means worker
#'   processes are being killed for lack of memory.
#' @export
ct_progress <- function(config = ct_config(), log_lines = 5L) {
  state <- ct_require_state(config)
  remote <- ct_remote_dir(config)
  r_code <- paste0(
    "p <- tryCatch(as.list(targets::tar_progress_summary()), error = function(e) NULL);",
    "if (!is.null(p)) cat('COUNTS', p$skipped, p$dispatched, p$completed, p$errored, p$canceled, '\\n')"
  )
  command <- paste(
    paste("cd", ct_shell_quote(remote)),
    "echo \"EXIT $(cat .cloudtargets-exit 2>/dev/null || echo none)\"",
    sprintf("Rscript -e %s 2>/dev/null || true", ct_shell_quote(r_code)),
    # Machine memory and the container's cgroup limit/usage/OOM-kill count (cgroup v2).
    "echo \"MEM $(awk '/MemTotal/{t=$2} /MemAvailable/{a=$2} END{print int(t/1024), int(a/1024)}' /proc/meminfo 2>/dev/null)\"",
    "echo \"CG $(cat /sys/fs/cgroup/memory.max 2>/dev/null || echo na) $(cat /sys/fs/cgroup/memory.current 2>/dev/null || echo na) $(awk '/^oom_kill /{print $2}' /sys/fs/cgroup/memory.events 2>/dev/null || echo na)\"",
    "echo LOG",
    sprintf("tail -n %d .cloudtargets-run.log 2>/dev/null || true", as.integer(log_lines)),
    sep = "\n"
  )
  res <- ct_ssh_run(state, config, command, timeout = 120)
  if (!identical(res$status, 0L)) {
    cli::cli_abort("Could not read progress over SSH: {trimws(res$stderr)}")
  }
  ct_parse_progress(res$stdout)
}

ct_parse_memory <- function(lines) {
  mem <- grep("^MEM ", lines, value = TRUE)[1L]
  if (is.na(mem)) {
    return(NULL)
  }
  num <- function(x) suppressWarnings(as.numeric(x))
  m <- num(strsplit(sub("^MEM ", "", mem), " +")[[1L]])
  out <- list(total_mb = m[1L], available_mb = m[2L], limit_mb = NA_real_, used_mb = NA_real_, oom_kills = NA_integer_)
  cg <- grep("^CG ", lines, value = TRUE)[1L]
  if (!is.na(cg)) {
    v <- strsplit(sub("^CG ", "", cg), " +")[[1L]]
    out$limit_mb <- num(v[1L]) / 1024^2
    out$used_mb <- num(v[2L]) / 1024^2
    out$oom_kills <- as.integer(num(v[3L]))
  }
  out
}

ct_format_memory <- function(memory) {
  if (is.null(memory) || is.na(memory$total_mb)) {
    return("")
  }
  limit <- if (is.na(memory$limit_mb)) memory$total_mb else memory$limit_mb
  used <- if (is.na(memory$used_mb)) memory$total_mb - memory$available_mb else memory$used_mb
  out <- sprintf(" | mem %.1f/%.0f GB", used / 1024, limit / 1024)
  if (!is.na(memory$oom_kills) && memory$oom_kills > 0L) {
    out <- paste0(out, sprintf(", OOM KILLS %d", memory$oom_kills))
  }
  out
}

ct_parse_progress <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  exit_line <- sub("^EXIT ", "", grep("^EXIT ", lines, value = TRUE)[1L])
  finished <- !is.na(exit_line) && exit_line != "none"
  counts_line <- grep("^COUNTS ", lines, value = TRUE)
  counts <- NULL
  if (length(counts_line)) {
    v <- suppressWarnings(as.integer(strsplit(sub("^COUNTS ", "", counts_line[1L]), " +")[[1L]]))
    counts <- stats::setNames(v, c("skipped", "dispatched", "completed", "errored", "canceled"))
  }
  log_at <- match("LOG", lines)
  list(
    finished = finished,
    exit_code = if (finished) as.integer(exit_line) else NA_integer_,
    counts = counts,
    memory = ct_parse_memory(lines),
    log = if (!is.na(log_at)) lines[-seq_len(log_at)] else character()
  )
}

# ssh arguments that run targets::tar_poll() inside the remote project directory.
ct_poll_args <- function(state, config, interval = 5, timeout = Inf) {
  r_code <- sprintf("targets::tar_poll(interval = %s, timeout = %s)", format(interval), format(timeout))
  command <- paste0(
    "[ -f /etc/profile.d/zz-container-env.sh ] && . /etc/profile.d/zz-container-env.sh; ",
    "cd ", ct_shell_quote(ct_remote_dir(config)), " && Rscript -e ", ct_shell_quote(r_code)
  )
  c(
    ct_ssh_options(config), "-t", "-p", as.character(state$port),
    paste0(state$user, "@", state$host), sprintf("bash -c %s", ct_shell_quote(command))
  )
}

#' Watch a run with targets' own poller
#'
#' Runs `targets::tar_poll()` on the machine over SSH and streams its live
#' table (skipped / dispatched / completed / errored / canceled) to this
#' console. This is the single-machine equivalent of calling `tar_poll()`
#' locally, which is not possible because the store lives on the machine.
#' (With [ct_crew_controller()] the store is local, so plain `tar_poll()` works.)
#' Interrupting only stops watching; the run keeps going.
#'
#' @inheritParams ct_provision
#' @param interval Seconds between updates.
#' @param timeout Seconds to watch; `Inf` (default) until interrupted or the run ends.
#' @param state Machine to watch; defaults to the one recorded by [ct_provision()].
#' @return Invisibly, the exit status of the remote command.
#' @export
ct_poll <- function(config = ct_config(), interval = 5, timeout = Inf,
                    state = ct_require_state(config)) {
  # system2() pastes its arguments into a shell line, so quote each one.
  status <- system2("ssh", shQuote(ct_poll_args(state, config, interval, timeout)))
  invisible(status)
}

#' Watch a run until it finishes
#'
#' Prints one status line per poll: elapsed time, cost so far, and target
#' counts. Stops when the run finishes or errors. Ctrl-C stops watching only;
#' the run and the machine keep going. After `max_failures` consecutive failed
#' polls it gives up with an error that says the machine is still running.
#'
#' @inheritParams ct_provision
#' @param interval Seconds between polls.
#' @param max_failures Consecutive failed polls before giving up.
#' @return Invisibly, the final [ct_progress()] result.
#' @export
ct_monitor <- function(config = ct_config(), interval = 15, max_failures = 10L) {
  state <- ct_require_state(config)
  failures <- 0L
  repeat {
    prog <- tryCatch(ct_progress(config), error = function(e) e)
    stamp <- format(Sys.time(), "%H:%M:%S")
    if (inherits(prog, "ct_error_no_machine")) {
      stop(prog)
    }
    if (inherits(prog, "error")) {
      failures <- failures + 1L
      if (failures >= max_failures) {
        cli::cli_abort(c(
          "Lost contact with the machine {state$id}: {max_failures} failed polls in a row ({conditionMessage(prog)}).",
          i = "The machine and the run are NOT stopped and are still billing (~{ct_format_usd(ct_cost_estimate(state))} so far).",
          i = "Check your connection, then call {.fn ct_monitor} again, or {.fn ct_teardown} to stop it."
        ))
      }
      cli::cli_alert_warning("[{stamp}] could not reach the machine ({conditionMessage(prog)}); retry {failures}/{max_failures}")
    } else {
      failures <- 0L
      counts <- if (is.null(prog$counts)) {
        "waiting for the pipeline to start"
      } else {
        paste(sprintf("%s %d", names(prog$counts), prog$counts)[prog$counts > 0L], collapse = ", ")
      }
      cli::cli_text(
        "[{stamp}] up {round(ct_runtime_hours(state) * 60)} min, ~{ct_format_usd(ct_cost_estimate(state))} | {counts}{ct_format_memory(prog$memory)}"
      )
      if (prog$finished) {
        if (identical(prog$exit_code, 0L)) {
          cli::cli_alert_success("Pipeline finished (exit 0).")
        } else {
          cli::cli_alert_danger("Pipeline exited with status {prog$exit_code}. Last log lines:")
          cat(prog$log, sep = "\n")
        }
        return(invisible(prog))
      }
    }
    Sys.sleep(interval)
  }
}

#' Copy results back from the machine
#'
#' Rsyncs each directory or file in `fetch` (default `_targets`, `output` and the
#' run log `.cloudtargets-run.log`) from the remote project directory to the local one, so a normal local `tar_read()` or
#' `tar_make()` sees the remote results. Existing local files are kept unless
#' the remote copy differs; nothing is deleted locally. Safe to call at any
#' time, including while a run is in progress (you then get whatever has been
#' written so far; the store is in flux and metadata may lag the objects).
#'
#' @inheritParams ct_provision
#' @param dirs Directories or files (relative to the project) to fetch; defaults to the
#'   `fetch` setting.
#' @return Invisibly, the directories that were fetched.
#' @export
ct_fetch <- function(config = ct_config(), dirs = unlist(config$fetch)) {
  state <- ct_require_state(config)
  remote <- ct_remote_dir(config)
  running <- ct_run_in_progress(state, config)
  if (isTRUE(running)) {
    cli::cli_alert_warning("A run is still in progress; fetching a snapshot of the store as it is now.")
  }
  fetched <- character()
  for (d in dirs) {
    path <- ct_shell_quote(file.path(remote, d))
    kind <- ct_ssh_run(state, config, sprintf("if test -d %s; then echo dir; elif test -f %s; then echo file; else echo none; fi", path, path))
    kind <- trimws(kind$stdout)
    if (!kind %in% c("dir", "file")) {
      cli::cli_alert_warning("Nothing to fetch for {.path {d}} (not present on the machine).")
      next
    }
    if (kind == "dir") {
      dir.create(d, showWarnings = FALSE, recursive = TRUE)
      from <- sprintf("%s@%s:%s/", state$user, state$host, file.path(remote, d))
      to <- paste0(d, "/")
    } else {
      from <- sprintf("%s@%s:%s", state$user, state$host, file.path(remote, d))
      to <- d
    }
    ct_rsync(state, config, from = from, to = to)
    fetched <- c(fetched, d)
  }
  state$fetched_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ct_state_write(config, state)
  cli::cli_alert_success("Fetched {.path {fetched}}")
  invisible(fetched)
}

ct_run_in_progress <- function(state, config) {
  res <- ct_ssh_run(state, config, "tmux has-session -t cloudtargets 2>/dev/null", timeout = 60)
  identical(res$status, 0L)
}
