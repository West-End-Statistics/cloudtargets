# A pool must not outlive the R session that controls it. Ctrl-C in a terminal
# signals the whole process group, and tools such as callr then kill the
# session that runs tar_make() without letting it clean up, so the cleanup
# code in the session itself cannot be relied on. The watchdog is a separate
# R process, started in its own session (so it does not receive that signal),
# that waits for the controller to disappear and then deletes every machine
# recorded in `pool-ids.json`. It exits quietly when the pool is stopped
# normally.

# Is process `pid` running (and not a zombie)?
ct_pid_alive <- function(pid) {
  stat <- sprintf("/proc/%d/stat", as.integer(pid))
  if (file.exists(stat)) {
    line <- tryCatch(readLines(stat, n = 1L, warn = FALSE), error = function(e) character())
    state <- sub("^.*\\) ([A-Za-z]) .*$", "\\1", line)
    if (length(line) && identical(state, "Z")) {
      return(FALSE)
    }
  }
  isTRUE(tools::pskill(as.integer(pid), 0L))
}

ct_watchdog_log <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), " ", ..., "\n", sep = "")
}

# Wait for process `pid` to end, then delete the machines in the id file.
# Returns "clean" (pool stopped normally), "deleted", "failed" (some machines
# could not be deleted), "nothing" or "expired".
ct_watchdog_run <- function(pid, config, provider = ct_provider(config), interval = 5,
                            alive = ct_pid_alive, sleep = Sys.sleep, now = ct_now,
                            delete_attempts = 6L, log = ct_watchdog_log) {
  ids_path <- file.path(config$state_dir, "pool-ids.json")
  t0 <- as.numeric(now())
  limit <- 3600 * (config$max_hours + 1)
  seen <- FALSE
  log("watching process ", pid)
  repeat {
    if (!alive(pid)) {
      break
    }
    if (file.exists(ids_path)) {
      seen <- TRUE
    } else if (seen) {
      log("pool stopped normally; exiting")
      return(invisible("clean"))
    }
    if (as.numeric(now()) - t0 > limit) {
      log("time limit reached; exiting")
      return(invisible("expired"))
    }
    sleep(interval)
  }
  ids <- if (file.exists(ids_path)) unlist(jsonlite::read_json(ids_path), use.names = FALSE) else character()
  if (!length(ids)) {
    log("controller ended; no machines recorded")
    return(invisible("nothing"))
  }
  log("controller ended; deleting ", length(ids), " machine(s): ", paste(ids, collapse = ", "))
  remaining <- ids
  for (attempt in seq_len(delete_attempts)) {
    for (id in remaining) {
      tryCatch(provider$delete(id), error = function(e) log("delete ", id, " failed: ", conditionMessage(e)))
    }
    listed <- tryCatch(
      vapply(provider$list(config$name_prefix), function(p) p$id, ""),
      error = function(e) NA_character_
    )
    remaining <- if (anyNA(listed)) remaining else intersect(remaining, listed)
    if (!length(remaining) && !anyNA(listed)) {
      break
    }
    sleep(10)
  }
  if (length(remaining)) {
    log("could not confirm deletion of: ", paste(remaining, collapse = ", "), ". Run ct_cleanup().")
    return(invisible("failed"))
  }
  unlink(c(ids_path, ct_pool_path(config), file.path(config$state_dir, "pool-status.json")))
  log("all machines deleted")
  invisible("deleted")
}

ct_watchdog_main <- function() {
  pid <- as.integer(Sys.getenv("CT_WATCHDOG_PID"))
  config <- readRDS(Sys.getenv("CT_WATCHDOG_CONFIG"))
  writeLines(as.character(Sys.getpid()), file.path(config$state_dir, "watchdog.pid"))
  ct_watchdog_run(pid, config)
  invisible(NULL)
}

# The shell command that starts the watchdog on its own, so that it is not a
# child of the controller (a tool that kills the controller's process tree
# cannot reach it) and, where `setsid` (or perl) exists, in a session of its
# own with no terminal (Ctrl-C in the terminal does not reach it).
ct_watchdog_command <- function(rscript, log, setsid = Sys.which("setsid"), perl = Sys.which("perl")) {
  isolate <- if (nzchar(setsid)) {
    shQuote(setsid)
  } else if (nzchar(perl)) {
    paste(shQuote(perl), "-MPOSIX -e", shQuote("POSIX::setsid(); exec @ARGV"))
  } else {
    ""
  }
  run <- paste(c(isolate, shQuote(rscript), "--no-save --no-restore -e", shQuote("cloudtargets:::ct_watchdog_main()")), collapse = " ")
  sprintf("%s >> %s 2>&1 < /dev/null &", trimws(run), shQuote(log))
}

# Start the watchdog for the current session. Returns an object whose `kill()`
# stops it (the pool does that after a clean shutdown), or NULL with a warning.
ct_watchdog_start <- function(config, pid = Sys.getpid()) {
  state_dir <- normalizePath(config$state_dir, mustWork = FALSE)
  dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
  config$state_dir <- state_dir
  rds <- file.path(state_dir, "watchdog.rds")
  pidfile <- file.path(state_dir, "watchdog.pid")
  unlink(pidfile)
  saveRDS(config, rds)
  # processx marks the processes it starts so that a whole tree can be killed at once;
  # the watchdog must not carry the mark.
  env <- Sys.getenv()
  env <- env[!grepl("^PROCESSX_", names(env))]
  env <- c(stats::setNames(as.character(env), names(env)), CT_WATCHDOG_PID = as.character(pid), CT_WATCHDOG_CONFIG = rds)
  command <- ct_watchdog_command(file.path(R.home("bin"), "Rscript"), file.path(state_dir, "watchdog.log"))
  res <- tryCatch(
    processx::run("sh", c("-c", command), env = env, error_on_status = FALSE, timeout = 15),
    error = function(e) list(status = 1L, stderr = conditionMessage(e))
  )
  if (!identical(res$status, 0L)) {
    cli::cli_warn(c(
      "Could not start the watchdog that deletes the machines if this session is killed: {trimws(res$stderr)}",
      i = "Machines still delete themselves when idle and after {.field max_hours}; use {.fn ct_cleanup} after an interrupted run."
    ))
    return(NULL)
  }
  list(kill = function() {
    watchdog_pid <- suppressWarnings(as.integer(readLines(pidfile, n = 1L, warn = FALSE)))
    if (length(watchdog_pid) == 1L && !is.na(watchdog_pid)) tools::pskill(watchdog_pid)
    invisible(NULL)
  })
}
