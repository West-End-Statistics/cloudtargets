# A job is a unit of background work that is polled, never waited on, so the
# pool can advance many machines from inside crew's scheduling loop without
# blocking it. A job has:
#
#   $poll()   advance and return "running", "ok" or "failed" (never blocks)
#   $status   the last status returned by poll()
#   $value    the result once "ok" (e.g. the tunnel process)
#   $error    a one-line reason once "failed"
#   $kill()   stop any process behind the job (idempotent)
#
# All timing goes through a `now` function so tests can use a fake clock.

ct_job_new <- function(step, kill = function() invisible(NULL)) {
  job <- new.env(parent = emptyenv())
  job$status <- "running"
  job$value <- NULL
  job$error <- NA_character_
  job$killed <- FALSE
  job$poll <- function() {
    if (identical(job$status, "running")) {
      res <- tryCatch(step(), error = function(e) list(status = "failed", error = conditionMessage(e)))
      if (!identical(res$status, "running")) {
        job$status <- res$status
        job$value <- res$value
        job$error <- res$error %||% NA_character_
      }
    }
    job$status
  }
  job$kill <- function() {
    if (!job$killed) {
      job$killed <- TRUE
      try(kill(), silent = TRUE)
      if (identical(job$status, "running")) {
        job$status <- "failed"
        job$error <- "killed"
      }
    }
    invisible(NULL)
  }
  job
}

# Run `fn` now and return a job that is already finished. Wraps the older
# synchronous hooks. With `truthy`, a result other than TRUE counts as failure.
ct_job_now <- function(fn, truthy = FALSE) {
  res <- tryCatch(list(value = fn()), error = function(e) list(error = conditionMessage(e)))
  status <- if (!is.null(res$error)) {
    "failed"
  } else if (truthy && !isTRUE(res$value)) {
    "failed"
  } else {
    "ok"
  }
  error <- if (!is.null(res$error)) res$error else if (identical(status, "failed")) "check failed" else NA_character_
  job <- ct_job_new(step = function() list(status = status, value = res$value, error = error))
  job$poll() # already finished: expose status, value and error immediately
  job
}

# The last `n` lines of a log file, for error messages.
ct_log_tail <- function(path, n = 20L) {
  if (is.null(path) || !file.exists(path)) {
    return("")
  }
  lines <- readLines(path, warn = FALSE)
  paste(utils::tail(lines, n), collapse = "\n")
}

# A job backed by a processx process.
#
# * Finishes "ok" when the process exits with status 0, or earlier if `ok_when()`
#   is TRUE while the process is alive (the process then stays running and is
#   returned as the job's value, e.g. an SSH tunnel).
# * Fails when the process exits non-zero (error = tail of `log`), or runs past
#   `timeout` seconds (it is killed).
ct_job_process <- function(proc, ok_when = NULL, timeout = Inf, now = ct_now, log = NULL) {
  t0 <- now()
  fail <- function(reason) {
    tail <- ct_log_tail(log)
    list(status = "failed", error = if (nzchar(tail)) paste0(reason, ": ", tail) else reason)
  }
  ct_job_new(
    step = function() {
      alive <- proc$is_alive()
      if (alive && !is.null(ok_when) && isTRUE(ok_when())) {
        return(list(status = "ok", value = proc))
      }
      if (alive) {
        if (as.numeric(difftime(now(), t0, units = "secs")) >= timeout) {
          try(proc$kill(), silent = TRUE)
          return(fail(sprintf("timed out after %s s", format(timeout))))
        }
        return(list(status = "running"))
      }
      code <- proc$get_exit_status()
      if (is.null(ok_when) && identical(code, 0L)) {
        list(status = "ok", value = proc)
      } else {
        fail(sprintf("exited with status %s", code))
      }
    },
    kill = function() proc$kill()
  )
}

# Run jobs one after another. `starters` is a list of functions, each returning
# a job (started only when the previous one succeeded). Fails on the first
# failure; the value is that of the last job.
ct_job_chain <- function(starters) {
  current <- NULL
  idx <- 0L
  ct_job_new(
    step = function() {
      repeat {
        if (is.null(current)) {
          idx <<- idx + 1L
          if (idx > length(starters)) {
            return(list(status = "ok"))
          }
          current <<- starters[[idx]]()
        }
        status <- current$poll()
        if (identical(status, "running")) {
          return(list(status = "running"))
        }
        if (identical(status, "failed")) {
          return(list(status = "failed", error = current$error))
        }
        current <<- NULL
      }
    },
    kill = function() {
      if (!is.null(current)) current$kill()
    }
  )
}
