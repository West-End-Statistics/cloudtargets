# A scriptable in-memory provider. `plan` maps candidate label -> outcome:
#   "capacity"    create signals no capacity
#   "unreachable" create works but the machine never answers SSH
#   "ok"          create works and the machine answers SSH
fake_provider <- function(plan, describe_error_for = NULL) {
  env <- new.env()
  env$pods <- list()
  env$deleted <- character()
  env$created <- character()
  cands <- lapply(names(plan), function(lbl) {
    v <- suppressWarnings(as.integer(sub(".* x ([0-9]+) vCPU.*", "\\1", lbl)))
    list(label = lbl, spec = list(data_center = sub(".* in ", "", lbl), vcpus = if (is.na(v)) NULL else v))
  })
  provider <- new_ct_provider(
    name = "fake",
    preflight = function(config) list(list(ok = TRUE, check = "fake", detail = "ok")),
    candidates = function(config) cands,
    create = function(candidate, config, name) {
      outcome <- plan[[candidate$label]]
      if (identical(outcome, "capacity")) ct_abort_capacity(paste("no capacity for", candidate$label))
      id <- paste0("pod", length(env$created) + 1L)
      env$created <- c(env$created, id)
      env$pods[[id]] <- list(id = id, name = name, outcome = outcome)
      list(id = id, cost_per_hr = 0.12, label = candidate$label, vcpus = candidate$spec$vcpus)
    },
    describe = function(id) {
      if (identical(id, describe_error_for)) stop("api exploded")
      list(status = "RUNNING", host = paste0(id, ".example"), port = 22L, cost_per_hr = 0.12)
    },
    delete = function(id) {
      env$deleted <- c(env$deleted, id)
      env$pods[[id]] <- NULL
      invisible(TRUE)
    },
    list = function(prefix) {
      lapply(unname(env$pods), function(p) list(id = p$id, name = p$name, cost_per_hr = 0.12))
    }
  )
  list(provider = provider, env = env)
}

# Fake clock: sleeping advances time, so timeouts are instant.
fake_hooks <- function(env) {
  clock <- new.env()
  clock$t <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  list(
    check = function(host, port, user, config) {
      id <- sub("\\.example$", "", host)
      identical(env$pods[[id]]$outcome, "ok")
    },
    sleep = function(s) clock$t <- clock$t + s,
    now = function() clock$t
  )
}

test_config <- function(dir = withr::local_tempdir(.local_envir = parent.frame())) {
  config <- ct_default_config()
  config$state_dir <- file.path(dir, ".cloudtargets")
  config$ready_timeout <- 30
  config
}

# Hooks for pool tests: fake clock/check plus recorded prepare and tunnel steps.
fake_pool_hooks <- function(env) {
  hooks <- fake_hooks(env)
  env$prepared <- character()
  env$tunnels <- list()
  hooks$prepare <- function(config, state) env$prepared <- c(env$prepared, state$id)
  hooks$tunnel <- function(state, config, port) {
    t <- new.env()
    t$killed <- FALSE
    t$kill <- function() t$killed <- TRUE
    env$tunnels[[state$id]] <- t
    t
  }
  hooks
}

# ---- helpers for the incremental pool: a fake clock and fake background jobs ----

fake_clock <- function(start = "2026-01-01 00:00:00") {
  clock <- new.env()
  clock$t <- as.numeric(as.POSIXct(start, tz = "UTC"))
  clock$now <- function() as.POSIXct(clock$t, origin = "1970-01-01", tz = "UTC")
  clock$advance <- function(s) clock$t <- clock$t + s
  clock
}

# A job that is "running" for `after` fake seconds, then "ok" or "failed".
fake_job <- function(clock, after = 0, outcome = "ok", value = NULL, on_kill = NULL) {
  t_start <- clock$t
  ct_job_new(
    step = function() {
      if (clock$t - t_start < after) {
        return(list(status = "running"))
      }
      if (identical(outcome, "ok")) {
        list(status = "ok", value = value)
      } else {
        list(status = "failed", error = paste("fake", outcome))
      }
    },
    kill = function() if (!is.null(on_kill)) on_kill()
  )
}

# A provider driven by a fake clock. `plan` maps a candidate label to what happens
# to machines created from it (one entry per create attempt, the last one repeating):
#   "ok"                 reachable and fine
#   "unreachable"        gets an address, SSH never answers
#   "noaddr"             never gets an address
#   "addr_after:<s>"     address (and then SSH) only <s> seconds after creation
#   "capacity"           no capacity
#   "capacity_until:<s>" no capacity until <s> seconds after the clock's start
#   "error"              the create request itself fails (HTTP-400-like)
fake_provider2 <- function(plan, clock) {
  env <- new.env()
  env$pods <- list()
  env$created <- character()
  env$deleted <- character()
  env$describes <- 0L
  env$attempts <- list()
  env$create_log <- list()
  env$delete_fails <- 0L
  t0 <- clock$t
  cands <- lapply(names(plan), function(lbl) {
    v <- as.integer(sub(".* x ([0-9]+) vCPU.*", "\\1", lbl))
    list(label = lbl, spec = list(data_center = sub(".* in ", "", lbl), vcpus = v))
  })
  provider <- new_ct_provider(
    name = "fake2",
    preflight = function(config) list(list(ok = TRUE, check = "fake", detail = "ok")),
    candidates = function(config) cands,
    create = function(candidate, config, name) {
      lbl <- candidate$label
      env$attempts[[lbl]] <- (env$attempts[[lbl]] %||% 0L) + 1L
      env$create_log[[length(env$create_log) + 1L]] <- list(label = lbl, t = clock$t - t0)
      outcomes <- plan[[lbl]]
      outcome <- outcomes[[min(env$attempts[[lbl]], length(outcomes))]]
      if (identical(outcome, "error")) stop("create failed: boom")
      if (identical(outcome, "capacity")) ct_abort_capacity(paste("no capacity for", lbl))
      if (startsWith(outcome, "capacity_until:")) {
        if (clock$t - t0 < as.numeric(sub(".*:", "", outcome))) ct_abort_capacity(paste("no capacity for", lbl))
        outcome <- "ok"
      }
      id <- paste0("pod", length(env$created) + 1L)
      env$created <- c(env$created, id)
      env$pods[[id]] <- list(id = id, name = name, label = lbl, outcome = outcome, created = clock$t)
      list(id = id, cost_per_hr = 0.12, label = lbl, vcpus = candidate$spec$vcpus)
    },
    describe = function(id) {
      env$describes <- env$describes + 1L
      p <- env$pods[[id]]
      if (is.null(p)) stop("no such pod")
      late <- startsWith(p$outcome, "addr_after:") && clock$t - p$created < as.numeric(sub(".*:", "", p$outcome))
      addr <- !identical(p$outcome, "noaddr") && !late
      list(status = "RUNNING", host = if (addr) paste0(id, ".example") else NULL, port = if (addr) 22L else NULL, cost_per_hr = 0.12)
    },
    delete = function(id) {
      if (env$delete_fails > 0L) {
        env$delete_fails <- env$delete_fails - 1L
        stop("delete failed")
      }
      env$deleted <- c(env$deleted, id)
      env$pods[[id]] <- NULL
      invisible(TRUE)
    },
    list = function(prefix) lapply(unname(env$pods), function(p) list(id = p$id, name = p$name, cost_per_hr = 0.12))
  )
  list(provider = provider, env = env, clock = clock)
}

# Async hooks on the fake clock. `sleep` fails: a tick must never block.
# Tunables on `fp$env`: prepare_secs, prepare_outcome, tunnel_secs, tunnel_alive[[id]].
fake_async_hooks <- function(fp) {
  env <- fp$env
  clock <- fp$clock
  env$prepare_secs <- 5
  env$prepare_outcome <- "ok"
  env$tunnel_secs <- 1
  env$tunnel_alive <- list()
  env$kills <- 0L
  env$prepare_jobs <- list()
  env$tunnels <- list()
  list(
    now = clock$now,
    sleep = function(s) stop("a pool tick must never sleep"),
    check_start = function(host, port, user, config) {
      id <- sub("\\.example$", "", host)
      ok <- identical(env$pods[[id]]$outcome %||% "", "ok") || startsWith(env$pods[[id]]$outcome %||% "", "addr_after:")
      fake_job(clock, after = 1, outcome = if (ok) "ok" else "unreachable", on_kill = function() env$kills <- env$kills + 1L)
    },
    prepare_start = function(config, state) {
      job <- fake_job(clock, after = env$prepare_secs, outcome = env$prepare_outcome, on_kill = function() env$kills <- env$kills + 1L)
      env$prepare_jobs[[length(env$prepare_jobs) + 1L]] <- job
      job
    },
    tunnel_start = function(state, config, port) {
      id <- state$id
      env$tunnel_alive[[id]] <- TRUE
      proc <- list(
        is_alive = function() isTRUE(env$tunnel_alive[[id]]),
        kill = function() env$tunnel_alive[[id]] <- FALSE
      )
      env$tunnels[[id]] <- proc
      fake_job(clock, after = env$tunnel_secs, value = proc, on_kill = function() env$kills <- env$kills + 1L)
    }
  )
}

# Advance the fake clock `secs` seconds in `step`-second ticks.
drive <- function(pool, clock, secs, step = 1) {
  for (i in seq_len(round(secs / step))) {
    clock$advance(step)
    ct_pool_tick(pool)
  }
  invisible(pool)
}

inc_pool <- function(plan, n_slots, config, tweak = NULL) {
  clock <- fake_clock()
  fp <- fake_provider2(plan, clock)
  hooks <- fake_async_hooks(fp)
  if (!is.null(tweak)) tweak(fp$env)
  pool <- ct_pool_new(config, fp$provider, hooks, n_slots)
  suppressMessages(ct_pool_begin(pool, 5555L))
  list(pool = pool, fp = fp, clock = clock, env = fp$env)
}
