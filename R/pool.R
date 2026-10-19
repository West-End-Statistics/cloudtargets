# A pool is a set of machines acting as one big worker host for a crew
# controller running in this R session. Machines are created, prepared and
# joined to this session by SSH reverse tunnels (so workers on them can reach
# the controller without any inbound port here) INCREMENTALLY: a machine takes
# workers as soon as it is ready, whatever the others are doing.
#
# Each machine moves through
#
#   waiting_address -> checking_ssh -> preparing -> tunnelling -> ready
#
# or ends up failed/lost, in which case it is deleted and replaced. Everything
# is advanced by ct_pool_tick(), which the crew launcher calls from its
# scheduling loop (every ~0.5 s) and which must never block: it only polls
# background jobs and makes a few short HTTP calls.
#
# `pool$machines` holds READY machines only (those that can take workers);
# machines still on their way are in `pool$pending`, keyed by id.

ct_pool_path <- function(config) file.path(config$state_dir, "pool.json")

ct_pool_new <- function(config, provider, hooks = ct_pool_hooks(), n_slots) {
  # Pool machines delete themselves when nothing has been connected to them for
  # a while (a single large machine must not: its run goes on while you are away).
  if (is.null(config$orphan_minutes) || is.na(config$orphan_minutes)) {
    config$orphan_minutes <- 30
  }
  pool <- new.env(parent = emptyenv())
  pool$config <- config
  pool$provider <- provider
  pool$hooks <- ct_pool_hooks_normalize(hooks)
  pool$n_slots <- as.integer(n_slots)
  pool$machines <- list()
  pool$pending <- list()
  pool$to_delete <- character()
  pool$gone <- character() # ids the provider has confirmed deleted
  pool$registered <- character()
  pool$blocked <- list() # candidate label -> time it may be tried again
  pool$backoff_n <- list() # candidate label -> consecutive capacity failures
  pool$cool <- list() # data center -> time its cool-down ends
  pool$attempts <- list()
  pool$prepare_failures <- 0L
  pool$ever_ready <- FALSE
  pool$t0 <- NULL
  pool$port <- NULL
  pool$candidates <- list()
  pool$min_vcpus <- 0
  pool$exhausted <- FALSE
  pool$stopped <- FALSE
  pool$in_tick <- FALSE
  pool$strict <- FALSE
  pool$http_budget <- 2
  pool$last_status_at <- -Inf
  pool$last_status_text <- ""
  pool
}

# Steps with side effects on real machines; tests substitute fakes. Each
# *_start returns a job (see R/jobs.R).
ct_pool_hooks <- function() {
  c(ct_hooks(), list(
    check_start = ct_ssh_check_start,
    prepare_start = ct_prepare_start,
    tunnel_start = ct_tunnel_start,
    watchdog = ct_watchdog_start
  ))
}

# Older hooks were synchronous functions (check, prepare, tunnel). Wrap them as
# already-finished jobs so both styles work.
ct_pool_hooks_normalize <- function(hooks) {
  if (is.null(hooks$check_start) && !is.null(hooks$check)) {
    hooks$check_start <- function(host, port, user, config) {
      ct_job_now(function() hooks$check(host, port, user, config), truthy = TRUE)
    }
  }
  if (is.null(hooks$prepare_start) && !is.null(hooks$prepare)) {
    hooks$prepare_start <- function(config, state) ct_job_now(function() hooks$prepare(config, state))
  }
  if (is.null(hooks$tunnel_start) && !is.null(hooks$tunnel)) {
    hooks$tunnel_start <- function(state, config, port) ct_job_now(function() hooks$tunnel(state, config, port))
  }
  hooks$watchdog <- hooks$watchdog %||% function(config) NULL
  hooks$now <- hooks$now %||% ct_now
  hooks$sleep <- hooks$sleep %||% Sys.sleep
  hooks
}

ct_pn <- function(pool) as.numeric(pool$hooks$now())

ct_pool_cfg <- function(pool, key, default) pool$config[[key]] %||% default

ct_pool_slots <- function(pool) {
  sum(vapply(pool$machines, function(m) as.integer(m$slots), integer(1)))
}

ct_pool_pending_vcpus <- function(pool) {
  sum(vapply(pool$pending, function(m) as.integer(m$vcpus), integer(1)))
}

ct_proc_alive <- function(proc) {
  if (is.null(proc) || !is.function(proc$is_alive)) TRUE else isTRUE(proc$is_alive())
}

# Worker slots that can take a new worker right now (ready machines only).
ct_pool_free_slots <- function(pool) {
  if (is.null(pool) || isTRUE(pool$stopped)) {
    return(0L)
  }
  free <- vapply(pool$machines, function(m) {
    busy <- sum(vapply(m$workers, function(w) ct_proc_alive(w), logical(1)))
    max(0L, as.integer(m$slots) - as.integer(busy))
  }, integer(1))
  as.integer(sum(free))
}

# How many of `n` requested workers can be started now.
ct_launch_cap <- function(pool, n) {
  max(0L, min(as.integer(n), ct_pool_free_slots(pool)))
}

ct_pool_save <- function(pool) {
  config <- pool$config
  dir.create(config$state_dir, showWarnings = FALSE, recursive = TRUE)
  rows <- lapply(pool$machines, function(m) m[c("id", "label", "host", "port", "user", "vcpus", "slots", "cost_per_hr", "started_at")])
  jsonlite::write_json(rows, ct_pool_path(config), auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(NULL)
}

ct_pool_cost_per_hr <- function(pool) {
  sum(vapply(pool$machines, function(m) as.numeric(m$cost_per_hr %||% NA_real_), numeric(1)), na.rm = TRUE)
}

#' Begin building a pool without waiting for it
#'
#' Creates the first machines and returns immediately. The pool then advances
#' each time [ct_pool_tick()] is called (the crew launcher does so from its
#' scheduling loop).
#'
#' @param pool A pool, from `ct_pool_new()`.
#' @param port Local TCP port of the controller's dispatcher.
#' @return The pool, invisibly.
#' @keywords internal
ct_pool_begin <- function(pool, port) {
  pool$port <- port
  if (isTRUE(pool$config$watchdog %||% TRUE)) {
    pool$watchdog <- pool$hooks$watchdog(pool$config)
  }
  pool$t0 <- ct_pn(pool)
  pool$candidates <- pool$provider$candidates(pool$config)
  sizes <- vapply(pool$candidates, function(c) as.numeric(c$spec$vcpus %||% 0L), numeric(1))
  pool$min_vcpus <- if (length(sizes)) min(sizes) else 0
  if (!pool$strict) {
    pool$http_budget <- 8 # the first tick may create several machines
    cli::cli_alert_info(
      "Building a pool of up to {pool$n_slots} worker slot{?s}; work starts as soon as the first machine is ready."
    )
  }
  ct_pool_tick(pool)
  if (!pool$strict) pool$http_budget <- ct_pool_cfg(pool, "pool_http_budget", 2)
  invisible(pool)
}

#' Start a pool and wait until it is complete
#'
#' Blocking wrapper around [ct_pool_begin()] and [ct_pool_tick()] for callers
#' that need the whole pool before doing anything. It fails fast (and deletes
#' everything created) if the candidate ladder runs out or a machine cannot be
#' prepared.
#'
#' @inheritParams ct_pool_begin
#' @return The pool, invisibly.
#' @keywords internal
ct_pool_start <- function(pool, port) {
  pool$strict <- TRUE
  pool$http_budget <- Inf
  ok <- FALSE
  on.exit(if (!ok) ct_pool_stop(pool, quiet = TRUE), add = TRUE)
  ct_pool_begin(pool, port)
  while (ct_pool_slots(pool) < pool$n_slots || length(pool$pending) > 0L) {
    pool$hooks$sleep(5)
    ct_pool_tick(pool)
  }
  ok <- TRUE
  cli::cli_alert_success(
    "Pool ready: {length(pool$machines)} machine{?s}, {ct_pool_slots(pool)} worker slot{?s}, ~{ct_format_usd(ct_pool_cost_per_hr(pool))}/hr"
  )
  invisible(pool)
}

#' Advance the pool one step
#'
#' Never blocks: polls background jobs, makes at most a few short HTTP calls,
#' and creates, replaces and deletes machines as needed. Errors from machines
#' become machine failures; only giving up (nothing ever became ready) and
#' configuration problems before the first machine is ready escape.
#'
#' @param pool A pool.
#' @return The pool, invisibly.
#' @keywords internal
ct_pool_tick <- function(pool) {
  if (isTRUE(pool$stopped) || isTRUE(pool$in_tick)) {
    return(invisible(pool))
  }
  pool$in_tick <- TRUE
  on.exit(pool$in_tick <- FALSE, add = TRUE)
  now <- ct_pn(pool)
  wall0 <- proc.time()[["elapsed"]]
  budget <- function() (proc.time()[["elapsed"]] - wall0) < pool$http_budget

  ct_pool_tick_ready(pool, now)
  ct_pool_tick_pending(pool, now, budget)
  ct_pool_tick_deletes(pool, budget)
  ct_pool_check_giveup(pool, now) # before creating: no point paying for a replacement we then delete
  ct_pool_tick_create(pool, now, budget)
  if (!pool$strict) ct_pool_status(pool, now)
  invisible(pool)
}

# ---- ready machines -------------------------------------------------------

# A tunnel that died takes its workers with it. Usually a network blip, so try
# reopening it a few times before giving the machine up.
ct_pool_tick_ready <- function(pool, now) {
  keep <- list()
  for (m in pool$machines) {
    if (ct_proc_alive(m$tunnel)) {
      keep[[length(keep) + 1L]] <- m
      next
    }
    for (w in m$workers) try(w$kill(), silent = TRUE)
    m$workers <- list()
    m$reconnects <- (m$reconnects %||% 0L) + 1L
    if (m$reconnects > ct_pool_cfg(pool, "pool_tunnel_reconnects_max", 3L)) {
      cli::cli_alert_warning("Lost the connection to {m$label} ({m$id}) for good; replacing it.")
      pool$to_delete <- c(pool$to_delete, m$id)
      pool$attempts <- ct_pool_note(pool, m$label, "lost connection")
      next
    }
    cli::cli_alert_warning("Lost the connection to {m$label} ({m$id}); reconnecting ({m$reconnects}).")
    state <- m[c("id", "host", "port", "user")]
    pend <- m
    pend$tunnel <- NULL
    pend$state <- "tunnelling"
    pend$state_since <- now
    pend$created <- pend$created %||% now
    pend$deadline <- now + ct_pool_cfg(pool, "prepare_timeout", 900)
    pend$tunnel_tries <- 0L
    pend$job <- pool$hooks$tunnel_start(state, pool$config, pool$port)
    pool$pending[[m$id]] <- pend
  }
  pool$machines <- keep
  invisible(NULL)
}

# ---- machines on their way ------------------------------------------------

ct_pool_tick_pending <- function(pool, now, budget) {
  if (length(pool$pending) == 0L) {
    return(invisible(NULL))
  }
  ids <- names(pool$pending)
  ids <- ids[order(vapply(pool$pending, function(m) m$created %||% 0, numeric(1)))]
  ctx <- new.env()
  ctx$describes <- 0L
  ctx$preparing <- sum(vapply(pool$pending, function(m) {
    identical(m$state, "preparing") && !isTRUE(m$queued)
  }, logical(1)))

  for (id in ids) {
    m <- pool$pending[[id]]
    if (is.null(m)) next
    m <- tryCatch(
      ct_pool_advance(pool, m, now, budget, ctx),
      ct_error_pool_fatal = function(e) stop(e),
      error = function(e) {
        kind <- switch(m$state, preparing = "prepare", tunnelling = "tunnel", "error")
        ct_pool_fail(pool, m, kind, conditionMessage(e), now)
      }
    )
    # A NULL result (promoted to ready, or failed) removes the machine from `pending`.
    pool$pending[[id]] <- m
  }
  ct_pool_save(pool)
  invisible(NULL)
}

ct_pool_set_state <- function(m, state, now) {
  m$state <- state
  m$state_since <- now
  m
}

ct_pool_advance <- function(pool, m, now, budget, ctx) {
  config <- pool$config
  hooks <- pool$hooks
  state_of <- function(m) m[c("id", "host", "port", "user")]

  if (identical(m$state, "waiting_address")) {
    if (now >= m$deadline) {
      return(ct_pool_fail(pool, m, "unreachable", "no network address was assigned in time", now))
    }
    if (now >= m$next_describe && ctx$describes < 2L && budget()) {
      ctx$describes <- ctx$describes + 1L
      m$next_describe <- now + ct_pool_cfg(pool, "pool_describe_interval", 10)
      info <- pool$provider$describe(m$id)
      if (!is.null(info$host)) {
        m$host <- info$host
        m$port <- info$port
        m$cost_per_hr <- info$cost_per_hr %||% m$cost_per_hr
        m <- ct_pool_set_state(m, "checking_ssh", now)
        m$next_check <- now
      }
    }
    return(m)
  }

  if (identical(m$state, "checking_ssh")) {
    if (now >= m$deadline) {
      return(ct_pool_fail(pool, m, "unreachable", "SSH never answered in time", now))
    }
    if (is.null(m$job)) {
      if (now >= m$next_check) m$job <- hooks$check_start(m$host, m$port, m$user, config)
      return(m)
    }
    status <- m$job$poll()
    if (identical(status, "ok")) {
      m$job <- NULL
      m <- ct_pool_set_state(m, "preparing", now)
      m$queued <- TRUE
      m$deadline <- now + ct_pool_cfg(pool, "prepare_timeout", 900)
    } else if (identical(status, "failed")) {
      m$job <- NULL
      m$next_check <- now + 5
    }
    return(m)
  }

  if (identical(m$state, "preparing")) {
    if (now >= m$deadline) {
      return(ct_pool_fail(pool, m, "prepare", "setting up the machine took too long", now))
    }
    if (isTRUE(m$queued)) {
      if (ctx$preparing < ct_pool_cfg(pool, "pool_prepare_parallel", 4L)) {
        m$job <- hooks$prepare_start(config, state_of(m))
        m$queued <- FALSE
        ctx$preparing <- ctx$preparing + 1L
      }
      return(m)
    }
    status <- m$job$poll()
    if (identical(status, "ok")) {
      m$job <- NULL
      m <- ct_pool_set_state(m, "tunnelling", now)
      m$tunnel_tries <- 0L
      m$job <- hooks$tunnel_start(state_of(m), config, pool$port)
    } else if (identical(status, "failed")) {
      return(ct_pool_fail(pool, m, "prepare", m$job$error, now))
    }
    return(m)
  }

  if (identical(m$state, "tunnelling")) {
    if (now >= m$deadline) {
      return(ct_pool_fail(pool, m, "tunnel", "the connection to the machine took too long", now))
    }
    status <- m$job$poll()
    if (identical(status, "ok")) {
      return(ct_pool_promote(pool, m, now))
    }
    if (identical(status, "failed")) {
      m$tunnel_tries <- (m$tunnel_tries %||% 0L) + 1L
      if (m$tunnel_tries > 2L) {
        return(ct_pool_fail(pool, m, "tunnel", m$job$error, now))
      }
      m$job <- hooks$tunnel_start(state_of(m), config, pool$port)
    }
    return(m)
  }
  m
}

# A machine is ready: it can take workers. Returns NULL (it leaves `pending`).
ct_pool_promote <- function(pool, m, now) {
  ready <- m[c("id", "label", "site", "vcpus", "user", "cost_per_hr", "started_at", "host", "port")]
  ready$slots <- m$vcpus
  ready$tunnel <- m$job$value
  ready$workers <- list()
  ready$reconnects <- m$reconnects %||% 0L
  pool$machines[[length(pool$machines) + 1L]] <- ready
  first <- !pool$ever_ready
  pool$ever_ready <- TRUE
  pool$last_ready_at <- now
  ct_pool_save(pool)
  if (first && !pool$strict) {
    cli::cli_alert_success("First machine ready: {m$label} ({m$id}). Calculations can start.")
  } else {
    cli::cli_alert_success("Ready: {m$label} ({m$id})")
  }
  NULL
}

# A machine did not work out: stop its job, queue it for deletion and let the
# next create pass replace it. Returns NULL (the machine leaves `pending`).
ct_pool_fail <- function(pool, m, kind, reason, now) {
  if (!is.null(m$job)) try(m$job$kill(), silent = TRUE)
  reason <- if (is.null(reason) || is.na(reason)) "no reason given" else reason
  if (isTRUE(pool$strict) && kind %in% c("prepare", "tunnel", "error")) {
    ct_pool_stop(pool, quiet = TRUE)
    cli::cli_abort("{reason}", class = "ct_error_pool_fatal", call = NULL)
  }
  pool$pending[[m$id]] <- NULL
  pool$to_delete <- c(pool$to_delete, m$id)
  if (kind %in% c("unreachable", "prepare", "tunnel") && !is.null(m$site)) {
    pool$cool[[m$site]] <- now + 60 * ct_pool_cfg(pool, "pool_site_cooldown_minutes", 60)
  }
  if (identical(kind, "prepare")) pool$prepare_failures <- pool$prepare_failures + 1L
  pool$attempts <- ct_pool_note(pool, m$label, paste0(kind, ": ", substr(gsub("\\s+", " ", reason), 1L, 200L)))
  if (!pool$strict) {
    age <- round((now - (m$created %||% now)) / 60, 1)
    cli::cli_alert_warning("{m$label} ({m$id}) failed after {age} min ({kind}); deleting it and replacing it.")
  }
  NULL
}

ct_pool_note <- function(pool, label, result) {
  out <- c(pool$attempts, list(list(label = label, result = result)))
  utils::tail(out, 100L)
}

# ---- deleting -------------------------------------------------------------

ct_pool_tick_deletes <- function(pool, budget) {
  done <- 0L
  for (id in pool$to_delete) {
    if (done >= 2L || !budget()) break
    ok <- tryCatch(
      {
        pool$provider$delete(id)
        TRUE
      },
      error = function(e) FALSE
    )
    done <- done + 1L
    if (ok) {
      pool$to_delete <- setdiff(pool$to_delete, id)
      pool$gone <- c(pool$gone, id)
    }
  }
  invisible(NULL)
}

# ---- creating -------------------------------------------------------------

ct_pool_block <- function(pool, cand, now, delay = NULL) {
  n <- (pool$backoff_n[[cand$label]] %||% 0L) + 1L
  pool$backoff_n[[cand$label]] <- n
  lo <- ct_pool_cfg(pool, "pool_backoff_min", 30)
  hi <- ct_pool_cfg(pool, "pool_backoff_max", 120)
  delay <- delay %||% if (pool$strict) Inf else min(hi, lo * 2^(n - 1L))
  pool$blocked[[cand$label]] <- now + delay
  invisible(NULL)
}

# First candidate in ladder order that is not backing off, not in a cooling
# data center, and not bigger than needed. If cool-downs alone leave nothing,
# release the oldest one so the pool cannot starve itself.
ct_pool_next_candidate <- function(pool, now, max_vcpus) {
  eligible <- function() {
    Filter(function(cand) {
      site <- cand$spec$data_center
      if ((cand$spec$vcpus %||% 0L) > max_vcpus) {
        return(FALSE)
      }
      until <- pool$blocked[[cand$label]]
      if (!is.null(until) && until > now) {
        return(FALSE)
      }
      cool <- if (is.null(site)) NULL else pool$cool[[site]]
      is.null(cool) || cool <= now
    }, pool$candidates)
  }
  pick <- function() {
    cands <- eligible()
    if (length(cands) == 0L) {
      return(NULL)
    }
    if (!isTRUE(pool$config$pool_spread %||% TRUE)) {
      return(cands[[1L]])
    }
    # Largest size first (ladder order), but among candidates of that size prefer
    # the data center with the fewest machines so far: one bad data center then
    # cannot take the whole pool with it.
    same <- Filter(function(cand) identical(cand$spec$vcpus, cands[[1L]]$spec$vcpus), cands)
    sites <- vapply(same, function(cand) cand$spec$data_center %||% "", "")
    used <- vapply(sites, function(site) {
      sum(vapply(c(pool$machines, unname(pool$pending)), function(m) identical(m$site, site), logical(1)))
    }, numeric(1))
    same[[which.min(used)]]
  }
  cand <- pick()
  if (is.null(cand) && length(pool$cool)) {
    active <- pool$cool[vapply(pool$cool, function(x) x > now, logical(1))]
    if (length(active)) {
      pool$cool[[names(active)[which.min(unlist(active))]]] <- NULL
      cand <- pick()
    }
  }
  cand
}

ct_pool_tick_create <- function(pool, now, budget) {
  pool$exhausted <- FALSE
  uncovered <- function() pool$n_slots - ct_pool_slots(pool) - ct_pool_pending_vcpus(pool)
  while (uncovered() > 0L && budget()) {
    cand <- ct_pool_next_candidate(pool, now, max_vcpus = max(uncovered(), pool$min_vcpus))
    if (is.null(cand)) {
      pool$exhausted <- TRUE
      if (pool$strict) {
        ct_pool_stop(pool, quiet = TRUE)
        ct_abort_exhausted(pool$attempts)
      }
      return(invisible(NULL))
    }
    pod <- tryCatch(
      pool$provider$create(cand, pool$config, ct_pod_name(pool$config)),
      ct_error_capacity = function(e) e,
      error = function(e) e
    )
    if (inherits(pod, "ct_error_capacity")) {
      ct_pool_block(pool, cand, now)
      pool$attempts <- ct_pool_note(pool, cand$label, "no capacity")
      next
    }
    if (inherits(pod, "error")) {
      # A problem with the request itself (bad data center, rejected key, ...).
      # Before anything has worked that is a setup error worth stopping for;
      # afterwards it only costs this candidate a long back-off.
      if (!pool$ever_ready && length(pool$pending) == 0L) {
        stop(pod)
      }
      cli::cli_alert_warning("Could not create {cand$label}: {conditionMessage(pod)}")
      ct_pool_block(pool, cand, now, delay = 300)
      pool$attempts <- ct_pool_note(pool, cand$label, paste("error:", conditionMessage(pod)))
      next
    }
    pool$backoff_n[[cand$label]] <- NULL
    pool$blocked[[cand$label]] <- NULL
    site <- cand$spec$data_center
    pod$site <- site
    pod$vcpus <- as.integer(pod$vcpus %||% cand$spec$vcpus)
    pod$user <- pool$config$ssh_user
    pod$created <- now
    pod$deadline <- now + pool$config$ready_timeout
    pod$started_at <- format(pool$hooks$now(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    pod$state <- "waiting_address"
    pod$state_since <- now
    pod$next_describe <- now
    pod$reconnects <- 0L
    ct_pool_register(pool, pod)
    pool$pending[[pod$id]] <- pod
    cli::cli_alert_info("Created {pod$label}")
  }
  invisible(NULL)
}

# Record a machine as soon as it exists so a crash can still find it.
ct_pool_register <- function(pool, pod) {
  pool$registered <- c(pool$registered, pod$id)
  config <- pool$config
  dir.create(config$state_dir, showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(as.list(pool$registered), file.path(config$state_dir, "pool-ids.json"), auto_unbox = TRUE)
  invisible(NULL)
}

# ---- giving up ------------------------------------------------------------

ct_pool_check_giveup <- function(pool, now) {
  if (pool$ever_ready) {
    return(invisible(NULL))
  }
  limit <- 60 * ct_pool_cfg(pool, "pool_give_up_minutes", 60)
  too_long <- (now - pool$t0) >= limit
  too_broken <- pool$prepare_failures >= ct_pool_cfg(pool, "pool_prepare_failures_max", 3L)
  if (!too_long && !too_broken) {
    return(invisible(NULL))
  }
  recent <- utils::tail(pool$attempts, 8L)
  lines <- vapply(recent, function(a) paste0(a$label, ": ", a$result), character(1))
  logs <- file.path(pool$config$state_dir, "logs")
  headline <- if (too_broken && !too_long) {
    sprintf("Setting up machines failed %d times and none ever became ready.", pool$prepare_failures)
  } else {
    sprintf("No machine became ready within %s minutes.", format(limit / 60))
  }
  ct_pool_stop(pool, quiet = TRUE)
  cli::cli_abort(c(
    headline,
    stats::setNames(lines, rep("*", length(lines))),
    i = "Logs of the setup steps are in {.path {logs}}.",
    i = "Nothing is left running."
  ), class = "ct_error_pool_giveup", call = NULL)
}

# ---- status ---------------------------------------------------------------

ct_pool_status_text <- function(pool) {
  ready <- length(pool$machines)
  starting <- length(pool$pending)
  uncovered <- max(0L, pool$n_slots - ct_pool_slots(pool) - ct_pool_pending_vcpus(pool))
  biggest <- max(1, vapply(pool$candidates, function(c) as.numeric(c$spec$vcpus %||% 1L), numeric(1)))
  waiting <- if (uncovered > 0L) as.integer(ceiling(uncovered / biggest)) else 0L
  sprintf(
    "%d/%d machines ready (%d/%d slots), %d starting, %d waiting for capacity",
    ready, ready + starting + waiting, ct_pool_slots(pool), pool$n_slots, starting, waiting
  )
}

# At most one line per `pool_status_interval`, and only when something changed
# (or every 5 minutes). Also records each machine's state in pool-status.json.
ct_pool_status <- function(pool, now) {
  interval <- ct_pool_cfg(pool, "pool_status_interval", 60)
  text <- ct_pool_status_text(pool)
  since <- now - pool$last_status_at
  if (since < interval || (identical(text, pool$last_status_text) && since < 300)) {
    return(invisible(NULL))
  }
  pool$last_status_at <- now
  pool$last_status_text <- text
  cli::cli_alert_info("{text}")
  ct_pool_write_status(pool, now)
  invisible(NULL)
}

ct_pool_write_status <- function(pool, now) {
  config <- pool$config
  dir.create(config$state_dir, showWarnings = FALSE, recursive = TRUE)
  ready <- lapply(pool$machines, function(m) list(id = m$id, label = m$label, state = "ready", age_s = NA))
  pending <- lapply(pool$pending, function(m) {
    list(id = m$id, label = m$label, state = m$state, age_s = round(now - (m$created %||% now)))
  })
  rows <- c(ready, unname(pending))
  try(jsonlite::write_json(rows, file.path(config$state_dir, "pool-status.json"), auto_unbox = TRUE, pretty = TRUE, na = "null"), silent = TRUE)
  invisible(NULL)
}

#' Delete every machine in a pool
#'
#' Kills the background jobs, tunnels and worker processes, deletes all machines
#' created for the pool (including ones still starting or waiting to be
#' deleted), and removes the pool records. Idempotent.
#'
#' @param pool A pool.
#' @param quiet Suppress the summary message.
#' @return Invisibly, the deleted ids.
#' @keywords internal
ct_pool_stop <- function(pool, quiet = FALSE) {
  if (isTRUE(pool$stopped)) {
    return(invisible(character()))
  }
  for (m in pool$pending) {
    if (!is.null(m$job)) try(m$job$kill(), silent = TRUE)
  }
  for (m in pool$machines) {
    for (w in m$workers) try(w$kill(), silent = TRUE)
    if (!is.null(m$tunnel)) try(m$tunnel$kill(), silent = TRUE)
  }
  ids <- unique(c(
    vapply(pool$machines, function(m) m$id, ""),
    names(pool$pending), pool$registered, pool$to_delete
  ))
  ids <- setdiff(ids, pool$gone)
  for (id in ids) try(pool$provider$delete(id), silent = TRUE)
  cost <- sum(vapply(c(pool$machines, unname(pool$pending)), function(m) {
    hrs <- tryCatch(ct_runtime_hours(m, now = pool$hooks$now()), error = function(e) NA_real_)
    as.numeric(m$cost_per_hr %||% NA_real_) * hrs
  }, numeric(1)), na.rm = TRUE)
  remaining <- tryCatch(
    vapply(pool$provider$list(pool$config$name_prefix), function(p) p$id, ""),
    error = function(e) NA_character_
  )
  if (any(ids %in% remaining)) {
    cli::cli_alert_danger(
      "Some pool machines are still listed by the provider: {intersect(ids, remaining)}. Run {.fn ct_cleanup}."
    )
  } else {
    state_dir <- pool$config$state_dir
    unlink(c(
      ct_pool_path(pool$config), file.path(state_dir, "pool-ids.json"),
      file.path(state_dir, "pool-status.json")
    ))
    pool$machines <- list()
    pool$pending <- list()
    pool$to_delete <- character()
    pool$stopped <- TRUE
    if (!is.null(pool$watchdog)) try(pool$watchdog$kill(), silent = TRUE)
    if (!quiet && length(ids)) {
      cli::cli_alert_success("Deleted {length(ids)} pool machine{?s}; about {ct_format_usd(cost)} used. Nothing is running.")
    }
  }
  invisible(ids)
}

# ---- tunnel and workers ---------------------------------------------------

# Reverse tunnel: connections to 127.0.0.1:<port> on the machine reach
# 127.0.0.1:<port> here. The process is a ControlMaster so workers can share
# the connection (see ct_worker_args()).
ct_tunnel_open <- function(state, config, port) {
  control <- ct_control_path(state)
  unlink(control)
  proc <- processx::process$new("ssh", ct_tunnel_args(state, config, port), stderr = "|")
  for (i in seq_len(60)) {
    if (!proc$is_alive()) {
      cli::cli_abort("SSH tunnel to {state$host} exited: {trimws(proc$read_error())}")
    }
    if (file.exists(control)) break
    Sys.sleep(0.5)
  }
  if (!file.exists(control)) {
    proc$kill()
    cli::cli_abort("SSH tunnel to {state$host} did not come up.")
  }
  proc
}

ct_tunnel_args <- function(state, config, port) {
  c(
    ct_ssh_options(config),
    "-o", "ExitOnForwardFailure=yes", "-o", "ServerAliveInterval=15", "-o", "ServerAliveCountMax=4",
    "-N", "-M", "-S", ct_control_path(state),
    "-R", sprintf("%d:127.0.0.1:%d", port, port),
    "-p", as.character(state$port), paste0(state$user, "@", state$host)
  )
}

# Non-blocking twin of ct_tunnel_open(): a job whose value is the ssh process.
# It is "ok" once the connection is up (control socket present and steady for
# `settle` seconds, so the port forward has been established) and the process
# is still alive; the caller must keep the process.
ct_tunnel_start <- function(state, config, port, now = ct_now, settle = 3) {
  control <- ct_control_path(state)
  unlink(control)
  log <- ct_log_path(config, state, "tunnel")
  proc <- processx::process$new("ssh", ct_tunnel_args(state, config, port), stdout = NULL, stderr = log)
  seen <- NULL
  ok_when <- function() {
    if (!file.exists(control)) {
      seen <<- NULL
      return(FALSE)
    }
    if (is.null(seen)) seen <<- now()
    as.numeric(difftime(now(), seen, units = "secs")) >= settle
  }
  ct_job_process(proc, ok_when = ok_when, timeout = 60, now = now, log = log)
}

ct_control_path <- function(state) file.path("/tmp", paste0("ct-", substr(state$id, 1L, 12L), ".sock"))

# The command that starts a crew worker on a pool machine: reuse the tunnel's
# ssh connection, enter the project directory and run the worker call in the
# foreground. -tt makes the remote process die with the local ssh client.
ct_worker_args <- function(state, config, call) {
  remote <- ct_remote_dir(config)
  script <- paste(
    "[ -f /etc/profile.d/zz-container-env.sh ] && . /etc/profile.d/zz-container-env.sh",
    paste("cd", ct_shell_quote(remote)),
    paste("exec Rscript -e", ct_shell_quote(call)),
    sep = "\n"
  )
  c(
    ct_ssh_options(config), "-tt", "-S", ct_control_path(state),
    "-p", as.character(state$port), paste0(state$user, "@", state$host),
    sprintf("bash -c \"$(echo %s | base64 -d)\"", ct_b64(script))
  )
}

# Pick the ready machine with the most free slots and start one worker on it.
ct_pool_launch <- function(pool, call, spawn = function(args) processx::process$new("ssh", args, stdout = "|", stderr = "|")) {
  load <- vapply(pool$machines, function(m) {
    sum(vapply(m$workers, function(w) ct_proc_alive(w), logical(1)))
  }, numeric(1))
  free <- vapply(pool$machines, function(m) as.numeric(m$slots), numeric(1)) - load
  i <- which.max(free)
  if (length(i) == 0L || free[[i]] <= 0) {
    cli::cli_abort("No free worker slots in the pool ({ct_pool_slots(pool)} slots, all busy).")
  }
  m <- pool$machines[[i]]
  handle <- spawn(ct_worker_args(m[c("id", "host", "port", "user")], pool$config, call))
  pool$machines[[i]]$workers <- c(m$workers, list(handle))
  handle
}
