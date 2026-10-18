# A pool is a set of machines acting as one big worker host for a crew
# controller running in this R session. Machines are created together (so
# their start-up overlaps), prepared, and joined to this session by SSH reverse
# tunnels so workers on them can reach the controller without any inbound port
# on the local machine.

ct_pool_path <- function(config) file.path(config$state_dir, "pool.json")

ct_pool_new <- function(config, provider, hooks = ct_pool_hooks(), n_slots) {
  pool <- new.env(parent = emptyenv())
  pool$config <- config
  pool$provider <- provider
  pool$hooks <- hooks
  pool$n_slots <- as.integer(n_slots)
  pool$machines <- list()
  pool$stopped <- FALSE
  pool
}

# Steps with side effects on real machines; tests substitute fakes.
ct_pool_hooks <- function() {
  c(ct_hooks(), list(
    prepare = function(config, state) {
      ct_bootstrap(config, state = state)
      if (!identical(config$sync, "none")) ct_sync(config, state = state)
    },
    tunnel = ct_tunnel_open
  ))
}

ct_pool_slots <- function(pool) {
  sum(vapply(pool$machines, function(m) as.integer(m$slots), integer(1)))
}

ct_pool_save <- function(pool) {
  config <- pool$config
  dir.create(config$state_dir, showWarnings = FALSE, recursive = TRUE)
  rows <- lapply(pool$machines, function(m) m[c("id", "label", "host", "port", "user", "vcpus", "slots", "cost_per_hr", "started_at")])
  jsonlite::write_json(rows, ct_pool_path(config), auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(NULL)
}

#' Start a pool of machines
#'
#' Creates machines until their combined worker slots cover `n_slots`, using
#' the provider's candidate ladder for each (the last machine is sized to what
#' is still needed). Machines start in parallel; any that never become
#' reachable are deleted and replaced. Each ready machine is prepared
#' (packages, code) and connected to this session by a reverse tunnel to
#' `port`. If the pool cannot be completed everything created is deleted.
#'
#' @param pool A pool from [ct_pool_new()].
#' @param port Local TCP port of the controller's dispatcher.
#' @return The pool, invisibly.
#' @keywords internal
ct_pool_start <- function(pool, port) {
  config <- pool$config
  provider <- pool$provider
  hooks <- pool$hooks
  candidates <- provider$candidates(config)
  min_vcpus <- min(vapply(candidates, function(c) c$spec$vcpus %||% 0L, numeric(1)))
  bad_sites <- character()
  pending <- list()
  attempts <- list()
  ok <- FALSE
  on.exit(if (!ok) ct_pool_stop(pool, quiet = TRUE), add = TRUE)

  slots_planned <- function() {
    ct_pool_slots(pool) + sum(vapply(pending, function(p) as.integer(p$vcpus), integer(1)))
  }

  repeat {
    # 1. Create machines for whatever is still uncovered.
    while (slots_planned() < pool$n_slots) {
      need <- pool$n_slots - slots_planned()
      created <- ct_pool_create_one(pool, candidates, bad_sites, max_vcpus = max(need, min_vcpus), attempts)
      attempts <- created$attempts
      if (is.null(created$pod)) {
        ct_abort_exhausted(attempts)
      }
      pending[[length(pending) + 1L]] <- created$pod
      ct_pool_register(pool, created$pod)
    }
    if (length(pending) == 0L) break

    # 2. Check every pending machine once; promote the ready, replace the dead.
    still <- list()
    for (p in pending) {
      info <- provider$describe(p$id)
      if (!is.null(info$host) && isTRUE(hooks$check(info$host, info$port, config$ssh_user, config))) {
        m <- p
        m$host <- info$host
        m$port <- info$port
        m$slots <- m$vcpus
        m$cost_per_hr <- info$cost_per_hr %||% m$cost_per_hr
        pool$machines[[length(pool$machines) + 1L]] <- m
        cli::cli_alert_success("Ready: {m$label} ({m$id})")
      } else if (hooks$now() >= p$deadline) {
        cli::cli_alert_warning("{p$label} never became reachable; deleting it and replacing it.")
        provider$delete(p$id)
        if (!is.null(p$site)) bad_sites <- c(bad_sites, p$site)
        attempts[[length(attempts) + 1L]] <- list(label = p$label, result = "unreachable")
      } else {
        still[[length(still) + 1L]] <- p
      }
    }
    pending <- still
    ct_pool_save(pool)
    if (length(pending) > 0L) hooks$sleep(5)
  }

  # 3. Prepare each machine and tie it to this session.
  for (i in seq_along(pool$machines)) {
    m <- pool$machines[[i]]
    state <- m[c("id", "host", "port", "user")]
    hooks$prepare(config, state)
    pool$machines[[i]]$tunnel <- hooks$tunnel(state, config, port)
  }
  ct_pool_save(pool)
  ok <- TRUE
  cli::cli_alert_success(
    "Pool ready: {length(pool$machines)} machine{?s}, {ct_pool_slots(pool)} worker slot{?s}, ~{ct_format_usd(ct_pool_cost_per_hr(pool))}/hr"
  )
  invisible(pool)
}

ct_pool_cost_per_hr <- function(pool) {
  sum(vapply(pool$machines, function(m) as.numeric(m$cost_per_hr %||% NA_real_), numeric(1)), na.rm = TRUE)
}

# Create one machine from the ladder, skipping known-bad sites and sizes bigger
# than needed. Returns list(pod = <pending machine or NULL>, attempts = ...).
ct_pool_create_one <- function(pool, candidates, bad_sites, max_vcpus, attempts) {
  config <- pool$config
  provider <- pool$provider
  hooks <- pool$hooks
  for (cand in candidates) {
    site <- cand$spec$data_center
    if (!is.null(site) && site %in% bad_sites) next
    if ((cand$spec$vcpus %||% 0L) > max_vcpus) next
    pod <- tryCatch(
      provider$create(cand, config, ct_pod_name(config)),
      ct_error_capacity = function(e) e
    )
    if (inherits(pod, "ct_error_capacity")) {
      attempts[[length(attempts) + 1L]] <- list(label = cand$label, result = "no capacity")
      next
    }
    pod$site <- site
    pod$vcpus <- pod$vcpus %||% cand$spec$vcpus
    pod$user <- config$ssh_user
    pod$deadline <- hooks$now() + config$ready_timeout
    pod$started_at <- format(hooks$now(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    cli::cli_alert_info("Created {pod$label}")
    return(list(pod = pod, attempts = attempts))
  }
  list(pod = NULL, attempts = attempts)
}

# Record a machine as soon as it exists so a crash can still find it.
ct_pool_register <- function(pool, pod) {
  pool$registered <- c(pool$registered, pod$id)
  config <- pool$config
  dir.create(config$state_dir, showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(as.list(pool$registered), file.path(config$state_dir, "pool-ids.json"), auto_unbox = TRUE)
  invisible(NULL)
}

#' Delete every machine in a pool
#'
#' Kills the tunnels and worker processes, deletes all machines created for the
#' pool (including ones still starting), and removes the pool records.
#' Idempotent.
#'
#' @param pool A pool.
#' @param quiet Suppress the summary message.
#' @return Invisibly, the deleted ids.
#' @keywords internal
ct_pool_stop <- function(pool, quiet = FALSE) {
  if (isTRUE(pool$stopped)) {
    return(invisible(character()))
  }
  for (m in pool$machines) {
    for (w in m$workers) try(w$kill(), silent = TRUE)
    if (!is.null(m$tunnel)) try(m$tunnel$kill(), silent = TRUE)
  }
  ids <- unique(c(vapply(pool$machines, function(m) m$id, ""), pool$registered))
  for (id in ids) try(pool$provider$delete(id), silent = TRUE)
  cost <- sum(vapply(pool$machines, function(m) {
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
    unlink(c(ct_pool_path(pool$config), file.path(pool$config$state_dir, "pool-ids.json")))
    pool$stopped <- TRUE
    if (!quiet && length(ids)) {
      cli::cli_alert_success("Deleted {length(ids)} pool machine{?s}; about {ct_format_usd(cost)} used. Nothing is running.")
    }
  }
  invisible(ids)
}

# Reverse tunnel: connections to 127.0.0.1:<port> on the machine reach
# 127.0.0.1:<port> here. The process is a ControlMaster so workers can share
# the connection (see ct_worker_command()).
ct_tunnel_open <- function(state, config, port) {
  control <- ct_control_path(state)
  unlink(control)
  proc <- processx::process$new("ssh", c(
    ct_ssh_options(config),
    "-o", "ExitOnForwardFailure=yes", "-o", "ServerAliveInterval=15", "-o", "ServerAliveCountMax=4",
    "-N", "-M", "-S", control,
    "-R", sprintf("%d:127.0.0.1:%d", port, port),
    "-p", as.character(state$port), paste0(state$user, "@", state$host)
  ), stderr = "|")
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

# Pick the machine with the most free slots and start one worker on it.
ct_pool_launch <- function(pool, call, spawn = function(args) processx::process$new("ssh", args, stdout = "|", stderr = "|")) {
  load <- vapply(pool$machines, function(m) {
    sum(vapply(m$workers, function(w) isTRUE(w$is_alive()), logical(1)))
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
