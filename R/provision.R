#' Check everything that can be checked before spending money
#'
#' Runs the provider's preflight checks and prints a pass/fail line for each.
#' Nothing billable is created.
#'
#' @inheritParams ct_provision
#' @param quiet Print only failing checks.
#' @return Invisibly `TRUE` if all checks pass; otherwise an error.
#' @export
ct_doctor <- function(config = ct_config(), provider = ct_provider(config), quiet = FALSE) {
  checks <- provider$preflight(config)
  for (chk in checks) {
    if (chk$ok) {
      if (!quiet) cli::cli_alert_success("{chk$check}: {chk$detail}")
    } else {
      cli::cli_alert_danger("{chk$check}: {chk$detail}")
    }
  }
  if (!all(vapply(checks, function(chk) chk$ok, logical(1)))) {
    cli::cli_abort("Preflight failed; fix the items marked above and retry.", call = NULL)
  }
  invisible(TRUE)
}

#' Provision a machine and wait until it accepts SSH
#'
#' Walks the provider's candidate list (largest size first, then smaller, across
#' the configured data centers). For each candidate it creates a machine and
#' waits up to `ready_timeout` seconds for SSH. Failures are handled so that
#' nothing is left billing:
#'
#' * "No capacity" is expected and just moves on to the next candidate.
#' * A machine that never becomes reachable is deleted, and its data center is
#'   skipped for the rest of the attempt.
#' * If R errors or is interrupted mid-attempt, the in-flight machine is deleted.
#'
#' The machine is recorded in `<state_dir>/state.json` so [ct_status()] and
#' [ct_teardown()] can find it later.
#'
#' @param config A configuration list from [ct_config()].
#' @param provider A [ct_provider()].
#' @param force Provision even if a state file already exists. The old machine
#'   is not touched; use [ct_teardown()] or [ct_cleanup()] for that.
#' @param hooks Test hooks: a list with `check`, `sleep` and `now` functions.
#' @return Invisibly, the state list (`id`, `host`, `port`, `cost_per_hr`, ...).
#' @export
ct_provision <- function(config = ct_config(), provider = ct_provider(config),
                         force = FALSE, hooks = ct_hooks()) {
  existing <- ct_state_read(config)
  if (!is.null(existing) && !force) {
    cli::cli_abort(c(
      "A machine is already recorded in {.file {ct_state_path(config)}} (id {existing$id}).",
      i = "Run {.fn ct_teardown} first, or use {.code force = TRUE} to provision another."
    ))
  }
  ct_doctor(config, provider, quiet = TRUE)

  candidates <- provider$candidates(config)
  attempts <- list()
  bad_sites <- character()
  inflight <- NULL
  on.exit(
    if (!is.null(inflight)) {
      try(provider$delete(inflight), silent = TRUE)
      ct_state_archive(config)
    },
    add = TRUE
  )

  for (cand in candidates) {
    site <- cand$spec$data_center
    if (!is.null(site) && site %in% bad_sites) {
      attempts[[length(attempts) + 1L]] <- list(label = cand$label, result = "skipped (unreachable earlier)")
      next
    }
    cli::cli_alert_info("Trying {cand$label}")
    pod <- tryCatch(
      provider$create(cand, config, ct_pod_name(config)),
      ct_error_capacity = function(e) e
    )
    if (inherits(pod, "ct_error_capacity")) {
      attempts[[length(attempts) + 1L]] <- list(label = cand$label, result = "no capacity")
      next
    }
    inflight <- pod$id
    state <- list(
      id = pod$id, label = pod$label, provider = provider$name,
      cost_per_hr = pod$cost_per_hr, vcpus = pod$vcpus, host = NULL, port = NULL,
      user = config$ssh_user, status = "starting",
      started_at = format(hooks$now(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    ct_state_write(config, state)

    ready <- ct_wait_ready(provider, pod$id, config, hooks)
    if (isTRUE(ready$ok)) {
      state$host <- ready$host
      state$port <- ready$port
      state$status <- "ready"
      state$cost_per_hr <- ready$cost_per_hr %||% state$cost_per_hr
      ct_state_write(config, state)
      inflight <- NULL
      cli::cli_alert_success(
        "Ready: {pod$label} at {ready$host}:{ready$port} ({ct_format_usd(as.numeric(state$cost_per_hr))}/hr)"
      )
      return(invisible(state))
    }
    cli::cli_alert_warning("{pod$label} never became reachable ({ready$reason}); deleting it.")
    provider$delete(pod$id)
    inflight <- NULL
    ct_state_archive(config)
    if (!is.null(site)) bad_sites <- c(bad_sites, site)
    attempts[[length(attempts) + 1L]] <- list(
      label = cand$label, result = paste("unreachable:", ready$reason)
    )
  }

  ct_abort_exhausted(attempts)
}

ct_abort_exhausted <- function(attempts) {
  lines <- vapply(attempts, function(a) paste0(a$label, ": ", a$result), character(1))
  results <- vapply(attempts, `[[`, "", "result")
  no_capacity <- all(grepl("^(no capacity|skipped)", results))
  cli::cli_abort(c(
    "Could not provision a machine; every candidate failed.",
    stats::setNames(lines, rep("*", length(lines))),
    i = if (no_capacity) {
      "Capacity fluctuates; retry in a few minutes, lower {.field min_vcpus}, or add data centers."
    } else if (any(grepl("no network address", results))) {
      paste(
        "Some machines were accepted but never scheduled (often a busy host or slow image pull) and were deleted.",
        "Retry, or raise {.field ready_timeout}."
      )
    } else {
      "Some machines were created but SSH never answered (and they were deleted); check your SSH key and network."
    },
    i = "Nothing is left running."
  ), call = NULL)
}

#' Wait until a machine accepts SSH
#'
#' @param provider A [ct_provider()].
#' @param id Machine id.
#' @param config Configuration list.
#' @param hooks Test hooks, see [ct_provision()].
#' @param interval Seconds between polls.
#' @return A list with `ok`; if `TRUE` also `host`, `port`, `cost_per_hr`,
#'   otherwise `reason`.
#' @keywords internal
ct_wait_ready <- function(provider, id, config, hooks = ct_hooks(), interval = 5) {
  deadline <- hooks$now() + config$ready_timeout
  reason <- "no network address assigned yet"
  repeat {
    info <- provider$describe(id)
    if (!is.null(info$host)) {
      if (isTRUE(hooks$check(info$host, info$port, config$ssh_user, config))) {
        return(list(ok = TRUE, host = info$host, port = info$port, cost_per_hr = info$cost_per_hr))
      }
      reason <- "address assigned but SSH not answering"
    }
    if (hooks$now() >= deadline) {
      return(list(ok = FALSE, reason = paste0(reason, " after ", config$ready_timeout, "s")))
    }
    hooks$sleep(interval)
  }
}
