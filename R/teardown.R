#' Show the current machine
#'
#' @inheritParams ct_provision
#' @return Invisibly, a list with `id`, `label`, `status`, `host`, `port`,
#'   `hours` and `cost_so_far`; `NULL` if no machine is recorded.
#' @export
ct_status <- function(config = ct_config(), provider = ct_provider(config)) {
  state <- ct_state_read(config)
  if (is.null(state)) {
    cli::cli_alert_info("No machine recorded in {.file {ct_state_path(config)}}.")
    return(invisible(NULL))
  }
  live <- provider$list(config$name_prefix)
  alive <- state$id %in% vapply(live, function(p) p$id, character(1))
  hours <- ct_runtime_hours(state)
  cost <- ct_cost_estimate(state)
  out <- list(
    id = state$id, label = state$label, status = if (alive) state$status else "gone",
    host = state$host, port = state$port, hours = hours, cost_so_far = cost
  )
  if (alive) {
    cli::cli_alert_info(
      "{state$label} ({state$id}) is {state$status}; up {round(hours, 2)}h, about {ct_format_usd(cost)} so far."
    )
  } else {
    cli::cli_alert_warning(
      "{state$id} is no longer listed by the provider; run {.fn ct_teardown} to clear the record."
    )
  }
  invisible(out)
}

#' Terminate the recorded machine
#'
#' Fetches the results first ([ct_fetch()]) because they live only on the
#' machine, then deletes it, confirms the provider no longer lists it, and
#' archives the state file to `state.last.json`. If the fetch fails the machine
#' is left running (so nothing is lost) unless `force = TRUE`.
#'
#' @inheritParams ct_provision
#' @param fetch Fetch results before deleting. Set `FALSE` to discard them.
#' @param force Delete even if fetching fails.
#' @param fetcher Function used to fetch; a test hook.
#' @return Invisibly, the final state (with `hours` and `cost`), or `NULL` if
#'   nothing was recorded.
#' @export
ct_teardown <- function(config = ct_config(), provider = ct_provider(config),
                        fetch = TRUE, force = FALSE, fetcher = ct_fetch) {
  state <- ct_state_read(config)
  if (is.null(state)) {
    cli::cli_alert_info("Nothing recorded to tear down. Use {.fn ct_cleanup} to look for strays.")
    return(invisible(NULL))
  }
  if (fetch && !is.null(state$host)) {
    outcome <- tryCatch(fetcher(config), error = function(e) e)
    if (inherits(outcome, "error")) {
      if (!force) {
        cli::cli_abort(c(
          "Could not fetch results before teardown: {conditionMessage(outcome)}",
          i = "The machine {state$id} is still running (and billing) so nothing is lost.",
          i = "Fix the connection and retry, or use {.code force = TRUE} / {.code fetch = FALSE} to delete anyway."
        ))
      }
      cli::cli_alert_warning("Fetch failed ({conditionMessage(outcome)}); deleting anyway because force = TRUE.")
    }
  }
  provider$delete(state$id)
  remaining <- vapply(provider$list(config$name_prefix), function(p) p$id, character(1))
  if (state$id %in% remaining) {
    cli::cli_abort(c(
      "Asked {provider$name} to delete {state$id}, but it is still listed.",
      i = "It may still be billing. Check the provider console."
    ))
  }
  state$hours <- ct_runtime_hours(state)
  state$cost <- ct_cost_estimate(state)
  ct_state_archive(config)
  cli::cli_alert_success(
    "Terminated {state$id}; ran {round(state$hours, 2)}h, about {ct_format_usd(state$cost)}. Nothing is running."
  )
  invisible(state)
}

#' Delete stray machines created by this package
#'
#' Finds machines whose name starts with `name_prefix` (including ones from
#' crashed sessions that have no state file) and deletes them.
#'
#' @inheritParams ct_provision
#' @param yes Delete without asking. Required in non-interactive sessions.
#' @return Invisibly, the ids that were deleted.
#' @export
ct_cleanup <- function(config = ct_config(), provider = ct_provider(config), yes = FALSE) {
  pods <- provider$list(config$name_prefix)
  if (length(pods) == 0L) {
    cli::cli_alert_success("No machines named {.val {config$name_prefix}-*}; nothing is running.")
    return(invisible(character()))
  }
  for (p in pods) {
    cli::cli_alert_info("{p$name} ({p$id}), {ct_format_usd(as.numeric(p$cost_per_hr))}/hr")
  }
  if (!yes) {
    if (!interactive()) {
      cli::cli_abort("Refusing to delete {length(pods)} machine{?s} non-interactively; pass {.code yes = TRUE}.")
    }
    if (!identical(tolower(trimws(readline("Delete these? [y/N] "))), "y")) {
      cli::cli_alert_info("Left them running.")
      return(invisible(character()))
    }
  }
  ids <- vapply(pods, function(p) p$id, character(1))
  for (id in ids) provider$delete(id)
  state <- ct_state_read(config)
  if (!is.null(state) && state$id %in% ids) ct_state_archive(config)
  cli::cli_alert_success("Deleted {length(ids)} machine{?s}.")
  invisible(ids)
}
