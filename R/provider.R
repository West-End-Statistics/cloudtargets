#' Create a provider object
#'
#' A provider is a list of functions that hides one cloud's API. The rest of
#' the package only talks to this interface, so adding a cloud means writing
#' one constructor that returns these functions:
#'
#' * `preflight(config)`: list of `list(ok, check, detail)`; must not create
#'   anything billable.
#' * `candidates(config)`: ordered list of `list(label, spec)` to try.
#' * `create(candidate, config, name)`: returns `list(id, cost_per_hr, label)`;
#'   signal "nothing available" with [ct_abort_capacity()].
#' * `describe(id)`: returns `list(status, host, port, cost_per_hr)`; `host` and
#'   `port` are `NULL` until the machine is reachable.
#' * `delete(id)`: terminates the machine.
#' * `list(prefix)`: list of `list(id, name, cost_per_hr)` for machines whose
#'   name starts with `prefix`.
#'
#' @param config A configuration list from [ct_config()].
#' @return An object of class `ct_provider`.
#' @export
ct_provider <- function(config = ct_config()) {
  switch(config$provider,
    runpod = ct_provider_runpod(config),
    cli::cli_abort(c(
      "Unknown provider {.val {config$provider}}.",
      i = "Available: {.val runpod}."
    ))
  )
}

new_ct_provider <- function(name, preflight, candidates, create, describe,
                            delete, list) {
  # `list` is also an argument name here, so call base::list explicitly.
  structure(
    base::list(
      name = name, preflight = preflight, candidates = candidates,
      create = create, describe = describe, delete = delete, list = list
    ),
    class = "ct_provider"
  )
}

#' Signal that a provider has no capacity for a request
#'
#' Retryable: provisioning moves on to the next candidate.
#'
#' @param message Human-readable reason.
#' @return Does not return; raises an error of class `ct_error_capacity`.
#' @export
ct_abort_capacity <- function(message) {
  cli::cli_abort("{message}", class = "ct_error_capacity", call = NULL)
}
