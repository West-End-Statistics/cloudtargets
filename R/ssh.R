ct_ssh_options <- function(config = NULL) {
  # Pods are ephemeral and addresses get reused, so host keys are neither
  # recorded nor checked. We only connect to an address the provider API gave
  # us seconds after creating the machine with our own public key.
  opts <- c(
    "-o", "BatchMode=yes",
    "-o", "StrictHostKeyChecking=no",
    "-o", "UserKnownHostsFile=/dev/null",
    "-o", "LogLevel=ERROR",
    "-o", "ConnectTimeout=8",
    "-o", "ServerAliveInterval=15"
  )
  if (!is.null(config$ssh_identity)) {
    opts <- c(opts, "-i", path.expand(config$ssh_identity), "-o", "IdentitiesOnly=yes")
  }
  opts
}

ct_ssh_target <- function(state, config) {
  list(host = state$host, port = state$port, user = config$ssh_user)
}

#' Test whether SSH is answering on a host
#'
#' @param host,port,user Connection details.
#' @param config Configuration list; `ssh_identity` (optional) selects the
#'   private key to use.
#' @return `TRUE` if a no-op remote command succeeds.
#' @keywords internal
ct_ssh_check <- function(host, port, user, config = NULL) {
  res <- processx::run(
    "ssh",
    c(ct_ssh_options(config), "-p", as.character(port), paste0(user, "@", host), "true"),
    error_on_status = FALSE, timeout = 20
  )
  identical(res$status, 0L)
}

ct_hooks <- function() {
  list(check = ct_ssh_check, sleep = Sys.sleep, now = Sys.time)
}
