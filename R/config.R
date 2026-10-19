#' Default cloudtargets configuration
#'
#' Returns the built-in defaults (`ssh_identity` is optional and unset). Override any field in a `_cloudtargets.yml`
#' file at the project root; see [ct_config()].
#'
#' @return A named list.
#' @export
ct_default_config <- function() {
  list(
    provider = "runpod",
    name_prefix = "cloudtargets",
    image = "runpod/base:1.4.1-ubuntu2404",
    container_disk_gb = 20L,
    ssh_user = "root",
    ssh_public_key = "~/.ssh/id_ed25519.pub",
    ssh_identity = NULL,
    ssh_setup = "runtime",
    sync = "rsync",
    dockerfile = NULL,
    dockerfile_target = NULL,
    ready_timeout = 900,
    prepare_timeout = 900,
    max_hours = 12,
    pool_give_up_minutes = 60,
    pool_spread = TRUE,
    pool_prepare_parallel = 4L,
    pool_prepare_failures_max = 3L,
    pool_describe_interval = 10,
    pool_backoff_min = 30,
    pool_backoff_max = 120,
    pool_site_cooldown_minutes = 60,
    pool_status_interval = 60,
    pool_tunnel_reconnects_max = 3L,
    state_dir = ".cloudtargets",
    remote_dir = NULL,
    command = "Rscript -e 'targets::tar_make()'",
    r_packages = list(),
    r_packages_exclude = list(),
    r_packages_from_description = TRUE,
    sync_exclude = list(".git", "_targets", "output", ".quarto", ".cloudtargets", ".Rproj.user", "renv/library"),
    fetch = list("_targets", "output", ".cloudtargets-run.log"),
    runpod = list(
      flavors = list("cpu3c"),
      data_centers = list("EUR-IS-1", "US-KS-2", "EU-RO-1"),
      vcpus = list(32L, 16L, 8L, 4L, 2L),
      min_vcpus = 4L
    )
  )
}

#' Read the cloudtargets configuration
#'
#' Merges `_cloudtargets.yml` (if present) over [ct_default_config()], then
#' `_cloudtargets.local.yml` (if present) over that for per-user settings such as
#' key paths.
#' Lists in the file replace the default list; scalar fields override. If
#' `dockerfile` (and optionally `dockerfile_target`) is set, the base image,
#' login user and `install2.r` packages are read from it; see
#' [ct_dockerfile_info()]. `ready_timeout` (default 900 s) is how long a machine may take to start answering SSH,
#' and `prepare_timeout` (900 s) how long to set it up and connect it, before it is
#' deleted and replaced; `pool_spread` (TRUE) puts machines of the same size in different data centers
#' where it can, so one bad data center cannot stall the pool; `pool_give_up_minutes` (60) is how long a pool may go without any
#' machine being ready before the run stops. `max_hours` (default 12) makes each machine delete itself
#' after that long, even if this session dies; see `inst/pod-start.sh`. `sync: none` skips copying the project to pool machines.
#' `ssh_setup: runtime` (default) installs sshd, rsync and
#' tmux when the machine starts; use `image` for images that already run sshd.
#'
#' @param path Path to the YAML file.
#' @return A named list.
#' @export
ct_config <- function(path = "_cloudtargets.yml") {
  config <- ct_default_config()
  user <- list()
  # Shared settings, then per-user overrides (keep the latter out of version control).
  for (file in c(path, sub("\\.yml$", ".local.yml", path))) {
    if (!file.exists(file)) next
    part <- yaml::read_yaml(file)
    if (!is.list(part)) {
      cli::cli_abort("{.file {file}} must contain a YAML mapping.")
    }
    user <- ct_merge(user, part)
  }
  if (length(user)) {
    config <- ct_apply_dockerfile(ct_merge(config, user), user)
  }
  ct_validate_config(config)
  config
}

# When `dockerfile` is set, fill in what the file knows and the user left unset:
# the base image, the login user, and the R packages it installs.
ct_apply_dockerfile <- function(config, user) {
  if (is.null(config$dockerfile)) {
    return(config)
  }
  info <- ct_dockerfile_info(config$dockerfile, config$dockerfile_target)
  if (is.null(user$image)) config$image <- info$image
  if (is.null(user$ssh_user) && !is.null(info$user)) config$ssh_user <- info$user
  config$dockerfile_packages <- as.list(info$packages)
  config
}

ct_merge <- function(base, over) {
  for (nm in names(over)) {
    both_named <- is.list(base[[nm]]) && !is.null(names(base[[nm]])) &&
      is.list(over[[nm]]) && !is.null(names(over[[nm]]))
    base[[nm]] <- if (both_named) ct_merge(base[[nm]], over[[nm]]) else over[[nm]]
  }
  base
}

ct_validate_config <- function(config) {
  if (!is.character(config$provider) || length(config$provider) != 1L) {
    cli::cli_abort("{.field provider} must be a single string.")
  }
  if (!is.numeric(config$max_hours) || config$max_hours <= 0) {
    cli::cli_abort("{.field max_hours} must be a positive number of hours.")
  }
  positive <- c(
    "ready_timeout", "prepare_timeout", "pool_give_up_minutes", "pool_prepare_parallel",
    "pool_prepare_failures_max", "pool_describe_interval", "pool_backoff_min", "pool_backoff_max",
    "pool_site_cooldown_minutes", "pool_status_interval", "pool_tunnel_reconnects_max"
  )
  for (key in positive) {
    value <- config[[key]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || value <= 0) {
      cli::cli_abort("{.field {key}} must be a single positive number.")
    }
  }
  invisible(config)
}
