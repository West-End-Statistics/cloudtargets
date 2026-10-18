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
    ready_timeout = 600,
    max_hours = 12,
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
#' Merges `_cloudtargets.yml` (if present) over [ct_default_config()].
#' Lists in the file replace the default list; scalar fields override. If
#' `dockerfile` (and optionally `dockerfile_target`) is set, the base image,
#' login user and `install2.r` packages are read from it; see
#' [ct_dockerfile_info()]. `max_hours` (default 12) makes each machine delete itself
#' after that long, even if this session dies; see `inst/pod-start.sh`. `sync: none` skips copying the project to pool machines.
#' `ssh_setup: runtime` (default) installs sshd, rsync and
#' tmux when the machine starts; use `image` for images that already run sshd.
#'
#' @param path Path to the YAML file.
#' @return A named list.
#' @export
ct_config <- function(path = "_cloudtargets.yml") {
  config <- ct_default_config()
  if (file.exists(path)) {
    user <- yaml::read_yaml(path)
    if (!is.list(user)) {
      cli::cli_abort("{.file {path}} must contain a YAML mapping.")
    }
    config <- ct_merge(config, user)
    config <- ct_apply_dockerfile(config, user)
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
  if (!is.numeric(config$ready_timeout) || config$ready_timeout <= 0) {
    cli::cli_abort("{.field ready_timeout} must be a positive number of seconds.")
  }
  invisible(config)
}
