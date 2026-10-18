#' RunPod CPU pod provider
#'
#' Creates ephemeral CPU pods through the RunPod REST API. The API key is read
#' from the `RUNPOD_API_KEY` environment variable, falling back to the
#' `api_key` entry of `~/.runpod/config.toml` (the file `runpodctl` writes).
#' The key is never printed or stored by this package.
#'
#' Relevant `runpod` configuration fields: `flavors` (e.g. `cpu3c`),
#' `data_centers`, `vcpus` (preference order, largest first) and `min_vcpus`.
#' Capacity on RunPod fluctuates, so provisioning walks vCPU counts downward
#' across data centers; see [ct_provision()].
#'
#' @param config A configuration list from [ct_config()].
#' @return A `ct_provider`.
#' @export
ct_provider_runpod <- function(config = ct_config()) {
  new_ct_provider(
    name = "runpod",
    preflight = function(config) rp_preflight(config),
    candidates = function(config) rp_candidates(config),
    create = function(candidate, config, name) rp_create(candidate, config, name),
    describe = function(id) rp_describe(id),
    delete = function(id) rp_delete(id),
    list = function(prefix) rp_list(prefix)
  )
}

rp_base_url <- function() "https://rest.runpod.io/v1"

rp_api_key <- function() {
  key <- Sys.getenv("RUNPOD_API_KEY", "")
  if (nzchar(key)) {
    return(key)
  }
  toml <- path.expand("~/.runpod/config.toml")
  if (file.exists(toml)) {
    hit <- grep("^\\s*api_key\\s*=", readLines(toml, warn = FALSE), value = TRUE)
    if (length(hit) > 0L) {
      return(gsub("^\\s*api_key\\s*=\\s*[\"']?|[\"']?\\s*$", "", hit[[1L]]))
    }
  }
  NULL
}

# All network access goes through this one function so tests can replace it.
rp_http <- function(method, path, body = NULL) {
  key <- rp_api_key()
  if (is.null(key)) {
    cli::cli_abort(c(
      "No RunPod API key found.",
      i = "Set {.envvar RUNPOD_API_KEY} or run {.code runpodctl config}."
    ))
  }
  req <- httr2::request(paste0(rp_base_url(), path))
  req <- httr2::req_auth_bearer_token(req, key)
  req <- httr2::req_method(req, method)
  req <- httr2::req_timeout(req, 60)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body)
  }
  resp <- httr2::req_perform(req)
  text <- if (httr2::resp_has_body(resp)) httr2::resp_body_string(resp) else ""
  parsed <- if (nzchar(text)) {
    tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) text)
  } else {
    NULL
  }
  list(status = httr2::resp_status(resp), body = parsed)
}

rp_error_text <- function(res) {
  msg <- if (is.list(res$body) && !is.null(res$body$error)) res$body$error else res$body
  msg <- if (is.character(msg)) msg else "no error message"
  substr(gsub("\\s+", " ", paste(msg, collapse = " ")), 1L, 200L)
}

rp_check <- function(res, what) {
  if (res$status >= 200L && res$status < 300L) {
    return(invisible(res))
  }
  if (res$status %in% c(401L, 403L)) {
    cli::cli_abort(c(
      "RunPod rejected the API key while trying to {what} (HTTP {res$status}).",
      i = "Check the key in {.envvar RUNPOD_API_KEY} or {.file ~/.runpod/config.toml}."
    ))
  }
  cli::cli_abort("RunPod could not {what} (HTTP {res$status}): {rp_error_text(res)}")
}

rp_preflight <- function(config) {
  key <- rp_api_key()
  pub <- path.expand(config$ssh_public_key)
  checks <- list(
    list(ok = !is.null(key), check = "RunPod API key", detail = if (is.null(key)) {
      "not found (set RUNPOD_API_KEY)"
    } else {
      "found"
    }),
    list(ok = file.exists(pub), check = "SSH public key", detail = if (file.exists(pub)) {
      pub
    } else {
      paste("missing:", pub)
    }),
    list(ok = nzchar(Sys.which("ssh")), check = "ssh client", detail = "on PATH")
  )
  if (!is.null(key)) {
    res <- tryCatch(rp_http("GET", "/pods"), error = function(e) NULL)
    ok <- !is.null(res) && res$status == 200L
    checks[[length(checks) + 1L]] <- list(
      ok = ok, check = "RunPod API reachable",
      detail = if (ok) "authenticated" else "request failed or key rejected"
    )
  }
  checks
}

rp_candidates <- function(config) {
  rp <- config$runpod
  vcpus <- sort(unlist(rp$vcpus), decreasing = TRUE)
  vcpus <- vcpus[vcpus >= rp$min_vcpus]
  out <- list()
  for (v in vcpus) {
    for (flavor in unlist(rp$flavors)) {
      for (dc in unlist(rp$data_centers)) {
        out[[length(out) + 1L]] <- list(
          label = sprintf("%s x %d vCPU in %s", flavor, v, dc),
          spec = list(flavor = flavor, vcpus = as.integer(v), data_center = dc)
        )
      }
    }
  }
  out
}

rp_create <- function(candidate, config, name) {
  spec <- candidate$spec
  pub <- trimws(paste(readLines(path.expand(config$ssh_public_key), warn = FALSE), collapse = ""))
  body <- list(
    name = name,
    computeType = "CPU",
    cpuFlavorIds = list(spec$flavor),
    vcpuCount = spec$vcpus,
    dataCenterIds = list(spec$data_center),
    imageName = config$image,
    containerDiskInGb = as.integer(config$container_disk_gb),
    volumeInGb = 0L,
    ports = list("22/tcp"),
    env = list(PUBLIC_KEY = pub, SSH_USER = config$ssh_user, CT_MAX_HOURS = as.character(config$max_hours))
  )
  if (identical(config$ssh_setup, "runtime")) {
    start <- paste(readLines(system.file("pod-start.sh", package = "cloudtargets")), collapse = "\n")
    body$dockerEntrypoint <- list("bash", "-c")
    body$dockerStartCmd <- list(start)
  }
  res <- rp_http("POST", "/pods", body)
  if (res$status >= 400L && grepl("no longer any instances available", rp_error_text(res), fixed = TRUE)) {
    ct_abort_capacity(paste("no capacity for", candidate$label))
  }
  rp_check(res, paste("create a pod for", candidate$label))
  list(
    id = res$body$id,
    cost_per_hr = res$body$costPerHr %||% NA_real_,
    label = candidate$label,
    vcpus = spec$vcpus
  )
}

rp_describe <- function(id) {
  res <- rp_http("GET", paste0("/pods/", id))
  rp_check(res, paste("read pod", id))
  pod <- res$body
  port <- if (is.list(pod$portMappings)) pod$portMappings[["22"]] else NULL
  host <- pod$publicIp
  if (is.null(host) || !nzchar(host) || is.null(port)) {
    host <- NULL
    port <- NULL
  }
  list(
    status = pod$desiredStatus %||% NA_character_,
    host = host,
    port = if (is.null(port)) NULL else as.integer(port),
    cost_per_hr = pod$costPerHr %||% NA_real_
  )
}

rp_delete <- function(id) {
  res <- rp_http("DELETE", paste0("/pods/", id))
  # 404 means it is already gone, which is the state we want.
  if (res$status != 404L) {
    rp_check(res, paste("terminate pod", id))
  }
  invisible(TRUE)
}

rp_list <- function(prefix) {
  res <- rp_http("GET", "/pods")
  rp_check(res, "list pods")
  pods <- Filter(function(p) startsWith(p$name %||% "", prefix), res$body %||% list())
  lapply(pods, function(p) {
    list(id = p$id, name = p$name, cost_per_hr = p$costPerHr %||% NA_real_)
  })
}

`%||%` <- function(x, y) if (is.null(x)) y else x
