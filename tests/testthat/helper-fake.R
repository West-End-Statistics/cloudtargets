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
