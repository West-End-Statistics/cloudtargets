ct_state_path <- function(config, last = FALSE) {
  file.path(config$state_dir, if (last) "state.last.json" else "state.json")
}

# The state file records the machine we created, so a crash or a new R session
# can still find and tear it down. It holds no secrets.
ct_state_read <- function(config) {
  path <- ct_state_path(config)
  if (!file.exists(path)) {
    return(NULL)
  }
  jsonlite::read_json(path, simplifyVector = FALSE)
}

ct_state_write <- function(config, state) {
  dir.create(config$state_dir, showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(state, ct_state_path(config), auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(state)
}

ct_state_archive <- function(config) {
  path <- ct_state_path(config)
  if (file.exists(path)) {
    file.rename(path, ct_state_path(config, last = TRUE))
  }
  invisible(NULL)
}

ct_runtime_hours <- function(state, now = Sys.time()) {
  started <- as.POSIXct(state$started_at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  as.numeric(difftime(now, started, units = "hours"))
}

ct_cost_estimate <- function(state, now = Sys.time()) {
  cost <- suppressWarnings(as.numeric(state$cost_per_hr))
  if (is.na(cost)) {
    return(NA_real_)
  }
  cost * ct_runtime_hours(state, now)
}

ct_format_usd <- function(x) {
  if (is.na(x)) "unknown" else sprintf("$%.2f", x)
}

ct_pod_name <- function(config) {
  paste0(config$name_prefix, "-", paste(sample(c(0:9, letters[1:6]), 6L, replace = TRUE), collapse = ""))
}
