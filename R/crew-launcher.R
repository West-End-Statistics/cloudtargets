#' @importFrom R6 R6Class
NULL

# A crew launcher whose workers run on a pool of cloud machines. The
# controller (the R session running tar_make()) stays local; the machines are
# created when the controller starts and deleted when it terminates.
# Pool machines share no file system with this session, so targets must send
# results through the controller rather than have workers write to the store.
# Checked before any machine is created.
ct_check_targets_storage <- function() {
  if (!requireNamespace("targets", quietly = TRUE)) {
    return(invisible(TRUE))
  }
  storage <- targets::tar_option_get("storage")
  retrieval <- targets::tar_option_get("retrieval")
  bad <- c(
    if (!identical(storage, "main")) sprintf("storage = \"%s\"", storage),
    if (!identical(retrieval, "main")) sprintf("retrieval = \"%s\"", retrieval)
  )
  if (length(bad)) {
    cli::cli_abort(c(
      "targets is set to {paste(bad, collapse = ' and ')}, which needs a file system shared with the workers.",
      i = "Machines in the pool do not share one with this session.",
      i = "Use {.code tar_option_set(storage = \"main\", retrieval = \"main\")} so results travel through the controller.",
      i = "No machines were created."
    ), call = NULL)
  }
  invisible(TRUE)
}

ct_launcher_class <- R6::R6Class(
  classname = "ct_launcher",
  inherit = crew::crew_class_launcher,
  public = list(
    cloud = NULL,
    pool = NULL,
    initialize = function(..., cloud) {
      super$initialize(...)
      self$cloud <- cloud
      invisible()
    },
    start = function(url = NULL, profile = NULL, sockets = NULL) {
      super$start(url = url, profile = profile, sockets = sockets)
      ct_check_targets_storage()
      port <- suppressWarnings(as.integer(sub("^.*:([0-9]+)/?$", "\\1", url %||% "")))
      if (is.na(port)) {
        cli::cli_abort("Could not read the controller port from the URL {.val {url}}.")
      }
      pool <- ct_pool_new(self$cloud$config, self$cloud$provider, self$cloud$hooks, self$cloud$n_slots)
      # Last resort if this R session exits without terminate() being called.
      reg.finalizer(pool, function(p) try(ct_pool_stop(p, quiet = TRUE), silent = TRUE), onexit = TRUE)
      self$pool <- pool
      # start() must return quickly and, if it fails, clean up after itself:
      # targets skips terminate() when start() throws.
      tryCatch(
        ct_pool_begin(pool, port),
        error = function(e) {
          ct_pool_stop(pool, quiet = TRUE)
          self$pool <- NULL
          stop(e)
        }
      )
      invisible()
    },
    # Called every ~0.5 s from targets' scheduling loop: move machines along
    # (create, check, prepare, connect, replace) without ever blocking.
    scale = function(status = NULL, throttle = NULL) {
      if (!is.null(self$pool)) {
        ct_pool_tick(self$pool)
      }
      super$scale(status = status, throttle = throttle)
    },
    # Start only as many workers as there are free slots on READY machines;
    # crew asks again on its next pass. `workers` stays constant, so the rest
    # simply start as more machines become ready.
    launch = function(n = 1L) {
      n <- if (is.null(self$pool)) 0L else ct_launch_cap(self$pool, n)
      if (n < 1L) {
        return(invisible(NULL))
      }
      super$launch(n = n)
    },
    launch_worker = function(call) {
      ct_pool_launch(self$pool, call)
    },
    terminate = function() {
      out <- super$terminate()
      if (!is.null(self$pool)) {
        ct_pool_stop(self$pool)
        self$pool <- NULL
      }
      invisible(out)
    }
  )
)

#' A crew controller that runs workers on cloud machines
#'
#' Use it as a drop-in `crew` controller in a targets pipeline:
#'
#' ```r
#' tar_option_set(controller = cloudtargets::ct_crew_controller(workers = 32))
#' ```
#'
#' The pipeline must set `tar_option_set(storage = "main", retrieval = "main")`
#' (targets' default is `"worker"`, which needs a shared file system); this is
#' checked before any machine is created.
#'
#' `tar_make()` runs on your machine as usual. Machines are created (as many as
#' `workers` needs, sized by the configured ladder), the project and its R
#' packages are put on them, and each is joined to your session by an SSH
#' reverse tunnel, so no inbound port is needed here. This happens
#' **incrementally**: work starts on the first machine that is ready while the
#' others are still starting. A machine that is not ready after `ready_timeout`
#' (15 minutes by default) is deleted and replaced; a partly filled order keeps
#' being topped up in the background, and a one-line status is printed at most
#' once a minute. If no machine has been ready for `pool_give_up_minutes` (60)
#' everything is deleted and the run stops with an error. Workers
#' then run targets on them; results return through the controller, so no
#' shared storage is required. When `tar_make()` ends the machines are deleted.
#' Changing `workers` changes how many machines are used.
#'
#' Pass `sync: none` in the configuration to skip copying the project, if the
#' machines' image already contains it. Each machine also deletes itself after
#' `max_hours` as a backstop.
#'
#' @param workers Maximum number of workers (one worker slot per vCPU).
#' @param config Configuration from [ct_config()].
#' @param provider A [ct_provider()].
#' @param hooks Test hooks for pool operations.
#' @param name Controller name.
#' @param seconds_interval,seconds_timeout,seconds_launch,seconds_idle,seconds_wall,tasks_max,tasks_timers
#'   Passed to the crew launcher; see [crew::crew_controller_local()]. A worker
#'   that stays idle for `seconds_idle` exits (the machine stays until the
#'   controller terminates).
#' @param crashes_max Passed to [crew::crew_controller()].
#' @return A crew controller.
#' @export
ct_crew_controller <- function(workers = 4L, config = ct_config(), provider = ct_provider(config),
                               hooks = ct_pool_hooks(), name = NULL, seconds_interval = 0.5,
                               seconds_timeout = 60, seconds_launch = 600, seconds_idle = 300,
                               seconds_wall = Inf, tasks_max = Inf, tasks_timers = 0,
                               crashes_max = 5L) {
  workers <- as.integer(workers)
  if (is.na(workers) || workers < 1L) {
    cli::cli_abort("{.arg workers} must be a positive whole number.")
  }
  tls <- crew::crew_tls()
  client <- crew::crew_client(
    host = "127.0.0.1", tls = tls,
    seconds_interval = seconds_interval, seconds_timeout = seconds_timeout
  )
  launcher <- ct_launcher_class$new(
    name = name %||% paste0("ct", paste(sample(c(letters, 0:9), 6L, replace = TRUE), collapse = "")),
    workers = workers, seconds_interval = seconds_interval, seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch, seconds_idle = seconds_idle, seconds_wall = seconds_wall,
    tasks_max = tasks_max, tasks_timers = tasks_timers, tls = tls,
    r_arguments = c("--no-save", "--no-restore"),
    options_metrics = crew::crew_options_metrics(),
    cloud = list(config = config, provider = provider, hooks = hooks, n_slots = workers)
  )
  controller <- crew::crew_controller(client = client, launcher = launcher, crashes_max = crashes_max)
  controller$validate()
  controller
}
