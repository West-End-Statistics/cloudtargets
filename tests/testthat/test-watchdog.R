write_ids <- function(config, ids) {
  dir.create(config$state_dir, showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(as.list(ids), file.path(config$state_dir, "pool-ids.json"), auto_unbox = TRUE)
}

# A process that is "alive" for `n` checks, then gone.
alive_for <- function(n) {
  calls <- 0L
  function(pid) {
    calls <<- calls + 1L
    calls <= n
  }
}

test_that("watchdog deletes the recorded machines once the controller is gone", {
  config <- test_config()
  fp <- fake_provider(list(a = "ok"))
  fp$env$pods <- list(p1 = list(id = "p1", name = "x"), p2 = list(id = "p2", name = "x"), other = list(id = "other", name = "x"))
  write_ids(config, c("p1", "p2"))
  out <- ct_watchdog_run(123L, config, fp$provider, alive = alive_for(2), sleep = function(s) NULL, log = function(...) NULL)
  expect_equal(out, "deleted")
  expect_setequal(fp$env$deleted, c("p1", "p2"))
  expect_named(fp$env$pods, "other") # machines it did not record are left alone
  expect_false(file.exists(file.path(config$state_dir, "pool-ids.json")))
})

test_that("watchdog leaves the machines alone when the pool was stopped normally", {
  config <- test_config()
  fp <- fake_provider(list(a = "ok"))
  fp$env$pods <- list(p1 = list(id = "p1", name = "x"))
  write_ids(config, "p1")
  checks <- 0L
  alive <- function(pid) {
    checks <<- checks + 1L
    if (checks == 2L) unlink(file.path(config$state_dir, "pool-ids.json")) # the pool stops cleanly
    TRUE
  }
  out <- ct_watchdog_run(123L, config, fp$provider, alive = alive, sleep = function(s) NULL, log = function(...) NULL)
  expect_equal(out, "clean")
  expect_length(fp$env$deleted, 0)
})

test_that("watchdog does nothing when the controller ends before any machine exists", {
  config <- test_config()
  fp <- fake_provider(list(a = "ok"))
  out <- ct_watchdog_run(123L, config, fp$provider, alive = alive_for(1), sleep = function(s) NULL, log = function(...) NULL)
  expect_equal(out, "nothing")
  expect_length(fp$env$deleted, 0)
})

test_that("watchdog keeps retrying a failing delete and reports what it could not delete", {
  config <- test_config()
  fp <- fake_provider(list(a = "ok"))
  fp$env$pods <- list(p1 = list(id = "p1", name = "x"))
  write_ids(config, "p1")
  attempts <- 0L
  provider <- fp$provider
  provider$delete <- function(id) {
    attempts <<- attempts + 1L
    if (attempts < 3L) stop("api down")
    fp$provider$delete(id)
  }
  out <- ct_watchdog_run(123L, config, provider, alive = alive_for(1), sleep = function(s) NULL, log = function(...) NULL)
  expect_equal(out, "deleted")
  expect_equal(attempts, 3L)

  write_ids(config, "p1")
  fp$env$pods <- list(p1 = list(id = "p1", name = "x"))
  provider$delete <- function(id) stop("api down")
  out <- ct_watchdog_run(123L, config, provider, alive = alive_for(1), sleep = function(s) NULL, log = function(...) NULL, delete_attempts = 2L)
  expect_equal(out, "failed")
  expect_true(file.exists(file.path(config$state_dir, "pool-ids.json"))) # kept so ct_cleanup() can find it
})

test_that("watchdog gives up waiting after max_hours plus an hour", {
  config <- test_config()
  config$max_hours <- 1
  fp <- fake_provider(list(a = "ok"))
  clock <- fake_clock()
  out <- ct_watchdog_run(
    123L, config, fp$provider, alive = function(pid) TRUE,
    sleep = function(s) clock$advance(600), now = clock$now, log = function(...) NULL
  )
  expect_equal(out, "expired")
})

test_that("a pool starts a watchdog and stops it after a clean shutdown", {
  config <- test_config()
  clock <- fake_clock()
  fp <- fake_provider2(list("A x 8 vCPU in X" = "ok"), clock)
  hooks <- fake_async_hooks(fp)
  started <- NULL
  killed <- FALSE
  hooks$watchdog <- function(config) {
    started <<- config$state_dir
    list(kill = function() killed <<- TRUE)
  }
  pool <- ct_pool_new(config, fp$provider, hooks, 8L)
  suppressMessages(ct_pool_begin(pool, 5555L))
  expect_equal(started, config$state_dir)
  expect_false(killed)
  suppressMessages(ct_pool_stop(pool, quiet = TRUE))
  expect_true(killed)

  config$watchdog <- FALSE
  started <- NULL
  pool <- ct_pool_new(config, fp$provider, hooks, 8L)
  suppressMessages(ct_pool_begin(pool, 5555L))
  expect_null(started)
  suppressMessages(ct_pool_stop(pool, quiet = TRUE))
})

test_that("a pool turns on the machines' idle check unless the user set it", {
  config <- test_config()
  fp <- fake_provider(list(a = "ok"))
  pool <- ct_pool_new(config, fp$provider, fake_pool_hooks(fp$env), 4L)
  expect_equal(pool$config$orphan_minutes, 30)
  config$orphan_minutes <- 0
  pool <- ct_pool_new(config, fp$provider, fake_pool_hooks(fp$env), 4L)
  expect_equal(pool$config$orphan_minutes, 0)
})

test_that("machines get the idle-check setting, off unless a pool asks for it", {
  config <- test_config()
  pub <- withr::local_tempfile(fileext = ".pub")
  writeLines("ssh-ed25519 AAAA test", pub)
  config$ssh_public_key <- pub
  sent <- NULL
  local_mocked_bindings(rp_http = function(method, path, body = NULL) {
    sent <<- body
    list(status = 201L, body = list(id = "abc", costPerHr = 0.24))
  })
  cand <- list(label = "L", spec = list(flavor = "cpu3c", vcpus = 8L, data_center = "EUR-IS-1"))
  rp_create(cand, config, "n")
  expect_equal(sent$env$CT_ORPHAN_MINUTES, "0") # a single large machine keeps running while you are away
  config$orphan_minutes <- 30
  rp_create(cand, config, "n")
  expect_equal(sent$env$CT_ORPHAN_MINUTES, "30")
})

test_that("orphan_minutes is validated", {
  config <- ct_default_config()
  config$orphan_minutes <- -1
  expect_error(ct_validate_config(config), "orphan_minutes")
  config$orphan_minutes <- 0
  expect_no_error(ct_validate_config(config))
  config$orphan_minutes <- NA_real_
  expect_no_error(ct_validate_config(config))
})

test_that("ct_pid_alive tells running processes from finished ones", {
  expect_true(ct_pid_alive(Sys.getpid()))
  p <- processx::process$new("true")
  p$wait()
  expect_false(ct_pid_alive(p$get_pid()))
})

test_that("the pod start script has valid syntax and schedules both safety nets", {
  script <- system.file("pod-start.sh", package = "cloudtargets")
  skip_if(script == "")
  expect_equal(system2("bash", c("-n", script)), 0L)
  text <- paste(readLines(script), collapse = "\n")
  expect_match(text, "CT_ORPHAN_MINUTES", fixed = TRUE)
  expect_match(text, "ClientAliveInterval", fixed = TRUE)
  expect_false(grepl("RUNPOD_API_KEY[^\"]*\\$\\{?RUNPOD_API_KEY\\}? *\\|", text)) # key never on a command line
})

test_that("the watchdog is started from a shell in its own session, away from the controller", {
  cmd <- ct_watchdog_command("/usr/lib/R/bin/Rscript", "/tmp/x y/watchdog.log", setsid = "/usr/bin/setsid", perl = "")
  expect_match(cmd, "^'/usr/bin/setsid' '/usr/lib/R/bin/Rscript' --no-save", perl = TRUE)
  expect_match(cmd, "ct_watchdog_main", fixed = TRUE)
  expect_match(cmd, "'/tmp/x y/watchdog.log' 2>&1 < /dev/null &$", perl = TRUE)
  cmd <- ct_watchdog_command("R", "log", setsid = "", perl = "/usr/bin/perl")
  expect_match(cmd, "POSIX::setsid", fixed = TRUE)
  cmd <- ct_watchdog_command("R", "log", setsid = "", perl = "")
  expect_match(cmd, "^'R' --no-save", perl = TRUE) # no isolation available
})
