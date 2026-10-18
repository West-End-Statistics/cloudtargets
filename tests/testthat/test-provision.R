test_that("walks past capacity failures to the first available candidate", {
  fp <- fake_provider(c(
    "cpu3c x 16 vCPU in A" = "capacity",
    "cpu3c x 8 vCPU in A" = "capacity",
    "cpu3c x 8 vCPU in B" = "ok"
  ))
  config <- test_config()
  state <- suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))
  expect_equal(state$label, "cpu3c x 8 vCPU in B")
  expect_equal(state$status, "ready")
  expect_length(fp$env$deleted, 0L)
  expect_true(file.exists(ct_state_path(config)))
})

test_that("deletes an unreachable machine and skips that data center", {
  fp <- fake_provider(c(
    "cpu3c x 8 vCPU in A" = "unreachable",
    "cpu3c x 4 vCPU in A" = "ok",
    "cpu3c x 4 vCPU in B" = "ok"
  ))
  config <- test_config()
  state <- suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))
  expect_equal(state$label, "cpu3c x 4 vCPU in B")
  expect_equal(fp$env$deleted, "pod1")
  expect_length(fp$env$created, 2L)
})

test_that("an exhausted ladder errors clearly and leaves nothing running", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "capacity", "cpu3c x 4 vCPU in A" = "capacity"))
  config <- test_config()
  err <- expect_error(
    suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env))),
    "every candidate failed"
  )
  expect_match(conditionMessage(err), "Nothing is left running", fixed = TRUE)
  expect_match(conditionMessage(err), "no capacity", fixed = TRUE)
  expect_length(fp$env$pods, 0L)
  expect_false(file.exists(ct_state_path(config)))
})

test_that("an error while waiting deletes the in-flight machine", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"), describe_error_for = "pod1")
  config <- test_config()
  expect_error(
    suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env))),
    "api exploded"
  )
  expect_equal(fp$env$deleted, "pod1")
  expect_length(fp$env$pods, 0L)
  expect_false(file.exists(ct_state_path(config)))
})

test_that("refuses to provision twice unless forced", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  config <- test_config()
  suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))
  expect_error(
    suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env))),
    "already recorded"
  )
  expect_no_error(
    suppressMessages(ct_provision(config, fp$provider, force = TRUE, hooks = fake_hooks(fp$env)))
  )
})

test_that("a failed preflight stops before anything is created", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  fp$provider$preflight <- function(config) list(list(ok = FALSE, check = "API key", detail = "missing"))
  expect_error(
    suppressMessages(ct_provision(test_config(), fp$provider, hooks = fake_hooks(fp$env))),
    "Preflight failed"
  )
  expect_length(fp$env$created, 0L)
})

test_that("teardown deletes, verifies, archives and reports", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  config <- test_config()
  suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))
  out <- suppressMessages(ct_teardown(config, fp$provider, fetch = FALSE))
  expect_equal(fp$env$deleted, "pod1")
  expect_false(file.exists(ct_state_path(config)))
  expect_true(file.exists(ct_state_path(config, last = TRUE)))
  expect_true(is.numeric(out$cost))
})

test_that("teardown errors if the provider still lists the machine", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  config <- test_config()
  suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))
  fp$provider$delete <- function(id) invisible(TRUE)
  expect_error(suppressMessages(ct_teardown(config, fp$provider, fetch = FALSE)), "still listed")
  expect_true(file.exists(ct_state_path(config)))
})

test_that("teardown with no state is a no-op", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  expect_null(suppressMessages(ct_teardown(test_config(), fp$provider)))
})

test_that("cleanup needs yes = TRUE when non-interactive, then deletes strays", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  config <- test_config()
  suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))
  expect_error(suppressMessages(ct_cleanup(config, fp$provider)), "yes = TRUE")
  ids <- suppressMessages(ct_cleanup(config, fp$provider, yes = TRUE))
  expect_equal(ids, "pod1")
  expect_length(fp$env$pods, 0L)
  expect_false(file.exists(ct_state_path(config)))
})

test_that("status reports a machine the provider no longer lists", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  config <- test_config()
  suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))
  fp$env$pods <- list()
  out <- suppressMessages(ct_status(config, fp$provider))
  expect_equal(out$status, "gone")
})

test_that("a never-scheduled machine gets an accurate hint, not 'check your SSH key'", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "unreachable"))
  fp$provider$describe <- function(id) list(status = "RUNNING", host = NULL, port = NULL, cost_per_hr = 0.12)
  err <- expect_error(
    suppressMessages(ct_provision(test_config(), fp$provider, hooks = fake_hooks(fp$env))),
    "every candidate failed"
  )
  expect_match(conditionMessage(err), "never scheduled", fixed = TRUE)
  expect_equal(fp$env$deleted, "pod1")
})

test_that("teardown fetches before deleting, and keeps the machine if the fetch fails", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  config <- test_config()
  suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))

  calls <- character()
  ok_fetch <- function(config) { calls <<- c(calls, "fetch", paste("deleted:", length(fp$env$deleted))); invisible() }
  failing <- function(config) stop("connection refused")

  err <- expect_error(
    suppressMessages(ct_teardown(config, fp$provider, fetcher = failing)),
    "Could not fetch results"
  )
  expect_match(conditionMessage(err), "still running", fixed = TRUE)
  expect_length(fp$env$deleted, 0L)
  expect_true(file.exists(ct_state_path(config)))

  suppressMessages(ct_teardown(config, fp$provider, fetcher = ok_fetch))
  expect_equal(calls, c("fetch", "deleted: 0"))  # fetched while the machine still existed
  expect_equal(fp$env$deleted, "pod1")
})

test_that("force deletes even when the fetch fails", {
  fp <- fake_provider(c("cpu3c x 8 vCPU in A" = "ok"))
  config <- test_config()
  suppressMessages(ct_provision(config, fp$provider, hooks = fake_hooks(fp$env)))
  suppressMessages(ct_teardown(config, fp$provider, force = TRUE, fetcher = function(config) stop("down")))
  expect_equal(fp$env$deleted, "pod1")
})
