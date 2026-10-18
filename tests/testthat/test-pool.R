start_pool <- function(fp, n_slots, config) {
  pool <- ct_pool_new(config, fp$provider, fake_pool_hooks(fp$env), n_slots)
  suppressMessages(ct_pool_start(pool, port = 5555L))
  pool
}

test_that("a pool covers the requested slots, sizing the last machine to what is left", {
  fp <- fake_provider(c("cpu3m x 16 vCPU in A" = "ok", "cpu3m x 8 vCPU in A" = "ok", "cpu3m x 4 vCPU in A" = "ok"))
  pool <- start_pool(fp, 20L, test_config())
  sizes <- vapply(pool$machines, function(m) m$vcpus, 1L)
  expect_equal(sort(sizes), c(4L, 16L))
  expect_equal(ct_pool_slots(pool), 20L)
  expect_setequal(fp$env$prepared, vapply(pool$machines, function(m) m$id, ""))
  expect_length(fp$env$tunnels, 2L)
})

test_that("more workers means more machines (one number scales the pool)", {
  mk <- function(n) {
    fp <- fake_provider(c("cpu3m x 16 vCPU in A" = "ok"))
    length(start_pool(fp, n, test_config())$machines)
  }
  expect_equal(c(mk(16L), mk(32L), mk(64L)), c(1L, 2L, 4L))
})

test_that("capacity failures fall through the ladder inside the pool", {
  fp <- fake_provider(c("cpu3m x 16 vCPU in A" = "capacity", "cpu3m x 8 vCPU in A" = "ok"))
  pool <- start_pool(fp, 16L, test_config())
  expect_equal(vapply(pool$machines, function(m) m$vcpus, 1L), c(8L, 8L))
})

test_that("an unreachable machine is deleted, its site avoided, and replaced", {
  fp <- fake_provider(c("cpu3m x 8 vCPU in A" = "unreachable", "cpu3m x 8 vCPU in B" = "ok"))
  pool <- start_pool(fp, 8L, test_config())
  expect_equal(fp$env$deleted, "pod1")
  expect_equal(pool$machines[[1]]$label, "cpu3m x 8 vCPU in B")
})

test_that("if the pool cannot be completed everything created is deleted", {
  fp <- fake_provider(c("cpu3m x 8 vCPU in A" = "ok", "cpu3m x 4 vCPU in A" = "capacity"))
  config <- test_config()
  pool <- ct_pool_new(config, fp$provider, fake_pool_hooks(fp$env), 12L)
  expect_error(suppressMessages(ct_pool_start(pool, 5555L)), "every candidate failed")
  expect_length(fp$env$pods, 0L)
  expect_true(fp$env$tunnels[["pod1"]]$killed %||% TRUE)
})

test_that("a failure while preparing a machine deletes the whole pool", {
  fp <- fake_provider(c("cpu3m x 8 vCPU in A" = "ok"))
  hooks <- fake_pool_hooks(fp$env)
  hooks$prepare <- function(config, state) stop("bootstrap exploded")
  pool <- ct_pool_new(test_config(), fp$provider, hooks, 16L)
  expect_error(suppressMessages(ct_pool_start(pool, 5555L)), "bootstrap exploded")
  expect_length(fp$env$pods, 0L)
})

test_that("stopping a pool kills tunnels, deletes machines, and is idempotent", {
  fp <- fake_provider(c("cpu3m x 8 vCPU in A" = "ok"))
  pool <- start_pool(fp, 16L, test_config())
  ids <- vapply(pool$machines, function(m) m$id, "")
  expect_true(file.exists(ct_pool_path(pool$config)))
  suppressMessages(ct_pool_stop(pool))
  expect_setequal(fp$env$deleted, ids)
  expect_true(all(vapply(fp$env$tunnels, function(t) t$killed, TRUE)))
  expect_false(file.exists(ct_pool_path(pool$config)))
  expect_length(fp$env$pods, 0L)
  suppressMessages(ct_pool_stop(pool))
  expect_setequal(fp$env$deleted, ids)
})

test_that("workers are placed on the least busy machine and the pool refuses to overfill", {
  fp <- fake_provider(c("cpu3m x 2 vCPU in A" = "ok"))
  pool <- start_pool(fp, 4L, test_config())
  expect_length(pool$machines, 2L)
  fake_worker <- function(args) list(is_alive = function() TRUE, kill = function() NULL, args = args)
  for (i in 1:4) ct_pool_launch(pool, "crew::crew_worker()", fake_worker)
  per_host <- table(vapply(pool$machines, function(m) length(m$workers), 1L))
  expect_equal(as.integer(names(per_host)), 2L)
  expect_error(ct_pool_launch(pool, "x", fake_worker), "No free worker slots")
})

test_that("the worker command reuses the tunnel connection and runs the call in the project dir", {
  config <- test_config()
  config$remote_dir <- "/work/my proj"
  state <- list(id = "abc123def456xyz", host = "1.2.3.4", port = 2222L, user = "dev")
  args <- ct_worker_args(state, config, "crew::crew_worker(settings = list(a = 'b'))")
  expect_true("-tt" %in% args)
  expect_equal(args[match("-S", args) + 1L], "/tmp/ct-abc123def456.sock")
  expect_true("dev@1.2.3.4" %in% args)
  script <- rawToChar(jsonlite::base64_dec(sub("^.*echo ([A-Za-z0-9+/=]+) \\|.*$", "\\1", args[length(args)])))
  expect_match(script, "cd '/work/my proj'", fixed = TRUE)
  expect_match(script, "exec Rscript -e 'crew::crew_worker(settings = list(a = '\\''b'\\''))'", fixed = TRUE)
})

test_that("the controller helper validates its input and builds a crew controller", {
  expect_error(ct_crew_controller(workers = 0L, config = ct_default_config(), provider = ct_provider_runpod(), hooks = list()), "positive")
  ctl <- ct_crew_controller(workers = 6L, config = ct_default_config(), provider = ct_provider_runpod(), hooks = list())
  expect_s3_class(ctl$launcher, "ct_launcher")
  expect_equal(ctl$launcher$workers, 6L)
})

test_that("the controller refuses targets' shared-file-system storage defaults before creating anything", {
  skip_if_not_installed("targets")
  withr::defer(targets::tar_option_reset())
  targets::tar_option_set(storage = "worker", retrieval = "worker")
  err <- expect_error(ct_check_targets_storage(), "shared|file system")
  expect_match(conditionMessage(err), "storage = \"main\"", fixed = TRUE)
  expect_match(conditionMessage(err), "No machines were created", fixed = TRUE)
  targets::tar_option_set(storage = "main", retrieval = "main")
  expect_true(ct_check_targets_storage())
})
