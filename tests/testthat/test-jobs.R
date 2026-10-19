wait_for <- function(job, secs = 10) {
  deadline <- Sys.time() + secs
  while (identical(job$poll(), "running") && Sys.time() < deadline) Sys.sleep(0.05)
  job$status
}

test_that("a process job is running, then ok on exit 0", {
  skip_on_os("windows")
  job <- ct_job_process(processx::process$new("sleep", "0.3"))
  expect_equal(job$poll(), "running")
  expect_equal(wait_for(job), "ok")
  expect_true(is.na(job$error))
})

test_that("a failing process job reports the exit status and the log tail", {
  skip_on_os("windows")
  log <- withr::local_tempfile()
  proc <- processx::process$new("sh", c("-c", "echo first >&2; echo boom >&2; exit 3"), stderr = log)
  job <- ct_job_process(proc, log = log)
  expect_equal(wait_for(job), "failed")
  expect_match(job$error, "exited with status 3", fixed = TRUE)
  expect_match(job$error, "boom", fixed = TRUE)
})

test_that("a process job past its timeout is killed and fails", {
  skip_on_os("windows")
  clock <- new.env(); clock$t <- as.POSIXct("2026-01-01", tz = "UTC")
  proc <- processx::process$new("sleep", "30")
  withr::defer(try(proc$kill(), silent = TRUE))
  job <- ct_job_process(proc, timeout = 60, now = function() clock$t)
  expect_equal(job$poll(), "running")
  clock$t <- clock$t + 59
  expect_equal(job$poll(), "running")
  clock$t <- clock$t + 2
  expect_equal(job$poll(), "failed")
  expect_match(job$error, "timed out after 60 s", fixed = TRUE)
  expect_false(proc$is_alive())
})

test_that("ok_when finishes a job while its process keeps running", {
  skip_on_os("windows")
  flag <- new.env(); flag$ready <- FALSE
  proc <- processx::process$new("sleep", "30")
  withr::defer(try(proc$kill(), silent = TRUE))
  job <- ct_job_process(proc, ok_when = function() flag$ready)
  expect_equal(job$poll(), "running")
  flag$ready <- TRUE
  expect_equal(job$poll(), "ok")
  expect_true(proc$is_alive())
  expect_identical(job$value, proc)
})

test_that("with ok_when, a process that exits first is a failure", {
  skip_on_os("windows")
  proc <- processx::process$new("sh", c("-c", "exit 0"))
  job <- ct_job_process(proc, ok_when = function() FALSE)
  expect_equal(wait_for(job), "failed")
})

test_that("killing a job stops its process and is idempotent", {
  skip_on_os("windows")
  proc <- processx::process$new("sleep", "30")
  job <- ct_job_process(proc)
  job$kill(); job$kill()
  expect_false(proc$is_alive())
  expect_equal(job$status, "failed")
  expect_equal(job$error, "killed")
})

test_that("ct_job_now wraps synchronous functions, errors and checks", {
  expect_equal(ct_job_now(function() 42)$poll(), "ok")
  expect_equal(ct_job_now(function() 42)$value, 42)
  bad <- ct_job_now(function() stop("nope"))
  expect_equal(bad$poll(), "failed")
  expect_equal(bad$error, "nope")
  expect_equal(ct_job_now(function() TRUE, truthy = TRUE)$poll(), "ok")
  expect_equal(ct_job_now(function() FALSE, truthy = TRUE)$poll(), "failed")
})

test_that("a chain starts each step only after the previous one succeeds, and stops at the first failure", {
  log <- character()
  step <- function(name, outcome) function() {
    log <<- c(log, name)
    ct_job_now(function() if (outcome == "fail") stop(paste(name, "failed")) else name)
  }
  ok <- ct_job_chain(list(step("a", "ok"), step("b", "ok")))
  expect_equal(ok$poll(), "ok")
  expect_equal(log, c("a", "b"))

  log <- character()
  bad <- ct_job_chain(list(step("a", "ok"), step("b", "fail"), step("c", "ok")))
  expect_equal(bad$poll(), "failed")
  expect_equal(bad$error, "b failed")
  expect_equal(log, c("a", "b"))
})

test_that("a chain keeps running while a step runs, and kills the running step", {
  skip_on_os("windows")
  proc <- processx::process$new("sleep", "30")
  chain <- ct_job_chain(list(function() ct_job_process(proc)))
  expect_equal(chain$poll(), "running")
  chain$kill()
  expect_false(proc$is_alive())
})

test_that("a job step that throws becomes a failure, never an error", {
  job <- ct_job_new(step = function() stop("exploded"))
  expect_equal(job$poll(), "failed")
  expect_equal(job$error, "exploded")
})
