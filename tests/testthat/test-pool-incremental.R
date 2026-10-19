quiet_drive <- function(...) suppressMessages(drive(...))

pending_states <- function(pool) vapply(pool$pending, function(m) m$state, "")

test_that("(a) the first ready machine takes workers while another is still starting", {
  config <- test_config(); config$ready_timeout <- 900
  s <- inc_pool(list("cpu3c x 2 vCPU in A" = c("ok", "addr_after:600")), 4L, config)
  quiet_drive(s$pool, s$clock, 60)
  expect_length(s$pool$machines, 1L)
  expect_equal(ct_pool_free_slots(s$pool), 2L)
  expect_equal(ct_launch_cap(s$pool, 5L), 2L)
  expect_equal(unname(pending_states(s$pool)), "waiting_address")
  # The slow machine does not hold the ready one back, and joins later.
  quiet_drive(s$pool, s$clock, 700)
  expect_length(s$pool$machines, 2L)
  expect_equal(ct_launch_cap(s$pool, 5L), 4L)
  expect_length(s$pool$pending, 0L)
})

test_that("(b) a machine stuck for 15 minutes is deleted and replaced in another data center", {
  config <- test_config(); config$ready_timeout <- 900
  s <- inc_pool(c("cpu3c x 8 vCPU in A" = "unreachable", "cpu3c x 8 vCPU in B" = "ok"), 8L, config)
  quiet_drive(s$pool, s$clock, 899)
  expect_length(s$env$deleted, 0L)
  expect_equal(names(s$pool$pending), "pod1")
  quiet_drive(s$pool, s$clock, 25)
  expect_equal(s$env$deleted, "pod1")
  expect_true("A" %in% names(s$pool$cool))
  expect_equal(s$env$pods[["pod2"]]$label, "cpu3c x 8 vCPU in B")
  quiet_drive(s$pool, s$clock, 40)
  expect_equal(s$pool$machines[[1]]$label, "cpu3c x 8 vCPU in B")
})

test_that("(b) if every data center is cooling, the oldest cool-down is released so the pool cannot starve", {
  config <- test_config(); config$ready_timeout <- 100
  s <- inc_pool(c("cpu3c x 8 vCPU in A" = "unreachable"), 8L, config)
  quiet_drive(s$pool, s$clock, 130)
  expect_true(length(s$env$created) >= 2L)
  expect_true("pod1" %in% s$env$deleted)
})

test_that("(c) a partly filled order keeps trying with 30/60/120 s back-off and fills later", {
  config <- test_config(); config$ready_timeout <- 900
  plan <- list(
    "cpu3c x 8 vCPU in A" = c("ok", "capacity_until:600"),
    "cpu3c x 8 vCPU in B" = "capacity"
  )
  s <- inc_pool(plan, 16L, config)
  quiet_drive(s$pool, s$clock, 300)
  expect_true(s$pool$ever_ready)
  expect_length(s$pool$machines, 1L)
  expect_match(ct_pool_status_text(s$pool), "1 waiting for capacity", fixed = TRUE)
  quiet_drive(s$pool, s$clock, 500)
  times_a <- vapply(Filter(function(x) x$label == "cpu3c x 8 vCPU in A", s$env$create_log), function(x) x$t, 1)
  # first machine at 0 (then no capacity for the second, in the same tick), retries at 30/90/210/330/450/570, success at 690
  expect_equal(times_a[1:9], c(0, 0, 30, 90, 210, 330, 450, 570, 690))
  expect_length(s$pool$machines, 2L)
})

test_that("(d) with no machine ready for the give-up time, everything is deleted and the run stops", {
  config <- test_config(); config$pool_give_up_minutes <- 60
  s <- inc_pool(c("cpu3c x 8 vCPU in A" = "capacity"), 8L, config)
  quiet_drive(s$pool, s$clock, 3590, step = 10)
  expect_false(s$pool$stopped)
  err <- expect_error(quiet_drive(s$pool, s$clock, 20, step = 10), class = "ct_error_pool_giveup")
  expect_match(conditionMessage(err), "within 60 minutes", fixed = TRUE)
  expect_match(conditionMessage(err), "no capacity", fixed = TRUE)
  expect_true(s$pool$stopped)
  expect_length(s$env$pods, 0L)
})

test_that("(d) repeated setup failures before anything was ready stop early, with the reason", {
  config <- test_config(); config$ready_timeout <- 900
  s <- inc_pool(c("cpu3c x 8 vCPU in A" = "ok"), 8L, config, tweak = function(e) NULL)
  s$env$prepare_outcome <- "setup-broke"
  err <- expect_error(quiet_drive(s$pool, s$clock, 600, step = 5), class = "ct_error_pool_giveup")
  expect_match(conditionMessage(err), "failed 3 times", fixed = TRUE)
  expect_match(conditionMessage(err), "fake setup-broke", fixed = TRUE)
  expect_length(s$env$pods, 0L)
  expect_equal(length(s$env$deleted), 3L)
})

test_that("(d) once any machine has been ready the run never gives up for lack of capacity", {
  config <- test_config(); config$pool_give_up_minutes <- 60; config$ready_timeout <- 900
  s <- inc_pool(list("cpu3c x 8 vCPU in A" = c("ok", "capacity")), 16L, config)
  expect_no_error(quiet_drive(s$pool, s$clock, 10 * 3600, step = 30))
  expect_true(s$pool$ever_ready)
  expect_length(s$pool$machines, 1L)
  expect_false(s$pool$stopped)
})

test_that("(e) launches are capped to free slots on ready machines and are a no-op with none", {
  config <- test_config(); config$ready_timeout <- 900
  s <- inc_pool(list("cpu3c x 2 vCPU in A" = c("ok", "addr_after:600")), 4L, config)
  expect_equal(ct_launch_cap(s$pool, 4L), 0L)
  quiet_drive(s$pool, s$clock, 60)
  alive <- new.env(); alive$up <- TRUE
  worker <- function(args) list(is_alive = function() isTRUE(alive$up), kill = function() alive$up <- FALSE)
  ct_pool_launch(s$pool, "call", worker)
  expect_equal(ct_launch_cap(s$pool, 4L), 1L)
  ct_pool_launch(s$pool, "call", worker)
  expect_equal(ct_launch_cap(s$pool, 4L), 0L)
  alive$up <- FALSE # workers exit (e.g. idle): their slots are free again
  expect_equal(ct_launch_cap(s$pool, 4L), 2L)
})

test_that("(f) a tick never sleeps, describes at most twice, and turns failures into machine failures", {
  config <- test_config(); config$ready_timeout <- 900
  s <- inc_pool(c("cpu3c x 2 vCPU in A" = "addr_after:100000"), 40L, config)
  expect_length(s$pool$pending, 20L)
  before <- s$env$describes
  for (i in 1:30) {
    s$clock$advance(1)
    suppressMessages(ct_pool_tick(s$pool))
    expect_lte(s$env$describes - before, 2L)
    before <- s$env$describes
  }
})

test_that("(f) a hook that throws fails that machine, not the tick, and the pool recovers", {
  config <- test_config(); config$ready_timeout <- 900
  clock <- fake_clock()
  fp <- fake_provider2(c("cpu3c x 8 vCPU in A" = "ok", "cpu3c x 8 vCPU in B" = "ok"), clock)
  hooks <- fake_async_hooks(fp)
  real_prepare <- hooks$prepare_start
  calls <- new.env(); calls$n <- 0L
  hooks$prepare_start <- function(config, state) {
    calls$n <- calls$n + 1L
    if (calls$n == 1L) stop("hook boom")
    real_prepare(config, state)
  }
  pool <- ct_pool_new(config, fp$provider, hooks, 8L)
  suppressMessages(ct_pool_begin(pool, 5555L))
  expect_no_error(suppressMessages(drive(pool, clock, 60)))
  expect_true("pod1" %in% fp$env$deleted)
  expect_equal(pool$prepare_failures, 1L)
  expect_length(pool$machines, 1L)
})

test_that("(f) a provider call that throws for one machine fails that machine only", {
  config <- test_config(); config$ready_timeout <- 900
  s <- inc_pool(list("cpu3c x 2 vCPU in A" = c("ok", "ok")), 4L, config)
  real <- s$fp$provider$describe
  s$pool$provider$describe <- function(id) if (identical(id, "pod1")) stop("api hiccup") else real(id)
  expect_no_error(quiet_drive(s$pool, s$clock, 60))
  expect_true("pod1" %in% s$env$deleted)
})

test_that("(g) stopping the pool in every state kills jobs, deletes machines and is idempotent", {
  config <- test_config(); config$ready_timeout <- 900
  scenarios <- list(
    waiting_address = list(plan = "noaddr", secs = 3, tweak = NULL),
    checking_ssh = list(plan = "unreachable", secs = 3, tweak = NULL),
    preparing = list(plan = "ok", secs = 6, tweak = function(e) e$prepare_secs <- 1000),
    tunnelling = list(plan = "ok", secs = 60, tweak = function(e) e$tunnel_secs <- 1000),
    ready = list(plan = "ok", secs = 60, tweak = NULL)
  )
  for (nm in names(scenarios)) {
    sc <- scenarios[[nm]]
    s <- inc_pool(stats::setNames(list(sc$plan), "cpu3c x 8 vCPU in A"), 8L, config, tweak = sc$tweak)
    quiet_drive(s$pool, s$clock, sc$secs)
    if (nm != "ready") expect_equal(unname(pending_states(s$pool)), nm, info = nm) else expect_length(s$pool$machines, 1L)
    suppressMessages(ct_pool_stop(s$pool))
    expect_true(s$pool$stopped, info = nm)
    expect_length(s$env$pods, 0L)
    expect_false(file.exists(ct_pool_path(config)), info = nm)
    suppressMessages(ct_pool_stop(s$pool))
    expect_equal(s$env$deleted, "pod1", info = nm)
  }
})

test_that("(g) stopping with a failing delete is retried and keeps the ids until it works", {
  config <- test_config(); config$ready_timeout <- 900
  s <- inc_pool(c("cpu3c x 8 vCPU in A" = "ok"), 8L, config)
  quiet_drive(s$pool, s$clock, 60)
  s$env$delete_fails <- 1L
  suppressMessages(ct_pool_stop(s$pool))
  expect_false(s$pool$stopped)
  expect_length(s$env$pods, 1L)
  suppressMessages(ct_pool_stop(s$pool))
  expect_true(s$pool$stopped)
  expect_length(s$env$pods, 0L)
})

test_that("(h) a lost tunnel is reopened; a machine that keeps losing it is replaced", {
  config <- test_config(); config$ready_timeout <- 900
  s <- inc_pool(c("cpu3c x 8 vCPU in A" = "ok"), 8L, config)
  quiet_drive(s$pool, s$clock, 60)
  expect_length(s$pool$machines, 1L)
  expect_equal(ct_pool_free_slots(s$pool), 8L)
  s$env$tunnel_alive[["pod1"]] <- FALSE
  quiet_drive(s$pool, s$clock, 1)
  expect_length(s$pool$machines, 0L)
  expect_equal(ct_pool_free_slots(s$pool), 0L)
  quiet_drive(s$pool, s$clock, 10)
  expect_length(s$pool$machines, 1L)
  expect_equal(s$pool$machines[[1]]$reconnects, 1L)
  for (i in 1:3) {
    s$env$tunnel_alive[["pod1"]] <- FALSE
    quiet_drive(s$pool, s$clock, 12)
  }
  expect_true("pod1" %in% s$env$deleted)
  quiet_drive(s$pool, s$clock, 60)
  expect_equal(s$pool$machines[[1]]$id, "pod2")
})

test_that("status lines are throttled to one per interval and only when something changed", {
  config <- test_config(); config$ready_timeout <- 900; config$pool_status_interval <- 60
  s <- inc_pool(list("cpu3c x 2 vCPU in A" = c("ok", "addr_after:100000")), 4L, config)
  msgs <- testthat::capture_messages(drive(s$pool, s$clock, 300, step = 1))
  lines <- grep("machines ready", msgs, value = TRUE)
  expect_lte(length(lines), 5L)
  expect_gte(length(lines), 1L)
})

test_that("preparing is capped at pool_prepare_parallel machines at a time", {
  config <- test_config(); config$ready_timeout <- 900; config$pool_prepare_parallel <- 4L
  s <- inc_pool(c("cpu3c x 2 vCPU in A" = "ok"), 12L, config, tweak = function(e) e$prepare_secs <- 100)
  most <- 0L
  for (i in 1:60) {
    s$clock$advance(1)
    suppressMessages(ct_pool_tick(s$pool))
    running <- sum(vapply(s$env$prepare_jobs, function(j) identical(j$status, "running"), logical(1)))
    most <- max(most, running)
  }
  expect_equal(most, 4L)
})

test_that("a create request that fails outright stops the start, but later only warns", {
  config <- test_config(); config$ready_timeout <- 900
  clock <- fake_clock()
  fp <- fake_provider2(c("cpu3c x 8 vCPU in A" = "error"), clock)
  pool <- ct_pool_new(config, fp$provider, fake_async_hooks(fp), 8L)
  expect_error(suppressMessages(ct_pool_begin(pool, 5555L)), "boom")

  s <- inc_pool(list("cpu3c x 8 vCPU in A" = c("ok", "error")), 16L, config)
  quiet_drive(s$pool, s$clock, 30)
  results <- vapply(s$pool$attempts, function(a) a$result, "")
  expect_true(any(grepl("^error: create failed: boom", results)))
  expect_length(s$pool$machines, 1L)
})

test_that("the new pool settings have sensible defaults and are validated", {
  config <- ct_default_config()
  expect_equal(config$ready_timeout, 900)
  expect_equal(config$prepare_timeout, 900)
  expect_equal(config$pool_give_up_minutes, 60)
  expect_equal(config$pool_prepare_parallel, 4L)
  dir <- withr::local_tempdir()
  path <- file.path(dir, "_cloudtargets.yml")
  writeLines("pool_give_up_minutes: .inf", path)
  expect_equal(ct_config(path)$pool_give_up_minutes, Inf)
  writeLines("pool_backoff_min: -1", path)
  expect_error(ct_config(path), "pool_backoff_min")
  writeLines("prepare_timeout: soon", path)
  expect_error(ct_config(path), "prepare_timeout")
})

test_that("command builders produce the expected ssh, rsync and tunnel arguments", {
  config <- test_config(); config$remote_dir <- "/work/p q"; config$ssh_identity <- "/k/id"
  state <- list(id = "abcdef123456xyz", host = "1.2.3.4", port = 2200L, user = "dev")
  chk <- ct_ssh_check_args("1.2.3.4", 2200L, "dev", config)
  expect_equal(utils::tail(chk, 2), c("dev@1.2.3.4", "true"))
  expect_true("IdentitiesOnly=yes" %in% chk)
  tun <- ct_tunnel_args(state, config, 41233L)
  expect_equal(tun[match("-R", tun) + 1L], "41233:127.0.0.1:41233")
  expect_equal(tun[match("-S", tun) + 1L], "/tmp/ct-abcdef123456.sock")
  expect_true("ExitOnForwardFailure=yes" %in% tun && "-N" %in% tun && "-M" %in% tun)
  sync <- ct_sync_rsync_args(config, state)
  expect_true("--delete" %in% sync)
  expect_equal(utils::tail(sync, 1), "dev@1.2.3.4:/work/p q/")
  expect_match(ct_sync_mkdir_command(config), "mkdir -p '/work/p q'", fixed = TRUE)
  expect_match(ct_bootstrap_command(c("dplyr", "qs2")), "bootstrap-r.sh 'dplyr' 'qs2'", fixed = TRUE)
})

test_that("the real crew launcher starts quickly, caps launches to ready machines, and cleans up", {
  skip_if_not_installed("targets")
  withr::defer(targets::tar_option_reset())
  targets::tar_option_set(storage = "main", retrieval = "main")
  config <- test_config(); config$ready_timeout <- 900
  clock <- fake_clock()
  fp <- fake_provider2(c("cpu3c x 2 vCPU in A" = "ok"), clock)
  ctl <- ct_crew_controller(workers = 4L, config = config, provider = fp$provider, hooks = fake_async_hooks(fp))
  launcher <- ctl$launcher
  suppressMessages(launcher$start(url = "tcp://127.0.0.1:41233", profile = "p"))
  expect_false(is.null(launcher$pool))
  expect_length(launcher$pool$pending, 2L) # created at once, nothing ready yet
  # Nothing is ready, so asking for workers starts none and does not fail.
  expect_null(launcher$launch(4L))
  expect_equal(nrow(launcher$.__enclos_env__$private$.launches), 0L)
  suppressMessages(drive(launcher$pool, clock, 30))
  expect_equal(ct_launch_cap(launcher$pool, 4L), 4L)
  suppressMessages(launcher$terminate())
  expect_length(fp$env$pods, 0L)
})

test_that("machines of the same size are spread over data centers, unless spreading is off", {
  config <- test_config(); config$ready_timeout <- 900
  plan <- c("cpu3c x 2 vCPU in A" = "ok", "cpu3c x 2 vCPU in B" = "ok")
  s <- inc_pool(plan, 8L, config)
  sites <- vapply(s$pool$pending, function(m) m$site, "")
  expect_equal(sort(table(sites)[["A"]]), 2L)
  expect_equal(table(sites)[["B"]], 2L)
  config$pool_spread <- FALSE
  s2 <- inc_pool(plan, 8L, config)
  expect_equal(unique(vapply(s2$pool$pending, function(m) m$site, "")), "A")
})

test_that("a dead data center cannot stall a spread pool: the healthy machine works while the dead one is replaced", {
  config <- test_config(); config$ready_timeout <- 240
  s <- inc_pool(c("cpu3c x 2 vCPU in DEAD" = "unreachable", "cpu3c x 2 vCPU in OK" = "ok"), 4L, config)
  quiet_drive(s$pool, s$clock, 60)
  expect_length(s$pool$machines, 1L)
  expect_equal(s$pool$machines[[1]]$site, "OK")
  expect_equal(ct_launch_cap(s$pool, 4L), 2L) # work can start now
  quiet_drive(s$pool, s$clock, 200) # the dead machine passes its 240 s limit
  expect_true("pod1" %in% s$env$deleted)
  quiet_drive(s$pool, s$clock, 60)
  expect_length(s$pool$machines, 2L)
  expect_equal(ct_launch_cap(s$pool, 4L), 4L)
})
