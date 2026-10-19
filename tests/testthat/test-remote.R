test_that("progress output is parsed into counts, exit code and log", {
  out <- "EXIT none\nCOUNTS 0 8 120 0 0 \nLOG\nline a\nline b\n"
  p <- ct_parse_progress(out)
  expect_false(p$finished)
  expect_true(is.na(p$exit_code))
  expect_equal(p$counts[["completed"]], 120L)
  expect_equal(p$counts[["dispatched"]], 8L)
  expect_equal(p$log, c("line a", "line b"))
})

test_that("a finished run reports its exit code; a missing store gives no counts", {
  p <- ct_parse_progress("EXIT 1\nLOG\nError in foo()\n")
  expect_true(p$finished)
  expect_equal(p$exit_code, 1L)
  expect_null(p$counts)
  expect_equal(p$log, "Error in foo()")
})

test_that("shell quoting survives single quotes", {
  expect_equal(ct_shell_quote("it's"), "'it'\\''s'")
  out <- system2("bash", c("-c", shQuote(paste("printf %s", ct_shell_quote("a'b $x")))), stdout = TRUE)
  expect_equal(out, "a'b $x")
})

test_that("base64 wrapper round-trips arbitrary commands", {
  cmd <- "echo 'hi' && echo \"$HOME\" # \u00e9"
  decoded <- rawToChar(jsonlite::base64_dec(ct_b64(cmd)))
  Encoding(decoded) <- "UTF-8"
  expect_equal(decoded, cmd)
})

test_that("package list unions config, DESCRIPTION and targets/crew, minus base", {
  dir <- withr::local_tempdir()
  writeLines(c(
    "Package: x", "Version: 1", "Depends: R (>= 4.1), dplyr",
    "Imports: stats, ggplot2 (>= 3.0), tools"
  ), file.path(dir, "DESCRIPTION"))
  config <- ct_default_config()
  config$r_packages <- list("qs2")
  expect_equal(ct_r_packages(config, dir), c("crew", "dplyr", "ggplot2", "qs2", "targets"))
  config$r_packages_exclude <- list("dplyr")
  expect_equal(ct_r_packages(config, dir), c("crew", "ggplot2", "qs2", "targets"))
  config$r_packages_exclude <- list()
  config$r_packages_from_description <- FALSE
  expect_equal(ct_r_packages(config, dir), c("crew", "qs2", "targets"))
})

test_that("remote operations refuse to run without a ready machine", {
  config <- test_config()
  expect_error(ct_progress(config), "ct_provision")
  expect_error(ct_sync(config), "ct_provision")
  expect_error(ct_launch(config), "ct_provision")
})

test_that("base64 is one line even for long commands", {
  long <- paste(rep("echo 'some fairly long line of shell script'", 40), collapse = "\n")
  enc <- ct_b64(long)
  expect_false(grepl("[\r\n]", enc))
  expect_equal(rawToChar(jsonlite::base64_dec(enc)), long)
})

test_that("the pod start script keeps the container env but filters secrets", {
  script <- paste(readLines(system.file("pod-start.sh", package = "cloudtargets")), collapse = "\n")
  expect_match(script, "export -p", fixed = TRUE)
  expect_match(script, "RUNPOD_", fixed = TRUE)
  expect_match(script, "KEY|SECRET|TOKEN|PASSWORD", fixed = TRUE)
  # Run the env-capture pipeline on a fake environment and check the output.
  filter <- regmatches(script, regexpr("export -p.*?/etc/profile.d/zz-container-env.sh", script))
  expect_length(filter, 1L)
  out <- system2("bash", c("-c", shQuote(paste(
    "env -i PATH=/bin:/usr/bin CMDSTAN=/opt/cmdstan RUNPOD_API_KEY=abc PUBLIC_KEY=pk MY_SECRET_TOKEN=x HOME=/h",
    "bash -c", shQuote(sub("\\| \\$\\{sudo_cmd\\} tee .*$", "", gsub("\\\\\n", " ", filter)))
  ))), stdout = TRUE)
  expect_true(any(grepl("export CMDSTAN=", out)))
  expect_false(any(grepl("RUNPOD_API_KEY|PUBLIC_KEY|SECRET|HOME", out)))
})

test_that("monitor gives up after repeated failures and says the machine is still running", {
  config <- test_config()
  ct_state_write(config, list(id = "pod1", host = "h", port = 22L, user = "u", status = "ready",
                              cost_per_hr = 0.24, started_at = "2026-01-01T00:00:00Z"))
  local_mocked_bindings(ct_progress = function(...) stop("ssh: connection timed out"))
  err <- expect_error(ct_monitor(config, interval = 0, max_failures = 3L), "Lost contact")
  expect_match(conditionMessage(err), "NOT stopped", fixed = TRUE)
})

test_that("monitor fails immediately when no machine is recorded", {
  expect_error(ct_monitor(test_config(), interval = 0), class = "ct_error_no_machine")
})

test_that("memory and OOM-kill info is parsed and formatted", {
  out <- paste(c(
    "EXIT none", "COUNTS 0 8 14 0 0 ", "MEM 64000 52000",
    "CG 17179869184 9663676416 3", "LOG", "x"
  ), collapse = "\n")
  p <- ct_parse_progress(out)
  expect_equal(p$memory$total_mb, 64000)
  expect_equal(p$memory$limit_mb, 16384)
  expect_equal(p$memory$used_mb, 9216)
  expect_equal(p$memory$oom_kills, 3L)
  expect_match(ct_format_memory(p$memory), "mem 9.0/16 GB, OOM KILLS 3", fixed = TRUE)
})

test_that("an unlimited cgroup falls back to machine memory; missing info is tolerated", {
  p <- ct_parse_progress("EXIT none\nMEM 32000 20000\nCG max na na\nLOG\n")
  expect_true(is.na(p$memory$limit_mb))
  expect_equal(ct_format_memory(p$memory), " | mem 11.7/31 GB")
  expect_null(ct_parse_progress("EXIT none\nLOG\n")$memory)
  expect_equal(ct_format_memory(NULL), "")
})

test_that("the runner script exports env, runs the command and records the exit code", {
  script <- ct_runner_script("/work/proj", "Rscript -e 'targets::tar_make()'", c(CLOUDTARGETS_VCPUS = "32", NOTE = "it's"))
  expect_match(script, "cd '/work/proj'", fixed = TRUE)
  expect_match(script, "export CLOUDTARGETS_VCPUS='32'", fixed = TRUE)
  expect_match(script, "export NOTE='it'\\''s'", fixed = TRUE)
  expect_match(script, "( Rscript -e 'targets::tar_make()' ) > .cloudtargets-run.log 2>&1", fixed = TRUE)
  expect_match(script, "echo $? > .cloudtargets-exit", fixed = TRUE)
  # It really runs under bash and passes env through.
  dir <- withr::local_tempdir()
  runner <- ct_runner_script(dir, "echo \"v=$CLOUDTARGETS_VCPUS\"", c(CLOUDTARGETS_VCPUS = "32"))
  f <- withr::local_tempfile(fileext = ".sh"); writeLines(runner, f)
  system2("bash", f)
  expect_equal(readLines(file.path(dir, ".cloudtargets-run.log")), "v=32")
  expect_equal(readLines(file.path(dir, ".cloudtargets-exit")), "0")
})

test_that("ct_poll runs tar_poll in the remote project directory over a tty", {
  config <- test_config()
  config$remote_dir <- "/work/my proj"
  state <- list(id = "abc", host = "1.2.3.4", port = 2222L, user = "dev")
  args <- ct_poll_args(state, config, interval = 2, timeout = 30)
  expect_true("-t" %in% args)
  expect_true("dev@1.2.3.4" %in% args)
  expect_equal(args[match("-p", args) + 1L], "2222")
  remote <- args[length(args)]
  # The remote shell sees `bash -c '<command>'`; let bash unquote it and check the command itself.
  script <- withr::local_tempfile(fileext = ".sh")
  writeLines(c(paste("set --", remote), "printf %s \"$3\""), script)
  inner <- system2("bash", script, stdout = TRUE)
  expect_match(remote, "^bash -c ")
  expect_match(inner, "cd '/work/my proj' && Rscript -e", fixed = TRUE)
  expect_match(inner, "targets::tar_poll(interval = 2, timeout = 30)", fixed = TRUE)
  expect_true(startsWith(inner, "[ -f /etc/profile.d/zz-container-env.sh ]"))
  expect_match(ct_poll_args(state, config)[length(ct_poll_args(state, config))], "timeout = Inf", fixed = TRUE)
})

test_that("the pod start script lifts sshd's 10-sessions-per-connection limit before starting sshd", {
  script <- paste(readLines(system.file("pod-start.sh", package = "cloudtargets")), collapse = "\n")
  expect_match(script, "MaxSessions 512", fixed = TRUE)
  expect_match(script, "sshd_config.d/10-cloudtargets.conf", fixed = TRUE)
  limit_at <- regexpr("MaxSessions 512", script, fixed = TRUE)
  sshd_at <- regexpr("exec ${sudo_cmd} /usr/sbin/sshd", script, fixed = TRUE)
  expect_lt(limit_at, sshd_at)
})
