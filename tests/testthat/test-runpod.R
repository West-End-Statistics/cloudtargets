test_that("config file overrides defaults and replaces lists", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "_cloudtargets.yml")
  writeLines(c("ready_timeout: 99", "runpod:", "  data_centers: [EU-RO-1]", "  min_vcpus: 8"), path)
  config <- ct_config(path)
  expect_equal(config$ready_timeout, 99)
  expect_equal(unlist(config$runpod$data_centers), "EU-RO-1")
  expect_equal(config$runpod$min_vcpus, 8L)
  expect_equal(unlist(config$runpod$flavors), "cpu3c")
})

test_that("a bad config value is rejected", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "_cloudtargets.yml")
  writeLines("ready_timeout: -5", path)
  expect_error(ct_config(path), "ready_timeout")
})

test_that("candidates go largest first and respect min_vcpus", {
  config <- ct_default_config()
  config$runpod$data_centers <- list("A", "B")
  config$runpod$vcpus <- list(2L, 16L, 8L, 4L)
  config$runpod$min_vcpus <- 4L
  labels <- vapply(rp_candidates(config), `[[`, "", "label")
  expect_equal(labels[1:2], c("cpu3c x 16 vCPU in A", "cpu3c x 16 vCPU in B"))
  expect_false(any(grepl("x 2 vCPU", labels)))
  expect_length(labels, 6L)
})

test_that("RunPod 'no instances' is classified as retryable capacity", {
  config <- test_config()
  pub <- withr::local_tempfile(fileext = ".pub")
  writeLines("ssh-ed25519 AAAA test", pub)
  config$ssh_public_key <- pub
  local_mocked_bindings(rp_http = function(method, path, body = NULL) {
    list(status = 500L, body = list(error = "create pod: There are no longer any instances available with the requested specifications."))
  })
  cand <- list(label = "cpu3c x 8 vCPU in A", spec = list(flavor = "cpu3c", vcpus = 8L, data_center = "A"))
  expect_error(rp_create(cand, config, "x"), class = "ct_error_capacity")
})

test_that("other RunPod errors are fatal, and bad keys say so", {
  config <- test_config()
  pub <- withr::local_tempfile(fileext = ".pub")
  writeLines("ssh-ed25519 AAAA test", pub)
  config$ssh_public_key <- pub
  cand <- list(label = "L", spec = list(flavor = "cpu3c", vcpus = 8L, data_center = "A"))
  local_mocked_bindings(rp_http = function(...) list(status = 400L, body = list(error = "bad request shape")))
  err <- expect_error(rp_create(cand, config, "x"), "HTTP 400")
  expect_false(inherits(err, "ct_error_capacity"))
  local_mocked_bindings(rp_http = function(...) list(status = 401L, body = NULL))
  expect_error(rp_create(cand, config, "x"), "rejected the API key")
})

test_that("create sends a CPU pod request pinned to the data center", {
  config <- test_config()
  pub <- withr::local_tempfile(fileext = ".pub")
  writeLines("ssh-ed25519 AAAA test", pub)
  config$ssh_public_key <- pub
  sent <- NULL
  local_mocked_bindings(rp_http = function(method, path, body = NULL) {
    sent <<- list(method = method, path = path, body = body)
    list(status = 201L, body = list(id = "abc", costPerHr = 0.24))
  })
  cand <- list(label = "L", spec = list(flavor = "cpu3c", vcpus = 8L, data_center = "EUR-IS-1"))
  pod <- rp_create(cand, config, "cloudtargets-1")
  expect_equal(pod$id, "abc")
  expect_equal(pod$cost_per_hr, 0.24)
  expect_equal(sent$body$computeType, "CPU")
  expect_equal(sent$body$dataCenterIds, list("EUR-IS-1"))
  expect_equal(sent$body$ports, list("22/tcp"))
  expect_equal(sent$body$env$PUBLIC_KEY, "ssh-ed25519 AAAA test")
  expect_equal(sent$body$env$SSH_USER, "root")
  # Default: the image does not run sshd, so the start script is injected.
  expect_equal(sent$body$dockerEntrypoint, list("bash", "-c"))
  expect_match(sent$body$dockerStartCmd[[1]], "openssh-server", fixed = TRUE)

  config$ssh_setup <- "image"
  rp_create(cand, config, "cloudtargets-1")
  expect_null(sent$body$dockerEntrypoint)
})

test_that("describe returns no host until both IP and SSH port exist", {
  local_mocked_bindings(rp_http = function(...) {
    list(status = 200L, body = list(desiredStatus = "RUNNING", publicIp = "1.2.3.4", portMappings = list(), costPerHr = 0.1))
  })
  expect_null(rp_describe("x")$host)
  local_mocked_bindings(rp_http = function(...) {
    list(status = 200L, body = list(desiredStatus = "RUNNING", publicIp = "1.2.3.4", portMappings = list(`22` = 22190L), costPerHr = 0.1))
  })
  info <- rp_describe("x")
  expect_equal(info$host, "1.2.3.4")
  expect_equal(info$port, 22190L)
})

test_that("delete treats 404 as already gone", {
  local_mocked_bindings(rp_http = function(...) list(status = 404L, body = NULL))
  expect_true(rp_delete("x"))
  local_mocked_bindings(rp_http = function(...) list(status = 500L, body = list(error = "boom")))
  expect_error(rp_delete("x"), "HTTP 500")
})

test_that("list filters by name prefix", {
  local_mocked_bindings(rp_http = function(...) {
    list(status = 200L, body = list(
      list(id = "1", name = "cloudtargets-a", costPerHr = 0.1),
      list(id = "2", name = "other-pod", costPerHr = 0.5)
    ))
  })
  pods <- rp_list("cloudtargets")
  expect_length(pods, 1L)
  expect_equal(pods[[1]]$id, "1")
})

test_that("rp_http copes with an empty 204 body (DELETE) and parses JSON bodies", {
  withr::local_envvar(RUNPOD_API_KEY = "test-key")
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 204L))
  res <- rp_http("DELETE", "/pods/x")
  expect_equal(res$status, 204L)
  expect_null(res$body)

  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 200L, body = list(id = "abc"))
  })
  res <- rp_http("GET", "/pods/x")
  expect_equal(res$body$id, "abc")
})

test_that("a missing API key is a clear error", {
  withr::local_envvar(RUNPOD_API_KEY = "", HOME = withr::local_tempdir())
  expect_error(rp_http("GET", "/pods"), "No RunPod API key")
})
