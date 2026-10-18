write_dockerfile <- function(lines) {
  path <- withr::local_tempfile(fileext = "Dockerfile", .local_envir = parent.frame())
  writeLines(lines, path)
  path
}

test_that("image, user and install2.r packages come from a multi-stage Dockerfile", {
  path <- write_dockerfile(c(
    "ARG BASE_TAG=r4.5.3-20260528",
    "FROM example/rstats-dev:${BASE_TAG} AS base",
    "USER root",
    "# comment",
    "RUN install2.r --error \\",
    "  qs2 rstanarm \\",
    "  metalite.ae",
    "ENV FOO=bar",
    "USER vscode",
    "FROM base AS extras",
    "RUN apt-get update && install2.r --ncpus 4 jsonlite && rm -rf /tmp/x"
  ))
  info <- ct_dockerfile_info(path, "base")
  expect_equal(info$image, "example/rstats-dev:r4.5.3-20260528")
  expect_equal(info$packages, c("qs2", "rstanarm", "metalite.ae"))
  expect_equal(info$user, "vscode")

  inherited <- ct_dockerfile_info(path, "extras")
  expect_equal(inherited$image, "example/rstats-dev:r4.5.3-20260528")
  expect_equal(inherited$packages, c("qs2", "rstanarm", "metalite.ae", "jsonlite"))
  expect_equal(inherited$user, "vscode")

  expect_equal(ct_dockerfile_info(path)$packages, inherited$packages)
})

test_that("a Dockerfile without install2.r or USER yields just the image", {
  info <- ct_dockerfile_info(write_dockerfile(c("FROM rocker/r-ver:4.5.0", "RUN echo hi")))
  expect_equal(info$image, "rocker/r-ver:4.5.0")
  expect_length(info$packages, 0L)
  expect_null(info$user)
})

test_that("bad Dockerfile inputs give clear errors", {
  expect_error(ct_dockerfile_info("/no/such/Dockerfile"), "does not exist")
  expect_error(ct_dockerfile_info(write_dockerfile("RUN echo hi")), "FROM")
  expect_error(ct_dockerfile_info(write_dockerfile("FROM a:1"), "nope"), "not found")
})

test_that("config fills image, user and packages from the Dockerfile unless set", {
  dir <- withr::local_tempdir()
  docker <- file.path(dir, "Dockerfile")
  writeLines(c("FROM example/img:1 AS base", "RUN install2.r qs2", "USER dev"), docker)
  yml <- file.path(dir, "_cloudtargets.yml")

  writeLines(c(paste0("dockerfile: ", docker), "dockerfile_target: base"), yml)
  config <- ct_config(yml)
  expect_equal(config$image, "example/img:1")
  expect_equal(config$ssh_user, "dev")
  expect_equal(unlist(config$dockerfile_packages), "qs2")
  expect_true("qs2" %in% ct_r_packages(config, dir))

  writeLines(c(paste0("dockerfile: ", docker), "image: my/own:2", "ssh_user: root"), yml)
  config <- ct_config(yml)
  expect_equal(config$image, "my/own:2")
  expect_equal(config$ssh_user, "root")
})
