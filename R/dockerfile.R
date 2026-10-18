#' Read the base image, user and R packages from a Dockerfile
#'
#' Lets a project reuse the Dockerfile it already develops in (for example a
#' devcontainer) instead of repeating it in the cloudtargets configuration.
#' Understood instructions: `ARG` defaults (for `${VAR}` in `FROM`), `FROM ...
#' AS`, `USER`, and `RUN install2.r pkg ...` (from the 'littler' tools). Build
#' stages that start `FROM` an earlier stage inherit that stage's packages.
#' Nothing is built or executed; the machine pulls the base image and the
#' packages are installed when it starts.
#'
#' @param path Path to the Dockerfile.
#' @param target Build stage name to resolve; the last stage by default.
#' @return A list with `image`, `packages` and `user` (`NULL` if no `USER`).
#' @export
ct_dockerfile_info <- function(path, target = NULL) {
  if (!file.exists(path)) {
    cli::cli_abort("Dockerfile {.file {path}} does not exist.")
  }
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- gsub("\\\\[ \t]*\r?\n", " ", text)
  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

  args <- list()
  stages <- list()
  for (line in lines) {
    ins <- toupper(sub("^(\\S+).*$", "\\1", line))
    rest <- trimws(sub("^\\S+\\s*", "", line))
    if (ins == "ARG") {
      kv <- strsplit(rest, "=", fixed = TRUE)[[1L]]
      if (length(kv) >= 2L) args[[kv[1L]]] <- gsub("^[\"']|[\"']$", "", paste(kv[-1L], collapse = "="))
    } else if (ins == "FROM") {
      parts <- strsplit(rest, "\\s+")[[1L]]
      parts <- parts[!startsWith(parts, "--")]
      stages[[length(stages) + 1L]] <- list(
        image = ct_dockerfile_subst(parts[1L], args),
        alias = if (length(parts) >= 3L && tolower(parts[2L]) == "as") parts[3L] else NA_character_,
        packages = character(), user = NULL
      )
    } else if (length(stages) > 0L) {
      i <- length(stages)
      if (ins == "RUN" && grepl("install2\\.r", rest)) {
        stages[[i]]$packages <- c(stages[[i]]$packages, ct_parse_install2r(rest))
      } else if (ins == "USER") {
        stages[[i]]$user <- sub(":.*$", "", strsplit(rest, "\\s+")[[1L]][1L])
      }
    }
  }
  if (length(stages) == 0L) {
    cli::cli_abort("No {.code FROM} instruction found in {.file {path}}.")
  }
  aliases <- vapply(stages, function(s) s$alias, "")
  idx <- if (is.null(target)) length(stages) else match(target, aliases)
  if (is.na(idx)) {
    cli::cli_abort("Build stage {.val {target}} not found in {.file {path}}.")
  }
  ct_dockerfile_resolve(stages, idx)
}

ct_dockerfile_resolve <- function(stages, idx) {
  stage <- stages[[idx]]
  earlier <- vapply(stages[seq_len(idx - 1L)], function(s) s$alias, "")
  parent <- match(stage$image, earlier)
  if (is.na(parent)) {
    return(list(image = stage$image, packages = unique(stage$packages), user = stage$user))
  }
  base <- ct_dockerfile_resolve(stages, parent)
  list(
    image = base$image,
    packages = unique(c(base$packages, stage$packages)),
    user = stage$user %||% base$user
  )
}

ct_dockerfile_subst <- function(x, args) {
  for (nm in names(args)) {
    x <- gsub(paste0("${", nm, "}"), args[[nm]], x, fixed = TRUE)
    x <- gsub(paste0("$", nm), args[[nm]], x, fixed = TRUE)
  }
  x
}

ct_parse_install2r <- function(run) {
  after <- sub("^.*install2\\.r", "", run)
  after <- sub("(&&|;|\\|).*$", "", after)
  tokens <- strsplit(trimws(after), "\\s+")[[1L]]
  tokens <- tokens[nzchar(tokens) & !startsWith(tokens, "-") & !grepl("^[0-9]+$", tokens)]
  tokens
}
