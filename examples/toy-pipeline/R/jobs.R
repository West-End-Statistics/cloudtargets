# Project functions are defined here to check they reach remote workers.
slow_square <- function(x) {
  Sys.sleep(15)
  stats::median(c(x * x, x * x))
}

run_job <- function(job_id) {
  data.frame(
    job_id = job_id,
    result = slow_square(job_id),
    host = Sys.info()[["nodename"]],
    pid = Sys.getpid()
  )
}

summarise_where <- function(jobs) {
  table(jobs$host)
}
