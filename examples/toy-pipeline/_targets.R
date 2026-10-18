# A small pipeline that shows work spreading over several cloud machines.
# Run it with: targets::tar_make()   (needs a RunPod API key, see README)
library(targets)

tar_option_set(
  # Change this one number to change how many machines are used:
  controller = cloudtargets::ct_crew_controller(workers = 4),
  # Machines share no file system with this session, so results travel back
  # through the controller instead of being written by the workers:
  storage = "main",
  retrieval = "main"
)

tar_source()

list(
  tar_target(job_id, 1:8),
  tar_target(job, run_job(job_id), pattern = map(job_id)),
  tar_target(where, summarise_where(job))
)
