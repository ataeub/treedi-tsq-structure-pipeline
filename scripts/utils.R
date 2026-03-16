.set_up_future <- function() {
  cores_to_use <- parallelly::availableCores(logical = FALSE) - 1
  future::plan(future::multisession, workers = cores_to_use)
  progressr::handlers("cli")
  options(progressr.show_after = 0)
}