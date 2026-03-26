extract_dtm <- function(
  cloud_path,
  dtm_path_out,
  center_x,
  center_y,
  p2_dir,
  p2_x,
  p2_y,
  dtm_dim,
  dtm_res,
  verbose = TRUE
) {
  if (isTRUE(verbose)) message("Processing: ", cloud_path)

  center <- c(center_x, center_y)
  p2 <- c(p2_x, p2_y)

  cloud <- rlas::read.las(cloud_path, "xyz") |>
    coi::as_pt_cld()
  if (!anyNA(c(p2_dir, p2_x, p2_y))) {
    p2 <- c(p2_x, p2_y)
    cloud <- cloud |>
      coi::align_to_north(
        p1 = center,
        p2 = p2,
        heading = p2_dir
      )
  }
  dtm <- cloud |>
    coi::clip_rect(dtm_dim, center = center) |>
    coi::extract_dtm(
      res = dtm_res,
      sm_type = "taubin",
      sm_lambda = 0.5,
      sm_mu = -0.53
    )
  Rvcg::vcgPlyWrite(dtm, dtm_path_out)
  dtm_path_out
}
