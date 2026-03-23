prep_tsq_cloud <- function(
  ln_cloud_path = NULL,
  tsq_cloud_path = NULL,
  center_x = NULL,
  center_y = NULL,
  tsq_dim = 1.29,
  verbose = TRUE
) {
  if (isTRUE(verbose)) message("Processing: ", ln_cloud_path)

  center <- c(center_x, center_y)

  tsq_cloud <- rlas::read.las(ln_cloud_path, "xyz") |>
    coi::as_pt_cld() |>
    coi::clip_rect(tsq_dim, center = center)
  coi::save_to_laz(tsq_cloud, tsq_cloud_path)
  tsq_cloud_path
}
