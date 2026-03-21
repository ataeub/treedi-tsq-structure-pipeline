source(here::here("scripts/clip_mesh.R"))

preprocess_cloud <- function(
  raw_cloud_path = NULL,
  dtm_path = NULL,
  ln_cloud_path = NULL,
  tsq_cloud_path = NULL,
  center_x = NULL,
  center_y = NULL,
  p2_dir = NULL,
  p2_x = NULL,
  p2_y = NULL,
  vox_res = 0.1,
  ln_dim = 2.58,
  tsq_dim = 1.29,
  lower_cutoff = 0.5,
  sor_n = 20,
  sor_s = 10,
  type = c("ln", "tsq")
) {
  type <- match.arg(type)
  center <- c(center_x, center_y)
  if (type == "ln") {
    p2 <- c(p2_x, p2_y)

    cloud <- rlas::read.las(raw_cloud_path, "xyz") |>
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

    dtm <- Rvcg::vcgPlyRead(dtm_path)

    ln_dtm <- dtm |>
      # we add 0.5 m as a buffer
      coi::clip_rect_mesh(dim_x = ln_dim + 0.5, center = center)

    ln_cloud <- cloud |>
      coi::clip_rect(ln_dim, center = center) |>
      coi::sor(n = sor_n, s = sor_s) |>
      coi::z_normalize(dtm = ln_dtm) |>
      coi::clip_z(lower_cutoff) |>
      coi::voxelize(vox_res)
    coi::save_to_laz(ln_cloud, ln_cloud_path)
    out <- ln_cloud_path
  } else {
    tsq_cloud <- rlas::read.las(ln_cloud_path, "xyz") |>
      coi::as_pt_cld() |>
      coi::clip_rect(tsq_dim, center = center)
    coi::save_to_laz(tsq_cloud, tsq_cloud_path)
    out <- tsq_cloud_path
  }
  out
}
