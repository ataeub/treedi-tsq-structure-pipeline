extract_dtms <- function(
  manifest,
  force = FALSE
) {
  dtm_dim <- manifest$params$dtmdim
  dtm_res <- manifest$params$dtmres

  if (force) {
    to_process <- manifest$manifest
  } else {
    to_process <- manifest$manifest |>
      dplyr::filter(!fs::file_exists(dtm_path))
  }

  if (nrow(to_process) == 0) {
    message("All DTMs already extracted. Use force = TRUE to reprocess.")
    return(manifest)
  }

  if (!force && nrow(to_process) < nrow(manifest$manifest)) {
    message(
      "Found DTMs already extracted. Only ", nrow(to_process),
      " new files will be processed. Use force = TRUE to reprocess"
    )
  }

  manifest_out <- to_process |>
    dplyr::mutate(
      res = purrr::pmap(
        list(year, center_x, center_y, p2_dir, p2_x, p2_y, cloud_path, dtm_path),
        function(year, center_x, center_y, p2_dir, p2_x, p2_y, cloud_path, dtm_path) {
          cloud <- invisible(rlas::read.las(cloud_path, "xyz")) |>
            coi::as_pt_cld()

          center <- c(center_x, center_y)


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
          Rvcg::vcgPlyWrite(dtm, dtm_path)
        },
        .progress = TRUE
      )
    )
  manifest_out
}
