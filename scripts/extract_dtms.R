extract_dtms <- function(
  manifest,
  out_dir = "data/processed",
  dtm_dim = 2.58 + 10,
  dtm_res = 0.3,
  force = FALSE
) {
  out_dir <- out_dir |> here::here()

  fs::dir_create(out_dir)

  manifest <- manifest |>
    dplyr::mutate(
      dtm_path = cloud_path |>
        fs::path_file() |>
        fs::path_ext_remove() |>
        (\(s) fs::path(out_dir, paste0(s, "_dtm_r", dtm_res), ext = "ply"))()
    )
  existing_files <- fs::dir_ls(out_dir, glob = "*.ply")
  if (force) {
    to_process <- manifest
  } else {
    to_process <- dplyr::filter(manifest, !dtm_path %in% existing_files)
  }

  if (nrow(to_process) == 0) {
    message("All DTMs already extracted. Use force = TRUE to reprocess.")
    return(manifest)
  }

  if (!force && nrow(to_process) < nrow(manifest)) {
    message(
      "Found DTMs already extracted. Only ", nrow(to_process),
      " new files will be processed. Use force = TRUE to reprocess"
    )
  }

  manifest_out <- to_process |>
    dplyr::mutate(
      res = purrr::pmap(
        list(year, cloud_path, pois, dtm_path),
        function(year, cloud_path, pois, dtm_path) {
          cloud <- invisible(rlas::read.las(cloud_path, "xyz")) |>
            coi::as_pt_cld()

          center <- pois |>
            dplyr::filter(type %in% c("shrub", "center")) |>
            # This makes sure that when both shrub and center are present
            # center is preferred ("center" sorts before "shrub" alphabetically)
            dplyr::arrange(type) |>
            dplyr::slice_head(n = 1) |>
            dplyr::select(x, y, z) |>
            unlist()

          if (year == 2024) {
            compass_row <- pois |>
              dplyr::filter(type %in% c("north", "east", "south", "west"))
            heading <- dplyr::pull(compass_row, type)
            p2 <- compass_row |>
              dplyr::select(x, y, z) |>
              unlist()
            cloud <- cloud |>
              coi::align_to_north(
                p1 = center,
                p2 = p2,
                heading = heading
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
