source(here::here("scripts/clip_mesh.R"))

preprocess_clouds <- function(
  manifest,
  out_dir = "data/processed",
  vox_res = 0.1,
  ln_dim = 2.58,
  tsq_dim = 1.29,
  lower_cutoff = 0.5,
  sor_n = 20,
  sor_s = 10,
  force = FALSE
) {
  out_dir <- out_dir |> here::here()

  fs::dir_create(out_dir)

  manifest <- manifest |>
    dplyr::mutate(
      ln_cloud_path = cloud_path |>
        fs::path_file() |>
        fs::path_ext_remove() |>
        (\(s) fs::path(out_dir, paste0(s, "_ln_prep_v", vox_res, "_lc", lower_cutoff), ext = "laz"))(),
      tsq_cloud_path = ln_cloud_path |>
        stringr::str_replace("ln_prep", "tsq_prep")
    )
  existing_clouds <- fs::dir_ls(out_dir, glob = "*laz")
  existing_dtms <- fs::dir_ls(out_dir, glob = "*ply")

  if (force) {
    to_process <- manifest
  } else {
    to_process <- manifest |>
      dplyr::filter(
        !ln_cloud_path %in% existing_clouds | !tsq_cloud_path %in% existing_clouds
        )
  }

  dtms_available <- to_process |>
    dplyr::filter(!dtm_path %in% existing_dtms)

  if(dtms_available < to_process) {
    diff <- nrow(to_process) - nrow(dtms_available)
    warning(paste0(diff, " clouds have no DTM available yet and will be skipped. Please run extract_dtms() first!"))
  }
  to_process <- dtms_available

  if (nrow(to_process) == 0) {
    message("All files already pre-processed. Use force = TRUE to reprocess.")
    return(manifest)
  }

  if (!force && nrow(to_process) < nrow(manifest)) {
    message(
      "Found files already pre-processed. Only ", nrow(to_process),
      " new files will be processed. Use force = TRUE to reprocess"
    )
  }

  manifest_out <- to_process |>
    dplyr::mutate(
      res = purrr::pmap(
        list(year, cloud_path, pois, dtm_path, ln_cloud_path, tsq_cloud_path),
        function(year, cloud_path, pois, dtm_path, preprocessed_path) {
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
          ln_dtm <- Rvcg::vcgPlyRead(dtm_path) |>
            clip_mesh(ln_dim + 0.5) # we add 0.5 m as a buffer

          ln_cloud <- cloud |>
            coi::clip_rect(ln_dim, center = center) |>
            coi::sor(n = sor_n, s = sor_s) |>
            coi::z_normalize(dtm = ln_dtm) |>
            coi::clip_z(lower_cutoff) |>
            coi::voxelize(vox_res)
          coi::save_to_laz(ln_cloud, ln_cloud_path)

          tsq_cloud <- ln_cloud |>
            coi::clip_rect(tsq_dim, center = center)
          coi::save_to_laz(tsq_cloud, tsq_cloud_path)
        },
        .progress = TRUE
      )
    )
  manifest_out
}
