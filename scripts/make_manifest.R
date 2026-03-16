source(
  here::here("scripts/parse_pois.R"),
  )

make_manifest <- function(raw_dir = here::here("data/raw")) {
  clouds <- fs::dir_ls(raw_dir, glob = "*.laz")

  dplyr::tibble(cloud_path = clouds) |>
    dplyr::mutate(
      basename = fs::path_ext_remove(fs::path_file(cloud_path)),
      year = stringr::str_extract(basename, "^\\d{4}(?=-)"),
      site = stringr::str_extract(basename, "(?<=_)[A-Z](?=_)"),
      plot = stringr::str_extract(basename, "[A-Z]\\d+(?=_\\d{4}$)"),
      tsq_id = stringr::str_extract(basename, "\\d{4}$"),
      track_path = fs::path(raw_dir, paste0(basename, ".ply")),
      poi_path = fs::path(raw_dir, paste0(basename, "_pois.txt")),
      track_path = dplyr::if_else(fs::file_exists(track_path), track_path, NA_character_),
      poi_path = dplyr::if_else(fs::file_exists(poi_path), poi_path, NA_character_)
    ) |>
    dplyr::select(year, site, plot, tsq_id, cloud_path, track_path, poi_path) |>
    dplyr::mutate(
      pois = purrr::map(poi_path, \(p) if (!is.na(p)) parse_pois(p) else NULL)
    )
}
