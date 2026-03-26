source(here::here("scripts/parse_pois.R"))

make_manifest <- function(
  raw_dir = here::here("data/raw"),
  dtm_dir = here::here("data/processed/dtms"),
  prep_dir = here::here("data/processed/preprocessed"),
  dtm_dim = 2.58 + 10,
  dtm_res = 0.3,
  save = TRUE
) {
  fs::dir_create(c(dtm_dir, prep_dir))

  params_list <- list(
    "dtmdim" = dtm_dim,
    "dtmres" = dtm_res
  )
  dtm_string <- glue::glue("dtmdim{dtm_dim}_dtmres{dtm_res}")
  prep_string <- glue::glue("INSERT PREP PARAMS")
  params_string <- glue::glue("{dtm_string}_{prep_string}")

  clouds <- fs::dir_ls(raw_dir, glob = "*.laz")

  manifest <- dplyr::tibble(cloud_path = clouds) |>
    dplyr::mutate(
      basename = fs::path_ext_remove(fs::path_file(cloud_path)),
      year = stringr::str_extract(basename, "^\\d{4}(?=-)"),
      site = stringr::str_extract(basename, "(?<=_)[A-Z](?=_)"),
      plot = stringr::str_extract(basename, "[A-Z]\\d+(?=_\\d{4}$)"),
      tsq_id = stringr::str_extract(basename, "\\d{4}$"),
      dtm_path = fs::path(
        dtm_dir, paste0(basename, "_dtm_", dtm_string),
        ext = "ply"
      ),
      prep_path = fs::path(
        prep_dir, paste0(basename, "_prep_", prep_string),
        ext = "laz"
      ),
      poi_path = {
        all_paths <- fs::path(raw_dir, paste0(basename, "_pois.txt"))
        dplyr::if_else(
          fs::file_exists(all_paths), all_paths, NA_character_
        )
      },
      poi_summary = purrr::map(poi_path, \(p) {
        if (is.na(p)) {
          return(tibble::tibble(
            center_x = NA,
            center_y = NA,
            p2_dir = NA,
            p2_x = NA,
            p2_y = NA
          ))
        }
        parse_pois(p)
      })
    ) |>
    tidyr::unnest_wider(poi_summary) |>
    dplyr::select(
      year,
      site,
      plot,
      tsq_id,
      center_x,
      center_y,
      p2_dir,
      p2_x,
      p2_y,
      cloud_path,
      dtm_path,
      prep_path
    )
  out <- list(
    "manifest" = manifest,
    "params" = params_list
  )
  if (isTRUE(save)) {
    out_name <- paste0("manifest_", params_string)
    saveRDS(out, fs::path(here::here("data/"), out_name, ext = "RDS"))
  }
  out
}
