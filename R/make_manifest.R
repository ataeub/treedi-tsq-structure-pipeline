source(here::here("scripts/parse_pois.R"))

make_manifest <- function(
  cloud_paths,
  poi_paths,
  dtm_dir = here::here("data/processed/dtms"),
  prep_dir = here::here("data/processed/preprocessed"),
  dtm_dim = 2.58 + 10,
  dtm_res = 0.3,
  vox_res = 0.1,
  ln_dim = 2.58,
  tsq_dim = 1.29,
  lower_cutoff = 0.5,
  sor_n = 20,
  sor_s = 10,
  save = TRUE
) {
  fs::dir_create(c(dtm_dir, prep_dir))

  params_list <- list(
    "dtmd" = dtm_dim,
    "dtmr" = dtm_res,
    "v" = vox_res,
    "lnd" = ln_dim,
    "tsqd" = tsq_dim,
    "lc" = lower_cutoff,
    "sorn" = sor_n,
    "sors" = sor_s
  )
  dtm_string <- paste(names(params_list[1:2]), unlist(params_list[1:2]), sep = "", collapse = "_")
  ln_string <- paste(names(params_list[c(1:4, 6:7)]), unlist(params_list[c(1:4, 6:7)]), sep = "", collapse = "_")
  tsq_string <- paste(names(params_list[c(1:3, 5:7)]), unlist(params_list[c(1:3, 5:7)]), sep = "", collapse = "_")
  params_string <- paste(names(params_list), unlist(params_list), sep = "", collapse = "_")

  manifest <- dplyr::tibble(cloud_path = cloud_paths) |>
    dplyr::mutate(
      raw_dir = fs::path_dir(cloud_path),
      basename = fs::path_ext_remove(fs::path_file(cloud_path)),
      year = stringr::str_extract(basename, "^\\d{4}(?=-)"),
      site = stringr::str_extract(basename, "(?<=_)[A-Z](?=_)"),
      plot = stringr::str_extract(basename, "[A-Z]\\d+(?=_\\d{4}$)"),
      tsq_id = stringr::str_extract(basename, "\\d{4}$"),
      dtm_path = fs::path(
        dtm_dir, paste0(basename, "_dtm_", dtm_string),
        ext = "ply"
      ),
      ln_prep_path = fs::path(
        prep_dir, paste0(basename, "_lnprep_", ln_string),
        ext = "laz"
      ),
      tsq_prep_path = fs::path(
        prep_dir, paste0(basename, "_tsqprep_", tsq_string),
        ext = "laz"
      ),
      poi_path = {
        virtual_poi_path <- fs::path(raw_dir, paste0(basename, "_pois.txt"))
        dplyr::if_else(virtual_poi_path %in% poi_paths,
          virtual_poi_path,
          NA_character_
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
      poi_path,
      dtm_path,
      ln_prep_path,
      tsq_prep_path
    )


  missing_pois <- manifest |>
    dplyr::filter(!is.na(cloud_path) & is.na(poi_path)) |>
    dplyr::pull(cloud_path)
  if (length(missing_pois) > 0) {
    warning(
      "The following cloud files don't have POI files and will be excluded:",
      missing_pois
    )
    manifest <- manifest |>
      dplyr::filter(!is.na(cloud_path) & !is.na(poi_path))
  }

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
