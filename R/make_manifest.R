make_manifest <- function(
  cloud_paths,
  poi_paths,
  params,
  dtm_dir = here::here("data/processed/dtms"),
  prep_dir = here::here("data/processed/preprocessed"),
  save = TRUE
) {
  fs::dir_create(c(dtm_dir, prep_dir))

  dtm_params <- params[c("dtmd", "dtmr")]
  ln_string <- append(dtm_params, params[c("v", "lnd", "lc", "sorn", "sors")])
  tsq_string <- append(dtm_params, params[c("v", "tsqd", "lc", "sorn", "sors")])

  dtm_string <- paste(
    names(dtm_params), unlist(dtm_params),
    sep = "", collapse = "_"
  )
  ln_string <- paste(
    names(ln_string), unlist(ln_string),
    sep = "", collapse = "_"
  )
  tsq_string <- paste(
    names(tsq_string), unlist(tsq_string),
    sep = "", collapse = "_"
  )
  params_string <- paste(
    names(params), unlist(params),
    sep = "", collapse = "_"
  )

  manifest <- dplyr::tibble(cloud_path = cloud_paths) |>
    dplyr::mutate(
      raw_dir = fs::path_dir(cloud_path),
      basename = fs::path_ext_remove(fs::path_file(cloud_path)),
      year = stringr::str_extract(basename, "^\\d{4}(?=-)"),
      site = dplyr::if_else(year == "2023", "A", "B"),
      plot = stringr::str_extract(basename, "[A-Z]\\d+(?=_\\d{4}$)"),
      tsq_pos = stringr::str_extract(basename, "\\d{4}$"),
      tsq_id = paste0(plot, "_", tsq_pos),
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
      tsq_pos,
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
      paste(
        "The following cloud files don't have POI files and will be excluded:",
        paste(missing_pois, collapse = "\n"),
        sep = "\n"
      )
    )
    manifest <- manifest |>
      dplyr::filter(!is.na(cloud_path) & !is.na(poi_path))
  }

  out <- list(
    "manifest" = manifest,
    "params" = params
  )
  if (isTRUE(save)) {
    out_name <- paste0("manifest_", params_string)
    saveRDS(out, fs::path(here::here("data/"), out_name, ext = "RDS"))
  }
  out
}
