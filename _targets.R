library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "coi",
    "dplyr",
    "fs",
    "ggplot2",
    "ggtext",
    "glue",
    "here",
    "patchwork",
    "purrr",
    "qs2",
    "readr",
    "rlang",
    "rlas",
    "Rvcg",
    "stringr",
    "tibble",
    "tidyr"
  )
)

tar_source()

list(
  tar_target(
    name = parameters,
    command = {
      set_parameters(
        dtmd = 2.58 + 10,
        dtmr = 0.3,
        v = 0.1,
        lnd = 2.58,
        tsqd = 1.29,
        lc = 0.5,
        sorn = 20,
        sors = 10,
        bdt = 0.1,
        enll = 1,
        chsr = 0.1,
        chsrnd = 1L,
        opnsrnd = 1L,
        enlrnd = 3L,
        bdrnd = 4L,
        slprnd = 1L,
        asprnd = 0L
      )
    },
    format = "qs"
  ),
  tar_target(
    name = raw_cloud_paths,
    command = fs::dir_ls(here::here("data/raw/"), glob = "*.laz"),
    format = "file"
  ),
  tar_target(
    name = poi_paths,
    command = fs::dir_ls(here::here("data/raw/"), glob = "*.txt"),
    format = "file"
  ),
  tar_target(
    name = raw_manifest,
    command = {
      make_manifest(
        cloud_paths = raw_cloud_paths,
        poi_paths = poi_paths,
        params = parameters
      )
    },
    format = "qs"
  ),
  tar_target(
    name = manifest_grouped,
    command = {
      raw_manifest$manifest |>
        dplyr::group_by(cloud_path) |> # one row per cloud = one branch
        targets::tar_group()
    },
    format = "qs"
  ),
  tar_target(
    name = dtms,
    command = extract_dtm(
      cloud_path   = manifest_grouped$cloud_path,
      dtm_path_out = manifest_grouped$dtm_path,
      center_x     = manifest_grouped$center_x,
      center_y     = manifest_grouped$center_y,
      p2_dir       = manifest_grouped$p2_dir,
      p2_x         = manifest_grouped$p2_x,
      p2_y         = manifest_grouped$p2_y,
      dtm_dim      = parameters$dtmd,
      dtm_res      = parameters$dtmr
    ),
    pattern = map(manifest_grouped),
    format = "file"
  ),
  tar_target(
    name = ln_clouds,
    command = prep_ln_cloud(
      raw_cloud_path = manifest_grouped$cloud_path,
      dtm_path = dtms,
      ln_cloud_path = manifest_grouped$ln_prep_path,
      center_x = manifest_grouped$center_x,
      center_y = manifest_grouped$center_y,
      p2_dir = manifest_grouped$p2_dir,
      p2_x = manifest_grouped$p2_x,
      p2_y = manifest_grouped$p2_y,
      vox_res = parameters$v,
      ln_dim = parameters$lnd,
      lower_cutoff = parameters$lc,
      sor_n = parameters$sorn,
      sor_s = parameters$sors
    ),
    pattern = map(manifest_grouped, dtms),
    format = "file"
  ),
  tar_target(
    name = tsq_clouds,
    command = prep_tsq_cloud(
      ln_cloud_path = ln_clouds,
      tsq_cloud_path = manifest_grouped$tsq_prep_path,
      center_x = manifest_grouped$center_x,
      center_y = manifest_grouped$center_y,
      tsq_dim = parameters$tsqd
    ),
    pattern = map(manifest_grouped, ln_clouds),
    format = "file"
  ),
  tar_target(
    name = ln_structure,
    command = compute_structure(
      cloud_path = ln_clouds,
      tsq_name = manifest_grouped$tsq_id,
      year = manifest_grouped$year,
      site = manifest_grouped$site,
      aspect = !is.na(manifest_grouped$p2_dir) &&
        !is.na(manifest_grouped$p2_x) &&
        !is.na(manifest_grouped$p2_y),
      dtm_path = dtms,
      boxdim_t = parameters$bdt,
      enl_layer_size = parameters$enll,
      chs_res = parameters$chsr,
      chs_rnd = parameters$chsrnd,
      opns_rnd = parameters$opnsrnd,
      enl_rnd = parameters$enlrnd,
      boxdim_rnd = parameters$bdrnd,
      slope_rnd = parameters$slprnd,
      asp_rnd = parameters$asprnd,
      plot = TRUE,
      type = "ln"
    ),
    pattern = map(manifest_grouped, ln_clouds, dtms),
    format = "qs"
  ),
  tar_target(
    name = tsq_structure,
    command = compute_structure(
      cloud_path = tsq_clouds,
      tsq_name = manifest_grouped$tsq_id,
      year = manifest_grouped$year,
      site = manifest_grouped$site,
      aspect = !is.na(manifest_grouped$p2_dir) &&
        !is.na(manifest_grouped$p2_x) &&
        !is.na(manifest_grouped$p2_y),
      dtm_path = dtms,
      boxdim_t = parameters$bdt,
      enl_layer_size = parameters$enll,
      chs_res = parameters$chsr,
      chs_rnd = parameters$chsrnd,
      opns_rnd = parameters$opnsrnd,
      enl_rnd = parameters$enlrnd,
      boxdim_rnd = parameters$bdrnd,
      slope_rnd = parameters$slprnd,
      asp_rnd = parameters$asprnd,
      plot = TRUE,
      type = "tsq"
    ),
    pattern = map(manifest_grouped, tsq_clouds, dtms),
    format = "qs"
  ),
  tar_target(
    name = structure_complete,
    command = {
      ln_structure |>
        dplyr::left_join(
          tsq_structure,
          by = dplyr::join_by(year, tsq, site, slope, aspect), suffix = c("_ln", "_tsq")
        )
    },
    format = "qs"
  ),
  tar_target(
    name = structure_file,
    command = {
      out_path <- here::here("data/structural_data.csv")
      structure_complete |>
        dplyr::select(-dplyr::contains("_plot")) |>
        readr::write_csv(out_path)
      out_path
    },
    format = "file"
  ),
  tar_target(
    name = plots,
    command = save_plt(structure_complete),
    pattern = map(structure_complete),
    format = "file"
  ),
  tar_quarto(
    report,
    path = ".",
    quiet = FALSE
  )
)
