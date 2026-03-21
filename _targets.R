# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "coi",
    "dplyr",
    "fs",
    "Rvcg",
    "stringr",
    "tibble"
  ) # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
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
        dtm_dim = 2.58 + 10,
        dtm_res = 0.3
      )
    }
  ),
  tar_target(
    name = manifest_grouped,
    command = {
      raw_manifest$manifest |>
        # dplyr::slice(267:300) |> # Add to test the pipeline on one row
        # dplyr::filter(cloud_path == "C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-16_04-16-10_B_P28_0606,0607.laz") |>
        dplyr::mutate(
          dtm_dim = raw_manifest$params$dtmd,
          dtm_res = raw_manifest$params$dtmr,
          vox_res = raw_manifest$params$v,
          ln_dim = raw_manifest$params$lnd,
          tsq_dim = raw_manifest$params$tsqd,
          lower_cutoff = raw_manifest$params$lc,
          sor_n = raw_manifest$params$sorn,
          sor_s = raw_manifest$params$sors
        ) |>
        dplyr::group_by(cloud_path) |> # one row per cloud = one branch
        targets::tar_group()
    }
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
      dtm_dim      = manifest_grouped$dtm_dim,
      dtm_res      = manifest_grouped$dtm_res
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
      vox_res = manifest_grouped$vox_res,
      ln_dim = manifest_grouped$ln_dim,
      lower_cutoff = manifest_grouped$lower_cutoff,
      sor_n = manifest_grouped$sor_n,
      sor_s = manifest_grouped$sor_s
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
      tsq_dim = manifest_grouped$tsq_dim
    ),
    pattern = map(manifest_grouped, ln_clouds),
    format = "file"
  )
)

# source(here::here("R/preprocess_cloud.R"))
# library(targets)

# dtms <- tar_read(dtms)
# manifest_grouped <- tar_read(manifest_grouped)
# preprocess_cloud(
#       raw_cloud_path = manifest_grouped$cloud_path,
#       dtm_path = dtms,
#       ln_cloud_path = manifest_grouped$ln_prep_path,
#       center_x = manifest_grouped$center_x,
#       center_y = manifest_grouped$center_y,
#       p2_dir = manifest_grouped$p2_dir,
#       p2_x = manifest_grouped$p2_x,
#       p2_y = manifest_grouped$p2_y,
#       vox_res = manifest_grouped$vox_res,
#       ln_dim = manifest_grouped$ln_dim,
#       lower_cutoff = manifest_grouped$lower_cutoff,
#       sor_n = manifest_grouped$sor_n,
#       sor_s = manifest_grouped$sor_s,
#       type = "ln"
#     )

# 1
# cloud <- rlas::read.las("C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-16_00-30-48_B_T22_0704.laz", "xyz") |>
#     coi::as_pt_cld() |>
#     coi::align_to_north(
#         p1 = c(-0.07, 0.76),
#         p2 = p2,
#         heading = "north"





# DONE
# North point == center
# -> north point set
# C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-16_00-46-24_B_S22_1404.laz
# 
# This seems to be a failed or worse scan since there is one 3 min later with many more points
# cloud deleted
# C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-16_01-30-45_B_T22_1708.laz

# No poi file -> cloud deleted
# C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-16_04-16-10_B_P28_0606,0607.laz

# No POI file -> cloud deleted
# C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-16_05-09-44_B_P28_1210.laz

# This seems to be a failed or worse scan since there is one 3 min later with many more points
# cloud deleted
# C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-17_11-28-19_B_I25_0607.laz

# This seems to be a failed or worse scan since there is one 3 min later with many more points
# cloud deleted
# C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-19_10-26-01_B_F28_0600.laz

# This was the one we scanned twice by accident
# redundant cloud deleted
# C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-26_07-46-13_B_P29_0018.laz

# The cloud has a wrong ID. Should be V24_1105
# -> Renamed
# C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-26_09-32-44_B_V24_1104.laz

# No POI file -> cloud deleted
# 2024-08-16_05-09-44_B_P28_1210.laz


# manifest_grouped |>
#   dplyr::mutate(num = dplyr::row_number()) |>
#   dplyr::filter(cloud_path == "C:/Users/alex-work/Desktop/repos/tsq_structure_new/data/raw/2024-08-26_09-32-44_B_V24_1104.laz") |>
#   dplyr::pull(num)
