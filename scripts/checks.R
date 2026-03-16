# --- Quick validation ---
cat("Rows:", nrow(manifest), "\n")
cat("cloud_path (laz):", sum(!is.na(manifest$cloud_path)), "\n")
cat("track_path (ply):", sum(!is.na(manifest$track_path)), "\n")
cat("poi_path   (txt):", sum(!is.na(manifest$poi_path)), "\n")

# Check all referenced files actually exist on disk
existing_clouds <- manifest$cloud_path[!is.na(manifest$cloud_path)]
existing_tracks <- manifest$track_path[!is.na(manifest$track_path)]
existing_pois <- manifest$poi_path[!is.na(manifest$poi_path)]

stopifnot("Some laz files missing" = all(fs::file_exists(existing_clouds)))
stopifnot("Some ply files missing" = all(fs::file_exists(existing_tracks)))
stopifnot("Some txt files missing" = all(fs::file_exists(existing_pois)))

cat("All file paths verified!\n")

# Find which poi file(s) trigger warnings
walk(
  fs::dir_ls(here::here("data/raw"), glob = "*_pois.txt"),
  \(p) tryCatch(
    parse_pois(p),
    warning = function(w) message("Warning in: ", p, "\n  ", w$message)
  )
)
