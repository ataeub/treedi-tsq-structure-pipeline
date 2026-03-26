parse_pois <- function(poi_path) {
  lines <- readLines(poi_path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  parts <- stringr::str_split(lines, ",", simplify = FALSE)

  labels <- sapply(parts, `[`, 1)

  compass_idx <- grep("north|south|east|west", labels)[1]
  center_idx <- grep("center", labels)[1]
  if (is.na(center_idx)) center_idx <- grep("shrub", labels)[1]

  tibble::tibble(
    p2_dir = if (!is.na(compass_idx)) {
      stringr::str_extract(labels[compass_idx], "north|south|east|west")
    } else {
      NA_character_
    },
    p2_x = if (!is.na(compass_idx)) {
      as.numeric(parts[[compass_idx]][2])
    } else {
      NA_real_
    },
    p2_y = if (!is.na(compass_idx)) {
      as.numeric(parts[[compass_idx]][3])
    } else {
      NA_real_
    },
    center_x = if (!is.na(center_idx)) {
      as.numeric(parts[[center_idx]][2])
    } else {
      NA_real_
    },
    center_y = if (!is.na(center_idx)) {
      as.numeric(parts[[center_idx]][3])
    } else {
      NA_real_
    }
  )
}
