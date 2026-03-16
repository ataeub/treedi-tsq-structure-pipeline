parse_pois <- function(poi_path) {
  lines <- readLines(poi_path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  parts <- stringr::str_split(lines, ",", simplify = FALSE)

  purrr::map_dfr(parts, function(p) {
    label <- p[1]
    # label is e.g. "C33_0609_tree1_Nysi", "C33_0609_shrub_Dimy",
    #               "O27_0502_center", "P29_0712_north"
    tokens <- stringr::str_split_1(label, "_")
    # tokens[1] = plot prefix (e.g. C33), tokens[2] = id (e.g. 0609)
    # tokens[3] = type (tree1, shrub, center, north, etc.)
    # tokens[4] = species (optional)
    poi_plot   <- tokens[1]
    poi_tsq_id <- tokens[2]
    type       <- tokens[3]
    species    <- if (length(tokens) >= 4) tokens[4] else NA_character_

    tibble::tibble(
      poi_plot   = poi_plot,
      poi_tsq_id = poi_tsq_id,
      type       = type,
      species    = species,
      x = as.numeric(p[2]),
      y = as.numeric(p[3]),
      z = as.numeric(p[4])
    )
  })
}