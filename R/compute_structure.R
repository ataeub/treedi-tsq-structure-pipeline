compute_structure <- function(
  cloud_path,
  tsq_name,
  year,
  site,
  aspect = FALSE,
  dtm_path = NULL,
  boxdim_t = 0.1,
  enl_layer_size = 1,
  chs_res = 0.1,
  chs_rnd = 1L,
  enl_rnd = 3L,
  boxdim_rnd = 4L,
  slope_rnd = 1L,
  asp_rnd = 0L,
  plot = TRUE,
  type = c("ln", "tsq"),
  verbose = TRUE
) {
  if (isTRUE(verbose)) message("Processing: ", cloud_path)

  type <- toupper(match.arg(type))
  plot_title <- glue::glue("{type}_{tsq_name}")

  cloud <- rlas::read.las(cloud_path, "xyz") |>
    coi::as_pt_cld()

  boxdim_res <- coi::boxdim(
    cloud,
    threshold = boxdim_t,
    plot = plot,
    plot_title = plot_title,
    warnings = FALSE
  )

  enl_res <- coi::enl(
    cloud,
    layer_thickness = enl_layer_size,
    plot = plot,
    plot_title = plot_title,
    warnings = FALSE
  )

  chs_res <- coi::canopy_stats(
    cloud,
    res = chs_res,
    lower_cutoff = NULL,
    plot = plot,
    plot_title = plot_title
  )

  dtm <- Rvcg::vcgPlyRead(dtm_path)
  if (aspect) {
    dtm_stats <- coi::extract_slope(dtm, aspect = TRUE)
    aspect <- dtm_stats$aspect
  } else {
    dtm_stats <- coi::extract_slope(dtm, aspect = FALSE)
    aspect <- NA_real_
  }
  slope <- dtm_stats$slope

  results <- tibble::tibble(
    year = year,
    site = site,
    tsq = tsq_name,
    chs_max = round(chs_res$max, chs_rnd),
    chs_mean = round(chs_res$mean, chs_rnd),
    chs_sd = round(chs_res$sd, chs_rnd),
    chs_cv = round(chs_res$cv, chs_rnd),
    chs_gini = round(chs_res$gini, chs_rnd),
    enl0 = enl_res$ENL0,
    enl1 = round(enl_res$ENL1, enl_rnd),
    enl2 = round(enl_res$ENL2, enl_rnd),
    boxdim = round(boxdim_res$boxdim, boxdim_rnd),
    slope = round(slope, slope_rnd),
    aspect = round(aspect, asp_rnd)
  )
  if (plot) {
    results <- results |>
      dplyr::mutate(
        chs_plot = list(chs_res$plot),
        enl_plot = list(enl_res$plot),
        boxdim_plot = list(boxdim_res$plot)
      )
  }
  results
}
