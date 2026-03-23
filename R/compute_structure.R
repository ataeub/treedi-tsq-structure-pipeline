compute_structure <- function(
  cloud_path,
  tsq_name,
  dtm_stats = FALSE,
  dtm_path = NULL,
  boxdim_t = 0.1,
  enl_layer_size = 1,
  chs_res = 0.1,
  plot = TRUE,
  verbose = TRUE
) {
  if (isTRUE(verbose)) message("Processing: ", cloud_path)

  cloud <- rlas::read.las(cloud_path, "xyz") |>
    coi::as_pt_cld()

  boxdim <- coi::boxdim(
    cloud,
    threshold = boxdim_t,
    warnings = FALSE
  )

  enl <- coi::enl(
    cloud,
    layer_thickness = enl_layer_size,
    plot = plot,
    plot_title = tsq_name,
    warnings = FALSE
  )

  chs <- coi::canopy_stats(
    cloud,
    res = chs_res,
    lower_cutoff = NULL,
    plot = plot,
    plot_title = tsq_name
  )

  if (dtm_stats && !is.null(dtm_path)) {
    dtm <- Rvcg::vcgPlyRead(dtm_path)
    dtm_stats <- coi::extract_slope(dtm, aspect = TRUE)
    slope <- dtm_stats$slope
    aspect <- dtm_stats$aspect
  } else {
    slope <- NA_real_
    aspect <- NA_real_
  }

  results <- tibble::tibble(
    tsq = tsq_name,
    chs_max = chs$max,
    chs_mean = chs$mean,
    chs_sd = chs$sd,
    chs_cv = chs$cv,
    chs_gini = chs$gini,
    enl0 = enl$ENL0,
    enl1 = enl$ENL1,
    enl2 = enl$ENL2,
    boxdim = boxdim,
    slope = slope,
    aspect = aspect
  )
  if (plot) {
    results <- results |>
      dplyr::mutate(
        chs_plot = list(chs$plot),
        enl_plot = list(enl$plot),
        boxdim_plot = list(boxdim$plot)
      )
  }
  results
}
