save_plt <- function(
  structure_row,
  out_dir = "data/figures"
) {
  out_dir <- here::here(out_dir)
  fs::dir_create(out_dir)

  tsq_id <- as.character(dplyr::pull(structure_row, "tsq")[[1]])

  chs_ln <- dplyr::pull(structure_row, "chs_plot_ln")[[1]]
  enl_ln <- dplyr::pull(structure_row, "enl_plot_ln")[[1]]
  boxdim_ln <- dplyr::pull(structure_row, "boxdim_plot_ln")[[1]]
  chs_tsq <- dplyr::pull(structure_row, "chs_plot_tsq")[[1]]
  enl_tsq <- dplyr::pull(structure_row, "enl_plot_tsq")[[1]]
  boxdim_tsq <- dplyr::pull(structure_row, "boxdim_plot_tsq")[[1]]

  plt <- patchwork::wrap_plots(
    chs_ln, enl_ln, boxdim_ln, chs_tsq, enl_tsq, boxdim_tsq,
    nrow = 2
  )

  out_path <- fs::path(
    out_dir,
    glue::glue("{tsq_id}_structure_plots"),
    ext = "svg"
  )

  ggplot2::ggsave(out_path, plt, width = 12, height = 8)

  out_path
}
