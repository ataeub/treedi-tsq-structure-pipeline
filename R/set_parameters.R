set_parameters <- function(
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
  enlrnd = 3L,
  bdrnd = 4L,
  slprnd = 1L,
  asprnd = 0L
) {
  #' Set Analysis Parameters
  #'
  #' @param dtmd Dimension of the initially extracted DTM. Default: `2.58 + 10`.
  #' @param dtmr Resolution of the rasterized ground point extraction for DTM. Default: `0.3`.
  #' @param v Voxel resolution of the point clouds. Default: `0.1`.
  #' @param lnd Dimension of the local neighbourhood square. Must be larger than `tsqd` and `v`. Default: `2.58`.
  #' @param tsqd Dimension of the TSQ square. Must be larger than `v`. Default: `1.29`.
  #' @param lc Height from the ground until which the cloud should be cut off to exclude ground points and ground vegetation. Default: `0.5`.
  #' @param sorn Number of points for statistical outlier filtering. Default: `20`.
  #' @param sors Sigma multiplier for statistical outlier filtering. Default: `10`.
  #' @param bdt Box dimension lower threshold. Default: `0.1`.
  #' @param enll Effective number of layers (ENL) layer size. Default: `1`.
  #' @param chsr Raster cell dimension for canopy height statistics. Default: `0.1`.
  #' @param chsrnd Digits to round for canopy height statistics. Default: `1L`.
  #' @param enlrnd Digits to round for ENL. Default: `3L`.
  #' @param bdrnd Digits to round for box dimension. Default: `4L`.
  #' @param slprnd Digits to round for slope. Default: `1L`.
  #' @param asprnd Digits to round for aspect. Default: `0L`.
  #'
  #' @return A named list of all parameters.
  parameters <- as.list(environment())

  if (!all(sapply(parameters, is.numeric))) {
    stop("All parameters must be numeric!")
  }
  rnd_params <- c("chsrnd", "enlrnd", "bdimrnd", "slprnd", "asprnd")
  poscheck_excluded <- c("lc", rnd_params)
  if (!all(sapply(parameters[!names(parameters) %in% poscheck_excluded], `>`, 0))) {
    stop("All parameters except lc must be positive!")
  }
  if (!(lnd > tsqd)) {
    stop("lnd must be larger than tsqd!")
  }
  strictv_params <- c("lnd", "tsqd")
  if (!all(sapply(parameters[names(parameters) %in% strictv_params], `>`, v))) {
    stop("lnd, tsqd must be larger than v!")
  }
  geqv_params <- c("chsr", "bdt", "enll")
  if (!all(sapply(parameters[names(parameters) %in% geqv_params], `>=`, v))) {
    stop("chsr, bdt, enll must be >= v!")
  }
  if (!all(sapply(parameters[names(parameters) %in% rnd_params], rlang::is_integerish))) {
    stop("All -rnd parameters must be integers!")
  }
  if (!all(sapply(parameters[names(parameters) %in% rnd_params], `>=`, 0))) {
    stop("All -rnd parameters must positive!")
  }

  parameters
}
