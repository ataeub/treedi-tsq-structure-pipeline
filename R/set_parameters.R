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
  if (!all(sapply(parameters[names(parameters) %in% rnd_params],  `>=`, 0))) {
    stop("All -rnd parameters must positive!")
  }

  parameters
}
