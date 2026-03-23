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
  chsr = 0.1
) {
  parameters <- as.list(environment())

  if (!all(sapply(parameters, is.numeric))) {
    stop("All parameters must be numeric!")
  }
  if (!all(sapply(parameters[names(parameters) != "lc"], `>`, 0))) {
    stop("All parameters except lc must be positive!")
  }
  if (!(lnd > tsqd)) {
    stop("lnd must be larger than tsqd!")
  }
  if (!(lnd > v)) {
    stop("lnd must be larger than v!")
  }
  if (!(tsqd > v)) {
    stop("tsqd must be larger than v!")
  }
  if (!(enll >= v)) {
    stop("enll must be larger than v!")
  }
  if (!(chsr >= v)) {
    stop("chsr must be larger than v!")
  }
  if (!(bdt >= v)) {
    stop("bdt must be larger than v!")
  }

  parameters
}
