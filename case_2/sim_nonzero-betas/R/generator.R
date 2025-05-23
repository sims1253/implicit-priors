make_prior = \(diricha) {
  p = prior(student_t(1, 0, 1),
      class = "sigma") +
    prior(R2D2(mean_R2=.95, prec_R2=1, cons_D2=diricha, main=TRUE),
      class = b)
  p[2,1] = sub("diricha", diricha, p[2,1])
  return(p)
}
my_generate_datasets = \(n, p, w, priors, nsim) {
  # generate catalytic dataset
  ## variance = prior median
  sigma = 1
  ## 10 nonzero betas
  stopifnot(p >= 10)
  beta = 10^sample(2:4, 10, TRUE) # cover some orders of magnitude
  beta = c(beta[1:5], rep(0, p - 10), beta[6:10])
  X = rmvnorm(n, mean = rep(0,p), sigma = diag(p))
  mu = X %*% beta # no intercept
  y = rnorm(n, mu, sigma)
  df = cbind(y = y, data.frame(X), w = w)
  # catalyze model
  fit = brm(
    paste0("y | weights(w) ~ 0 +",paste0("X",1:p, collapse = "+")),
    data = df, prior = priors, init = 0, refresh = 0,
    warmup = 10000, iter = 10000 + 50*nsim, thin = 50,
    chains = 1, control = list(adapt_delta = 0.95),
    stanvars = stanvar(
      block = "genquant", scode ="
      vector[K] phi;
      phi = R2D2_phi;"
    )
  )
  # generate SBC datasets
  y = 0
  X = rmvnorm(n, mean = rep(0,p), sigma = diag(p))
  df2 = cbind(y = y, data.frame(X), w = 1)
  newdf = SBC:::brms_full_ppred(fit, newdata = df2)
  sbcdata = lapply(newdf, bind_rows, df)
  sbcpars = as_draws_matrix(fit)
  # return as SBC object
  structure(list(
    variables = sbcpars,
    generated = sbcdata,
    meta = list(obs_weight=w, prior=priors)
  ), class = "SBC_datasets")
}