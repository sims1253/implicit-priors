make_backend = \(data) {
  SBC_backend_brms(
    paste0("y | weights(w) ~ 0 +",paste0("X",1:250, collapse = "+")),
    prior = data$meta$prior, chains = 4,
    template_data = data$generated[[1]],
    control = list(adapt_delta = 0.95),
    stanvars = stanvar(
      block = "genquant", scode ="
      vector[K] phi;
      phi = R2D2_phi;"
    )
  )
}
my_compute_SBC = \(data) {
  backend = make_backend(data)
  res = compute_SBC(data, backend)
  res$meta = data$meta
  return(res)
}