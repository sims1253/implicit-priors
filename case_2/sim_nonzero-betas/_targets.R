# Setup ----
library(brms)
library(cmdstanr)
library(dplyr)
library(ggplot2)
library(mvtnorm)
library(here)
library(qs2)
library(SBC) # available via github: hyunjimoon/SBC
library(tarchetypes)
library(targets)
library(tidyr)
library(future)
plan(multisession)
options(
  SBC.min_chunk_size = 5,
  brms.backend = "cmdstanr"
)
tar_option_set(
  format = "qs", memory = "transient",
  garbage_collection = TRUE
)
cache_dir = "cache"
tar_source("R")
# Simulation code ----
tar_plan(
  # simconfig
  ## SBC size
  nsims = 5, nbatches = 40,
  ## generator params
  weights = c(0, 0.01, 0.1),
  diricha = c(0.01),
  tar_target(
    all_priors,
    make_prior(diricha),
    pattern = map(diricha),
    iteration = "list"
  ),
  # generator setup
  tar_target(
    generator_params,
    list(n=100,p=250,w=weights,prior=all_priors,nsim=nsims),
    pattern = cross(all_priors, weights),
    iteration = "list"
  ),
  all_generators = rep(generator_params, each = nbatches),
  tar_target(
    each_generator,
    all_generators[[1]],
    pattern = map(all_generators),
    iteration = "list"
  ),
  tar_target(
    all_datasets,
    do.call(my_generate_datasets, each_generator),
    pattern = map(each_generator),
    iteration = "list"
  ),
  tar_target(
    each_dataset,
    all_datasets,
    pattern = map(all_datasets),
    iteration = "list"
  ),
  # compute
  tar_target(
    results,
    my_compute_SBC(each_dataset),
    pattern = map(each_dataset),
    iteration = "list"
  ),
  tar_target(
    all_summaries,
    (\(res){res$fits = list(NULL);res})(results),
    pattern = map(results),
    iteration = "list"
  ),
  sbc_summary = merge_summs(all_summaries),
  stats_table = sbc_summary$stats %>%
    mutate(tag = sub("//.*", "", tag)) %>%
    filter(tag != "w:0.001",
      grepl("^b_X[0-9]+$", variable) |
      grepl("^sdb_X[0-9]+$", variable) |
      variable %in% c("sigma", "R2D2_R2")
    ),
  # plots
  ecdf_plotobj = lapply(c("w:0", "w:0.01"), \(x) {
    if(x == "w:0") {
      title = ggtitle("Original prior")
    } else {
      #title = ggtitle("Primed prior (fixed coeff.)", paste0("M = 100, τ = ", substr(x,3,100)))
      title = ggtitle("Primed prior", "Fixed coefficients")
    }
    plot_ecdf_diff(
      stats_table %>% filter(tag == x, variable %in% c("R2D2_R2", "sigma")),
      combine_variables = list(
      `R2` = "R2D2_R2",
      `Error std. dev.`= "sigma"
    )) + title +
    facet_wrap(~group, scales = "free_y", nrow = 1) +
    theme_bw(base_size = 12) +
    theme(
      strip.text.x = element_text(size = 8),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      plot.subtitle = element_text(size = 10)
    )
  })
)