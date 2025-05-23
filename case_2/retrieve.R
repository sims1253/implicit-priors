# Retrieving plots and summaries
library(dplyr)
library(targets)
library(ggplot2)
library(patchwork)
quantile_df <- function(x, probs = c(0.05, 0.5, 0.95)) {
  tibble(
    value = quantile(x, probs, na.rm = TRUE),
    quant = paste0("q", probs*100)
  )
}

plots1 = tar_read(ecdf_plotobj, store = "sim_intercept-only/_targets")
plots2 = tar_read(ecdf_plotobj, store = "sim_nonzero-betas/_targets")
stats1 = tar_read(stats_table, store = "sim_intercept-only/_targets")
stats2 = tar_read(stats_table, store = "sim_nonzero-betas/_targets")

plots = (plots1[[1]] + plots1[[2]] + plots2[[2]]) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("ecdf_plots_r2d2.pdf", plot = plots, width = 210, height = (297 / 4)*0.85, units = "mm", device = cairo_pdf)

# compare beta values
stats1 %>% filter(grepl("^b_X.+$", variable)) %>%
  rename(value = simulated_value) %>%
  mutate(value = log(abs(value), 10)) %>% group_by(tag) %>%
  reframe(quantile_df(value, probs = c(0.5, 0.9, 0.99))) %>%
  pivot_wider(names_from = quant, values_from = value) %>%
  bind_cols(variable = "allzero", .)
stats2 %>% filter(grepl("^b_X.+$", variable)) %>%
  rename(value = simulated_value) %>%
  mutate(value = log(abs(value), 10)) %>%
  mutate(variable = case_when(
    (variable %in% paste0("b_X", c(1:5,246:250))) ~ "nonzero",
    TRUE ~ "zero")
  ) %>% group_by(variable, tag) %>%
  reframe(quantile_df(value, probs = c(0.5, 0.9, 0.99))) %>%
  pivot_wider(names_from = quant, values_from = value)
# -------

data1 = stats1 %>%
  select(variable, value = simulated_value, tag) %>%
  mutate(variable = case_when(
    grepl("^b_X[0-9]+$", variable) ~ "Slopes",
    grepl("^sdb_X[0-9]+$", variable) ~ "Local scales",
    variable == "R2D2_R2" ~ "R2",
    variable == "sigma" ~ "Error std. dev.",
    TRUE ~ NA_character_
  ))

data2 = stats2 %>%
  select(variable, value = simulated_value, tag) %>%
  mutate(variable = case_when(
    variable %in% paste0("b_X", c(1:5, 246:250)) ~ "Non-zero slopes",
    variable %in% paste0("sdb_X", c(1:5, 246:250)) ~ "Non-zero scales",
    variable == "R2D2_R2" ~ "R2",
    grepl("^b_X[0-9]+$", variable) ~ "Null slopes",
    grepl("^sdb_X[0-9]+$", variable) ~ "Null scales",
    variable == "sigma" ~ "Error std. dev.",
    TRUE ~ NA_character_
  ))

table1 = data1 %>% group_by(variable, tag) %>% reframe(quantile_df(value)) %>%
  pivot_wider(names_from = quant, values_from = value)
table2 = data2 %>% group_by(variable, tag) %>% reframe(quantile_df(value)) %>%
  pivot_wider(names_from = quant, values_from = value)

saveRDS(list(plots1, plots2), "plots.RDS")
saveRDS(list(data1, data2), "data.RDS")
saveRDS(list(table1, table2), "tables.RDS")