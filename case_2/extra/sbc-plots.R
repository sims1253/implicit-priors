# Simulate some plots to illustrate SBC diagnostics
# code provided by Paul Buerkner
library(SBC)
library(posterior)
library(ggplot2)
library(patchwork)
library(bayesplot)
library(extrafont)
loadfonts(device = "pdf")
theme_set(theme_default())

Nsim <- 100
S <- 999

set.seed(987)
x_unif <- round(runif(Nsim) * S) + 1
hist(x_unif)

set.seed(13)
x_over <- round(rbeta(Nsim, 1.5, 2) * S) + 1
hist(x_over)

set.seed(321)
x_narrow <- round(rbeta(Nsim, 0.35, 0.35) * S) + 1
hist(x_narrow)

names_x <- c("x_a_unif", "x_b_over", "x_c_narrow")

ranks <- data.frame(
 sim_id = rep(1:Nsim, length(names_x)),
 variable = factor(
   rep(names_x, each = Nsim),
   levels = names_x
 ),
 rank = c(x_unif, x_over, x_narrow)
)

gg1 <- ggplot(ranks, aes(rank)) +
 geom_histogram(
  fill = "#9eccfb", color = "black",
  binwidth = 100, alpha = 0.8
) +
 facet_wrap("variable") +
 theme(legend.title=element_blank(),
       strip.text.x = element_blank(),
       axis.title.y = element_blank(),
       text = element_text(size = 16, family = "Palatino")) +
 NULL

gg2 <- plot_ecdf_diff(ranks, max_rank = S + 1) +
 guides(color = "none", fill = "none") +
 theme(legend.title=element_blank(),
       strip.text.x = element_blank(),
       text = element_text(size = 16, family = "Palatino")) +
 NULL

gg2

ggsave("extra/SBC_plots.pdf", width = 10, height = 2.5)