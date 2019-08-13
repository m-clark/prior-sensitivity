library(tidyverse)
library(visibly)

# load("results_raw.RData")
load("results_summaries.RData")

results_clean

mus <- results_clean %>%
  filter(parameter == "mu")

mean(!mus$sensitive)

mus %>%
  arrange(mus, desc(sigmas)) %>%
  select(setting, sensitive) %>%
  data.frame()

# plot of mu sd vs. prior sd
ggplot(mus) +
  geom_vline(aes(xintercept = .1), alpha = .5) +
  geom_density(
    aes(x = ratio),
    fill = "#ff550080",
    color = "#ff5500"
  ) +
  labs(x = "ratio", y = NULL) +
  theme_clean()

# plot of mu z vs. ratio
mus %>%
  mutate(Z = abs((mean - mus) / sigmas)) %>%
  ggplot() +
  geom_hline(aes(yintercept = .1), alpha = .25) +
  geom_point(aes(Z, ratio, size = N, alpha = .1), color = "#ff5500") +
  scale_size_continuous(range = c(1, 4), trans = 'reverse') +
  scale_alpha(range = .1, guide = 'none') +
  theme_clean()
ggsave('z_vs_ratio.png')


mus %>%
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = ratio)) +
  scico::scale_fill_scico(trans = "log") +
  labs(x = "mu", y = "sigma") +
  theme_clean()

mus %>%
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = ratio)) +
  facet_grid(~N, labeller = label_both) +
  scico::scale_fill_scico(trans = "log", breaks = c(.01, .1, 1)) +
  labs(x = "mu", y = "sigma") +
  theme_clean()

ggsave("main_plot.png")


mus %>%
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = sensitive)) +
  facet_grid(~N, labeller = label_both) +
  scico::scale_fill_scico_d(begin = .25, end = .75) +
  labs(x = "mu", y = "sigma") +
  theme_clean()

mus %>%
  ggplot() +
  geom_point(
    aes(x = as.factor(N), y = ratio, size = mus * sigmas),
    color = "#ff5500",
    alpha = .1
  ) +
  scico::scale_fill_scico() +
  labs(x = "N", y = "ratio") +
  theme_clean()
