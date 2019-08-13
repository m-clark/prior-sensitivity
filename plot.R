library(tidyverse)
library(visibly)

load('results.RData')

results_clean

mus = results_clean %>% 
  filter(parameter == 'mu')

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
    fill = '#ff550080',
    color = '#ff5500'
    ) + 
  labs(x = 'ratio', y = NULL) +
  theme_clean()

mus %>% 
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = log(ratio))) +
  scico::scale_fill_scico() +
  labs(x = 'mu', y = 'sigma') + 
  theme_clean()

mus %>% 
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = log(ratio))) +
  # geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = ratio)) +
  facet_grid(~N, labeller = label_both) +
  scico::scale_fill_scico() +
  labs(x = 'mu', y = 'sigma') + 
  theme_clean()


mus %>% 
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = sensitive)) +
  facet_grid(~N, labeller = label_both) +
  scico::scale_fill_scico_d(begin = .25, end = .75) +
  labs(x = 'mu', y = 'sigma') + 
  theme_clean()


mus %>% 
  ggplot() +
  geom_point(
    aes(x = as.factor(N), y = ratio, size = mus*sigmas), 
    color = '#ff5500',
    alpha = .1
    ) +
  scico::scale_fill_scico() +
  labs(x = 'N', y = 'ratio') + 
  theme_clean()
