library(tidyverse)
load('results.RData')

result_summary

result_summary %>% 
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = log(ratio))) +
  scico::scale_fill_scico() +
  labs(x = 'mu', y = 'sigma') + 
  visibly::theme_clean()

result_summary %>% 
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = sensitive)) +
  scico::scale_fill_scico_d(begin = .25, end = .75) +
  labs(x = 'mu', y = 'sigma') + 
  visibly::theme_clean()


