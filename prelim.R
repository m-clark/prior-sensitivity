library(tidyverse)


# create data settings
sigmas = c(1:5, 10, 25, 50, 75, 100)
mus = c(0, 1:5, 10, 25, 50, 100)

priors = crossing(mus, sigmas) %>% 
  mutate(setting = glue::glue("mu = {mus} sigma = {sigmas}")) %>% 
  data.frame()


# stan model code
model_string = "
data {
  real mu_mu;
  real sigma_mu;
  int N;
  vector[N] y;
}
parameters {
  real mu;
  real sigma;
}
model {
  y ~ normal(mu, sigma);
  mu ~ normal(mu_mu, sigma_mu);
  sigma ~ normal(0, 10);
}
"

# function to run models

run_model = function(data_settings) {
  model <- stan_model(model_code = model_string)
  y <- rnorm(10)
  
  data_settings = unlist(data_settings[1:2])
  mu_mu = data_settings[1]
  sigma_mu = data_settings[2]
  
  data <- list(
    N = length(y),
    y = y,
    mu_mu = mu_mu,
    sigma_mu = sigma_mu
  )
  
  fit <- sampling(
    model,
    data,
    iter = 1e5,
    cores = 1,
    refresh = 0,
    control = list(adapt_delta = .99),
    seed = 8675309
  )
  
  print(sprintf("mu_mu = %10.5f  sigma_mu = %10.5f", mu_mu, sigma_mu), quote = FALSE)
  print(fit, prob = c(), digits = 4, pars = c("mu", "sigma"))
  
  summary(fit)$summary
}

# testing
# library(rstan)
# test = apply(priors[1:2, 1:2], 1, run_model)


library(rstan)
library(future)
library(furrr)
plan(multiprocess(workers = 6))

# takes ~ 10+ minutes on my machine
system.time({
  result <- priors %>%
    group_split(setting) %>% 
    future_map(run_model)
})


result_summary = result %>% 
  map_df(function(x) select(data.frame(x), mean, sd)) %>% 
  bind_cols(priors, .) %>% 
  mutate(ratio = sd/sigmas,
         sensitive = ratio > .1) %>% 
  arrange(sigmas)


result_summary %>% 
  ggplot() +
  geom_tile(aes(x = as.factor(mus), y = as.factor(sigmas), fill = ratio)) +
  scico::scale_fill_scico() +
  labs(x = 'mu', y = 'sigma') + 
  visibly::theme_clean()


