library(tidyverse)


# create data settings
sigmas <- c(1:5, 10, 25, 50, 75, 100)
mus <- c(0, 1, 5, 10, 25, 50, 100)
N <- c(10, 25, 50, 100)


priors <- crossing(mus, sigmas, N) %>%
  mutate(setting = glue::glue("mu = {mus} sigma = {sigmas} N = {N}")) %>%
  data.frame()

nrow(priors)

# Bob's stan model code

model_string <- "
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

run_model <- function(
                      data_settings,
                      model,
                      verbose = TRUE,
                      ...) {
  data_settings <- unlist(data_settings[1:3])

  mu_mu <- data_settings[1]
  sigma_mu <- data_settings[2]
  y <- rnorm(data_settings[3])

  data <- list(
    N = length(y),
    y = y,
    mu_mu = mu_mu,
    sigma_mu = sigma_mu
  )

  fit <- sampling(
    model,
    data,
    refresh = 0,
    seed = 8675309,
    ...
  )
  
  if (verbose) {
    print(sprintf("mu_mu = %10.5f  sigma_mu = %10.5f", mu_mu, sigma_mu), quote = FALSE)
    print(fit, prob = c(), digits = 4, pars = c("mu", "sigma"))
  }

  fit
}

# testing
library(rstan)
# debugonce(run_model)
#
init_model <- stan_model(model_code = model_string)
#
# run_model(priors[1, , drop = F], model = init_model)


library(rstan)
library(future)
library(furrr)

plan(multiprocess(workers = 10))

# takes ~ 1
system.time({
  results_raw <- priors %>%
    group_split(setting) %>%
    future_map(run_model,
      model = stan_model(model_code = model_string),
      verbose = FALSE,
      iter = 1e4,
      thin = 20, # to reduce size (1e4/2/20*4 = 1k post warmup)
      control = list(
        adapt_delta = .99,
        max_treedepth = 15
      ),
      .progress = TRUE
    )
})


names(results_raw) <- priors$setting
plan(sequential)


clean_up <- function(raw_result) {
  summary(raw_result)$summary %>%
    as_tibble(rownames = "parameter")
}

results_summaries <- results_raw %>%
  map_df(clean_up, .id = "setting")


results_clean <- results_summaries %>%
  left_join(priors) %>%
  mutate(
    ratio = sd / sigmas,
    sensitive = ratio > .1
  ) %>%
  arrange(sigmas)

save(results_raw, file = "results_raw.RData")
save(results_summaries, results_clean, file = "results_summaries.RData")
