source("code.R")
source("models.R")

set.seed(1000)

# Settings
runs <- 10
n_min <- 1000
n_max <- 10000

# XGBoost -----------------------------------------------------------------

# Parameters
params_xgb <- expand.grid(nrounds = c(50, 100, 150, 250, 500),
                          eta = c(0.3),
                          gamma = 0,
                          max_depth = c(3, 6, 9),
                          min_child_weight = 1,
                          subsample = 1,
                          colsample_bytree = c(0.8, 1),
                          lambda = c(0, 1),
                          alpha = c(0, 1),
                          early_stopping_rounds = 5,
                          nthread = 6)

# Run
result_xgb <- runs %>% 
  runif(min = n_min, max = n_max) %>% 
  round() %>% 
  as.list() %>% 
  map(~ run_experiment(n = .x, list(fit_xgb, predict_xgb), params_xgb)) %>% 
  map2(.x = ., .y = seq(length(.)), ~mutate(.x, id = .y)) %>% 
  bind_rows()

path <- paste0("results/", Sys.Date(), "_simulation_results.RData")
save(result, file = path)


# RandomForest ------------------------------------------------------------

# Parameters
params_rf <- expand.grid(trees = c(25, 50, 75, 100, 150, 200, 250))

# Run
result_rf <- runs %>% 
  runif(min = n_min, max = n_max) %>% 
  round() %>% 
  as.list() %>% 
  map(~ run_experiment(n = .x, list(fit_rf, predict_rf), params_rf)) %>% 
  map2(.x = ., .y = seq(length(.)), ~mutate(.x, id = .y)) %>% 
  bind_rows()

