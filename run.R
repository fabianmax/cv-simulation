source("libs.R")
source("functions.R")
source("models.R")

set.seed(1000)

# Settings
runs <- 10
n_min <- 1000
n_max <- 10000
folds <- 10
cpus <- 4

# XGBoost -----------------------------------------------------------------

# Parameters
params_xgb <- expand.grid(nrounds = c(250),
                          eta = c(0.3),
                          gamma = c(0, 2),
                          max_depth = c(3, 6, 9),
                          min_child_weight = 1,
                          subsample = c(0.7, 1),
                          colsample_bytree = c(0.7, 1),
                          lambda = c(0, 1),
                          alpha = c(0, 1),
                          early_stopping_rounds = 3,
                          nthread = cpus)

# Run
result_xgb <- runs %>% 
  runif(min = n_min, max = n_max) %>% 
  round() %>% 
  as.list() %>% 
  map(~ run_experiment(n = .x, list(fit_xgb, predict_xgb), params_xgb, folds = folds)) %>% 
  map2(.x = ., .y = seq(length(.)), ~mutate(.x, id = .y)) %>% 
  bind_rows()

path_xgb <- paste0("results/", Sys.Date(), "_simulation_results_xgb.RData")
save(result_xgb, params_xgb, file = path_xgb)



# CatBoost ----------------------------------------------------------------

# Parameters
params_cat <- expand.grid(nrounds = c(100, 250),
                          l2_leaf_reg = c(0, 1),
                          depth = c(3, 6, 9),
                          min_data_in_leaf = 1,
                          rsm = c(0.7, 1),
                          od_pval = c(0, 10^-1, 10^-2, 10^-3),
                          thread_count = cpus)

# Run
result_cat <- runs %>% 
  runif(min = n_min, max = n_max) %>% 
  round() %>% 
  as.list() %>% 
  map(~ run_experiment(n = .x, list(fit_cat, predict_cat), params_cat, folds = folds)) %>% 
  map2(.x = ., .y = seq(length(.)), ~mutate(.x, id = .y)) %>% 
  bind_rows()

path_cat <- paste0("results/", Sys.Date(), "_simulation_results_cat.RData")
save(result_cat, params_cat, file = path_cat)


# LightGBM ----------------------------------------------------------------

# Parameters
params_lgb <- expand.grid(num_iterations = c(100, 250),
                          learning_rate = 0.1,
                          num_leaves = c(8, 32, 64, 128),
                          max_depth = -1,
                          min_data_in_leaf = c(1, 10, 100),
                          bagging_fraction = 1,
                          feature_fraction = c(0.7, 1),
                          lambda_l1 = 0,
                          lambda_l2 = c(0, 1),
                          early_stopping_rounds = 3,
                          num_threads = cpus)

# Run
result_lgb <- runs %>% 
  runif(min = n_min, max = n_max) %>% 
  round() %>% 
  as.list() %>% 
  map(~ run_experiment(n = .x, list(fit_lgb, predict_lgb), params_lgb, folds = folds)) %>% 
  map2(.x = ., .y = seq(length(.)), ~mutate(.x, id = .y)) %>% 
  bind_rows()

path_lgb <- paste0("results/", Sys.Date(), "_simulation_results_lgb.RData")
save(result_lgb, params_lgb, file = path_lgb)


# RandomForest ------------------------------------------------------------

# Parameters
params_rf <- expand.grid(trees = c(25, 50, 75, 100, 150, 200, 250),
                         mtry = c(2, 4, 6, 8, 10),
                         nthread = cpus)

# Run
result_rf <- runs %>% 
  runif(min = n_min, max = n_max) %>% 
  round() %>% 
  as.list() %>% 
  map(~ run_experiment(n = .x, list(fit_rf, predict_rf), params_rf, folds = folds)) %>% 
  map2(.x = ., .y = seq(length(.)), ~mutate(.x, id = .y)) %>% 
  bind_rows()

path_rf <- paste0("results/", Sys.Date(), "_simulation_results_rf.RData")
save(result_rf, params_rf, file = path_rf)

