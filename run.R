set.seed(1000)

# Parameters
params <- expand.grid(nrounds = c(50, 100, 150, 250, 500),
                      eta = c(0.3),
                      gamma = 0,
                      max_depth = c(3, 6, 9),
                      min_child_weight = 1,
                      subsample = 1,
                      colsample_bytree = c(0.8, 1),
                      lambda = c(0, 1),
                      alpha = c(0, 1),
                      nthread = 6)

# Settings
runs <- 10

# Run
result <- runs %>% 
  runif(min = 1000, max = 10000) %>% 
  round() %>% 
  as.list() %>% 
  map(~ run_experiment(n = .x, params)) %>% 
  map2(.x = ., .y = seq(length(.)), ~mutate(.x, id = .y)) %>% 
  bind_rows()

path <- paste0("results/", Sys.Date(), "_simulation_results.RData")
save(result, file = path)
