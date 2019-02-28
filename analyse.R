source("libs.R")

# RandomForest ------------------------------------------------------------

load("results/2019-02-20_simulation_results_rf.RData")

result_rf <- result_rf %>% 
  dplyr::mutate(mape_cv_1 = abs(test_errors - cv_1_error) / test_errors,
                mape_cv_2 = abs(test_errors - cv_2_error) / test_errors)

result_rf %>% 
  dplyr::select(mape_cv_1, mape_cv_2) %>% 
  melt() %>% 
  ggplot(aes(x = value, fill = variable)) + 
    geom_density(color = "white", alpha = .5)

result_rf %>% 
  dplyr::select(n, mape_cv_1, mape_cv_2) %>% 
  melt(id.vars = "n") %>% 
  ggplot(aes(x = n, y = value, color = variable)) + 
    geom_point() + 
    geom_smooth(method = "lm")


# XGBoost -----------------------------------------------------------------

load("results/2019-02-20_simulation_results_xgb.RData")

result_xgb <- result_xgb %>% 
  dplyr::mutate(mape_cv_1 = abs(test_errors - cv_1_error) / test_errors,
                mape_cv_2 = abs(test_errors - cv_2_error) / test_errors)

result_xgb %>% 
  dplyr::select(mape_cv_1, mape_cv_2) %>% 
  melt() %>% 
  ggplot(aes(x = value, fill = variable)) + 
  geom_density(color = "white", alpha = .5)

result_xgb %>% 
  dplyr::select(n, mape_cv_1, mape_cv_2) %>% 
  melt(id.vars = "n") %>% 
  ggplot(aes(x = n, y = value, color = variable)) + 
  geom_point() + 
  geom_smooth(method = "lm")
