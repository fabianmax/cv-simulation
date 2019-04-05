source("libs.R")

# RandomForest ------------------------------------------------------------

load("results/2019-03-04_simulation_results_rf.RData")

result_rf <- result_rf %>% 
  dplyr::mutate(mape_cv_1_to_test = abs(test_errors - cv_1_error) / test_errors,
                mape_cv_2_to_test = abs(test_errors - cv_2_error) / test_errors,
                mape_cv_1_to_true = abs(true_error_test - cv_1_error) / test_errors,
                mape_cv_2_to_true = abs(true_error_test - cv_2_error) / test_errors)

result_rf %>% 
  dplyr::select(mape_cv_1_to_test, mape_cv_2_to_test, 
                mape_cv_1_to_true, mape_cv_2_to_true) %>% 
  melt() %>% 
  dplyr::mutate(cv = substr(variable, 9, 9),
                baseline = substr(variable, 14, 17)) %>% 
  ggplot(aes(x = value, fill = cv)) + 
    geom_density(color = "white", alpha = .5) + 
    facet_wrap(~baseline, scales = "free") + 
    theme_minimal()

result_rf %>% 
  dplyr::select(n, mape_cv_1_to_test, mape_cv_2_to_test, 
                mape_cv_1_to_true, mape_cv_2_to_true) %>% 
  melt(id.vars = "n") %>% 
  dplyr::mutate(cv = substr(variable, 9, 9),
                baseline = substr(variable, 14, 17)) %>% 
  ggplot(aes(x = n, y = value, color = cv)) + 
    geom_point(alpha = .25, position = "jitter") + 
    geom_smooth(method = "lm") + 
    facet_wrap(~baseline, scales = "free") + 
    theme_minimal()


# XGBoost -----------------------------------------------------------------

load("results/2019-03-04_simulation_results_xgb.RData")

result_xgb <- result_xgb %>% 
  dplyr::mutate(mape_cv_1_to_test = abs(test_errors - cv_1_error) / test_errors,
                mape_cv_2_to_test = abs(test_errors - cv_2_error) / test_errors,
                mape_cv_1_to_true = abs(true_error_test - cv_1_error) / test_errors,
                mape_cv_2_to_true = abs(true_error_test - cv_2_error) / test_errors)

result_xgb %>% 
  dplyr::select(mape_cv_1_to_test, mape_cv_2_to_test, 
                mape_cv_1_to_true, mape_cv_2_to_true) %>% 
  melt() %>% 
  dplyr::mutate(cv = substr(variable, 9, 9),
                baseline = substr(variable, 14, 17)) %>% 
  ggplot(aes(x = value, fill = cv)) + 
  geom_density(color = "white", alpha = .5) + 
  facet_wrap(~baseline, scales = "free") + 
  theme_minimal()

result_xgb %>% 
  dplyr::select(n, mape_cv_1_to_test, mape_cv_2_to_test, 
                mape_cv_1_to_true, mape_cv_2_to_true) %>% 
  melt(id.vars = "n") %>% 
  dplyr::mutate(cv = substr(variable, 9, 9),
                baseline = substr(variable, 14, 17)) %>% 
  ggplot(aes(x = n, y = value, color = cv)) + 
  geom_point(alpha = .25, position = "jitter") + 
  geom_smooth(method = "lm") + 
  facet_wrap(~baseline, scales = "free") + 
  theme_minimal()
