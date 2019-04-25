source("libs.R")

show_results <- function(.data, .name) {
  
  .data <- .data %>% 
    dplyr::mutate(mape_cv_1_to_test = abs(test_errors - cv_1_error) / test_errors,
                  mape_cv_2_to_test = abs(test_errors - cv_2_error) / test_errors,
                  mape_cv_1_to_true = abs(true_error_test - cv_1_error) / test_errors,
                  mape_cv_2_to_true = abs(true_error_test - cv_2_error) / test_errors)
  
  # Labeller
  lab <- c("test" = "CV vs. Test Data",
           "true" = "CV vs. True DGP") %>% 
    as_labeller()
  
  # Density plots
  p1 <- .data %>% 
    dplyr::select(mape_cv_1_to_test, mape_cv_2_to_test, 
                  mape_cv_1_to_true, mape_cv_2_to_true) %>% 
    melt() %>% 
    dplyr::mutate(cv = substr(variable, 9, 9),
                  baseline = substr(variable, 14, 17)) %>% 
    ggplot(aes(x = value, fill = cv)) + 
      geom_density(color = "white", alpha = .5) + 
      facet_wrap(~baseline, scales = "free", labeller = lab) + 
      theme_minimal() + 
      theme(strip.background = element_rect(fill = "gray97", size = 0)) + 
      scale_x_continuous(labels = scales::percent) + 
      labs(title = .name) + xlab("MAPE") + ylab("Density")
  
  # Points with smoothing
  p2 <- .data %>% 
    dplyr::select(n, mape_cv_1_to_test, mape_cv_2_to_test, 
                  mape_cv_1_to_true, mape_cv_2_to_true) %>% 
    melt(id.vars = "n") %>% 
    dplyr::mutate(cv = substr(variable, 9, 9),
                  baseline = substr(variable, 14, 17)) %>% 
    ggplot(aes(x = n, y = value, color = cv)) + 
      geom_point(alpha = .25, position = "jitter") + 
      geom_smooth(method = "lm") + 
      facet_wrap(~baseline, scales = "free", labeller = lab) + 
      theme_minimal() + 
      theme(strip.background = element_rect(fill = "gray97", size = 0)) + 
      scale_y_continuous(labels = scales::percent) + 
      labs(title = .name) + xlab("MAPE") + ylab("Number of observations")
  
  # Boxplots
  p3 <- .data %>% 
    dplyr::select(n,
                  mape_cv_1_to_test, mape_cv_2_to_test, 
                  mape_cv_1_to_true, mape_cv_2_to_true) %>% 
    melt(id.vars = "n") %>% 
    dplyr::mutate(cv = substr(variable, 9, 9),
                  baseline = substr(variable, 14, 17)) %>% 
    ggplot(aes(x = factor(n), y = value, color = cv)) + 
      geom_boxplot(outlier.alpha = .25) + 
      facet_wrap(~baseline, scales = "free", labeller = lab) + 
      theme_minimal() + 
      theme(strip.background = element_rect(fill = "gray97", size = 0)) +
      scale_y_continuous(labels = scales::percent) + 
      labs(title = .name) + xlab("Number of observations") + ylab("MAPE")
  
  # Output
  print(p1)
  print(p2)
  print(p3)
  
}

# Data from simulations
load("results/2019-03-04_simulation_results_rf.RData")
load("results/2019-04-15_simulation_results_xgb.RData")
load("results/2019-04-18_simulation_results_cat.RData")
load("results/2019-04-25_simulation_results_lgb.RData")

# Apply analysis of results
show_results(result_rf, "RandomForest")
show_results(result_xgb, "XGBoost")
show_results(result_cat, "CatBoost")
show_results(result_lgb, "LightGBM")
    
