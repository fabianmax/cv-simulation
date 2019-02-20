source("code.R")

load("results/2019-02-18_simulation_results.RData")

result <- result %>% 
  dplyr::mutate(mape_cv_1 = abs(test_errors - cv_1_error) / test_errors,
                mape_cv_2 = abs(test_errors - cv_2_error) / test_errors)



result %>% 
  dplyr::select(mape_cv_1, mape_cv_2) %>% 
  melt() %>% 
  ggplot(aes(x = value, fill = variable)) + 
    geom_density()


result %>% 
  dplyr::select(n, mape_cv_1, mape_cv_2) %>% 
  melt(id.vars = "n") %>% 
  ggplot(aes(x = n, y = value, color = variable)) + 
    geom_point() + 
    geom_smooth(method = "lm")
