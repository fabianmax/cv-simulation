source("libs.R")

# Separate Train/Test partition from rsample object
get_train_test <- function(obj, index) {
  
  df_train <- obj$splits[[index]] %>% 
    rsample::analysis()
  
  df_test <- obj$splits[[index]] %>% 
    rsample::assessment()
  
  return(list(train = df_train, test = df_test))
  
}

# Calculates RootMeanSquaredError
rmse <- function(act, pred) sqrt(mean((act - pred)^2))

# Calculate MeanAbsolutErrpr
mae <- function(act, pred) mean(abs(act - pred))

# Convert data.frame row to named list
row_to_list <- function(row) {
  
  a_list <- as.list(row)
  attr(a_list, "out.attrs") <- NULL
  
  return(a_list)
}

# Prepare params for model call
prepare_params <- function(params) {
  
  if (class(params) %in% c("data.frame", "list")) {
    if ("id" %in% names(params)) {
      params$id <- NULL
    }
    if (class(params) == "data.frame") {
      params <- row_to_list(params)
    }
  } else {
    stop("params should be list or data.frame")
  }
  
  return(params)
  
}

# Decompose Xy into target, signal and noise
Xy_decompose <- function(x) {
  
  if (class(x) != "Xy_sim") stop("x must be an object of class Xy_sim")
  
  # Transform Xy object and get names of true estimators
  x_transformed <- transform(x)
  sig_vars <- names(x_transformed)[!grepl("^NOISE_*", names(x_transformed)) & 
                                 names(x_transformed) != "y"]
  
  #  Extract target column, calculate signal and noise
  target <- x_transformed$y
  signal <- rowSums(x_transformed[, ..sig_vars])
  noise <- target - signal
  
  out <- data.frame(target, signal, noise)
  
  return(out)
  
}

# Runs Cross-Validation in version 1 (fixed fold assignment):
# - 1) Loop over folds
# - 2) Loop over params in each fold
cv_1 <- function(df, funs, params, k = 10) {
  
  # Create result container and folds object
  container <- list()
  folds <- rsample::vfold_cv(df, v = k)
  
  # Loop over folds
  pb <- txtProgressBar(min = 0, max = nrow(folds), style = 3)
  for (i in seq(nrow(folds))) {
    
    # Get data and create container for cv errors
    df_fold <- get_train_test(folds, index = i)
    cv_errors <- rep(NA, nrow(params))
    
    # Within each fold, loop over params
    for (j in seq(nrow(params))) {
      
      # Fit model on training, predict on holdout and calculate error
      mod <- do.call(funs[[1]], list(df = df_fold$train, params = params[j, ]))
      p <- do.call(funs[[2]], list(mod = mod, newdata = df_fold$test))
      cv_errors[j] <- rmse(df_fold$test$y, p)
      
    }
    
    # Save parameters, folds, and cv errors
    container[[i]] <- bind_cols(params, data.frame(cv_error = cv_errors,
                                                   fold = i))
    
    setTxtProgressBar(pb, i)
    
  }
  
  # Calculate the crossvalidated error as mean over folds
  container <- container %>% 
    dplyr::bind_rows() %>% 
    dplyr::group_by_at(vars(-cv_error, -fold)) %>% 
    dplyr::summarise(cv_error = mean(cv_error))
  
  return(container)
  
}

# Runs Cross-Validation in version 2 (varying fold assignment):
# - 1) Loop over params
# - 2) Loop over folds in each param
cv_2 <- function(df, funs, params, k = 10) {
  
  # Loop over parameters
  pb <- txtProgressBar(min = 0, max = 10, style = 3)
  for (i in seq(nrow(params))) {
    
    # Create new fold-assignment for each parameterset 
    folds <- rsample::vfold_cv(df, v = k)
    cv_errors <- rep(NA, nrow(folds))
    
    # Loop over folds
    for (j in seq(nrow(folds))) {
      
      # Get the data, fit model in training, predict on holdout, and calculate
      # errors
      df_fold <- get_train_test(folds, index = j)
      mod <- do.call(funs[[1]], list(df = df_fold$train, params = params[i, ]))
      p <- do.call(funs[[2]], list(mod = mod, newdata = df_fold$test))
      cv_errors[j] <- rmse(df_fold$test$y, p)
      
    }
    
    # Calculate the crossvalidated error over folds
    params$cv_error[i] <- mean(cv_errors)
    
    setTxtProgressBar(pb, i)
    
  }
  
  return(params)
  
}

# Function for running one entire experiment with n samples
run_experiment <- function(n, funs, params, folds) {
  
  # Simulate data
  my_sim <- Xy(n = n, 
               numvars = c(10, 10), 
               catvars = 0, 
               noisevars = 0, 
               task = Xy_task(), 
               nlfun = function(x) x^2, 
               interactions = 1, 
               sig = c(1, 4),  
               cor = c(0), 
               weights = c(-10, 10), 
               intercept = FALSE, 
               stn = 4)
  
  # Extract data
  df_sim <- as.data.frame(my_sim$data)
  
  # Create hold-out
  in_train <- createDataPartition(y = df_sim$y, p = 0.7, list = FALSE)
  df_train <- df_sim[in_train, ]
  df_test <- df_sim[-in_train, ]
  
  # Parameter id
  params$id <- 1:nrow(params)
  
  # Run cv version 1
  print("Start running cv version 1")
  cv_1_res <- cv_1(df_train, funs, params, k = folds)
  
  # Run cv version 2
  print("Start running cv version 2")
  cv_2_res <- cv_2(df_train, funs, params, k = folds)
  
  # Apply models on holdout
  print("Fit models on holdout")
  test_errors <- params %>% 
    purrr::transpose() %>% 
    purrr::map(function(params) do.call(funs[[1]], list(df = df_train, params = params))) %>% 
    purrr::map(function(mod) do.call(funs[[2]], list(mod = mod, newdata = df_test))) %>% 
    purrr::map(~ rmse(act = df_test$y, pred = .x)) %>% 
    unlist()
  
  # Calculate "true" error
  print("Calculate true error")
  my_sim_decomposed <- Xy_decompose(my_sim)
  
  true_error_train <- rmse(my_sim_decomposed$target[in_train], 
                           my_sim_decomposed$signal[in_train])
  
  true_error_test <- rmse(my_sim_decomposed$target[-in_train], 
                          my_sim_decomposed$signal[-in_train])
  
  # Output
  out <- tibble(n = n,
                folds = folds,
                cv_1_error = cv_1_res$cv_error,
                cv_2_error = cv_2_res$cv_error,
                test_errors = test_errors,
                true_error_train = true_error_train,
                true_error_test = true_error_test)
  
  return(out)
  
}




