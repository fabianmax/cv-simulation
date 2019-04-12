source("libs.R")

# RandomForest ------------------------------------------------------------

# Function for fitting ranger model
fit_rf <- function(df, params, ...) {
  
  # Run training via formula interface
  mod <- ranger(y ~ ., 
                data = df, 
                num.trees = params$trees)
  
  return(mod)
  
}

# Predict function for fitted ranger model
predict_rf <- function(mod, newdata, ...) {
  
  # Predict
  predict(mod, newdata)$predictions
  
}

# XGBoost -----------------------------------------------------------------

# Function for fitting XGBoost model
fit_xgb <- function(df, params, ...) {
  
  # Which columns is the target
  target_idx <- which(colnames(df) == "y")
  
  # Check if early stopping should be applied
  if (!is.null(params$early_stopping_rounds)) {
    
    # Create train/valid split and convert to DMatrix format
    in_train <- createDataPartition(y = df$y, p = 0.8, list = FALSE)
    train_dmatrix <- xgb.DMatrix(data = as.matrix(df[in_train, -target_idx]), label = df$y[in_train])
    valid_dmatrix <- xgb.DMatrix(data = as.matrix(df[-in_train, -target_idx]), label = df$y[-in_train])
    watchlist <- list(validation = valid_dmatrix)
  
  } else {
    
    # Convert directly to DMatrix format
    target_idx <- which(colnames(df) == "y")
    train_dmatrix <- xgb.DMatrix(data = as.matrix(df[, -target_idx]), label = df$y)
    watchlist <- NULL
    
  }
  
  # Run training
  mod <- xgb.train(params = list(eta = params$eta,
                                 gamma = params$gamma,
                                 max_depth = params$max_depth,
                                 min_child_weight = params$min_child_weight,
                                 subsample = params$subsample,
                                 colsample_bytree = params$colsample_bytree,
                                 lambda = params$lambda,
                                 alpha = params$alpha,
                                 nthread = params$nthread), 
                   data = train_dmatrix,
                   nrounds = params$nrounds,
                   early_stopping_rounds = params$early_stopping_rounds,
                   watchlist = watchlist,
                   booster = "gbtree",
                   objective = "reg:linear",
                   eval_metric = "rmse",
                   verbose = 0)
  
  return(mod)
  
}

# Predict function for fitted XGBoost model
predict_xgb <- function(mod, newdata, ...) {
  
  # Convert to specific DMatrix and predict
  test_dmatrix <- xgb.DMatrix(data = as.matrix(newdata), label = newdata$y)
  predict(mod, test_dmatrix)
  
}


# CatBoost ----------------------------------------------------------------

# Function for fitting CatBoost model
fit_cat <- function(df, params, ...) {
  
  # Which columns is the target
  target_idx <- which(colnames(df) == "y")
  
  # Check if early stopping should be applied
  if (params$od_pval > 0) {
    
    # Create train/valid split and convert to catboost.Pool format
    in_train <- createDataPartition(y = df$y, p = 0.8, list = FALSE)
    train_pool <- catboost.load_pool(data = as.matrix(df[in_train, -target_idx]), label = df$y[in_train])
    valid_pool <- catboost.load_pool(data = as.matrix(df[-in_train, -target_idx]), label = df$y[-in_train])
    
  } else {
    
    # Convert directly to catboost.Pool format
    train_pool <- catboost.load_pool(data = as.matrix(df[, -target_idx]), label = df$y)
    valid_pool <- NULL
    
  }
  
  # Run training
  mod <- catboost.train(learn_pool = train_pool,
                        test_pool = valid_pool,
                        params = list(iterations = params$nrounds,
                                      #learning_rate = params$eta, defined automatically
                                      l2_leaf_reg = params$lambda_l2,
                                      depth = params$max_depth,
                                      min_data_in_leaf = params$min_data_in_leaf,
                                      rsm = params$colsample_bytree,
                                      thread_count = params$nthread,
                                      loss_function = "RMSE",
                                      eval_metric = "RMSE",
                                      grow_policy = "SymmetricTree",
                                      logging_level = "Silent",
                                      # Overfitting detector
                                      od_type = "IncToDec",
                                      od_pval = params$od_pval,
                                      od_wait = 10)
                        )
  
  return(mod)
  
}


# Predict function for fitted CatBoost model
predict_cat <- function(mod, newdata, ...) {
  
  # Convert to specific catboost.Pool and predict
  test_pool <- catboost.load_pool(data = as.matrix(newdata))
  catboost.predict(mod, test_pool)
  
}

