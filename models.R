source("libs.R")

# RandomForest ------------------------------------------------------------

# Function for fitting ranger model
fit_rf <- function(df, params, ...) {
  
  # Run training
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
  
  # Check if early stopping should be applied
  if (!is.null(params$early_stopping_rounds)) {
    
    # Create train/valid split and convert to DMatrix format
    in_train <- createDataPartition(y = df$y, p = 0.8, list = FALSE)
    xgb_train <- xgb.DMatrix(data = as.matrix(df[in_train, ]), label = df$y[in_train])
    xgb_valid <- xgb.DMatrix(data = as.matrix(df[-in_train, ]), label = df$y[-in_train])
    watchlist <- list(validation = xgb_valid)
  
  } else {
    
    # Convert directly to DMatrix format
    xgb_train <- xgb.DMatrix(data = as.matrix(df), label = df$y)
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
                   data = xgb_train,
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
  xgb_test <- xgb.DMatrix(data = as.matrix(newdata), label = newdata$y)
  predict(mod, xgb_test)
  
}