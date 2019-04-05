# LightGBM ----------------------------------------------------------------

fit_lgb <- function(df, params, ...) {
  
  # Check if early stopping should be applied
  if (!is.null(params$early_stopping_rounds)) {
    
    # Create train/valid split and convert to DMatrix format
    in_train <- createDataPartition(y = df$y, p = 0.8, list = FALSE)
    target_idx <- which(colnames(df) == "y")
    lgb_train <- lgb.Dataset(data = as.matrix(df[in_train, -target_idx]), label = df$y[in_train])
    lgb_valid <- lgb.Dataset(data = as.matrix(df[-in_train, -target_idx]), label = df$y[-in_train])
    watchlist <- list(eval = lgb_valid)
    
  } else {
    
    # Convert directly to DMatrix format
    lgb_train <- lgb.Dataset(data = as.matrix(df), label = df$y)
    lgb_valid <- list()
    watchlist <- list()
    
  }
  
  # Run training
  mod <- lgb.train(params = list(eta = params$eta,
                                 min_gain_to_split = params$min_gain_to_split,
                                 num_leaves = params$num_leaves,
                                 min_child_weight = params$min_child_weight,
                                 subsample = params$subsample,
                                 colsample_bytree = params$colsample_bytree,
                                 lambda_l1 = params$lambda_l1,
                                 lambda_l2 = params$lambda_l2,
                                 nthread = params$nthread),
                   data = lgb_train,
                   nrounds = params$nrounds,
                   valids = watchlist,
                   early_stopping_rounds = params$early_stopping_rounds,
                   obj = "regression",
                   eval = "mean_squared_error")
  
  # mod <- lgb.train(data = lgb_train,
  #                  obj = "regression")
  # 
  # lightgbm(data = lgb.Dataset(data = as.matrix(df[in_train, -21])),
  #          label = df$y[in_train],
  #          num_leaves = 4,
  #          learning_rate = 1,
  #          nrounds = 2,
  #          objective = "regression")
  
  
}


# LightGBM ----------------------------------------------------------------

# Parameters
params_lgb <- expand.grid(nrounds = c(250),
                          eta = c(0.3),
                          min_gain_to_split = c(0, 2),
                          max_depth = 2^c(3, 6, 9), # simple rule of thumb for getting from max_depth to num_leaves
                          min_child_weight = 1,
                          subsample = c(0.7, 1),
                          colsample_bytree = c(0.7),
                          lambda_l1 = c(0, 1),
                          lambda_l2 = c(0, 1),
                          early_stopping_rounds = 3)


params <- expand.grid(nrounds = c(250),
                      eta = c(0.3),
                      min_gain_to_split = c(0),
                      max_depth = 2^c(3), # simple rule of thumb for getting from max_depth to num_leaves
                      min_child_weight = 1,
                      subsample = c(1),
                      colsample_bytree = c(0.7),
                      lambda_l1 = c(1),
                      lambda_l2 = c(1),
                      early_stopping_rounds = 3)

source("libs.R")

# Simulate data
my_sim <- Xy(n = 1000, 
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

test <- fit_lgb(df_sim, params)

df_sim <- SLC14_1(1000)



