# Install from CRAN
install.packages(c("dplyr",
                   "caret",
                   "rsample",
                   "purrr",
                   "furrr",
                   "ggplot2",
                   "ranger",
                   "xgboost",
                   "viridis",
                   "devtools"))

# Install from github
devtools::install_github("andrebleier/Xy")
devtools::install_github("catboost/catboost", subdir = "catboost/R-package")
