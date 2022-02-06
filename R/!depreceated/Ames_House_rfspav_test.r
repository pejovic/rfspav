library(tidyverse)
library(tidymodels)
library(sf)
library(ranger)
library(doParallel)
library(foreach)
library(AmesHousing)

ames <- make_ames()
glimpse(ames)
ames$Sale_Price <- as.numeric(ames$Sale_Price)

set.seed(123)
ames_split <- initial_split(ames, prop = 0.70, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_folds <- vfold_cv(ames_train, v = 5)

ames_fun <- as.formula(paste("Sale_Price", paste(names(ames_train)[c(80, 81, 1:78)], collapse = "+"), sep = "~"))


rf_grid <- grid_regular(
  neighbors =  neighbors(range = c(7, 14)),
  trees(range = c(400,2000)),
  mtry(range = c(10, 30)),
  min.node.size = min_n(range = c(2, 8)),
  levels = 5
)

best_rf_params <- tune.rfspav(formula = ames_fun,
                           data_resample = ames_folds,
                           coord_names = c("Longitude", "Latitude"),
                           param_grid = rf_grid,
                           metric = "rmse",
                           type = "rf",
                           cpus = 6,
                           importance = "impurity",
                           gower_vars = NA,
                           clust_style = "quantile",
                           num_class = 15,
                           feature_select = FALSE,
                           data_crs = NA,
                           predict_resamples = FALSE, seed = 124)



best_params
