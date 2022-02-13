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

set.seed(124)
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

rf_grid <- dplyr::sample_n(rf_grid, 6)

#================ RF ===========================================================

rf_tune_results <- tune.rfspav(formula = ames_fun,
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
                                  data_crs = 4326,
                                  predict_resamples = FALSE, seed = 124)


best_rf_params <- select_best_params(rf_tune_results)



rf_final_model <- rfspav::rfspav(formula = ames_fun,
                                    data = ames_train,
                                    coord_names = c("Longitude", "Latitude"),
                                    neighbors = best_rf_params$neighbors,
                                    type = "rf",
                                    trees = best_rf_params$trees,
                                    mtry = best_rf_params$mtry,
                                    min.node.size = best_rf_params$min.node.size,
                                    progress = TRUE,
                                    importance = "impurity",
                                    gower_vars = NA,
                                    clust_style = "quantile",
                                    num_class = 10,
                                    feature_select = FALSE,
                                    data_crs = 4326)



ames_test$rf.pred <- rfspav::predict.rfspav(rf_final_model, new_data = ames_test)

yardstick::rsq(ames_test, truth = Sale_Price, estimate = rf.pred)

#============ RFSP ============================================================


rfsp_tune_results <- tune.rfspav(formula = ames_fun,
                                  data_resample = ames_folds,
                                  coord_names = c("Longitude", "Latitude"),
                                  param_grid = rf_grid,
                                  metric = "rmse",
                                  type = "rfsp",
                                  cpus = 6,
                                  importance = "impurity",
                                  gower_vars = NA,
                                  clust_style = "quantile",
                                  num_class = 15,
                                  feature_select = FALSE,
                                  data_crs = 4326,
                                  predict_resamples = FALSE, seed = 124)


best_rfsp_params <- select_best_params(rfsp_tune_results)



rfsp_final_model <- rfspav::rfspav(formula = ames_fun,
                                    data = ames_train,
                                    coord_names = c("Longitude", "Latitude"),
                                    neighbors = best_rfsp_params$neighbors,
                                    type = "rfsp",
                                    trees = best_rfsp_params$trees,
                                    mtry = best_rfsp_params$mtry,
                                    min.node.size = best_rfsp_params$min.node.size,
                                    progress = TRUE,
                                    importance = "impurity",
                                    gower_vars = NA,
                                    clust_style = "quantile",
                                    num_class = 10,
                                    feature_select = FALSE,
                                    data_crs = 4326)



ames_test$rfsp.pred <- rfspav::predict.rfspav(rfsp_final_model, new_data = ames_test)

yardstick::rsq(ames_test, truth = Sale_Price, estimate = rfsp.pred)

#============ RFSI ============================================================


rfsi_tune_results <- tune.rfspav(formula = ames_fun,
                                  data_resample = ames_folds,
                                  coord_names = c("Longitude", "Latitude"),
                                  param_grid = rf_grid,
                                  metric = "rmse",
                                  type = "rfsi",
                                  cpus = 6,
                                  importance = "impurity",
                                  gower_vars = NA,
                                  clust_style = "quantile",
                                  num_class = 15,
                                  feature_select = FALSE,
                                  data_crs = 4326,
                                  predict_resamples = FALSE, seed = 124)


best_rfsi_params <- select_best_params(rfsi_tune_results)



rfsi_final_model <- rfspav::rfspav(formula = ames_fun,
                                    data = ames_train,
                                    coord_names = c("Longitude", "Latitude"),
                                    neighbors = best_rfsi_params$neighbors,
                                    type = "rfsi",
                                    trees = best_rfsi_params$trees,
                                    mtry = best_rfsi_params$mtry,
                                    min.node.size = best_rfsi_params$min.node.size,
                                    progress = TRUE,
                                    importance = "impurity",
                                    gower_vars = NA,
                                    clust_style = "quantile",
                                    num_class = 10,
                                    feature_select = FALSE,
                                    data_crs = 4326)



ames_test$rfsi.pred <- rfspav::predict.rfspav(rfsi_final_model, new_data = ames_test)

yardstick::rsq(ames_test, truth = Sale_Price, estimate = rfsi.pred)



#============ RFSIG ============================================================


rfsig_tune_results <- tune.rfspav(formula = ames_fun,
                           data_resample = ames_folds,
                           coord_names = c("Longitude", "Latitude"),
                           param_grid = rf_grid,
                           metric = "rmse",
                           type = "rfsig",
                           cpus = 6,
                           importance = "impurity",
                           gower_vars = NA,
                           clust_style = "quantile",
                           num_class = 15,
                           feature_select = FALSE,
                           data_crs = 4326,
                           predict_resamples = FALSE, seed = 124)


best_rfsig_params <- select_best_params(rfsig_tune_results)



rfsig_final_model <- rfspav::rfspav(formula = ames_fun,
                                    data = ames_train,
                                    coord_names = c("Longitude", "Latitude"),
                                    neighbors = best_rfsig_params$neighbors,
                                    type = "rfsig",
                                    trees = best_rfsig_params$trees,
                                    mtry = best_rfsig_params$mtry,
                                    min.node.size = best_rfsig_params$min.node.size,
                                    progress = TRUE,
                                    importance = "impurity",
                                    gower_vars = NA,
                                    clust_style = "quantile",
                                    num_class = 10,
                                    feature_select = FALSE,
                                    data_crs = 4326)



ames_test$rfsig.pred <- rfspav::predict.rfspav(rfsig_final_model, new_data = ames_test)

yardstick::rsq(ames_test, truth = Sale_Price, estimate = rfsig.pred)

#===============================================================================



object = rfsig_model
new_data = ames_test

formula = ames_fun
data = ames_train
coord_names = c("Longitude", "Latitude")
neighbors = 5
type = "rfsp"
trees = best_rfsp_params$trees
mtry = best_rfsp_params$mtry
min.node.size = best_rfsp_params$min.node.size
progress = TRUE
importance = "impurity"
gower_vars = NA
clust_style = "quantile"
num_class = 10
feature_select = FALSE
data_crs = 4326



best_params
