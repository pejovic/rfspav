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

ames.fun <- as.formula(paste("Sale_Price", paste(names(ames_train)[c(80, 81, 1:78)], collapse = "+"), sep = "~"))

#================ Set recipe ===================================================
rf_rec <-
  recipe(ames.fun,
         data = ames_train)

#========== Set Random Forest model ============================================

rf_model <-
  rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")


#========= Specifying tuning parameters grid ==================================

rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 6)),
  trees(range = c(200,600)),
  levels = 2
)

#======== Define metrics =======================================================

ames_metrics <- metric_set(rmse, rsq)

#======== Tuning control =======================================================

#========= Set workflow ========================================================

rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_model)

#===============================================================================

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "resamples",
    save_workflow = TRUE
  )


doParallel::registerDoParallel()

ames_tune_res <- tune_grid(
  rf_wf,
  resamples = ames_folds,
  grid = rf_grid,
  metrics = ames_metrics,
  control = grid_ctrl
)

debug(tune_grid)

#================ Selection of best hyper-parameters ===========================
rf_best <- select_best(ames_tune_res, "rmse")

#================ Finaliza model with best hyper-parameters ====================
final_rf_model <- finalize_model(
  rf_model,
  rf_best
)
#================ Finalize workflow with best parameters =======================
final_rf_wf <- rf_wf %>% finalize_workflow(rf_best)
#============== Performance on test set ========================================

predict(final_rf_model, data = ames_test)


for(i in length(ames_folds$splits)){

}

ames_res <- ames_folds %>% dplyr::mutate(.metrics = NA, .notes = NA, .predictions = NA)













