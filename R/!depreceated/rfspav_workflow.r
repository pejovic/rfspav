library(tidyverse)
library(tidymodels)
library(sf)
library(ranger)
library(doParallel)
library(foreach)

source(here::here("R", "!depreceated", "rfsi_parnsip_model.r"))
source(here::here("R", "!depreceated", "rf_parnsip_model.r"))


library(AmesHousing)
ames <- make_ames()
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split) %>% as.data.frame()
ames_test <- testing(data_split) %>% as.data.frame()

#ames_train_sp <- ames_train
#coordinates(ames_train_sp) <- ~Longitude + Latitude

ames.fun <- as.formula(paste("Sale_Price", paste(names(ames)[c(80, 81, 1:78)], collapse = "+"), sep = "~"))
ames_train <- dplyr::sample_n(ames_train, 1000)

#============== Test RFSI parnsip implementation ==================

rfspav_spec <- rf(trees = 500) %>%
  set_engine("rfspav")

translate(rfspav_spec)


rfspav_fit <- rfspav_spec %>%
  fit(formula = ames.fun, data = ames_train, data_crs = 4326, engine = "rfspav")
rfspav_fit


ames_test_pred <- predict(rfspav_fit, new_data = ames_test) %>%
  bind_cols(ames_test)


ames_test_pred$.pred

# Tidymodels



#============ s1 data spliting =================================================
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

#========== Set recipe =========================================================

rfsi_rec <-
  recipe(ames.fun,
         data = ames_train) %>%
  step_naomit(everything(), skip = TRUE) %>%
  step_normalize(all_numeric(), -all_outcomes())


#========== Set Random Forest model ============================================

rfsi_model <-
  rfsi(neighbors = tune(), mtry = tune(), trees = tune(), min.node.size = tune()) %>%
  set_engine("rfspav") %>%
  set_mode("regression")


#========= Specifying tuning parameters grid ==================================

rfsi_grid <- grid_regular(
  neighbors =  neighbors(range = c(5, 10)),
  trees(range = c(200,400)),
  mtry(range = c(10, 20)),
  min.node.size = min_n(range = c(2, 4)),
  levels = 2
)

#======== Define metrics =======================================================

ames_metrics <- metric_set(rsq, rmse)

#======== Tuning control =======================================================

#========= Set workflow ========================================================

rfsi_wf <- workflow() %>%
  #add_formula(ames.fun) %>%
  add_recipe(rfsi_rec) %>%
  add_model(rfsi_model)

#rfsi_wf$fit$actions$model$formula <- ames.fun


#===============================================================================

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = NULL,
    save_workflow = FALSE
  )


doParallel::registerDoParallel()
#rfsi_wf$fit$actions$model$formula <- ames.fun
rfsi_tune_res <- tune_grid(
  rfsi_wf,
  resamples = ames_folds,
  grid = rfsi_grid,
  metrics = ames_metrics,
  control = grid_ctrl
)


all_cores <- parallel::detectCores(logical = FALSE)
library(doParallel)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

rfsi_tune_res <- debug(tune_grid(
  rfsi_wf,
  resamples = ames_folds,
  grid = rfsi_grid,
  metrics = ames_metrics,
  control = grid_ctrl
))

debug(tune_grid)


fit(rfsi_wf, ames_train)



rfsi_model <-
  rfsi(neighbors = 5, mtry = 5, trees = 500, min.node.size = 5) %>%
  set_engine("rfspav", importance = "impurity") %>%
  set_mode("regression")

rfsi_wf <- workflow() %>%
  add_recipe(rfsi_rec) %>%
  add_model(rfsi_model)

fit(rfsi_wf, data = ames_train)

rfsi_fit <- rfsi_model %>%
  fit(formula = ames.fun, data = ames_train, engine = "rfspav")
rfspav_fit



rfspav_spec <- rfsi(trees = 500, neighbors = 4) %>%
  set_engine("rfspav")

translate(rfspav_spec)


rfspav_fit <- rfspav_spec %>%
  fit(formula = ames.fun, data = ames_train, data_crs = 4326, engine = "rfspav")
rfspav_fit


ames_test_pred <- predict(rfspav_fit, new_data = ames_test) %>%
  bind_cols(ames_test)


ames_test_pred$.pred

#==============================================
rfsi_model <-
  rfsi(neighbors = tune(), mtry = tune(), trees = tune(), min.node.size = tune()) %>%
  set_engine("rfspav") %>%
  set_mode("regression")

rfsi_tune_res <- rfsi_model %>%
  tune_grid(ames.fun, ames_folds, grid = rfsi_grid)
show_best(rfsi_tune_res, metric = "rsq")
