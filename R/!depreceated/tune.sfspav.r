library(tidyverse)
library(tidymodels)
library(sf)
library(ranger)
library(doParallel)
library(foreach)



# ================= Fitting rfspav models ======================================
formula = meuse.fm
data_resample = meuse_folds
coord_names = c("x", "y")
param_grid = param_grid
metric = "rmse"
type = "rfsi"
cpus = 6
importance = "impurity"
gower_vars = NA
clust_style = "quantile"
num_class = 10
feature_select = FALSE
data_crs = NA
predict_resamples = FALSE

tune.rfspav <- function(formula,
                        data_resample,
                        coord_names,
                        param_grid,
                        metric = c("rmse", "rsq"),
                        type = c("rf", "rfsp", "rfsi", "rfsig"),
                        progress = TRUE,
                        cpus = 1,
                        importance = "impurity",
                        gower_vars = NA,
                        clust_style = c("sd", "equal", "pretty", "quantile", "kmeans", "hclust"),
                        num_class = 10,
                        feature_select = FALSE,
                        data_crs = NA,
                        predict_resamples = FALSE, ...){

  type <- match.arg(type)
  clust_style = match.arg(clust_style)
  formula <- as.formula(formula)
  all_vars <- all.vars(formula)
  y_name <- all_vars[1]
  x_names <- all_vars[-1]


  grid_info <- param_grid %>% dplyr::mutate(.model = paste("Model", 1:dim(param_grid)[1], sep = "_"))
  grid_results <- data_resample
  grid_results$.results <- NA

  cl <- makeCluster(cpus, outfile='LOG.TXT')
  registerDoParallel(cl)
  for(i in 1:length(grid_results$splits)){
    grid_results$.results[i] <- list(foreach::foreach(d = iter(param_grid, by='row'), .packages = c("rfspav", "foreach", "ranger")) %dopar% rfspav::predict.rfspav(rfspav::rfspav(formula = formula, data = grid_results$splits[[i]]$data[grid_results$splits[[i]]$in_id, ], coord_names = coord_names, type = type, clust_style = clust_style, neighbors = d$neighbors, mtry = d$mtry, trees = d$trees, min.node.size = d$min.node.size, ...), new_data = grid_results$splits[[i]]$data[-grid_results$splits[[i]]$in_id, ], ...))
  }
  stopImplicitCluster()

  for(i in 1:length(grid_results$splits)){
    grid_results$.results[[i]] <- lapply(grid_results$.results[i], function(x) data.frame(t(do.call(rbind, x))) %>%
                                           magrittr::set_colnames(paste("Model", 1:dim(.)[2], sep = "_")) %>%
                                           dplyr::mutate(.ind = which(!(1:dim(grid_results$splits[[i]]$data)[1] %in% grid_results$splits[[i]]$in_id)), .obs = grid_results$splits[[i]]$data[-grid_results$splits[[i]]$in_id, all.vars(formula)[1]]) %>%
                                           tidyr::pivot_longer(., cols = starts_with("Model"), names_to = ".model", values_to = ".pred"))
  }

  for(i in 1:length(grid_results$splits)){
    grid_results$.results[[i]] <- suppressMessages(dplyr::full_join(grid_info, grid_results$.results[[i]][[1]]))
  }

  grid_results$.results <- purrr::map(.x = grid_results$.results, .f = ~dplyr::group_by(.x, trees, neighbors, mtry, min.node.size, .model) %>%
                                        nest() %>%
                                        dplyr::mutate(rmse = unlist(map(.x = data, .f = ~yardstick::rmse(.x, truth = .x$.obs, estimate = .x$.pred)$.estimate)),
                                                      rsq = unlist(map(.x = data, .f = ~yardstick::rsq(.x, truth = .x$.obs, estimate = .x$.pred)$.estimate))))

  select_best_params <- function(x, metric = c("rmse", "rsq")){
    metric = match.arg(metric)
    summary_results <- do.call(rbind, x$.results) %>% group_by(trees, neighbors, mtry, min.node.size, .model) %>% dplyr::summarise(rmse = mean(rmse), rsq = mean(rsq))
    best_results <- if(metric == "rmse") {summary_results %>% dplyr::arrange(rmse) %>% .[1, ]}else{summary_results %>% dplyr::arrange(desc(rsq)) %>% .[1, ]}
    return(best_results)
  }


  best_params <- select_best_params(grid_results, metric = metric)
  if(predict_resamples){return(grid_results)} else {return(best_params)}
}



# ================= Preparing Meuse data =================================
library(sp)
demo(meuse, echo=FALSE)
meuse <- meuse[complete.cases(meuse@data),]

meuse.df <- as.data.frame(meuse)

set.seed(123)
meuse_split <- initial_split(meuse.df, prop = 0.70, strata = zinc)
meuse_train <- training(meuse_split)
meuse_test  <-  testing(meuse_split)

meuse_folds <- vfold_cv(meuse_train, v = 5)

meuse.fm <- as.formula("zinc ~ x + y + dist.m + soil + ffreq + landuse")


param_grid <- grid_regular(
  neighbors =  neighbors(range = c(7, 14)),
  trees(range = c(200,800)),
  mtry(range = c(2, 4)),
  min.node.size = min_n(range = c(2, 4)),
  levels = 2
)

#================================================================================


best_params <- tune.rfspav(formula = meuse.fm,
                           data_resample = meuse_folds,
                           coord_names = c("x", "y"),
                           param_grid = param_grid,
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



best_params$.results

final.model <- rfspav::rfspav(formula = meuse.fm,
                              data = meuse_train,
                              type = "rf",
                              clust_style = "quantile",
                              coord_names = c("x", "y"),
                              neighbors = best_params$neighbors,
                              mtry = best_params$mtry,
                              trees = best_params$trees,
                              min.node.size = best_params$min.node.size)


meuse_test$pred <- predict.rfspav(final.model, new_data = meuse_test)

yardstick::rsq(meuse_test, truth = zinc, estimate = pred)





