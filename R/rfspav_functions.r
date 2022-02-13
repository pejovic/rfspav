#' @title rfspav
#' @description Random Forest SPatial Variants
#' @param formula Formula
#' @param data Data
#' @param coord_names Names of columns containing spatial coordinates
#' @param neighbors Neighbours, Default: 5
#' @param type Type of Random Forest Spatial Variants, Default: c("rf", "rfsp", "rfsi", "rfsig")
#' @param mtry Number of variables to possibly split at in each node. Default is the (rounded down) square root of the number variables. Alternatively, a single argument function returning an integer, given the number of independent variables.
#' @param num.trees Number of trees.
#' @param min.node.size Minimal node size. Default 1 for classification, 5 for regression, 3 for survival, and 10 for probability.
#' @param progress Progress, Default: TRUE
#' @param cpus Number of cores, Default: detectCores() - 1
#' @param importance Type of variable importance, Default: 'impurity'
#' @param gower_vars Variables for gower distance calculation, Default: NA
#' @param clust_style Type of methods for choosing univariate class intervals of target variables (only for "rfsp" approach), Default: c("sd", "equal", "pretty", "quantile", "kmeans", "hclust")
#' @param num_class Number of class intervals, Default: 10
#' @param feature_select Should Feature Selection algorithm be applied (only for "rfsig" approach), Default: FALSE
#' @param data_crs Data CRS, Default: NA
#' @param ... Additional parameters for ranger Random Forest implementation.
#' @return List of the following elements: Model, Data, Formula, etc.
#' @details DETAILS
#' @seealso
#' @rdname rfspav
#' @export
#' @importFrom stats as.formula
#' @importFrom sf st_as_sf st_drop_geometry st_distance
#' @importFrom classInt classIntervals
#' @importFrom dplyr group_by summarise across
#' @importFrom tidyselect starts_with
#' @importFrom nngeo st_nn
#' @importFrom magrittr set_colnames %>%
#' @importFrom FSelector cfs
#' @importFrom gower gower_topn
#' @importFrom ranger ranger
rfspav <- function(formula,
                 data,
                 coord_names,
                 neighbors = 5,
                 type = c("rf", "rfsp", "rfsi", "rfsig"),
                 trees = 500,
                 mtry = NULL,
                 min.node.size = NULL,
                 progress = TRUE,
                 importance = "impurity",
                 gower_vars = NA,
                 clust_style = c("sd", "equal", "pretty", "quantile", "kmeans", "hclust"),
                 num_class = 10,
                 feature_select = FALSE,
                 data_crs = NA, ...){

  type <- match.arg(type)
  clust_style = match.arg(clust_style)
  formula <- as.formula(formula)
  all_vars <- all.vars(formula)
  y_name <- all_vars[1]
  x_names <- all_vars[-1]

  if (!all(all.vars(formula) %in% unique(c(names(data))))) {
    stop("data` must contain all variables specified in `formula`")
  }

  data.df <- data

  if(type == "rf"){
    if (progress) print('Fitting RF model ...')
  } else if (type == "rfsp"){
    if (progress) print('Fitting RFSP model ...')
    if(any(is.na(coord_names))){stop("coord_names must be specified for RFSP model")}

    data.sf <- data.df %>% sf::st_as_sf(., coords = coord_names, crs = data_crs, remove = FALSE)
    zcol_classes <- classInt::classIntervals(data.sf[, y_name][[1]], n = num_class, style = clust_style)
    data.sf$zcol_cut_classes <- cut(data.sf[, y_name][[1]], breaks = zcol_classes$brks, ordered_result = TRUE, include.lowest =TRUE)

    dist.matrix <- matrix(sf::st_distance(data.sf, data.sf), nrow = dim(data.sf)[1])
    diag(dist.matrix) <- NA
    dist.data.df <- data.frame(dist.matrix, classes = data.sf$zcol_cut_classes)
    dist.data.df <- dist.data.df %>% dplyr::group_by(classes) %>%
      dplyr::summarise(dplyr::across(tidyselect::starts_with("X"), ~ min(., na.rm = TRUE)))
    dist.data.df <- t(dist.data.df[, -1]) %>% as.data.frame() %>% magrittr::set_colnames(paste("dist_class", 1:num_class, sep = "_"))

    data.df <- cbind(sf::st_drop_geometry(data.sf), dist.data.df)
    formula <- as.formula(paste(y_name, paste(c(x_names, names(dist.data.df)), collapse = "+"), sep = "~"))

  } else if (type == "rfsi") {
    if (progress) print('Fitting RFSI model ...')
    if(any(is.na(coord_names))){stop("coord_names must be specified for RFSP model")}
    data.sf <- data.df %>% sf::st_as_sf(., coords = coord_names, crs = data_crs, remove = FALSE)

    obs.dist.data <- nngeo::st_nn(data.sf, data.sf, k = neighbors+1, returnDist = TRUE, progress = FALSE)
    obs.data <- do.call(rbind, obs.dist.data[[1]])[, -1]
    for(i in 1:dim(obs.data)[1]){
      obs.data[i, ] <- data.sf[[y_name]][obs.data[i, ]]
    }
    obs.data <- obs.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("near_obs", 1:neighbors, sep = "_"))

    dist.data <- do.call(rbind, obs.dist.data[[2]])[, -1]
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("dist", 1:neighbors, sep = "_"))

    data.df <- cbind(sf::st_drop_geometry(data.sf), obs.data, dist.data)
    formula <- as.formula(paste(y_name, paste(c(x_names, names(obs.data), names(dist.data)), collapse = "+"), sep = "~"))

  } else if (type == "rfsig") {
    if (progress) print('Fitting RFSIG model ...')
    if(any(is.na(coord_names))){stop("coord_names must be specified for RFSIG model")}
    if(all(is.na(gower_vars))){print("All independent variables are used for gower distance calculation")}
    if(!all(is.na(gower_vars))){feature_select <- FALSE}
    if(all(is.na(gower_vars))){gower_vars <- x_names}
    if(all(x_names %in% coord_names)){feature_select <- FALSE}
    if(feature_select){
      f.fun <- as.formula(paste(paste(y_name,"~"), paste(x_names, collapse="+")))
      feature.selection <- FSelector::cfs(formula = f.fun, data = data)
      x_names <- unique(c(feature.selection, coord_names))
      formula <- as.formula(paste(paste(y_name,"~"), paste(x_names, collapse="+")))
    }

    obs.dist.data <- gower::gower_topn(data.df[, gower_vars], data.df[, gower_vars], n = neighbors+1)
    obs.data <- t(obs.dist.data$index)[, -1]
    for(i in 1:dim(obs.data)[1]){
      obs.data[i, ] <- data.df[[y_name]][obs.data[i, ]]
    }
    obs.data <- obs.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("near_obs", 1:neighbors, sep = "_"))

    dist.data <- t(obs.dist.data$distance)[, -1]
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("gower_dist", 1:neighbors, sep = "_"))

    data.df <- cbind(data.df, obs.data, dist.data)
    formula <- as.formula(paste(y_name, paste(c(x_names, names(obs.data), names(dist.data)), collapse = "+"), sep = "~"))
  }
  model <- ranger::ranger(formula, data = data.df, num.trees = trees, mtry = mtry, min.node.size = min.node.size, ...)
  data.df <- data.df %>% dplyr::relocate(y_name, x_names, everything())

  return(list(model = model, data = data.df, formula = formula, type = type, neighbors = neighbors, num_class = num_class, gower_vars = gower_vars, data_crs = data_crs, coord_names = coord_names))
}


#' @title predict.rfspav
#' @description Make prediction of rfspav object
#' @param object Model
#' @param new_data New data
#' @param progress Progress, Default: TRUE
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#' @rdname predict.rfspav
#' @export
#' @importFrom stats as.formula predict
#' @importFrom sf st_as_sf st_drop_geometry st_distance
#' @importFrom dplyr group_by summarise across
#' @importFrom tidyselect starts_with
#' @importFrom nngeo st_nn
#' @importFrom magrittr set_colnames %>%
#' @importFrom gower gower_topn
predict.rfspav <- function(object, new_data, progress = TRUE, ...){
  model <- object$model
  training_data <- object$data
  formula <- object$formula
  type = object$type
  neighbors <- object$neighbors
  num_class <- object$num_class
  gower.vars <- object$gower_vars
  coord_names <- object$coord_names
  crs <- object$data_crs
  y_name <- all.vars(formula)[1]
  x_names <- all.vars(formula)[-1]
  gower_vars <- object$gower_vars

  trees <- object$model$num.trees
  mtry <- object$model$mtry
  min.node <- object$model$min.node.size

  if(type == "rf"){
    if (progress) print('Predicting RF model ...')
    new_data.df <- new_data
  } else if (type == "rfsp"){
    if (progress) print('Predicting RFSP model ...')
    training_data.sf <- training_data %>% sf::st_as_sf(., coords = coord_names, crs = crs, remove = FALSE)
    new_data.sf <- new_data %>% sf::st_as_sf(., coords = coord_names, crs = crs, remove = FALSE)

    dist.matrix <- t(matrix(sf::st_distance(new_data.sf, training_data.sf), nrow = dim(new_data.sf)[1]))

    dist.data.df <- data.frame(dist.matrix, classes = training_data.sf$zcol_cut_classes)
    dist.data.df <- dist.data.df %>% dplyr::group_by(classes) %>%
      dplyr::summarise(dplyr::across(tidyselect::starts_with("X"), ~ min(., na.rm = TRUE)))
    dist.data.df <- t(dist.data.df[, -1]) %>% as.data.frame() %>%
      magrittr::set_colnames(paste("dist_class", 1:num_class, sep = "_"))

    new_data.df <- cbind(sf::st_drop_geometry(new_data.sf), dist.data.df)

  } else if (type == "rfsi") {
    if (progress) print('Predicting RFSI model ...')
    training_data.sf <- training_data %>% sf::st_as_sf(., coords = coord_names, crs = crs, remove = FALSE)
    new_data.sf <- new_data %>% sf::st_as_sf(., coords = coord_names, crs = crs, remove = FALSE)

    obs.dist.data <- nngeo::st_nn(new_data.sf, training_data.sf, k = neighbors, returnDist = TRUE, progress = FALSE)
    obs.data <- do.call(rbind, obs.dist.data[[1]])
    for(i in 1:dim(obs.data)[1]){
      obs.data[i, ] <- training_data.sf[[y_name]][obs.data[i, ]]
    }
    obs.data <- obs.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("near_obs", 1:neighbors, sep = "_"))

    dist.data <- do.call(rbind, obs.dist.data[[2]])
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("dist", 1:neighbors, sep = "_"))

    new_data.df <- cbind(sf::st_drop_geometry(new_data.sf), obs.data, dist.data)

  } else if (type == "rfsig") {
    if (progress) print('Predicting RFSIG model ...')

    obs.dist.data <- gower::gower_topn(new_data[, gower_vars], training_data[, gower_vars], n = neighbors)
    obs.data <- t(obs.dist.data$index)
    for(i in 1:dim(obs.data)[1]){
      obs.data[i, ] <- training_data[[y_name]][obs.data[i, ]]
    }
    obs.data <- obs.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("near_obs", 1:neighbors, sep = "_"))

    dist.data <- t(obs.dist.data$distance)
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("gower_dist", 1:neighbors, sep = "_"))

    new_data.df <- cbind(new_data, obs.data, dist.data)
  }
  predict(model, data = new_data.df, ...)$prediction
}


#' @title Tune rfspav model (select best parameters)
#' @description Do cross-validation procedured for tuning meta-parameters
#' @param formula formula
#' @param data_resample 'rsample' folds
#' @param coord_names PARAM_DESCRIPTION
#' @param param_grid grid of four meta-parameters: neighbors, mtry, trees, min.node,size.
#' @param metric Regression metrics, Default: c("rmse", "rsq")
#' @param type Type of Random Forest Spatial Variants, Default: c("rf", "rfsp", "rfsi", "rfsig")
#' @param progress PARAM_DESCRIPTION, Default: TRUE
#' @param cpus Number of cores, Default: 1
#' @param importance Variable importance, Default: 'impurity'
#' @param gower_vars Variable used of gower-distance calculation, Default: NA
#' @param clust_style Type of methods for choosing univariate class intervals of target variables (only for "rfsp" approach), Default: c("sd", "equal", "pretty", "quantile", "kmeans", "hclust")
#' @param num_class Number of class intervals, Default: 10
#' @param feature_select Should Feature Selection algorithm be applied (only for "rfsig" approach), Default: FALSE
#' @param data_crs Data CRS, Default: NA
#' @param ... Additional parameters for 'ranger' Random Forest
#' @return Tuning list
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname tune.rfspav
#' @export
#' @importFrom dplyr mutate full_join group_by
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom foreach foreach
#' @importFrom magrittr set_colnames
#' @importFrom tidyr pivot_longer nest
#' @importFrom purrr map
#' @importFrom yardstick rmse rsq
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
                        data_crs = NA, ...){

  type <- match.arg(type)
  clust_style = match.arg(clust_style)
  formula <- as.formula(formula)
  all_vars <- all.vars(formula)
  y_name <- all_vars[1]
  x_names <- all_vars[-1]


  grid_info <- param_grid %>% dplyr::mutate(.model = paste("Model", 1:dim(param_grid)[1], sep = "_"))
  grid_results <- data_resample
  grid_results$.results <- NA

  cl <- parallel::makeCluster(cpus, outfile='LOG.TXT')
  doParallel::registerDoParallel(cl)
  for(i in 1:length(grid_results$splits)){
    grid_results$.results[i] <- list(foreach::foreach(d = iter(param_grid, by='row'), .packages = c("foreach", "ranger")) %dopar% predict.rfspav(rfspav(formula = formula, data = grid_results$splits[[i]]$data[grid_results$splits[[i]]$in_id, ], coord_names = coord_names, type = type, clust_style = clust_style, neighbors = d$neighbors, mtry = d$mtry, trees = d$trees, min.node.size = d$min.node.size, ...), new_data = grid_results$splits[[i]]$data[-grid_results$splits[[i]]$in_id, ], ...))
  }
  doParallel::stopImplicitCluster()

  for(i in 1:length(grid_results$splits)){
    grid_results$.results[[i]] <- lapply(grid_results$.results[i], function(x) data.frame(t(do.call(rbind, x))) %>%
                                           magrittr::set_colnames(paste("Model", 1:dim(.)[2], sep = "_")) %>%
                                           dplyr::mutate(.ind = which(!(1:dim(grid_results$splits[[i]]$data)[1] %in% grid_results$splits[[i]]$in_id)), .obs = grid_results$splits[[i]]$data[-grid_results$splits[[i]]$in_id, ][[all.vars(formula)[1]]]) %>%
                                           tidyr::pivot_longer(., cols = starts_with("Model"), names_to = ".model", values_to = ".pred"))
  }

  for(i in 1:length(grid_results$splits)){
    grid_results$.results[[i]] <- suppressMessages(dplyr::full_join(grid_info, grid_results$.results[[i]][[1]]))
  }

  grid_results$.results <- purrr::map(.x = grid_results$.results, .f = ~dplyr::group_by(.x, trees, neighbors, mtry, min.node.size, .model) %>%
                                        tidyr::nest() %>%
                                        dplyr::mutate(rmse = unlist(map(.x = data, .f = ~yardstick::rmse(.x, truth = .x$.obs, estimate = .x$.pred)$.estimate)),
                                                      rsq = unlist(map(.x = data, .f = ~yardstick::rsq(.x, truth = .x$.obs, estimate = .x$.pred)$.estimate))))


  return(return(grid_results))

}



#' @title select_best_params
#' @description Select best parameters
#' @param x Results of tune.rfspav function
#' @param metric Regression metrics, Default: c("rmse", "rsq")
#' @return Best parameters
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname select_best_params
#' @export
#' @importFrom dplyr summarise arrange
select_best_params <- function(x, metric = c("rmse", "rsq")){
  metric = match.arg(metric)
  summary_results <- do.call(rbind, x$.results) %>% group_by(trees, neighbors, mtry, min.node.size, .model) %>% dplyr::summarise(rmse = mean(rmse), rsq = mean(rsq))
  best_results <- if(metric == "rmse") {summary_results %>% dplyr::arrange(rmse) %>% .[1, ]}else{summary_results %>% dplyr::arrange(desc(rsq)) %>% .[1, ]}
  return(best_results)
}

