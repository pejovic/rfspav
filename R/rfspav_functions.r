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
#' @param n_class_levels Number of class intervals, Default: 10
#' @param feature_select Should Feature Selection algorithm be applied (only for "rfsig" approach), Default: FALSE
#' @param data_crs Data CRS, Default: NA
#' @param ... Additional parameters for ranger Random Forest implementation.
#' @return List of the following elements: Model, Data, Formula, etc.
#' @details DETAILS
#' @seealso
#' @rdname rfspav
#' @export
#' @importFrom stats as.formula
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom classInt classIntervals
#' @importFrom doParallel registerDoParallel detectCores
#' @importFrom foreach foreach
#' @importFrom purrr map
#' @importFrom dplyr group_split relocate
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
                 cpus = detectCores()-1,
                 importance = "impurity",
                 gower_vars = NA,
                 clust_style = c( "sd", "equal", "pretty", "quantile", "kmeans", "hclust"),
                 n_class_levels = 10,
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
    data.sf <- data %>% sf::st_as_sf(., coords = coord_names, crs = data_crs, remove = FALSE)
    zcol_classes <- classInt::classIntervals(data[, y_name][[1]], n = n_class_levels, style = clust_style)
    data.sf$zcol_cut_classes <- cut(data[, y_name][[1]], breaks = zcol_classes$brks, ordered_result = TRUE, include.lowest =TRUE)
    doParallel::registerDoParallel(cores = cpus)
    dist.data <- foreach::foreach(i = 1:dim(data.sf)[1], .combine = rbind) %dopar% purrr::map(.x = dplyr::group_split(data.sf[-i, ], zcol_cut_classes, .keep = FALSE), .f = ~unlist(nngeo::st_nn( data.sf[i, ], .x, returnDist = TRUE, progress = TRUE)$dist[[1]]))
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("dist_class", 1:n_class_levels, sep = "_"))
    data.df <- cbind(sf::st_drop_geometry(data.sf), dist.data)
    formula <- as.formula(paste(y_name, paste(c(x_names, names(dist.data)), collapse = "+"), sep = "~"))
  } else if (type == "rfsi") {
    if (progress) print('Fitting RFSI model ...')
    if(any(is.na(coord_names))){stop("coord_names must be specified for RFSP model")}
    data.sf <- data.df %>% sf::st_as_sf(., coords = coord_names, crs = data_crs, remove = FALSE)
    doParallel::registerDoParallel(cores = cpus)
    obs.dist.data <- foreach::foreach(i = 1:dim(data.sf)[1]) %dopar% nngeo::st_nn(data.sf[i, ], data.sf[-i, ], k = neighbors, returnDist = TRUE, progress = FALSE)
    doParallel::registerDoParallel(cores = cpus)
    obs.data <- foreach::foreach(i = 1:dim(data.sf)[1], .combine = rbind) %dopar% data.df[-i, ][[y_name]][obs.dist.data[[i]]$nn[[1]]]
    obs.data <- obs.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("near_obs", 1:neighbors, sep = "_"))
    dist.data <- purrr::map(.x = obs.dist.data, .f = ~.x$dist[[1]]) %>% do.call("rbind", .)
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("distance", 1:neighbors, sep = "_"))
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
    obs.dist.data <- as.list(rep(NA, dim(data.df)[1]))
    for(i in 1:dim(data.df)[1]){
      obs.dist.data[[i]] <- gower::gower_topn(data.df[i, gower_vars], data.df[-i, gower_vars], n = neighbors)
    }
    doParallel::registerDoParallel(cores = cpus)
    obs.data <- foreach::foreach(i = 1:dim(data.df)[1], .combine = rbind) %dopar% data.df[-i, ][[y_name]][obs.dist.data[[i]]$index[, 1]]
    obs.data <- obs.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("near_obs", 1:neighbors, sep = "_"))
    doParallel::registerDoParallel(cores = cpus)
    dist.data <- foreach::foreach(i = 1:dim(data.df)[1], .combine = rbind) %dopar% obs.dist.data[[i]]$distance[, 1]
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("distance", 1:neighbors, sep = "_"))
    data.df <- cbind(data.df, obs.data, dist.data)
    formula <- as.formula(paste(y_name, paste(c(x_names, names(obs.data), names(dist.data)), collapse = "+"), sep = "~"))
  }
  model <- ranger::ranger(formula, data = data.df, num.trees = trees, mtry = mtry, min.node.size = min.node.size, ...)
  data.df <- data.df %>% dplyr::relocate(y_name, x_names, everything())

  return(list(model = model, data = data.df, formula = formula, type = type, neighbors = neighbors, gower_vars = gower_vars, data_crs = data_crs, coord_names = coord_names))
}


#' @title predict.rfspav
#' @description Make prediction of rfspav object
#' @param object Model
#' @param new_data New data
#' @param cpus Number of cores, Default: detectCores() - 1
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#' @rdname predict.rfspav
#' @export
#' @importFrom stats as.formula predict
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom doParallel registerDoParallel detectCores
#' @importFrom foreach foreach
#' @importFrom purrr map
#' @importFrom dplyr group_split
#' @importFrom nngeo st_nn
#' @importFrom magrittr set_colnames %>%
#' @importFrom gower gower_topn
predict.rfspav <- function(object, new_data, cpus = detectCores()-1, ...){
  model <- object$model
  training_data <- object$data
  formula <- object$formula
  type = object$type
  neighbors <- object$neighbors
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
  } else if (type == "rfsp"){
    if (progress) print('Predicting RFSP model ...')
    training_data.sf <- training_data %>% sf::st_as_sf(., coords = coord_names, crs = crs, remove = FALSE)
    new_data.sf <- new_data %>% sf::st_as_sf(., coords = coord_names, crs = crs, remove = FALSE)
    doParallel::registerDoParallel(cores = cpus)
    dist.data <- foreach::foreach(i = 1:dim(new_data.sf)[1], .combine = rbind) %dopar% purrr::map(.x = dplyr::group_split(training_data.sf, zcol_cut_classes, .keep = FALSE), .f = ~unlist(nngeo::st_nn(new_data.sf[i, ], .x, returnDist = TRUE, progress = TRUE)$dist[[1]]))
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("dist_class", 1:n_class_levels, sep = "_"))
    new_data.df <- cbind(sf::st_drop_geometry(new_data.sf), dist.data)

  } else if (type == "rfsi") {
    if (progress) print('Predicting RFSI model ...')
    training_data.sf <- training_data %>% sf::st_as_sf(., coords = coord_names, crs = crs, remove = FALSE)
    new_data.sf <- new_data %>% sf::st_as_sf(., coords = coord_names, crs = crs, remove = FALSE)
    doParallel::registerDoParallel(cores = cpus)
    obs.dist.data <- foreach::foreach(i = 1:dim(new_data.sf)[1]) %dopar% nngeo::st_nn(new_data.sf[i, ], training_data.sf, k = neighbors, returnDist = TRUE, progress = FALSE)
    doParallel::registerDoParallel(cores = cpus)
    obs.data <- foreach::foreach(i = 1:dim(new_data.sf)[1], .combine = rbind) %dopar% training_data.sf[[y_name]][obs.dist.data[[i]]$nn[[1]]]
    obs.data <- obs.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("near_obs", 1:neighbors, sep = "_"))
    dist.data <- purrr::map(.x = obs.dist.data, .f = ~.x$dist[[1]]) %>% do.call("rbind", .)
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("distance", 1:neighbors, sep = "_"))
    new_data.df <- cbind(sf::st_drop_geometry(new_data.sf), obs.data, dist.data)

  } else if (type == "rfsig") {
    if (progress) print('Predicting RFSIG model ...')
    obs.dist.data <- as.list(rep(NA, dim(new_data.df)[1]))
    for(i in 1:dim(new_data.df)[1]){
      obs.dist.data[[i]] <- gower::gower_topn(new_data[i, gower_vars], training_data[, gower_vars], n = neighbors)
    }
    doParallel::registerDoParallel(cores = cpus)
    obs.data <- foreach::foreach(i = 1:dim(new_data)[1], .combine = rbind) %dopar% training_data[[y_name]][obs.dist.data[[i]]$index[, 1]]
    obs.data <- obs.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("near_obs", 1:neighbors, sep = "_"))
    doParallel::registerDoParallel(cores = cpus)
    dist.data <- foreach::foreach(i = 1:dim(new_data)[1], .combine = rbind) %dopar% obs.dist.data[[i]]$distance[, 1]
    dist.data <- dist.data %>% as.data.frame(.) %>% magrittr::set_colnames(paste("distance", 1:neighbors, sep = "_"))
    new_data.df <- cbind(new_data, obs.data, dist.data)
  }
  predict(model, data = new_data.df, ...)$prediction
}
