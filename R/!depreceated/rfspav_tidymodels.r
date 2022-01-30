library(tidyverse)
library(tidymodels)
library(sf)
library(ranger)
library(doParallel)
library(foreach)


#============== Tidymodels implementaiton of rfspav ============================
set_new_model("rfspav")
set_model_mode("rfspav", "regression")
set_model_engine("rfspav", "regression", "rfspav")
set_dependency("rfspav", eng = "rfspav", pkg = "rfspav")


show_model_info("rfspav")



# Set model arguments

set_model_arg(
  model = "rfspav",
  eng = "rfspav",
  parsnip = "neighbors",
  original = "neighbors",
  func = list(pkg = "dials", fun = "neighbors", range = c(5, 10)),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfspav",
  eng = "rfspav",
  parsnip = "trees",
  original = "trees",
  func = list(pkg = "dials", fun = "trees", range = c(500, 2000)),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfspav",
  eng = "rfspav",
  parsnip = "mtry",
  original = "mtry",
  func = list(pkg = "dials", fun = "mtry", range = c(1, unknown())),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfspav",
  eng = "rfspav",
  parsnip = "min.node.size",
  original = "min.node.size",
  func = list(pkg = "dials", fun = "min_n", range = c(2L, 40L)),
  has_submodel = FALSE
)



rfspav <- function(mode = "regression",
                             neighbors = NULL,
                             trees = 500,
                             mtry = NULL,
                             min.node.size = NULL
) {
  args <- list(
    neighbors   = rlang::enquo(neighbors),
    trees = rlang::enquo(trees),
    mtry  = rlang::enquo(mtry),
    min.node.size  = rlang::enquo(min.node.size)
  )

  new_model_spec(
    "rfspav",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

set_fit(
  model = "rfspav",
  eng = "rfspav",
  mode = "regression",
  value = list(
    interface = "formula",
    #data = c(formula = "formula", data = "data", coord_names = "coord_names", type = "type"),
    protect = c("formula", "data"),
    func = c(pkg = "rfspav", fun = "rfspav"),
    #defaults = list()
    defaults = list(importance = "impurity", type = "rfsi", coord_names = c("Longitude", "Latitude"), data_crs = 4326,  cpus = 6, progress = TRUE)
  )
)

show_model_info("rfspav")


set_encoding(
  model = "rfspav",
  eng = "rfspav",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)


set_pred(
  model = "rfspav",
  eng = "rfspav",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict.rfspav"),
    args =
      list(
        object = quote(object$fit),
        new_data = quote(new_data)
        )
  )
)


#================= TEST ====================================
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

rfspav(trees = 600, mtry = 4, min.node.size = 10) %>%
  translate(engine = "rfspav")


rfspav_spec <- rfspav(trees = 500) %>%
  set_engine("rfspav")

translate(rfspav_spec)


rfspav_fit <- rfspav_spec %>%
  fit(formula = ames.fun, data = ames_train, data_crs = 4326, engine = "rfspav")
rfspav_fit

rfspav(formula = ames.fun, data = ames_train, coord_names = c("Longitude", "Latitude"), type = "rfsi", clust_style = "hclust",  data_crs = 4326, cpus = 6)

formula = ames.fun; data = ames_train; coord_names = c("Longitude", "Latitude"); type = "rfsp"; clust_style = "hclust"; n_class_levels = 10;  data_crs = 4326


ames_test_pred <- predict(rfspav_fit, new_data = ames_test, type = "numeric") %>%
  bind_cols(ames_test)


ames_test_pred$.pred


