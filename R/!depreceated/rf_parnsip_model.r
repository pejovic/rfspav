#============== Tidymodels implementaiton of rfspav ============================
set_new_model("rf")
set_model_mode("rf", "regression")
set_model_engine("rf", "regression", "rfspav")
set_dependency("rf", eng = "rfspav", pkg = "rfspav")

# Set model arguments

set_model_arg(
  model = "rf",
  eng = "rfspav",
  parsnip = "trees",
  original = "trees",
  func = list(pkg = "dials", fun = "trees", range = c(500, 2000)),
  has_submodel = FALSE
)

set_model_arg(
  model = "rf",
  eng = "rfspav",
  parsnip = "mtry",
  original = "mtry",
  func = list(pkg = "dials", fun = "mtry", range = c(1, unknown())),
  has_submodel = FALSE
)

set_model_arg(
  model = "rf",
  eng = "rfspav",
  parsnip = "min.node.size",
  original = "min.node.size",
  func = list(pkg = "dials", fun = "min_n", range = c(2L, 40L)),
  has_submodel = FALSE
)



rf <- function(mode = "regression",
                 trees = 500,
                 mtry = NULL,
                 min.node.size = NULL
) {
  args <- list(
    trees = rlang::enquo(trees),
    mtry  = rlang::enquo(mtry),
    min.node.size  = rlang::enquo(min.node.size)
  )

  new_model_spec(
    "rf",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

set_fit(
  model = "rf",
  eng = "rfspav",
  mode = "regression",
  value = list(
    interface = "formula",
    #data = c(formula = "formula", data = "data", coord_names = "coord_names", type = "type"),
    protect = c("formula", "data"),
    func = c(pkg = "rfspav", fun = "rfspav"),
    #defaults = list()
    defaults = list(importance = "impurity", type = "rf", coord_names = c("Longitude", "Latitude"), data_crs = 4326,  cpus = 6, progress = TRUE)
  )
)

show_model_info("rf")


set_encoding(
  model = "rf",
  eng = "rfspav",
  mode = "regression",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = TRUE,
    remove_intercept = TRUE,
    allow_sparse_x = FALSE
  )
)


set_pred(
  model = "rf",
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


