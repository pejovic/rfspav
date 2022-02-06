#============== Tidymodels implementaiton of rfspav ============================
set_new_model("rfsi")
set_model_mode("rfsi", "regression")
set_model_engine("rfsi", "regression", "rfspav")
set_dependency("rfsi", eng = "rfspav", pkg = "rfspav")

# Set model arguments

set_model_arg(
  model = "rfsi",
  eng = "rfspav",
  parsnip = "neighbors",
  original = "neighbors",
  func = list(pkg = "dials", fun = "neighbors", range = c(5, 10)),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfsi",
  eng = "rfspav",
  parsnip = "trees",
  original = "trees",
  func = list(pkg = "dials", fun = "trees", range = c(500, 2000)),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfsi",
  eng = "rfspav",
  parsnip = "mtry",
  original = "mtry",
  func = list(pkg = "dials", fun = "mtry", range = c(1, unknown())),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfsi",
  eng = "rfspav",
  parsnip = "min.node.size",
  original = "min.node.size",
  func = list(pkg = "dials", fun = "min_n", range = c(2L, 40L)),
  has_submodel = FALSE
)



rfsi <- function(mode = "regression",
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
    "rfsi",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

set_fit(
  model = "rfsi",
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

show_model_info("rfsi")


set_encoding(
  model = "rfsi",
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
  model = "rfsi",
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


