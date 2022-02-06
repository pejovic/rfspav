#============== Tidymodels implementaiton of rfspav ============================
set_new_model("rfsp")
set_model_mode("rfsp", "regression")
set_model_engine("rfsp", "regression", "rfspav")
set_dependency("rfsp", eng = "rfspav", pkg = "rfspav")



# Set model arguments

num_class <- function(range = c(5L, 10L), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_class = "num_class"),
    finalize = NULL
  )
}

set_model_arg(
  model = "rfsp",
  eng = "rfspav",
  parsnip = "num_class",
  original = "num_class",
  func = list(fun = "num_class"),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfsp",
  eng = "rfspav",
  parsnip = "trees",
  original = "trees",
  func = list(pkg = "dials", fun = "trees", range = c(500, 2000)),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfsp",
  eng = "rfspav",
  parsnip = "mtry",
  original = "mtry",
  func = list(pkg = "dials", fun = "mtry", range = c(1, unknown())),
  has_submodel = FALSE
)

set_model_arg(
  model = "rfsp",
  eng = "rfspav",
  parsnip = "min.node.size",
  original = "min.node.size",
  func = list(pkg = "dials", fun = "min_n", range = c(2L, 40L)),
  has_submodel = FALSE
)



rfsp <- function(mode = "regression",
                   num_class = 5,
                   trees = 500,
                   mtry = NULL,
                   min.node.size = NULL
) {
  args <- list(
    num_class   = rlang::enquo(num_class),
    trees = rlang::enquo(trees),
    mtry  = rlang::enquo(mtry),
    min.node.size  = rlang::enquo(min.node.size)
  )

  new_model_spec(
    "rfsp",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}

set_fit(
  model = "rfsp",
  eng = "rfspav",
  mode = "regression",
  value = list(
    interface = "formula",
    #data = c(formula = "formula", data = "data", coord_names = "coord_names", type = "type"),
    protect = c("formula", "data"),
    func = c(pkg = "rfspav", fun = "rfspav"),
    #defaults = list()
    defaults = list(importance = "impurity", type = "rfsp", coord_names = c("Longitude", "Latitude"), data_crs = 4326,  cpus = 6, progress = TRUE)
  )
)

show_model_info("rfsp")


set_encoding(
  model = "rfsp",
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
  model = "rfsp",
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
