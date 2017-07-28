

fit_single_h2o_grid <- function(grid.algorithm, training_frame, y, x, family = "binomial", model_contrl, fold_column, validation_frame  = NULL, ...) {
  if (gvars$verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                    intercept = TRUE,
                    seed = 1,
                    # fold_column = fold_column,
                    keep_cross_validation_predictions = TRUE,
                    keep_cross_validation_fold_assignment = TRUE,
                    family = family,
                    standardize = TRUE,
                    # solver = "L_BFGS",
                    lambda = 0L,
                    max_iterations = 100,
                    ignore_const_cols = FALSE,
                    # missing_values_handling = "Skip"
                    missing_values_handling = c("MeanImputation")
                  )

  if (is.null(grid.algorithm)) stop("must specify the 'algorithm' name when running 'h2o.grid'")
  if (!is.character(grid.algorithm)) stop("'algorithm' must be a string naming the 'algorithm' for 'h2o.grid'")
  algo_fun_name <- "h2o."%+%grid.algorithm

  if (!'package:h2o' %in% search()) stop("Could not locate h2o among loaded packages. Please run: 'library('h2o')'")
  if (!exists(algo_fun_name, where='package:h2o', mode='function')) stop("Could not locate the function " %+% grid.algorithm)

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs[["fold_column"]] <- fold_column
      validation_frame <- NULL
      mainArgs[["validation_frame"]] <- NULL
    }
  }

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs[["validation_frame"]] <- validation_frame

  if ("distribution" %in% names(model_contrl) && ("bernoulli" %in% model_contrl[["distribution"]])) {
    mainArgs[["training_frame"]][[y]] <- h2o::as.factor(mainArgs[["training_frame"]][[y]])
    if (!is.null(mainArgs[["validation_frame"]])) mainArgs[["validation_frame"]][[y]] <- h2o::as.factor(mainArgs[["validation_frame"]][[y]])
  }

  ## doesn't work if h2o namespace is not loaded:
  # algo_fun <- get0(algo_fun_name, mode = "function", inherits = TRUE)
  algo_fun <- utils::getFromNamespace(algo_fun_name, ns='h2o')
  mainArgs <- keep_only_fun_args(mainArgs, fun = algo_fun)   # Keep only the relevant args in mainArgs list:
  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = algo_fun) # Add user args that pertain to this specific learner:
  mainArgs[["algorithm"]] <- grid.algorithm
  mainArgs[["search_criteria"]] <- model_contrl[["search_criteria"]]
  mainArgs[["hyper_params"]] <- model_contrl[["param_grid"]]

  # Remove any args from mainArgs that also appear in hyper_params:
  common_hyper_args <- intersect(names(mainArgs), names(mainArgs$hyper_params))
  if(length(common_hyper_args) > 0) mainArgs <- mainArgs[!(names(mainArgs) %in% common_hyper_args)]

  if (("lambda_search" %in% names(mainArgs)))
    if (mainArgs[["lambda_search"]]) mainArgs[["lambda"]] <- NULL

  if (gvars$verbose) print("running h2o.grid with algorithm: " %+% grid.algorithm)

  model_fit <- try(do.call(h2o::h2o.grid, mainArgs), silent = FALSE)
  if (inherits(model_fit, "try-error")) return(model_fit)
  ## sort the grid by increasing MSE:
  model_fit <- h2o::h2o.getGrid(model_fit@grid_id)
  # model_fit <- h2o::h2o.getGrid(model_fit@grid_id, sort_by = "mse", decreasing = FALSE)
  # str(model_fit@model_ids[[1]])
  # h2o.getModel(model_fit@model_ids[[1]])

  return(model_fit)
}

fit.h2ogrid <- function(fit.class, params, training_frame, y, x, model_contrl, fold_column, ...) {
  family <- model_contrl[["family"]]
  if (is.null(family)) family <- "binomial"

  grid.algorithm <- model_contrl[["grid.algorithm"]]
  if (is.null(grid.algorithm)) grid.algorithm <- model_contrl[["fit.algorithm"]]

  modelfits_grid <- fit_single_h2o_grid(grid.algorithm = grid.algorithm[[1]],
                                        training_frame = training_frame,
                                        y = y,
                                        x = x,
                                        family = family,
                                        model_contrl = model_contrl,
                                        fold_column = fold_column, ...)

  if (inherits(modelfits_grid, "try-error")) return(modelfits_grid)
  if (length(modelfits_grid@model_ids)<1) {
    stop("...all h2o.grid models have failed...")
  }

  ## Extract the top model (first on the list)
  topmodel_grid <- h2o::h2o.getModel(modelfits_grid@model_ids[[1]])

  ## Put all model fits from the grid into a single list for easier access:
  modelfits_all <- lapply(modelfits_grid@model_ids, function(model_id) h2o::h2o.getModel(model_id))
  model_algorithms <- lapply(modelfits_all, function(model) model@algorithm)
  model_ids <- lapply(modelfits_all, function(model) model@model_id)
  ngridmodels <- length(modelfits_all)

  if (gvars$verbose) {
    print("grid search: " %+% modelfits_grid@grid_id)
    print("grid models: "); print(modelfits_grid)
    print("grid search top performing model:"); print(topmodel_grid)
  }

  # ----------------------------------------------------------------------------------------------------
  # Saving the fits:
  # ----------------------------------------------------------------------------------------------------
  # if (!is.null(model_contrl$save.ensemble) && model_contrl$save.ensemble) {
  #   if (is.null(model_contrl$ensemble.dir.path) || (model_contrl$ensemble.dir.path %in% "")) {
  #     stop("when saving ensemble must specify the directory path with 'ensemble.dir.path' parameter")
  #   }
  #   saveres <- lapply(modelfits_all, function(h2omodel) h2o::h2o.saveModel(object = h2omodel, path = model_contrl$ensemble.dir.path, force = TRUE))
  #   # h2oEnsemble::h2o.save_ensemble(stacked.fit, path = model_contrl$ensemble.dir.path, force = TRUE)
  #   # h2oEnsemble::h2o.save_ensemble(stacked.fit, path = model_contrl$ensemble.dir.path, force = TRUE, export_levelone = TRUE)
  # }

  ## Assign names to each grid model, these are the names that are presented in the output tables
  model_names <- "h2o." %+% unlist(model_algorithms[1:ngridmodels])
  if (!is.null(params[["Model_idx"]])) model_names <- "m." %+% params[["Model_idx"]] %+% "." %+% model_names
  if (ngridmodels > 1) model_names <- model_names %+% ".grid." %+% (1:ngridmodels)
  if (!is.null(model_contrl[["name"]]))  model_names <- model_names %+% "." %+% model_contrl[["name"]]

  names(modelfits_all) <- names(model_algorithms) <- names(model_ids) <- model_names

  fit <- list(
    params = params,
    modelfits_grid = modelfits_grid,
    modelfits_all = modelfits_all,
    topmodel_grid = topmodel_grid,
    ngridmodels = ngridmodels,
    grid_id = modelfits_grid@grid_id,
    model_algorithms = model_algorithms,
    model_ids = model_ids,
    model_names = model_names
    # grid_objects = grid_objects,
    # grid_ids = lapply(grid_objects, function(grids_object) grids_object@grid_id),
    )

  class(fit) <- c(class(fit)[1], c("H2Ogrid"))
  return(fit)
}