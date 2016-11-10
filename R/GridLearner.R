# default metalearner (NN least squares)
h2o.glm_nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)

# Train a model using a single h2o learner (user-spec) with cross-validation & keep CV predictions
#' @export
SLfit.h2oLearner <- function(learner, training_frame, y, x, family = "binomial", fold_column, model_contrl, validationH2Oframe = NULL, ...) {
  # if (is.numeric(seed)) set.seed(seed)  #If seed given, set seed prior to next step
  # h2o.no_progress()
  learner_fun <- match.fun(learner)
  mainArgs <- list(y = y, training_frame = training_frame, family = family, keep_cross_validation_folds = TRUE)

  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }
  if (!is.null(model_contrl$nfolds)) mainArgs$nfolds <- model_contrl$nfolds
  if (!is.null(model_contrl$fold_assignment)) mainArgs$fold_assignment <- model_contrl$fold_assignment

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = learner_fun)

  if (!is.null(validationH2Oframe)) mainArgs$validation_frame <- validationH2Oframe

  if (("x" %in% names(formals(learner))) && (as.character(formals(learner)$x)[1] != "")) {
    # Special case where we pass a subset of the colnames, x, in a custom learner function wrapper
    # model.fit <- learner_fun(y = y, training_frame = training_frame, validation_frame = NULL, family = family, fold_column = fold_column, keep_cross_validation_folds = TRUE)
  } else {
    # Use all predictors in training set for training
    mainArgs$x <- x
    # model.fit <- learner_fun(y = y, x = x, training_frame = training_frame, validation_frame = NULL, family = family, fold_column = fold_column, keep_cross_validation_folds = TRUE)
  }

  model.fit <- do.call(learner_fun, mainArgs)

  fit <- vector(mode = "list")
  fit$fitfunname <- learner;
  if (gvars$verbose) {
    print("grid search fitted models:"); print(model.fit)
  }
  fit$H2O.model.object <- model.fit
  class(fit) <- c(class(fit)[1], c("H2Omodel"))
  return(fit)
}

# S3 method for h2o deeplearning fit, takes BinDat data object:
# use "bernoulli" when doing classification and use "gaussian" when doing regression
#' @export
SLfit.h2ogrid <- function(grid.algorithm, training_frame, y, x, family = "binomial", fold_column, model_contrl, validationH2Oframe  = NULL, ...) {
  # h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                  intercept = TRUE,
                  seed = 1,
                  # fold_column = fold_column,
                  # keep_cross_validation_predictions = TRUE,
                  family = family,
                  standardize = TRUE,
                  solver = "L_BFGS",
                  lambda = 0L,
                  max_iterations = 100,
                  ignore_const_cols = FALSE,
                  missing_values_handling = "Skip")

  if (is.null(grid.algorithm)) stop("must specify 'grid.algorithm' name when running 'h2o.grid'")
  if (!is.character(grid.algorithm)) stop("'grid.algorithm' must be a string naming the grid.algorithm for 'h2o.grid'")
  algo_fun_name <- "h2o."%+%grid.algorithm
  if (!exists(algo_fun_name)) stop("could not locate the function %+% " %+% grid.algorithm)

  if (!is.null(validationH2Oframe)) {
    mainArgs$validation_frame <- validationH2Oframe
  }

  algo_fun <- get0(algo_fun_name, mode = "function", inherits = TRUE)
  mainArgs <- keep_only_fun_args(mainArgs, fun = algo_fun)   # Keep only the relevant args in mainArgs list:

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = algo_fun) # Add user args that pertain to this specific learner:
  mainArgs$algorithm <- grid.algorithm
  mainArgs$search_criteria <- model_contrl[["search_criteria"]]
  mainArgs$hyper_params <- model_contrl[[grid.algorithm]]

  if (is.null(mainArgs$hyper_params)) stop("must specify hyper parameters for grid search with '" %+% algo_fun_name %+% "' by defining a SuperLearner params list item named '" %+% grid.algorithm %+% "'")

  if (!is.null(mainArgs$hyper_params[["search_criteria"]])) {
    mainArgs$search_criteria <- mainArgs$hyper_params[["search_criteria"]]
    mainArgs$hyper_params[["search_criteria"]] <- NULL
  }
  if (is.null(mainArgs$search_criteria)) stop("must specify 'search_criteria' when running 'h2o.grid' for grid.algorithm " %+% grid.algorithm)

  # Remove any args from mainArgs that also appear in hyper_params:
  common_hyper_args <- intersect(names(mainArgs), names(mainArgs$hyper_params))
  if(length(common_hyper_args) > 0) mainArgs <- mainArgs[!(names(mainArgs) %in% common_hyper_args)]

  if (gvars$verbose) {
    print("running h2o.grid grid.algorithm: " %+% grid.algorithm);
  }

  model.fit <- do.call(h2o::h2o.grid, mainArgs)
  # sort the grid by increasing MSE:
  model.fit <- h2o::h2o.getGrid(model.fit@grid_id, sort_by = "mse", decreasing = FALSE)

  fit <- vector(mode = "list")
  fit$fitfunname <- "h2o.h2ogrid";
  fit$H2O.model.object <- model.fit
  fit$top.model <- h2o::h2o.getModel(model.fit@model_ids[[1]])
  # h2o.performance(top.model)
  # h2o.performance(top.model, valid = TRUE)

  if (gvars$verbose) {
    # print("grid search fitted models:"); print(model.fit)
    print("grid search: " %+% model.fit@grid_id)
    print("grid search top performing model:"); print(fit$top.model)
    # print(h2o::h2o.performance(fit$top.model))
    # print(h2o::h2o.performance(fit$top.model, valid = TRUE))
    # print("grid search top model summary:")
    # getParms(fit$top.model)
    # str(fit$top.model)
    # .model.parts(fit$top.model@model)
    # if( !is.null(m$coefficients_table) ) print(m$coefficients_table)
    # fit$top.model@model$model_summary
    # fit$top.model$model_summary
    # fit$top.model@summary
    # str(fit$top.model)
    # fit$top.model@model$model_summary
    # str(fit$top.model@model$model_summary)
  }
  class(fit) <- c(class(fit)[1], c("H2Omodel"))
  return(fit)
}

#' @export
fit.h2oGridLearner <- function(fit.class, fit, training_frame, y, x, model_contrl, fold_column, ...) {
  # if (is.null(fold_column)) stop("must define the column of CV fold IDs using data$define_CVfolds()")
  family <- model_contrl$family
  grid.algorithms <- model_contrl$grid.algorithm
  learners <- model_contrl$learner
  if (is.null(family)) family <- "binomial"

  # Will put all fitted models in a single list for stacking:
  fitted_models_all <- NULL
  ngridmodels <- 0
  nfolds <- model_contrl$nfolds
  fold_assignment <- model_contrl$fold_assignment

  # When fold_column is passed as an external arg, then null nfolds as it should no longer be used.
  # Note that fold_column name could have still been specified separately in model_contrl
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      model_contrl$nfolds <- NULL
      model_contrl$fold_assignment <- NULL
    }
  }

  if (is.null(grid.algorithms) && is.null(learners)) {
    stop("must specify either 'grid.algorithm' or 'learner' when performing estimation with GridLearner")
  }

  if (!is.null(grid.algorithms)) {
    if (!is.character(grid.algorithms)) stop("'grid.algorithm' must be a vector of strings naming the grid.algorithms to use in 'h2o.grid'")
    fitted_models <- grid_objects <- top_grid_models <- vector(mode = "list", length = length(grid.algorithms));
    names(fitted_models) <- names(grid_objects) <- names(top_grid_models) <- grid.algorithms

    for (grid.algorithm in grid.algorithms) {
      grid_model_fit <- SLfit.h2ogrid(grid.algorithm = grid.algorithm, training_frame = training_frame, y = y, x = x, family = family,
                                      fold_column = fold_column, model_contrl = model_contrl, ...)
      top_grid_models[[grid.algorithm]] <- grid_model_fit$top.model
      grid_model_H2O <- grid_model_fit$H2O.model.object
      grid_objects[[grid.algorithm]] <- grid_model_H2O
      fitted_models[[grid.algorithm]] <- lapply(grid_model_H2O@model_ids, function(model_id) h2o::h2o.getModel(model_id))
    }
    for (grid.algorithm in grid.algorithms) {
      fitted_models_all <- c(fitted_models_all, fitted_models[[grid.algorithm]])
    }
  }
  ngridmodels <- length(fitted_models_all)

  if (!is.null(learners)) {
    if (!is.character(learners)) stop("'learner' must be a vector of strings naming specific wrappers/learners")
    fitted_models_l <- vector(mode = "list", length = length(learners))
    names(fitted_models_l) <- learners
    for (learner in learners) {
      learner_fit <- SLfit.h2oLearner(learner = learner, training_frame = training_frame, y = y, x = x, family = family,
                                      fold_column = fold_column, model_contrl = model_contrl, ...)
      learner_model_H2O <- learner_fit$H2O.model.object
      fitted_models_l[[learner]] <- learner_model_H2O
    }
    fitted_models_all <- c(fitted_models_all, fitted_models_l)
  }

  # to by-pass error check in h2o.stack:
  for (idx in seq_along(fitted_models_all)) {
    fitted_models_all[[idx]]@allparameters$fold_assignment <- "Modulo"
    fitted_models_all[[idx]]@allparameters$nfolds <- nfolds
  }

  # ----------------------------------------------------------------------------------------------------
  # Saving the fits:
  # ----------------------------------------------------------------------------------------------------
  if (!is.null(model_contrl$save.ensemble) && model_contrl$save.ensemble) {
    if (is.null(model_contrl$ensemble.dir.path) || (model_contrl$ensemble.dir.path %in% "")) {
      stop("when saving ensemble must specify the directory path with 'ensemble.dir.path' parameter")
    }
    saveres <- lapply(fitted_models_all, function(h2omodel) h2o::h2o.saveModel(object = h2omodel, path = model_contrl$ensemble.dir.path, force = TRUE))
    # h2oEnsemble::h2o.save_ensemble(stacked.fit, path = model_contrl$ensemble.dir.path, force = TRUE)
    # h2oEnsemble::h2o.save_ensemble(stacked.fit, path = model_contrl$ensemble.dir.path, force = TRUE, export_levelone = TRUE)
  }

  fit$grid_objects <- grid_objects
  fit$grid_ids <- lapply(fit$grid_objects, function(grids_object) grids_object@grid_id)
  fit$top_grid_models <- top_grid_models
  # TO DIRECTLY SAVE ALL MODEL FITS FROM GRID SEARCH (base-learners)
  fit$fitted_models_all <- fitted_models_all
  fit$ngridmodels <- ngridmodels

  fit$model_algorithms <- lapply(fit$fitted_models_all, function(model) model@algorithm)
  fit$model_ids <- lapply(fit$fitted_models_all, function(model) model@model_id)

  # Assign names to each grid model, keep individual learner names intact (unless a $name arg was passed by the user):
  GRIDmodel_names <- "grid." %+% unlist(fit$model_algorithms[1:ngridmodels]) %+% "." %+% (1:ngridmodels)
  learner_names <- names(fit$fitted_models_all)[-(1:ngridmodels)]
  model_names <- c(GRIDmodel_names, learner_names)
  if (!is.null(model_contrl$name))  model_names <- model_names %+% "." %+% model_contrl$name
  names(fit$fitted_models_all) <- names(fit$model_algorithms) <- names(fit$model_ids) <- model_names

  class(fit) <- c(class(fit)[1], c("H2Ogridmodel"))
  return(fit)
}