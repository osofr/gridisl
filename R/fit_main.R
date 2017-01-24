#' S3 methods for printing a collection of learners
#'
#' Prints the stack models
#' @param modelstack An object (list) of class ModelStack
#' @param ... Additional options passed on to \code{print.PredictionModel}.
#' @export
print.ModelStack <- function(modelstack, ...) {
  str(modelstack)
  return(invisible(NULL))
}

# ---------------------------------------------------------------------------------------
# Define modeling algorithm(s), package and parameters
# @param estimator A character string name of package and estimator (algorithm) name, separated by "__".
# @param ... Additional modeling parameters to be passed to modeling function.
# @rdname defModel
# @export
defLearner <- function(estimator, x, ...) {
  pkg_est <- strsplit(estimator, "__", fixed = TRUE)[[1]]
  pkg <- pkg_est[1]
  if (length(pkg_est) > 1) est <- pkg_est[2] else est <- NULL
  learner <- defModel(estimator, x, ...)

  if (!(pkg %in% c("h2o", "xgboost"))) {
    learner[[1]][["fit.algorithm"]] <- learner[[1]][["grid.algorithm"]]
    learner[[1]][["grid.algorithm"]] <- NULL
  }
  return(learner)
}

# ---------------------------------------------------------------------------------------
#' Interface for defining models
#'
#' Specify either a single model or a grid of multple models by invoking the optional argument \code{param_grid}.
#' @param estimator A character string name of package and estimator (algorithm) name, separated by "__".
#' @param x A vector containing the subset of the names of the predictor variables to use in building this
#' particular learner or grid. This argument can be used to over-ride the values of \code{x} provided to \code{fit} function.
#' As such, the names supplied here must always be a subset of the names specified to \code{fit}.
#' When this argument is missing (default) the column names provided to \code{fit} are used as predictors in building this model / grid.
#' @param search_criteria Search criteria
#' @param param_grid Named list of model hyper parameters (optional).
#' Each named item defines either a fixed model parameter value or a vector of possible values.
#' In the latter case this function call would define a grid (ensemble) of models.
#' Each model in the ensemble corresponds by a particular fixed parameter value.
#' @param ... Additional modeling parameters to be passed on directly to the modeling function.
#' @export
defModel <- function(estimator, x, search_criteria, param_grid, ...) {
  pkg_est <- strsplit(estimator, "__", fixed = TRUE)[[1]]
  pkg <- pkg_est[1]
  if (length(pkg_est) > 1) est <- pkg_est[2] else est <- NULL

  ## call outside fun that parses ... and checks all args are named
  sVar.exprs <- capture.exprs(...)

  GRIDparams = list(fit.package = pkg, fit.algorithm = "grid", grid.algorithm = est)
  if (!missing(x)) GRIDparams[["x"]] <- x
  if (!missing(search_criteria)) GRIDparams[["search_criteria"]] <- search_criteria
  if (!missing(param_grid)) {
    if (!is.list(param_grid)) stop("'param_grid' must be a named list of model hyper parameters")
    if (is.null(names(param_grid)) || "" %in% names(param_grid)) stop("all items in 'param_grid' must be named")
    GRIDparams[["param_grid"]] <- param_grid
  }

  if (length(sVar.exprs) > 0) GRIDparams <- c(GRIDparams, sVar.exprs)
  GRIDparams <- list(GRIDparams)
  class(GRIDparams) <- c(class(GRIDparams), "ModelStack")
  return(GRIDparams)
}

# S3 method '+' for adding two ModelStack objects
# Summary measure lists in both get added as c(,) into the summary measures in model1 / model2 objects
#' @rdname defModel
#' @param model1 An object returned by a call to \code{defModel} function.
#' @param model2 An object returned by a call to \code{defModel} function.
#' @export
`+.ModelStack` <- function(model1, model2) {
  assert_that(is.ModelStack(model1))
  assert_that(is.ModelStack(model2))
  newStack <- append(model1, model2)
  class(newStack) <- c(class(newStack), "ModelStack")
  return(newStack)
}

#' @rdname fit.ModelStack
#' @export
fit <- function(...) { UseMethod("fit") }

# ---------------------------------------------------------------------------------------
#' Fit Discrete SuperLearner (Ensemble)
#'
#' Define and fit discrete SuperLearner for growth curve modeling.
#' Model selection (scoring) is based on MSE for a single random (or last) holdout data-point for each subject.
#' This is in contrast to the model selection with V-fold cross-validated MSE in \code{\link{fit_cvSL}},
#' which leaves the entire subjects (entire growth curves) outside of the training sample.
#' @param models ...
#' @param method The type of model selection procedure when fitting several models at once. Possible options are "none", "cv", and "holdout".
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param params Parameters specifying the type of modeling procedure to be used.
#' @param nfolds Number of folds to use in cross-validation.
#' @param fold_column The name of the column in the input data that contains the cross-validation fold indicators (must be an ordered factor).
#' @param hold_column The name of the column that contains the holdout observation indicators (TRUE/FALSE) in the input data.
#' This holdout column must be defined and added to the input data prior to calling this function.
#' @param hold_random Logical, specifying if the holdout observations should be selected at random.
#' If FALSE then the last observation for each subject is selected as a holdout.
#' @param seed Random number seed for selecting a random holdout.
#' @param refit Set to \code{TRUE} (default) to refit the best estimator using the entire dataset.
#' When \code{FALSE}, it might be impossible to make predictions from this model fit.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(GriDiSL.verbose=TRUE)}.
#' @param ... Additional arguments that will be passed on to \code{fit_model} function.
#' @return ...
# @seealso \code{\link{GriDiSL-package}} for the general overview of the package,
# @example tests/examples/1_GriDiSL_example.R
#' @export
fit.ModelStack <- function(models,
                           method = c("none", "cv", "holdout"),
                           data,
                           ID,
                           t_name,
                           x,
                           y,
                           nfolds = NULL,
                           fold_column = NULL,
                           hold_column = NULL,
                           hold_random = FALSE,
                           seed = NULL,
                           refit = TRUE,
                           verbose = getOption("GriDiSL.verbose"),
                           ...) {
  method <- method[1L]
  gvars$method <- method
  gvars$verbose <- verbose

  if (!is.ModelStack(models)) stop("argument models must be of class 'ModelStack'")
  if (!(method %in% c("none", "cv", "holdout")))
    stop("argument method must be one of: 'none', 'cv', 'holdout'")
  if (!data.table::is.data.table(data) && !is.DataStorageClass(data))
    stop("argument data must be of class 'data.table, please convert the existing data.frame to data.table by calling 'data.table::as.data.table(...)'")

  if (missing(ID)) ID <- names(data)[1]
  if (missing(t_name)) t_name <- names(data)[2]
  if (missing(y)) y <- names(data)[3]
  if (missing(x)) x <- names(data)[4:ncol(data)]

  nodes <- list(Lnodes = x, Ynode = y, IDnode = ID, tnode = t_name)
  orig_colnames <- colnames(data)

  if (method %in% "none") {
    ## Fit models based on all available data
    modelfit <- fit_model(ID, t_name, x, y, data, models = models, verbose = verbose, ...)
  } else if (method %in% "cv") {
    modelfit <- fit_cvSL(ID, t_name, x, y, data, models = models, nfolds = nfolds, fold_column = fold_column, refit = refit, seed = seed, verbose = verbose, ...)
  } else if (method %in% "holdout") {
    modelfit <- fit_holdoutSL(ID, t_name, x, y, data, models = models, hold_column = hold_column, hold_random = hold_random, refit = refit, seed = seed, verbose = verbose, ...)
  }

  return(modelfit)
}

#' Save the best performing h2o model
#'
#' @param modelfit A model object of class \code{PredictionModel} returned by functions \code{fit_model}, \code{fit_holdoutSL} or \code{fit_cvSL}.
#' @export
save_best_model <- function(modelfit, file.path = getOption('GriDiSL.file.path')) {
  stop("...not implemented...")
  assert_that(is.PredictionModel(modelfit))
  best_model_name <- modelfit$get_best_model_names(K = 1)
  message("saving the best model fit: " %+% best_model_name)
  ## Will obtain the best model object trained on TRAINING data only
  ## If CV SL was used this model is equivalent to the best model trained on all data
  ## However, for holdout SL this model will be trained only on non-holdout observations
  best_model_traindat <- modelfit$get_best_models(K = 1)[[1]]
  h2o.saveModel(best_model_traindat, file.path, force = TRUE)
  ## This model is always trained on all data (if exists)
  best_model_alldat <- modelfit$BestModelFitObject$model.fit$modelfits_all
  if (!is.null(best_model_alldat))
    h2o.saveModel(best_model_alldat[[1]], file.path, force = TRUE)

  return(invisible(NULL))
}

validate_convert_input_data <- function(input_data, ID, t_name, x, y, useH2Oframe = FALSE, dest_frame = "all_train_H2Oframe") {
  # browser()
  if (is.DataStorageClass(input_data)) {
    OData_input <- input_data
    # if (useH2Oframe && is.null(OData_input$H2Oframe)) stop("fatal error: useH2Oframe was set to TRUE, but the H2Oframe could not be located in the input data.")
  } else if (is.data.frame(input_data) || data.table::is.data.table(input_data)) {
    OData_input <- importData(data = input_data, ID = ID, t_name = t_name, covars = x, OUTCOME = y) ## Import input data into R6 object, define nodes
  } else {
    stop("input training / validation data must be either data.frame, data.table or DataStorageClass object")
  }
  if (useH2Oframe && is.null(OData_input$H2Oframe)) OData_input$fast.load.to.H2O(saveH2O = TRUE, destination_frame = dest_frame)
  return(OData_input)
}

# ---------------------------------------------------------------------------------------
#' Discrete SuperLearner with one-out holdout validation
#'
#' Define and fit discrete SuperLearner for growth curve modeling.
#' Model selection (scoring) is based on MSE for a single random (or last) holdout data-point for each subject.
#' This is in contrast to the model selection with V-fold cross-validated MSE in \code{\link{fit_cvSL}},
#' which leaves the entire subjects (entire growth curves) outside of the training sample.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param models Parameters specifying the type of modeling procedure to be used.
#' @param hold_column The name of the column that contains the holdout observation indicators (TRUE/FALSE) in the input data.
#' This holdout column must be defined and added to the input data prior to calling this function.
#' @param hold_random Logical, specifying if the holdout observations should be selected at random.
#' If FALSE then the last observation for each subject is selected as a holdout.
#' @param refit Set to \code{TRUE} (default) to refit the best estimator using the entire dataset.
#' When \code{FALSE}, it might be impossible to make predictions from this model fit.
#' @param seed Random number seed for selecting a random holdout.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(GriDiSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{GriDiSL-package}} for the general overview of the package,
# @example tests/examples/1_GriDiSL_example.R
#' @export
fit_holdoutSL <- function(ID,
                          t_name,
                          x,
                          y,
                          data,
                          models,
                          hold_column = NULL,
                          hold_random = TRUE,
                          refit = TRUE,
                          seed = NULL,
                          verbose = getOption("GriDiSL.verbose"),
                          ...) {
  gvars$verbose <- verbose
  gvars$method <- "holdout"
  nodes <- list(Lnodes = x, Ynode = y, IDnode = ID, tnode = t_name)
  orig_colnames <- colnames(data)

  if (is.null(hold_column)) {
    hold_column <- "hold"
    message("...selecting holdout observations...")
    data <- add_holdout_ind(data, ID, hold_column = hold_column, random = hold_random, seed = seed)
  }

  train_data <- data[!data[[hold_column]], ]

  ## ------------------------------------------------------------------------------------------
  ## Define validation data (includes the holdout only, each summary is created without the holdout observation):
  ## ------------------------------------------------------------------------------------------
  valid_data <- data[data[[hold_column]], ]

  ## ------------------------------------------------------------------------------------------
  ## Perform fitting based on training set (and model scoring based on holdout validation set)
  ## ------------------------------------------------------------------------------------------
  modelfit <- fit_model(ID, t_name, x, y, train_data = train_data, valid_data = valid_data, models = models, verbose = verbose, ...)

  ## ------------------------------------------------------------------------------------------
  ## Re-fit the best scored model using all available data
  ## ------------------------------------------------------------------------------------------
  if (refit) {
    message("refitting the best scored model on all data...")
    OData_all <- importData(data = data, ID = ID, t_name = t_name, covars = x, OUTCOME = y) ## Import input data into R6 object, define nodes
    best_fit <- modelfit$refit_best_model(OData_all, ...)
  }

  return(modelfit)
}

# ---------------------------------------------------------------------------------------
#' Discrete SuperLearner with V-fold cross-validation.
#'
#' Define and fit discrete SuperLearner for growth curve modeling.
#' Model selection (scoring) is based on V-fold cross-validated MSE that leaves entire subjects outside of the training sample.
#' This is in contrast to holdout SuperLearner in \code{\link{fit_holdoutSL}} that leaves only a single random (or last) growth measurement  outside of the training sample.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param models Parameters specifying the type of modeling procedure to be used.
#' @param nfolds Number of folds to use in cross-validation.
#' @param fold_column The name of the column in the input data that contains the cross-validation fold indicators (must be an ordered factor).
#' @param refit Set to \code{TRUE} (default) to refit the best estimator using the entire dataset.
#' When \code{FALSE}, it might be impossible to make predictions from this model fit.
#' @param seed Random number seed for selecting a random holdout.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(GriDiSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{GriDiSL-package}} for the general overview of the package,
# @example tests/examples/1_GriDiSL_example.R
#' @export
fit_cvSL <- function(ID,
                     t_name,
                     x,
                     y,
                     data,
                     models,
                     nfolds = 5,
                     fold_column = NULL,
                     refit = TRUE,
                     seed = NULL,
                     verbose = getOption("GriDiSL.verbose"),
                     ...) {
  gvars$verbose <- verbose
  gvars$method <- "cv"
  nodes <- list(Lnodes = x, Ynode = y, IDnode = ID, tnode = t_name)
  orig_colnames <- colnames(data)

  if (is.null(fold_column)) {
    fold_column <- "fold"
    data <- add_CVfolds_ind(data, ID, nfolds = nfolds, fold_column = fold_column, seed = seed)
  }

  ## ------------------------------------------------------------------------------------------
  ## Perform CV fitting (no scoring yet)
  ## ------------------------------------------------------------------------------------------
  modelfit <- fit_model(ID, t_name, x, y, train_data = data, models = models, fold_column = fold_column, verbose = verbose, ...)

  ## ------------------------------------------------------------------------------------------
  ## Re-fit the best manually-scored model on all data
  ## ------------------------------------------------------------------------------------------
  if (refit) best_fit <- modelfit$refit_best_model(modelfit$OData_train, ...)

  return(modelfit)
}