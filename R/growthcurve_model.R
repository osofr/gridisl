#' Predict for a holdout set (NOT IMPLEMENTED)
#'
#' @param OData Input data object created by \code{importData} function.
#' @param tmin ...
#' @param tmax ...
#' @param modelIDs ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
predict_fullcurve <- function(OData, tmin, tmax, modelIDs, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  nodes <- OData$nodes
  modelfit <- OData$modelfit
  sel_vars <- c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData$new.factor.names), nodes$Ynode)
  # browser()

  if (missing(tmin)) tmin <- OData$min.t
  if (missing(tmax)) tmax <- min(OData$max.t, 700)
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")

  # Predict a full curve based on a grid for each subject. The grid is tmin to tmax. Covariates are carried forward:
  gridDT <- CJ(unique(OData$dat.sVar[[nodes$IDnode]]), tmin:tmax)
  colnames(gridDT) <- c(nodes$IDnode, nodes$tnode)
  setkeyv(gridDT, cols = c(nodes$IDnode, nodes$tnode))

  gridDT <- OData$dat.sVar[, sel_vars, with = FALSE][gridDT, roll = TRUE]
  gridDT <- OData$dat.sVar[][gridDT, roll = TRUE]

  # gridDT[1:100, ]

  newOData <- OData$clone()
  newOData$dat.sVar <- gridDT

  preds <- modelfit$predict(newdata = newOData, subset_vars = NULL, subset_exprs = NULL)

  if (is.matrix(preds$getprobA1)) {
    gridDT[, (colnames(preds$getprobA1)) := as.data.table(preds$getprobA1)]
  } else {
    gridDT[, ("PredModel1") := preds$getprobA1]
  }

  return(gridDT)
}

# ---------------------------------------------------------------------------------------
#' Define and fit growth models evaluated on holdout observations.
#'
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param params Parameters specifying the type of modeling procedure to be used.
# @param holdout Set to TRUE to train the model fit on non-holdout observations only. Set to FALSE to train the model on the entire input dataset.
#' @param hold_column The name of the column that contains the holdout observation indicators (TRUE/FALSE) in the input data.
#' This holdout column must be defined and added to the input data prior to calling this function.
#' @param random Logical, specifying if the holdout observations should be selected at random.
#' If FALSE then the last observation for each subject is selected as a holdout.
#' @param seed Random number seed for selecting a random holdout.
#' @param expr_to_train Additional logical expression which will further subset observations (rows) for training data.
#' Use this to restrict the model fitting to a specific subsample of the training datset.
#' @param use_new_features ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
fit_growthSL_holdout <- function(ID, t_name, x, y, data, params, hold_column = NULL, random = FALSE, seed = NULL, expr_to_train = NULL, use_new_features = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  nodes <- list(Lnodes = x, Ynode = y, IDnode = ID, tnode = t_name)
  orig_colnames <- colnames(data)

  if (is.null(hold_column)) {
    hold_column <- "hold"
    message("...selecting holdout observations...")
    data <- add_holdout_ind(data, ID, hold_column = hold_column, random = random, seed = seed)
  }
  params$hold_column <- hold_column
  train_data <- data[!data[[hold_column]], ]

  ## ------------------------------------------------------------------------------------------
  ## Define training data summaries (excludes holdouts, summaries are created without the holdout observations):
  ## ------------------------------------------------------------------------------------------
  train_data <- define_features(train_data, nodes, train_set = TRUE, holdout = TRUE, hold_column = hold_column)
  if (!is.null(expr_to_train)) train_data <- train_data[eval(parse(text=expr_to_train)), ]

  ## ------------------------------------------------------------------------------------------
  ## Define validation data (includes the holdout only, each summary is created without the holdout observation):
  ## ------------------------------------------------------------------------------------------
  valid_data <- define_features(data, nodes, train_set = FALSE, hold_column = hold_column)
  valid_data <- valid_data[valid_data[[hold_column]], ]

  ## ------------------------------------------------------------------------------------------
  ## Add new features as predictors?
  ## ------------------------------------------------------------------------------------------
  if (use_new_features) {
    new_features <- setdiff(colnames(train_data), c(orig_colnames,hold_column))
    x <- c(x, new_features)
  }

  ## ------------------------------------------------------------------------------------------
  ## Perfom fitting (and scoring based on validation set)
  ## ------------------------------------------------------------------------------------------
  modelfit <- fit_model(ID, t_name, x, y, train_data = train_data, valid_data = valid_data, params = params)

  return(list(modelfit = modelfit, train_data = train_data, valid_data = valid_data))
}

# ---------------------------------------------------------------------------------------
#' Predict for a holdout set
#'
#' @param modelfit Model fit object returned by \code{\link{get_fit}} function.
#' @param valid_data Subject-specific holdout (validation) data for which predictions should be obtained.
#' @param predict_only_bestK_models Specify the total number of top-ranked models (validation or C.V. MSE) for which predictions should be obtained.
#' Leave missing to obtain predictions for all models that were fit as part of this ensemble.
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
predict_holdouts <- function(modelfit, valid_data, predict_only_bestK_models, add_subject_data = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes
  if (missing(valid_data)) valid_data <- modelfit$OData_valid$dat.sVar
  return(predict_model(modelfit = modelfit, newdata = valid_data, predict_only_bestK_models, evalMSE = TRUE, add_subject_data = add_subject_data))
}

# ---------------------------------------------------------------------------------------
#' Define and fit growth models with full cross-validation.
#'
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param params Parameters specifying the type of modeling procedure to be used.
#' @param nfolds Number of folds to use in cross-validation.
#' @param fold_column The name of the column in the input data that contains the cross-validation fold indicators (must be an ordered factor).
#' @param seed Random number seed for selecting a random holdout.
#' @param expr_to_train Additional logical expression which will further subset observations (rows) for training data.
#' Use this to restrict the model fitting to a specific subsample of the training datset.
#' @param use_new_features ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
fit_growthSL_CV <- function(ID, t_name, x, y, data, params, nfolds = 5, fold_column = NULL, seed = NULL, expr_to_train = NULL, use_new_features = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  nodes <- list(Lnodes = x, Ynode = y, IDnode = ID, tnode = t_name)
  orig_colnames <- colnames(data)

  if (is.null(fold_column)) {
    fold_column <- "fold"
    data <- add_CVfolds_ind(data, ID, nfolds = nfolds, fold_column = fold_column, seed = seed)
  }
  params$fold_column <- fold_column

  ## ------------------------------------------------------------------------------------------
  ## Define training data summaries (using all observations):
  ## ------------------------------------------------------------------------------------------
  train_data <- define_features(data, nodes, train_set = TRUE, holdout = FALSE)
  if (!is.null(expr_to_train)) train_data <- train_data[eval(parse(text=expr_to_train)), ]

  ## ------------------------------------------------------------------------------------------
  ## Define validation data to be used for scoring during CV (each summary is created without the holdout observation):
  ## ------------------------------------------------------------------------------------------
  valid_data <- define_features(data, nodes, train_set = FALSE, holdout = FALSE)

  ## ------------------------------------------------------------------------------------------
  ## Add new features as predictors?
  ## ------------------------------------------------------------------------------------------
  if (use_new_features) {
    new_features <- setdiff(colnames(train_data), c(orig_colnames, fold_column))
    x <- c(x, new_features)
  }

  ## ------------------------------------------------------------------------------------------
  ## Perfom CV fitting (no scoring yet)
  ## ------------------------------------------------------------------------------------------
  modelfit <- fit_model(ID, t_name, x, y, train_data = train_data, params = params, fold_column = fold_column)

  ## ------------------------------------------------------------------------------------------
  ## Score models based on validation set
  ## ------------------------------------------------------------------------------------------
  OData_valid <- importData(data = valid_data, ID = nodes$IDnode, t_name = nodes$tnode, covars = modelfit$predvars, OUTCOME = modelfit$outvar)
  modelfit <- modelfit$score_CV(validation_data = OData_valid) # returns the modelfit object intself, but does the scoring of each CV model
  # preds <- predict_CV(modelfit, valid_data) # will return the matrix of predicted cv values (one column per model)

  return(list(modelfit = modelfit, train_data = train_data, valid_data = valid_data))
}

# ---------------------------------------------------------------------------------------
#' Predict for cross-validation sets
#'
#' @param modelfit Model fit object returned by \code{\link{get_fit}} function.
#' @param valid_data New validation data for external CV
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return new CV-based MSEs
#' @export
score_CVgrowthcurveSL <- function(modelfit, valid_data, verbose = getOption("growthcurveSL.verbose")) {
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  ## Get OData_train from modelfit
  OData_train <- modelfit$OData_train
  nodes <- OData_train$nodes
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  if (missing(valid_data)) {
    # DO NOT RESCORE, USE LAST CV PREDICTIONS INSTEAD
    OData_valid <- modelfit$OData_valid
  } else {
    # RE-SCORE THE CV MODELS BASED ON NEW VALIDATION DATA
    OData_valid <- importData(data = valid_data, ID = nodes$IDnode, t_name = nodes$tnode, covars = modelfit$predvars, OUTCOME = modelfit$outvar)
    ## Rescore the model based on CV fold predictions (out of sample) based on external validation set:
    t_CV <- system.time(
      modelfit <- modelfit$score_CV(validation_data = OData_valid)
    )
    print("t_CV: "); print(t_CV)
  }
  print("MSE: "); print(unlist(modelfit$getMSE))
  return(modelfit$getMSE)
}