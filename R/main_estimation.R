#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggplot geom_point geom_errorbar theme_bw coord_flip aes position_dodge alpha
#' @import ggiraph
# @importFrom ggiraph geom_point_interactive ggiraph
NULL

# ---------------------------------------------------------------------------------------
# **** ALLOW data to be an R6 object of class DataStorageClass. This will enable calling h2o SL from other pkgs *****
# **** CONSIDER brining the nfold / fold_column / fold_assignment as args of this function
# 1. Making this work with random/last holdout schemes?
# 2. Making this work with h2o-internal C.V., but using either manually defined or automatic folds?
# 3. Working C.V. when predictors on validation set have to be defined differently from predictors in training set?
# 4. Is it at all possible to make 4. work with h2o-internal CV?
# 5. [LONG-TERM] Need wrappers for various h2o modeling functions
# ---------------------------------------------------------------------------------------
#' Generic modeling function for any longitudinal data.
#'
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param data_validation Optional \code{data.frame} or \code{data.table} with validation data. When provided, this dataset will be used for scoring the model fit(s).
#' @param params Parameters specifying the type of modeling procedure to be used.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
fit_model <- function(ID, t_name, x, y, data, data_validation, params, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  if (missin(data)) stop("data arg must be specified")

  if (is.DataStorageClass(data)) {
    OData_train <- data
  } else if (is.data.frame(data) || data.table::is.data.table(data)) {
    OData_train <- importData(data = data, ID = ID, t_name = t_name, covars = x, OUTCOME = y) ## Import input data into R6 object, define nodes
  } else {
    stop("data arg must be either data.frame, data.table or DataStorageClass object")
  }

  nodes <- OData_train$nodes ## Extract nodes list

  if (missing(x)) x <- nodes$Lnodes ## Assign predictors if missing (everything but ID and outcome in y)

  ## **** If we wanted to define folds manually, this would be one way to do it:****
  # OData_train$define_CVfolds(nfolds = 3, fold_column = "fold_id", seed = 12345)

  ## If fold_column specified in the model parameters, add it to the data object:
  if (!is.null(params$fold_column)) OData_train$fold_column <- params$fold_column

  ## Define R6 object with validation data:
  if (!missing(data_validation)) {
    OData_valid <- importData(data = data_validation, ID = ID, t_name = t_name, covars = x, OUTCOME = y)
  } else {
    OData_valid <- NULL
  }

  ## Define R6 regression class (specify subset_exprs to select only specific obs during fitting, e.g., only non-holdouts)
  regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = x, outvar.class = list("binary"), model_contrl = params)
  # regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = x, outvar.class = list("binary"), subset_exprs = list("!hold"), model_contrl = params)

  ## Define a modeling object, perform fitting (real data is being passed for the first time here):
  modelfit <- PredictionModel$new(reg = regobj)$fit(data = OData_train, validation_data = OData_valid)

  return(modelfit)
}

# ---------------------------------------------------------------------------------------
#' Define and fit growth models.
#'
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param params Parameters specifying the type of modeling procedure to be used.
#' @param holdout Set to TRUE to train the model fit on non-holdout observations only. Set to FALSE to train the model on the entire input dataset.
#' @param hold_column The name of the column that contains the holdout observation indicators (TRUE/FALSE) in the input data.
#' This holdout column must be defined and added to the input data prior to calling this function.
#' @param random Logical, specifying if the holdout observations should be selected at random.
#' If FALSE then the last observation for each subject is selected as a holdout.
#' @param seed Random number seed for selecting a random holdout.
#' @param expr_to_train Additional logical expression which will further subset observations (rows) for training data.
#' Use this to restrict the model fitting to a specific subsample of the training datset.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
# stratify = NULL, reg,
fit_model_holdout <- function(ID, t_name, x, y, data, params, holdout = TRUE, hold_column = NULL, random = FALSE, seed = NULL, expr_to_train = NULL, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  nodes <- list(Lnodes = x, Ynode = y, IDnode = ID, tnode = t_name)

  if (holdout && is.null(hold_column)) {
    hold_column <- "hold"
    data <- add_holdout_ind(data, ID, hold_column = hold_column, random = random, seed = seed)
  }
  data_train <- data[!data[[hold_column]], ]

  ## ------------------------------------------------------------------------------------------
  ## Define training data summaries (excludes holdouts, summaries are created without the holdout observations):
  ## ------------------------------------------------------------------------------------------
  data_train <- define_features(data_train, nodes, train_set = TRUE, holdout = holdout, hold_column = hold_column)
  if (!is.null(expr_to_train)) data_train <- data_train[eval(parse(text=expr_to_train)), ]

  ## ------------------------------------------------------------------------------------------
  ## Define validation data (includes the holdout only, each summary is created without the holdout observation):
  ## ------------------------------------------------------------------------------------------
  data_valid <- define_features(data, nodes, train_set = FALSE, hold_column = hold_column)
  data_valid <- data_valid[data_valid[[hold_column]], ]

  ## ------------------------------------------------------------------------------------------
  ## Perfom fitting (and scoring based on validation set)
  ## ------------------------------------------------------------------------------------------
  return(fit_model(ID, t_name, x, y, data = data_train, data_validation = data_valid, params = data_valid))
}

# ---------------------------------------------------------------------------------------
#' Predict for cross-validation sets
#'
#' @param OData Input data object created by \code{importData} function.
#' @param OData_valid ...
#' @param modelfit Model fit object returned by \code{\link{get_fit}} function.
#' @param modelIDs ... Not implemented ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
## @param all_obs Predict values for all observations (including holdout) or just the holdout observations?
## all_obs = FALSE,
predictCV <- function(modelfit, data_valid, modelIDs, verbose = getOption("growthcurveSL.verbose")) {
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  ## 1. Get OData_train from modelfit
  OData_train <- modelfit$OData_train
  nodes <- OData_train$nodes

  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  # if (all_obs) {
  #   subset_exprs <- NULL
  # } else {
    # subset_exprs <- "hold == TRUE"
  # }

  if (missing(data_valid)) {
    nodes <- OData_train$nodes
    sel_vars <- c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData_train$new.factor.names), nodes$Ynode, OData_train$hold_column, OData_train$fold_column)
    dataDT <- OData_train$dat.sVar[, sel_vars, with = FALSE]
    data_valid <- define_features(dataDT, nodes, train_set = FALSE, hold_column = OData_train$hold_column)
    # OData_valid$dat.sVar <- data_valid[data_valid[[OData_valid$hold_column]], ]
  }

  ## 2. Get predictions for each CV model based on external validation CV:
  t_cvs <- system.time(
    pred_mat <- predictP1_externalCV(m.fit = modelfit$getfit, data_valid = data_valid, DataStorageObject = OData_train)
    )
  print(t_cvs)

  return(pred_mat)

  # 3. Obtain the CV-based MSE

  # Get predictions for holdout data only when the actual outcome was also not missing:
  preds <- modelfit$predict(newdata = OData_valid, subset_exprs = subset_exprs)
  # preds <- modelfit$predict(newdata = OData, subset_exprs = subset_exprs)
  holdoutDT <- OData_valid$dat.sVar[, c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData_valid$new.factor.names), nodes$Ynode, OData_valid$hold_column), with = FALSE]
  holdoutDT[, (colnames(preds$getprobA1)) := as.data.table(preds$getprobA1)]
  # browser()
  # MSE <- modelfit$evalMSE(OData_valid)
  print("MSE: "); print(unlist(preds$getMSE))
  return(holdoutDT)
}

# ---------------------------------------------------------------------------------------
#' Predict for a holdout set
#'
#' @param OData Input data object created by \code{importData} function.
#' @param OData_valid ...
#' @param modelfit Model fit object returned by \code{\link{get_fit}} function.
#' @param modelIDs ... Not implemented ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
## @param all_obs Predict values for all observations (including holdout) or just the holdout observations?
## all_obs = FALSE,
predictHoldout <- function(OData, OData_valid, modelfit, modelIDs, verbose = getOption("growthcurveSL.verbose")) {
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose

  if (missing(OData_valid)) {
    assert_that(is.DataStorageClass(OData))
    OData_valid <- OData$clone()
    nodes <- OData$nodes
    sel_vars <- c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData$new.factor.names), nodes$Ynode, OData$hold_column)
    dataDT <- OData$dat.sVar[, sel_vars, with = FALSE]
    data_valid <- define_features(dataDT, nodes, train_set = FALSE, hold_column = OData$hold_column)
    OData_valid$dat.sVar <- data_valid[data_valid[[OData_valid$hold_column]], ]
  }

  assert_that(is.DataStorageClass(OData_valid))
  nodes <- OData_valid$nodes

  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  # if (all_obs) {
  #   subset_exprs <- NULL
  # } else {
    subset_exprs <- "hold == TRUE"
  # }

  # Get predictions for holdout data only when the actual outcome was also not missing:
  preds <- modelfit$predict(newdata = OData_valid, subset_exprs = subset_exprs)
  # preds <- modelfit$predict(newdata = OData, subset_exprs = subset_exprs)
  holdoutDT <- OData_valid$dat.sVar[, c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData_valid$new.factor.names), nodes$Ynode, OData_valid$hold_column), with = FALSE]
  holdoutDT[, (colnames(preds$getprobA1)) := as.data.table(preds$getprobA1)]
  # browser()
  # MSE <- modelfit$evalMSE(OData_valid)
  print("MSE: "); print(unlist(preds$getMSE))
  return(holdoutDT)
}

# ---------------------------------------------------------------------------------------
# TO DO: Add prediction only based on the subset of models (rather than predicting for all models)
# using modelIDs argument
# ---------------------------------------------------------------------------------------
#' Predict for a holdout set
#'
#' @param OData Input data object created by \code{importData} function.
#' @param tmin ...
#' @param tmax ...
#' @param modelIDs ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
predictCurve <- function(OData, tmin, tmax, modelIDs, verbose = getOption("growthcurveSL.verbose")) {
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







