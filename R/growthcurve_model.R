
# ---------------------------------------------------------------------------------------
# TO DO
# **** ALLOW data to be an R6 object of class DataStorageClass. This will enable calling h2o SL from other pkgs *****
# 3. Predicting the entire growth curve (need to sort out the summaries, how to handle them, how to define them)

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

  if (missing(tmin)) tmin <- OData$min.t
  if (missing(tmax)) tmax <- min(OData$max.t, 700)
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")

  # Predict a full curve based on a grid for each subject. The grid is tmin to tmax. Covariates are carried forward:
  gridDT <- CJ(unique(OData$dat.sVar[[nodes$IDnode]]), tmin:tmax)
  colnames(gridDT) <- c(nodes$IDnode, nodes$tnode)
  setkeyv(gridDT, cols = c(nodes$IDnode, nodes$tnode))

  gridDT <- OData$dat.sVar[, sel_vars, with = FALSE][gridDT, roll = TRUE]
  gridDT <- OData$dat.sVar[][gridDT, roll = TRUE]

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
fit_holdoutSL <- function(ID, t_name, x, y, data, params, hold_column = NULL, random = FALSE, seed = NULL, expr_to_train = NULL, use_new_features = FALSE, verbose = getOption("growthcurveSL.verbose")) {
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
  # train_data <- define_features(train_data, nodes, train_set = TRUE, holdout = TRUE, hold_column = hold_column)
  train_data <- define_features_drop(train_data, ID = ID, t_name = t_name, y = y, train_set = TRUE)
  if (!is.null(expr_to_train)) train_data <- train_data[eval(parse(text=expr_to_train)), ]

  ## ------------------------------------------------------------------------------------------
  ## Define validation data (includes the holdout only, each summary is created without the holdout observation):
  ## ------------------------------------------------------------------------------------------
  # valid_data <- define_features(data, nodes, train_set = FALSE, hold_column = hold_column)
  # valid_data <- valid_data[valid_data[[hold_column]], ]
  # by giving the hold_column the non-holdout observations will be automatically dropped (could have also done it manually)
  valid_data <- define_features_drop(data, ID = ID, t_name = t_name, y = y, train_set = FALSE, hold_column = hold_column)

  ## ------------------------------------------------------------------------------------------
  ## Add new features as predictors?
  ## ------------------------------------------------------------------------------------------
  if (use_new_features) {
    new_features <- setdiff(colnames(train_data), c(orig_colnames, hold_column))
    x <- c(x, new_features)
  }

  ## ------------------------------------------------------------------------------------------
  ## Perform fitting based on training set (and model scoring based on holdout validation set)
  ## ------------------------------------------------------------------------------------------
  modelfit <- fit_model(ID, t_name, x, y, train_data = train_data, valid_data = valid_data, params = params)

  ## ------------------------------------------------------------------------------------------
  ## Re-fit the best scored model using all available data
  ## ------------------------------------------------------------------------------------------
  ## Define training data summaries (using all observations):
  # data <- define_features(data, nodes, train_set = TRUE, holdout = FALSE)
  data <- define_features_drop(data, ID = ID, t_name = t_name, y = y, train_set = TRUE)
  OData_all <- importData(data = data, ID = ID, t_name = t_name, covars = x, OUTCOME = y) ## Import input data into R6 object, define nodes
  best_fit <- modelfit$refit_best_model(OData_all)
  return(modelfit)
  # return(list(modelfit = modelfit, train_data = train_data, valid_data = valid_data))
}

# ---------------------------------------------------------------------------------------
#' Define and fit growth curve SuperLearner with full cross-validation.
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
#' Use this to restrict the model fitting to a specific subsample of the training dataset.
#' @param use_new_features ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
fit_curveSL <- function(ID, t_name, x, y, data, params, nfolds = 5, fold_column = NULL, seed = NULL, expr_to_train = NULL, use_new_features = FALSE, verbose = getOption("growthcurveSL.verbose")) {
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
  # train_data <- define_features(data, nodes, train_set = TRUE, holdout = FALSE)
  train_data <- define_features_drop(data, ID = ID, t_name = t_name, y = y, train_set = TRUE)
  if (!is.null(expr_to_train)) train_data <- train_data[eval(parse(text=expr_to_train)), ]

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
  ## Score CV models based on validation set
  ## ------------------------------------------------------------------------------------------
  ## Define validation data to be used for scoring during CV (each summary row (X_i,Y_i) is created by first dropping this row):
  # valid_data <- define_features(data, nodes, train_set = FALSE, holdout = FALSE)
  valid_data <- define_features_drop(data, ID = ID, t_name = t_name, y = y, train_set = FALSE)

  OData_valid <- importData(data = valid_data, ID = nodes$IDnode, t_name = nodes$tnode, covars = modelfit$predvars, OUTCOME = modelfit$outvar)
  modelfit <- modelfit$score_CV(validation_data = OData_valid) # returns the modelfit object intself, but does the scoring of each CV model
  print("CV MSE after manual CV model rescoring: "); print(unlist(modelfit$getMSE))
  # preds <- predict_CV(modelfit, valid_data) # will return the matrix of predicted cv values (one column per model)

  ## ------------------------------------------------------------------------------------------
  ## Re-fit the best manually-scored model on all data
  ## Even though we don't need to do this for CV (best model is already trained), we do this to be consistent with holdoutSL (and for additional error checking)
  ## ------------------------------------------------------------------------------------------
  best_fit <- modelfit$refit_best_model(modelfit$OData_train)

  return(modelfit)
  # return(list(modelfit = modelfit, train_data = train_data, valid_data = valid_data))
}

# ---------------------------------------------------------------------------------------
#' Predict for holdout or curve SuperLearner fits
#'
#' @param modelfit Model fit object returned by \code{\link{fit_holdoutSL}} or  \code{\link{fit_curveSL}}.
#' @param newdata Subject-specific data for which predictions should be obtained.
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
predict_SL <- function(modelfit, newdata, add_subject_data = FALSE, verbose = getOption("growthcurveSL.verbose")) {
# predict_holdoutSL <- function(modelfit, newdata, add_subject_data = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  if (is.null(modelfit)) stop("must call fit_holdoutSL() or fit_curveSL() prior to obtaining predictions")
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes
  if (missing(newdata)) {
    # newdata <- modelfit$OData_valid$dat.sVar
    newdata <- modelfit$OData_train$dat.sVar
  } else {
    # newdata <- define_features(newdata, nodes, train_set = TRUE, holdout = FALSE)
    newdata <- define_features_drop(newdata, ID = nodes$IDnode, t_name = nodes$tnode, y = nodes$Ynode, train_set = TRUE)
  }

  ## will use the best model retrained on all data for prediction:
  modelfit$use_best_retrained_model <- TRUE
  preds <- predict_model(modelfit = modelfit, newdata = newdata, add_subject_data = add_subject_data)
  modelfit$use_best_retrained_model <- FALSE
  ## will obtain predictions for all models in the ensemble that were trained on non-holdout observations only:
  # preds2 <- predict_model(modelfit = modelfit, newdata = newdata, add_subject_data = add_subject_data)
  return(preds)
}

# ---------------------------------------------------------------------------------------
#' Predict for holdout observations only
#'
#' @param modelfit Model fit object returned by \code{\link{fit_holdoutSL}} function.
#' @param predict_only_bestK_models Specify the total number of top-ranked models (validation or C.V. MSE) for which predictions should be obtained.
#' Leave missing to obtain predictions for all models that were fit as part of this ensemble.
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
predict_holdouts_only <- function(modelfit, predict_only_bestK_models, add_subject_data = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  if (is.null(modelfit)) stop("must call fit_holdoutSL() prior to obtaining predictions")
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes
  valid_data <- modelfit$OData_valid$dat.sVar
  return(predict_model(modelfit = modelfit, newdata = valid_data, predict_only_bestK_models, evalMSE = FALSE, add_subject_data = add_subject_data))
}
