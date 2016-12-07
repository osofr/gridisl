#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggplot geom_point geom_errorbar theme_bw coord_flip aes position_dodge alpha
#' @import ggiraph
# @importFrom ggiraph geom_point_interactive ggiraph
NULL

# ---------------------------------------------------------------------------------------
# TO DO
# **** ALLOW data to be an R6 object of class DataStorageClass. This will enable calling h2o SL from other pkgs *****
# 1. Perform model re-fitting when doing holdout using the entire training data (to be able to obtain the SL) for one or several specific models
# 2. Enable best model prediction for growthcurve SL (holdout and CV)
# 3. Predicting the entire growth curve (need to sort out the summaries, how to handle them, how to define them)
# 4. Double (nested) residual learner with CV. Need to make residual part of the model class def'n so that it works seamlessly
# 5. [LONG-TERM] Need wrappers for various h2o modeling functions
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#' Generic modeling function for any longitudinal data.
#'
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param train_data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param valid_data Optional \code{data.frame} or \code{data.table} with validation data. When provided, this dataset will be used for scoring the model fit(s).
#' @param params Parameters specifying the type of modeling procedure to be used.
#' @param nfolds ...
#' @param fold_column ...
#' @param seed ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
fit_model <- function(ID, t_name, x, y, train_data, valid_data, params, nfolds, fold_column, seed, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose

  if (missing(train_data)) stop("train_data arg must be specified")

  if (!missing(nfolds) && missing(fold_column)) {
    fold_column <- "fold"
    train_data <- add_CVfolds_ind(train_data, ID, nfolds = nfolds, fold_column = fold_column, seed = seed)
    ## **** If we wanted to define folds manually, this would be one way to do it:****
    # OData_train$define_CVfolds(nfolds = 3, fold_column = "fold_id", seed = 12345)
    params$fold_column <- fold_column
    runCV <- TRUE
  } else if (missing(nfolds) && !missing(fold_column)) {
    params$fold_column <- fold_column
    runCV <- TRUE
  } else {
    runCV <- FALSE
  }

  if (is.DataStorageClass(train_data)) {
    OData_train <- train_data
  } else if (is.data.frame(train_data) || data.table::is.data.table(train_data)) {
    OData_train <- importData(data = train_data, ID = ID, t_name = t_name, covars = x, OUTCOME = y) ## Import input data into R6 object, define nodes
  } else {
    stop("train_data arg must be either data.frame, data.table or DataStorageClass object")
  }

  nodes <- OData_train$nodes ## Extract nodes list

  if (missing(x)) x <- nodes$Lnodes ## Assign predictors if missing (everything but ID and outcome in y)

  ## If fold_column specified in the model parameters, add it to the data object:
  if (!is.null(params$fold_column)) OData_train$fold_column <- params$fold_column
  ## Same for hold_column
  if (!is.null(params$hold_column)) OData_train$hold_column <- params$hold_column

  ## Define R6 object with validation data:
  if (!missing(valid_data)) {
    # OData_valid <- importData(data = valid_data, ID = ID, t_name = t_name, covars = x, OUTCOME = y)
    OData_valid <- OData_train$clone()
    OData_valid$dat.sVar <- data.table::data.table(valid_data)
  } else {
    OData_valid <- NULL
  }

  ## Define R6 regression class (specify subset_exprs to select only specific obs during fitting, e.g., only non-holdouts)
  regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = x, model_contrl = params)
  # regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = x, subset_exprs = list("!hold"), model_contrl = params)
  ## Define a modeling object, perform fitting (real data is being passed for the first time here):
  modelfit <- PredictionModel$new(reg = regobj)$fit(data = OData_train, validation_data = OData_valid)
  ## ------------------------------------------------------------------------------------------
  ## If validation data supplied, score the models based on validation set as well
  ## ------------------------------------------------------------------------------------------
  if (!is.null(OData_valid)) preds <- modelfit$predict(newdata = OData_valid)
  ## ------------------------------------------------------------------------------------------
  ## If CV was used, then score the models based on CV out-of-sample predictions
  ## ------------------------------------------------------------------------------------------
  if (runCV) {
    mse <- eval_MSE_CV(modelfit, yvals = OData_train$dat.sVar[[nodes$Ynode]])
    print("Mean cross-validated MSE by model: "); print(data.frame(mse))
  }
  return(modelfit)
}

# ---------------------------------------------------------------------------------------
#' Predict for new dataset
#'
#' @param modelfit Model fit object returned by \code{\link{fit_model}} function.
#' @param newdata Subject-specific data for which predictions should be obtained.
#' @param predict_only_bestK_models Specify the total number of top-ranked models (validation or C.V. MSE) for which predictions should be obtained.
#' Leave missing to obtain predictions for all models that were fit as part of this ensemble.
#' @param evalMSE Use newdata for model scoring (based on MSE)
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return A matrix of subject level predictions (subject are rows, columns are different models) or a data.table with subject level covariates added along with model-based predictions.
#' @export
predict_model <- function(modelfit, newdata, predict_only_bestK_models, evalMSE = FALSE, add_subject_data = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes

  if (!missing(newdata))
    newdata <- importData(data = newdata, ID = nodes$IDnode, t_name = nodes$tnode, covars = modelfit$predvars, OUTCOME = modelfit$outvar)

  if (!missing(predict_only_bestK_models)) {
    assert_that(is.integerish(predict_only_bestK_models))
    predict_model_names = modelfit$get_best_model_names(K = predict_only_bestK_models)
    message(paste0("obtaining predictions for the best models: ", paste0(predict_model_names, collapse = ",")))
    modelfit$predict(newdata = newdata, predict_model_names = predict_model_names, MSE = evalMSE)
  } else {
    message("obtaining predictions for all models...")
    modelfit$predict(newdata = newdata, MSE = evalMSE)
  }

  preds <- modelfit$getprobA1 # actual matrix of predictions needs to be extracted

  if (add_subject_data) {
    if (missing(newdata)) newdata <- modelfit$OData_train
    predsDT <- newdata$dat.sVar[, c(nodes$IDnode, modelfit$predvars), with = FALSE]
    predsDT[, (colnames(modelfit$getprobA1)) := as.data.table(modelfit$getprobA1)]
    preds <- predsDT
  }

  return(preds)
}

# ---------------------------------------------------------------------------------------
#' Evaluate cross-validated MSE
#'
#' @param modelfit Model fit object returned by \code{\link{fit_model}} function.
#' @param newdata ...
#' @param modelIDs ... Not implemented ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
eval_MSE_CV <- function(modelfit, newdata, yvals, verbose = getOption("growthcurveSL.verbose")) {
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes

  if (missing(newdata)) {
    modelfit <- modelfit$score_CV(yvals = yvals)
  } else {
    newOData <- importData(data = newdata, ID = nodes$IDnode, t_name = nodes$tnode, covars = modelfit$predvars, OUTCOME = modelfit$outvar)
    ## 2. Get predictions for each CV model based on external validation CV:
    t_CV <- system.time(
      modelfit <- modelfit$score_CV(validation_data = newOData, yvals = yvals)
    )
    print("t_CV: "); print(t_CV)
  }
  return(modelfit$getMSE)
}





