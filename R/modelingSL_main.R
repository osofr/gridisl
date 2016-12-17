#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggplot geom_point geom_errorbar theme_bw coord_flip aes position_dodge alpha
#' @import ggiraph
# @importFrom ggiraph geom_point_interactive ggiraph
NULL

#' Get training data used by the modeling object
#'
#' Wrapper function for obtaining the training dataset saved in the modeling object.
#' @param modelfit A model object of class \code{PredictionModel} returned by functions \code{fit_model}, \code{fit_holdoutSL} or \code{fit_cvSL}.
#' @return \code{data.table} that was used for model training.
#' @export
get_train_data <- function(modelfit) {
  assert_that(is.PredictionModel(modelfit))
  return(modelfit$OData_train$dat.sVar)
}
#' Get validation data used by the modeling object
#'
#' Wrapper function for obtaining the validation dataset saved in the modeling object.
#' @param modelfit A model object of class \code{PredictionModel} returned by functions \code{fit_model}, \code{fit_holdoutSL} or \code{fit_cvSL}.
#' @return \code{data.table} that was used for model scoring (CV-MSE).
#' @export
get_validation_data <- function(modelfit) {
  assert_that(is.PredictionModel(modelfit))
  return(modelfit$OData_valid$dat.sVar)
}

#' Save the best performing h2o model
#'
#' @param modelfit A model object of class \code{PredictionModel} returned by functions \code{fit_model}, \code{fit_holdoutSL} or \code{fit_cvSL}.
#' @export
save_best_h2o_model <- function(modelfit, file.path = getOption('growthcurveSL.file.path')) {
  assert_that(is.PredictionModel(modelfit))
  best_model_name <- modelfit$get_best_model_names(K = 1)
  message("saving the best model fit: " %+% best_model_name)
  ## Will obtain the best model object trained on TRAINING data only
  ## If CV SL was used this model is equivalent to the best model trained on all data
  ## However, for holdout SL this model will be trained only on non-holdout observations
  best_model_traindat <- modelfit$get_best_models(K = 1)[[1]]
  h2o.saveModel(best_model_traindat, file.path, force = TRUE)
  ## This model is always trained on all data (if exists)
  best_model_alldat <- modelfit$BestModelFitObject$model.fit$fitted_models_all
  if (!is.null(best_model_alldat))
    h2o.saveModel(best_model_alldat[[1]], file.path, force = TRUE)

  return(invisible(NULL))
}

validate_convert_input_data <- function(input_data, ID, t_name, x, y, useH2Oframe = FALSE, dest_frame = "all_train_H2Oframe") {
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
#' @param useH2Oframe ...
#' @param subset_exprs ...
#' @param subset_idx ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
fit_model <- function(ID, t_name, x, y, train_data, valid_data, params, nfolds, fold_column, seed,
                      useH2Oframe = FALSE, subset_exprs = NULL, subset_idx = NULL,
                      verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose

  if (missing(train_data)) stop("train_data arg must be specified")

  if (!missing(nfolds) && missing(fold_column)) {
    runCV <- TRUE
    train_data <- add_CVfolds_ind(train_data, ID, nfolds = nfolds, fold_column = "fold", seed = seed)
    fold_column <- "fold"
    nfolds <- NULL
    # params$fold_column <- fold_column
  } else if (missing(nfolds) && !missing(fold_column)) {
    runCV <- TRUE
    # params$fold_column <- fold_column
    nfolds <- NULL
  } else {
    runCV <- FALSE
    fold_column <- NULL
    nfolds <- NULL
  }

  if (any(c("nfolds","fold_column") %in% names(params))) stop("Cannot have fields named 'nfolds' or 'fold_column' inside  params argument." %+%
                                                              "To perform V fold cross-validation use the corresponding arguments 'nfolds' or 'fold_column' of this function.")

  train_data <- validate_convert_input_data(train_data, ID = ID, t_name = t_name, x = x, y = y, useH2Oframe = useH2Oframe, dest_frame = "all_train_H2Oframe")
  CheckVarNameExists(train_data$dat.sVar, y)

  nodes <- train_data$nodes ## Extract nodes list

  if (missing(x)) x <- nodes$Lnodes ## Assign predictors if missing (everything, but ID and outcome in y)

  ## If fold_column specified in the model parameters, add it to the data object:
  if (!is.null(fold_column)) train_data$fold_column <- fold_column

  ## Define R6 object with validation data:
  if (!missing(valid_data)) {
    valid_data <- validate_convert_input_data(valid_data, ID = ID, t_name = t_name, x = x, y = y, useH2Oframe = useH2Oframe, dest_frame = "all_valid_H2Oframe")
    CheckVarNameExists(valid_data$dat.sVar, y)
  } else {
    valid_data <- NULL
  }

  if (!is.null(subset_exprs)) {
    # ... Check that the subset_exprs is a valid expression ...
  }
  if (!is.null(subset_idx))
    if (!is.integer(subset_idx)) stop("subset_idx must be an integer vector, current class: " %+% class(subset_idx))

  ## Define R6 regression class (specify subset_exprs to select only specific obs during fitting, e.g., only non-holdouts)
  regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = x, model_contrl = params, runCV = runCV, subset_exprs = subset_exprs, fold_column = fold_column)
  # regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = x, subset_exprs = list("!hold"), model_contrl = params)
  ## Define a modeling object, perform fitting (real data is being passed for the first time here):
  modelfit <- PredictionModel$new(reg = regobj, useH2Oframe = useH2Oframe)$fit(data = train_data, validation_data = valid_data, subset_exprs = subset_idx)

  ## ------------------------------------------------------------------------------------------
  ## If validation data supplied then score the models based on validation set
  ## ------------------------------------------------------------------------------------------
  if (!missing(valid_data) && !is.null(valid_data)) {
    # ************** NEED TO MODIFY THIS TO EXPLICIT FUNCTION CALL THAT WOULD EVALUATE MSE FOR HOLDOUT **************
    # preds <- modelfit$predict(newdata = valid_data)
    modelfit$score_models(validation_data = valid_data, subset_exprs = subset_idx)
  }

  ## ------------------------------------------------------------------------------------------
  ## If CV was used, then score the models based on out-of-sample predictions on the training data (automatically done by h2o)
  ## ------------------------------------------------------------------------------------------
  if (runCV) {
    mse <- modelfit$score_models(subset_exprs = subset_idx)$getMSE
    # modelfit$getMSE
    # mse <- eval_MSE_cv(modelfit)
    print("Mean CV MSE (for out of sample predictions) as evaluated by h2o: "); print(data.frame(mse))
  }
  return(modelfit)
}

.predict_generic <- function(modelfit, newdata, predict_only_bestK_models, add_subject_data = FALSE,
                             subset_idx = NULL, pred_holdout = FALSE,
                             verbose = getOption("growthcurveSL.verbose")) {
  if (is.null(modelfit)) stop("must call fit_holdoutSL() or fit_cvSL() prior to obtaining predictions")
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes

  if (!is.null(subset_idx))
    if (!is.integer(subset_idx)) stop("subset_idx must be an integer vector, current class: " %+% class(subset_idx))

  if (!missing(newdata))
    newdata <- validate_convert_input_data(newdata, ID = nodes$IDnode, t_name = nodes$tnode,
                                           x = modelfit$predvars, y = modelfit$outvar,
                                           useH2Oframe = modelfit$useH2Oframe, dest_frame = "prediction_H2Oframe")

  if (!missing(predict_only_bestK_models)) {
    assert_that(is.integerish(predict_only_bestK_models))
    predict_model_names = modelfit$get_best_model_names(K = predict_only_bestK_models)
    message(paste0("obtaining predictions for the best models: ", paste0(predict_model_names, collapse = ",")))
  } else {
    predict_model_names <- NULL
    message("obtaining predictions for all models...")
  }

  if (!pred_holdout) {
    preds <- modelfit$predict(newdata = newdata, subset_exprs = subset_idx, predict_model_names = predict_model_names)
  } else {
    preds <- modelfit$predict_out_of_sample(newdata = newdata, subset_exprs = subset_idx, predict_model_names = predict_model_names)
  }

  preds <- as.data.table(preds)

  if (add_subject_data) {
    if (missing(newdata)) newdata <- modelfit$OData_train
    covars <- c(nodes$IDnode, nodes$tnode, modelfit$outvar)
    ## to protect against an error if some variables are dropped from new data
    sel_covars <- names(newdata$dat.sVar)[names(newdata$dat.sVar) %in% covars]
    predsDT <- newdata$dat.sVar[, sel_covars, with = FALSE]
    if (!is.null(subset_idx)) predsDT <- predsDT[subset_idx, ]
    predsDT[, (colnames(preds)) := preds]
    preds <- predsDT
  }

  return(preds)
}

# ---------------------------------------------------------------------------------------
#' Predict for new dataset
#'
#' @param modelfit Model fit object returned by \code{\link{fit_model}} function.
#' @param newdata Subject-specific data for which predictions should be obtained.
#' @param predict_only_bestK_models Specify the total number of top-ranked models (validation or C.V. MSE) for which predictions should be obtained.
#' Leave missing to obtain predictions for all models that were fit as part of this ensemble.
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param subset_idx ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console.
#' Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return A matrix of subject level predictions (subject are rows, columns are different models)
#' or a data.table with subject level covariates added along with model-based predictions.
#' @export
predict_model <- function(modelfit, newdata, predict_only_bestK_models, add_subject_data = FALSE,
                          subset_idx = NULL,
                          verbose = getOption("growthcurveSL.verbose")) {
  return(.predict_generic(modelfit, newdata, predict_only_bestK_models, add_subject_data, subset_idx, pred_holdout = FALSE, verbose))
}

# ---------------------------------------------------------------------------------------
#' Prediction for holdout (out-of-sample) model
#'
#' When \code{newdata} is missing there are two possible types of holdout predictions, depending on the modeling approach.
#' 1. For \code{fit_holdoutSL} the default holdout predictions will be based on validation data.
#' 2. For \code{fit_cvSL} the default is to leave use the previous out-of-sample (holdout) predictions from the training data.
#' @param modelfit Model fit object returned by \code{\link{fit_holdoutSL}} or \code{\link{fit_cvSL}} functions.
#' @param newdata Subject-specific data for which holdout (out-of-sample) predictions should be obtained.
#' @param predict_only_bestK_models Specify the total number of top-ranked models (validation or C.V. MSE) for which predictions should be obtained.
#' Leave missing to obtain predictions for all models that were fit as part of this ensemble.
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param subset_idx ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
predict_holdout <- function(modelfit, newdata, predict_only_bestK_models, add_subject_data = FALSE,
                            subset_idx = NULL,
                            verbose = getOption("growthcurveSL.verbose")) {
  if (missing(newdata) && !modelfit$runCV) newdata <- modelfit$OData_valid
  return(.predict_generic(modelfit, newdata, predict_only_bestK_models, add_subject_data, subset_idx, pred_holdout = TRUE, verbose))
}

# ---------------------------------------------------------------------------------------
#' Predict for holdout or curve SuperLearner fits
#'
#' @param modelfit Model fit object returned by \code{\link{fit_holdoutSL}} or  \code{\link{fit_cvSL}}.
#' @param newdata Subject-specific data for which predictions should be obtained.
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
predict_SL <- function(modelfit, newdata, add_subject_data = FALSE, grid = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  if (is.null(modelfit)) stop("must call fit_holdoutSL() or fit_cvSL() prior to obtaining predictions")
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes

  if (missing(newdata)) {
    newdata <- modelfit$OData_train$dat.sVar
  } else if (!grid) {
    newdata <- define_features_drop(newdata, ID = nodes$IDnode, t_name = nodes$tnode, y = nodes$Ynode, train_set = TRUE)
  } else {
    ## in the future might define the grid data internally, right now it is assumed the user correctly defines the grid data
  }

  ## will use the best model retrained on all data for prediction:
  modelfit$use_best_retrained_model <- TRUE
  best_fit_name <- names(modelfit$getRetrainedfit$model_ids)
  preds <- predict_model(modelfit = modelfit, newdata = newdata, add_subject_data = add_subject_data)
  data.table::setnames(preds, old = best_fit_name, new = "SL.preds")
  modelfit$use_best_retrained_model <- FALSE

  ## will obtain predictions for all models in the ensemble that were trained on non-holdout observations only:
  # preds2 <- predict_model(modelfit = modelfit, newdata = newdata, add_subject_data = add_subject_data)

  return(preds)
}

#' Get the combined out of sample predictions from V cross-validation models
#'
#' @param modelfit A model object of class \code{PredictionModel} returned by functions \code{fit_model} or \code{fit_cvSL}.
#' @return A vector of out-of-sample predictions from the best selected model (CV-MSE).
#' @export
get_out_of_sample_predictions <- function(modelfit) {
  assert_that(is.PredictionModel(modelfit))
  return(modelfit$get_out_of_sample_preds)
}

# ---------------------------------------------------------------------------------------
#' Evaluate MSE for model fits, possibly using new data
#'
#' By default this function will extract out-of-sample predictions from original training data (automatically done by h2o) to evaluate the cross-validated MSE.
#' However, when \code{newdata} is supplied, the predictions for each CV model will be based on this external validation dataset.
#' These predictions and the outcome stored in \code{newdata} are then used to re-evalute the CV MSE. Note that \code{newdata} must be of the same
#' dimensionality as the original training data used for fitting the h2o models.
#' @param modelfit Model fit object returned by \code{\link{fit_model}} function.
#' @param newdata ...
#' @param subset_idx ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
eval_MSE <- function(modelfit, newdata, subset_idx = NULL, verbose = getOption("growthcurveSL.verbose")) {
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes

  if (!is.null(subset_idx))
    if (!is.integer(subset_idx)) stop("subset_idx must be an integer vector, current class: " %+% class(subset_idx))

  if (missing(newdata)) {
    ## Use out-of-sample predictions from original training data (automatically done by h2o) to evaluate the CV MSE
    modelfit <- modelfit$score_models(subset_exprs = subset_idx)
  } else {
    newdata <- validate_convert_input_data(newdata, ID = nodes$IDnode, t_name = nodes$tnode,
                                           x = modelfit$predvars, y = modelfit$outvar,
                                           useH2Oframe = modelfit$useH2Oframe, dest_frame = "prediction_H2Oframe")
    ## Get predictions for each CV model based on external validation CV dataset, then use those predictions and outcome in newdata to evalute the CV MSE
    modelfit <- modelfit$score_models(validation_data = newdata, subset_exprs = subset_idx)
  }
  return(modelfit$getMSE)
}