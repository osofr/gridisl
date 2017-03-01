#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggplot geom_point geom_errorbar theme_bw coord_flip aes position_dodge alpha
# @import ggiraph
# @importFrom ggiraph geom_point_interactive ggiraph
NULL

#' Get training data used by the modeling object
#'
#' Wrapper function for obtaining the training dataset saved in the modeling object.
#' @param modelfit A model object of class \code{PredictionModel} returned by function \code{fit_model} or \code{fit}.
#' @return \code{data.table} that was used for model training.
#' @export
get_train_data <- function(modelfit) {
  assert_that(is.PredictionModel(modelfit) || is.PredictionStack(modelfit))
  return(modelfit$OData_train$dat.sVar)
}
#' Get validation data used by the modeling object
#'
#' Wrapper function for obtaining the validation dataset saved in the modeling object.
#' @param modelfit A model object of class \code{PredictionModel} returned by functions \code{fit_model} or \code{fit}.
#' @return \code{data.table} that was used for model scoring (CV-MSE).
#' @export
get_validation_data <- function(modelfit) {
  assert_that(is.PredictionModel(modelfit) || is.PredictionStack(modelfit))
  return(modelfit$OData_valid$dat.sVar)
}

#' Get the combined out of sample predictions from V cross-validation models
#'
#' @param modelfit A model object of class \code{PredictionModel} returned by functions \code{fit_model} or \code{fit}.
#' @return A vector of out-of-sample predictions from the best selected model (CV-MSE).
#' @export
get_out_of_sample_predictions <- function(modelfit) {
  assert_that(is.PredictionModel(modelfit) || is.PredictionStack(modelfit))
  return(modelfit$get_out_of_sample_preds)
}

# ---------------------------------------------------------------------------------------
#' Generic modeling function for longitudinal data.
#'
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param train_data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param valid_data Optional \code{data.frame} or \code{data.table} containing the validation data.
#' When provided, this dataset will be used for scoring the final model fit(s). Can be used with either method = "cv" or "holdout".
#' When method = "cv", the validation data must have exactly the same number of rows as the \code{train_data}.
#' Each CV model will be re-scored (MSE) based on validation fold rows in \code{valid_data}.
#' @param models Parameters specifying the model(s) to fit. This must be a result of calling \code{defModel(...) + defModel(...)} functions.
#' @param nfolds Number of folds to use in cross-validation.
#' @param fold_column The name of the column in the input data that contains the cross-validation fold indicators (must be an ordered factor).
#' @param seed Random number seed for selecting random holdouts or validation folds.
#' @param useH2Oframe Use existing H2OFrame object (if modeling with h2o R package) in input data object, rather than loading a new H2OFrame.
#' @param subset_exprs (Optional) Specify a logical R expression (as character string) for selecting training / validation rows in the input data.
#' The expression will be evaluated in the environment of the input data.
#' By default all rows of the input data will be used.
#' @param subset_idx (Optional) Specify an vector index of rows in the input data to be used in model fitting / validation.
#' By default all rows of the input data will be used.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(gridisl.verbose=TRUE)}.
#' @return An R6 object containing the model fit(s).
# @seealso \code{\link{gridisl-package}} for the general overview of the package,
# @example tests/examples/1_gridisl_example.R
#' @export
fit_model <- function(ID,
                      t_name,
                      x,
                      y,
                      train_data,
                      valid_data,
                      models,
                      nfolds,
                      fold_column,
                      seed,
                      useH2Oframe = FALSE,
                      subset_exprs = NULL,
                      subset_idx = NULL,
                      verbose = getOption("gridisl.verbose")) {
  gvars$verbose <- verbose

  if (missing(train_data)) stop("train_data arg must be specified")

  if (!missing(nfolds) && missing(fold_column)) {
    runCV <- TRUE
    train_data <- add_CVfolds_ind(train_data, ID, nfolds = nfolds, fold_column = "fold", seed = seed)
    fold_column <- "fold"
    nfolds <- NULL
  } else if (missing(nfolds) && !missing(fold_column)) {
    runCV <- TRUE
    nfolds <- NULL
  } else {
    runCV <- FALSE
    fold_column <- NULL
    nfolds <- NULL
  }

  if (any(c("nfolds","fold_column") %in% names(models)))
    stop("Cannot have fields named 'nfolds' or 'fold_column' inside  models argument." %+%
         "To perform V fold cross-validation use the corresponding arguments 'nfolds' or 'fold_column' of this function.")

  train_data <- validate_convert_input_data(train_data, ID = ID, t_name = t_name, x = x, y = y, useH2Oframe = useH2Oframe, dest_frame = "all_train_H2Oframe")

  # CheckVarNameExists(train_data$dat.sVar, y)

  nodes <- train_data$nodes ## Extract nodes list

  if (missing(x)) x <- nodes$Lnodes ## Assign predictors if missing (everything, but ID and outcome in y)

  ## If fold_column specified in the model parameters, add it to the data object:
  if (!is.null(fold_column)) train_data$fold_column <- fold_column

  ## Define R6 object with validation data:
  if (!missing(valid_data)) {
    valid_data <- validate_convert_input_data(valid_data, ID = ID, t_name = t_name, x = x, y = y, useH2Oframe = useH2Oframe, dest_frame = "all_valid_H2Oframe")

    # CheckVarNameExists(valid_data$dat.sVar, y)

  } else {
    valid_data <- NULL
  }

  if (!is.null(subset_exprs)) {
    # ... Check that the subset_exprs is a valid expression ...
  }

  if (!is.null(subset_idx))
    if (!is.integer(subset_idx)) stop("subset_idx must be an integer vector, current class: " %+% class(subset_idx))

  modelfits <- vector(mode = "list", length = length(models))
  for (learner_idx in seq_along(models)) {
    model <- models[[learner_idx]]
    estimator <- model[["estimator"]]
    model[["estimator"]] <- NULL

    ## Define R6 regression class (specify subset_exprs to select only specific obs during fitting, e.g., only non-holdouts)
    regobj <- RegressionClass$new(Model_idx = learner_idx, outvar = y, predvars = x, runCV = runCV,
                                  subset_exprs = subset_exprs,
                                  fold_column = fold_column,
                                  estimator = estimator,
                                  model_contrl = model)

    ## Define a modeling object, perform fitting (real data is being passed for the first time here):
    modelfit <- PredictionModel$new(reg = regobj, useH2Oframe = useH2Oframe)
    modelfit <- modelfit$fit(data = train_data, validation_data = valid_data, subset_exprs = subset_idx)
    modelfits[[learner_idx]] <- modelfit
  }

  modelfit_stack <- do.call(make_PredictionStack, modelfits)
  modelfit_stack$runCV <- runCV
  modelfit_stack$useH2Oframe <- useH2Oframe
  modelfit_stack$OData_train <- train_data
  modelfit_stack$OData_valid <- valid_data

  if (!is.null(valid_data)) {
    ## If validation data supplied then score the models based on validation set
    modelfit_stack$score_models(validation_data = valid_data, subset_exprs = subset_idx)
  } else if (runCV) {
    ## If CV was used and no validation data provided,
    ## then score the models based on pre-saved out-of-sample predictions.
    ## CV model predictions from validation folds.
    modelfit_stack$score_models(subset_exprs = subset_idx)
  }

  if ((!is.null(valid_data) || runCV) && verbose) {
    print("Internally evaluated holdout / CV metrics: "); print(modelfit_stack$getMSEtab)
  }

  return(modelfit_stack)
}

# ---------------------------------------------------------------------------------------
#' Generic SuperLearner prediction function
#'
#' @param modelfit Model fit object returned by \code{\link{fit}} functions. Must be an object of class \code{PredictionModel} or \code{PredictionStack}.
#' @param newdata Subject-specific data for which predictions should be obtained.
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param subset_idx A vector of row indices in \code{newdata} for which the predictions should be obtain.
#' Default is \code{NULL} in which case all observations in \code{newdata} will be used for prediction.
#' @param best_only Set to \code{TRUE} (default) to obtain predictions from the top-ranked model (based on validation or CV MSE).
#' When \code{FALSE} the attempt will to made to obtain predictions from all models.
#' Note that when \code{holdout} is \code{FALSE} and \code{best_only} is \code{TRUE},
#' the predictions will be based on the best scoring model that was re-trained on all available data.
#' @param holdout Set to \code{TRUE} for out-of-sample predictions for validation folds or holdouts.
#' @param force_data.table Force the prediction result to be \code{data.table}.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console.
#' Turn this on by default using \code{options(gridisl.verbose=TRUE)}.
#' @return A data.table of subject level predictions (subject are rows, columns are different models)
#' or a data.table with subject level covariates added along with model-based predictions.
#' @export
predict_generic <- function(modelfit,
                            newdata,
                            add_subject_data = FALSE,
                            subset_idx = NULL,
                            best_only = TRUE,
                            holdout = FALSE,
                            force_data.table = TRUE,
                            verbose = getOption("gridisl.verbose")) {

  if (is.null(modelfit)) stop("must fit the model prior to obtaining predictions")
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit) || is.PredictionStack(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes

  if (!is.null(subset_idx))
    if (!is.integer(subset_idx)) stop("subset_idx must be an integer vector, current class: " %+% class(subset_idx))

  if (!missing(newdata))
    newdata <- validate_convert_input_data(newdata, ID = nodes$IDnode, t_name = nodes$tnode,
                                           x = modelfit$predvars, y = modelfit$outvar,
                                           useH2Oframe = modelfit$useH2Oframe, dest_frame = "prediction_H2Oframe")

  if (!best_only && verbose) message("obtaining predictions for all models...")

  if (!holdout) {
    preds <- modelfit$predict(newdata, subset_exprs = subset_idx, best_only = best_only, convertResToDT = force_data.table)
  } else {
    preds <- modelfit$predict_out_of_sample(newdata, subset_exprs = subset_idx, best_only = best_only, convertResToDT = force_data.table)
  }

  if (force_data.table) {
    preds <- as.data.table(preds)
    if (best_only) names(preds)[1] <- "preds"
    # if (!holdout && best_only) names(preds)[1] <- "SL.preds"
    # if (holdout && best_only) names(preds)[1] <- "holdout.preds"

    if (add_subject_data && !missing(newdata)) {
      # if (missing(newdata)) newdata <- modelfit$OData_train
      covars <- c(nodes$IDnode, nodes$tnode, modelfit$outvar)
      ## to protect against an error if some variables are dropped from new data
      sel_covars <- names(newdata$dat.sVar)[names(newdata$dat.sVar) %in% covars]
      predsDT <- newdata$dat.sVar[, sel_covars, with = FALSE]
      if (!is.null(subset_idx)) predsDT <- predsDT[subset_idx, ]
      predsDT[, (colnames(preds)) := preds]
      preds <- predsDT
    }
  }

  return(preds)
}


#' @export
predict_SL <- function(modelfit, newdata, ...) { UseMethod("predict_SL") }

# ---------------------------------------------------------------------------------------
#' Predict from SuperLearner fit
#'
#' @param modelfit Model fit object returned by \code{\link{fit}}.
#' @param newdata Subject-specific data for which predictions should be obtained.
#' If missing then the predictions for the training data will be typically returned.
#' See \code{holdout} for discussion of alternative cases.
#' @param add_subject_data Set to \code{TRUE} to add the subject-level data to the resulting predictions (returned as a data.table).
#' When \code{FALSE} (default) only the actual predictions are returned (as a matrix with each column representing predictions from a specific model).
#' @param subset_idx A vector of row indices in \code{newdata} for which the predictions should be obtain.
#' Default is \code{NULL} in which case all observations in \code{newdata} will be used for prediction.
# @param best_refit_only Set to \code{TRUE} to obtained the predictions from the best scoring model that was re-trained on all observed data.
#' @param holdout Set to \code{TRUE} for out-of-sample predictions for validation folds (out-of-sample observations) or holdouts.
#'  When \code{newdata} is missing there are two possible types of holdout predictions, depending on the modeling approach.
#'  1. For \code{method = "holdout"} the default holdout predictions will be based on validation data.
#'  2. For \code{method = "cv"} the default is to leave use the previous out-of-sample (holdout) predictions from the training data.
# @param force_data.table Force the output predictions to be a \code{data.table}
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(gridisl.verbose=TRUE)}.
#' @return A data.table of subject level predictions (subject are rows, columns are different models)
#' or a data.table with subject level covariates added along with model-based predictions.
#' @export
predict_SL.PredictionStack <- function(modelfit,
                       newdata,
                       add_subject_data = FALSE,
                       subset_idx = NULL,
                       holdout = FALSE,
                       verbose = getOption("gridisl.verbose")) {

  force_data.table <- TRUE

  if (is.null(modelfit)) stop("must call fit() before obtaining predictions")
  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  assert_that(is.PredictionModel(modelfit) || is.PredictionStack(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes

  if (missing(newdata) && holdout && !modelfit$runCV) {
    ## For holdout predictions with holdoutSL the default is to use the previous validation data
    newdata <- modelfit$OData_valid
  } else if (missing(newdata)) {
    ## For all other cases, the default is to use the training data
    newdata <- modelfit$OData_train
  }

  preds <- predict_generic(modelfit,
                           newdata,
                           add_subject_data,
                           subset_idx,
                           best_only = TRUE,
                           holdout,
                           force_data.table,
                           verbose)

  return(preds)
}


# ---------------------------------------------------------------------------------------
# Predict for new dataset for models trained on non-holdouts only
predict_nonholdouts <- function(modelfit,
                                newdata,
                                best_only = TRUE,
                                add_subject_data = FALSE,
                                subset_idx = NULL,
                                verbose = getOption("gridisl.verbose")) {
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes
  if (!missing(newdata))
    newdata <- validate_convert_input_data(newdata, ID = nodes$IDnode, t_name = nodes$tnode,
                                           x = modelfit$predvars, y = modelfit$outvar,
                                           useH2Oframe = modelfit$useH2Oframe, dest_frame = "prediction_H2Oframe")
  if (!best_only && verbose) message("obtaining predictions for all models...")
  preds <- modelfit$predict_within_sample(newdata, subset_exprs = subset_idx, best_only = best_only, convertResToDT = TRUE)

  preds <- data.table::as.data.table(preds)
  if (best_only) names(preds)[1] <- "nonholdout.preds"
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
# Out-of-sample predictions for validation folds or holdouts.
# When \code{newdata} is missing there are two possible types of holdout predictions, depending on the modeling approach.
# 1. For \code{fit_holdoutSL} the default holdout predictions will be based on validation data.
# 2. For \code{fit_cvSL} the default is to leave use the previous out-of-sample (holdout) predictions from the training data.
predict_holdout <- function(modelfit,
                            newdata,
                            best_only = TRUE,
                            add_subject_data = FALSE,
                            subset_idx = NULL,
                            verbose = getOption("gridisl.verbose")) {
  if (missing(newdata) && !modelfit$runCV) newdata <- modelfit$OData_valid
  return(predict_generic(modelfit,
                         newdata,
                         add_subject_data,
                         subset_idx,
                         best_only = best_only,
                         holdout = TRUE,
                         force_data.table = TRUE,
                         verbose))
}

# ---------------------------------------------------------------------------------------
#' Evaluate MSE based on holdout/validation predictions
#'
#' By default this function will extract out-of-sample/validation/holdout predictions
#' from original training data (automatically done by h2o) to evaluate the cross-validated MSE.
#' However, when \code{newdata} is supplied, the predictions for each CV model will
#' be based on this external validation dataset.
#' These predictions and the outcome stored in \code{newdata} are then used to re-evalute the CV MSE.
#' Note that \code{newdata} must be of the same
#' dimensionality as the original training data used for fitting the h2o models.
#' @param modelfit Model fit object returned by \code{\link{fit_model}} function.
#' @param newdata Optional new validation data for evaluating MSE, either a \code{data.table} or \code{DataStorageClass} object.
#' @param subset_idx Optional row indices if MSE needs to be evaluating for a subset of the input data.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(gridisl.verbose=TRUE)}.
#' @return A list of MSEs by model.
#' @export
eval_MSE <- function(modelfit,
                     newdata,
                     subset_idx = NULL,
                     verbose = getOption("gridisl.verbose")) {

  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  assert_that(is.PredictionModel(modelfit) || is.PredictionStack(modelfit))
  gvars$verbose <- verbose
  nodes <- modelfit$OData_train$nodes

  if (!is.null(subset_idx))
    if (!is.integer(subset_idx)) stop("subset_idx must be an integer vector, current class: " %+% class(subset_idx))

  if (missing(newdata)) {
    ## Use out-of-sample predictions from original training data (automatically saved by h2o) to evaluate the CV MSE
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