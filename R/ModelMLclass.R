predict_h2o_new <- function(model_id, frame_id, returnVector = TRUE) {
  # h2o.no_progress()
  # waitOnJob = FALSE,
  url <- paste0('Predictions/models/', model_id, '/frames/',  frame_id)
  res <- h2o:::.h2o.__remoteSend(url, method = "POST", h2oRestApiVersion = 4)
  job_key <- res$key$name
  dest_key <- res$dest$name

  h2o:::.h2o.__waitOnJob(job_key, pollInterval = 0.01)
  newpreds <- h2o.getFrame(dest_key)

  if (returnVector) {
    if ("p1" %in% colnames(newpreds)) {
      return(as.vector(newpreds[,"p1"]))
    } else {
      return(as.vector(newpreds[,"predict"]))
    }
  } else {
    return(newpreds)
  }
}

## ----------------------------------------------------------------
## Obtain h2oFrame to be used for prediction
## ----------------------------------------------------------------
getPredictH2OFRAME <- function(m.fit, ParentObject, DataStorageObject, subset_idx) {
  # assert_that(!is.null(subset_idx))
  if (missing(DataStorageObject)) {

    return(h2o::h2o.getFrame(ParentObject$get_train_H2Oframe_ID))

  } else {

    data <- DataStorageObject

    if (ParentObject$useH2Oframe) {

      prediction_H2Oframe <- data$H2Oframe[subset_idx, ]

    } else {

      newdat <- data$dat.sVar[subset_idx, m.fit$params$predvars, with = FALSE]
      fold_column <- ParentObject$fold_column
      if (!is.null(fold_column)) {
        if (fold_column %in% names(data$dat.sVar)) {
          newdat[, (fold_column) := data$dat.sVar[subset_idx, fold_column, with = FALSE]]
        } else {
          ####### Temporary fix to avoid the current bug in h2o  ######
          # newdat[, (fold_column) := factor(sample(c(1:10), nrow(newdat), TRUE))]
        }
      }
      prediction_H2Oframe <- fast.load.to.H2O(newdat, destination_frame = "prediction_H2Oframe")

    }
    return(prediction_H2Oframe)
  }
}

check_out_of_sample_consistency <- function(models_list, valid_H2Oframe, predvars, fold_column) {
  all_folds_h2o <- lapply(models_list, h2o.cross_validation_fold_assignment)
  train_frame_ID_1 <- models_list[[1]]@parameters$training_frame

  if (length(all_folds_h2o) > 1) {
    ## 1. Test that the exactly the same fold assignments were used by all CV models in the ensemble.
    for (idx in 2:length(all_folds_h2o) ) {
      if (!h2o.all(all_folds_h2o[[1]]==all_folds_h2o[[idx]])  )
        stop("Out-of-sample (holdout) predictions for new data has failed. The fold assignmets of the following CV model do not match to others: " %+% names(models_list)[idx])
    }

    ## 2. Test that same training h2oFrame was used for all models in the ensemble (just in case).
    for (idx in 2:length(all_folds_h2o) ) {
      if (!all.equal(train_frame_ID_1, models_list[[idx]]@parameters$training_frame))
        stop("Out-of-sample (holdout) predictions for new data has failed. It appears that some of the CV models in ensemble used different training frames.")
    }
  }

  ## 3. Test that the validation and training data have exactly the same fold assignments (in h2oFrame)
  if (!all(valid_H2Oframe[[fold_column]] == all_folds_h2o[[1]]))
    stop("Out-of-sample (holdout) predictions for new data has failed. The fold assignments in new data (validation_data) and training data appear to be different.")

  ## 4a. Test that the new validation data (in h2oFrame) has the same number of observations as the training data
  if (!(nrow(valid_H2Oframe) == nrow(h2o.getFrame(train_frame_ID_1))))
    stop("Out-of-sample (holdout) predictions for new data has failed. The number of rows in new data (validation_data) does not match that of the training data.")

  ## 4b. Test that all predictors are present in the validation data (in h2oFrame)
  if (!all(c(predvars,fold_column) %in% colnames(valid_H2Oframe)))
    stop("Out-of-sample (holdout) predictions for new data has failed. Some of the predictors were not found in new data (validation_data).")
  return(invisible(TRUE))
}

## ----------------------------------------------------------------------------------------------------------------------------------
## Evaluate out-of-sample predictions from V cross-validation models, based on new validation_data.
## Can be useful for re-scoring the models when the validation data has to change from the training data in V-fold cross-validation.
## Will only perform the out-sample predictions for each model V_i
## (i.e., predictions in validation_data will be only made for rows that were not used for training the model V_i)
## In the end we generate a vector of n=nrow(validation_data) predictions by combining predictions from all models V=(V_1,...,V_v)
## This procedure is repeated for each cross-validated model in the ensemble, resulting in a matrix of predictions (n,k),
## where k is the total number of models trained by this ensemble (with h2o.grid, etc) and is equal to length(models_list)
## ----------------------------------------------------------------------------------------------------------------------------------
predict_out_of_sample_cv <- function(m.fit, ParentObject, validation_data, subset_idx, predict_model_names, ...) {
  # h2o.no_progress()
  models_list <- m.fit$fitted_models_all
  if (!missing(predict_model_names)) models_list <- models_list[predict_model_names]

  ## Grab the internallly stored h2o out of sample predictions for each CV model (cross-validation predictions are combined into a single vector of length n)
  if (missing(validation_data)) {

    message("Obtaining the out-of-sample CV predictions for h2o-stored training data")
    # pAoutDT <- sapply(m.fit$fitted_models_all, function(h2omodel) as.vector(h2o.cross_validation_holdout_predictions(h2omodel)))
    pAoutDT <- lapply(models_list, function(h2omodel) h2o.cross_validation_holdout_predictions(h2omodel))
    pAoutDT <- as.data.table(h2o.cbind(pAoutDT))
    setnames(pAoutDT, names(models_list))
    return(pAoutDT)

  } else {

    outvar <- m.fit$params$outvar
    predvars <- m.fit$params$predvars
    fold_column <- ParentObject$fold_column

    ## **** Allows re-using existing h2oFrame is it was already pre-loaded in validation_data object ****
    valid_H2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, validation_data, subset_idx)

    message("Obtaining the out-of-sample CV predictions for new data")
    res <- check_out_of_sample_consistency(models_list, valid_H2Oframe, predvars, fold_column)

    ## Get the fold assignments for the 1st model in ensemble:
    h2o_model_1 <- models_list[[1]]
    fold_h2o <- h2o.cross_validation_fold_assignment(h2o_model_1)
    vfolds_cat_h2o <- sort(h2o.levels(fold_h2o)) # # vfolds_ncat_h2o <- h2o.nlevels(fold_h2o)

    pAoutMat_h2o <- NULL
    CV_loop_t <- system.time({
    for (vfold_idx in seq_along(vfolds_cat_h2o)) {

      message("Obtaining out-of-sample CV predictions for all models and validation fold: " %+% vfolds_cat_h2o[vfold_idx])
      fold_CV_i_logical <- fold_h2o == vfolds_cat_h2o[vfold_idx]

      ## Define validation frame for this fold:
      valid_H2Oframe_CV.i <- valid_H2Oframe[fold_CV_i_logical, ]
      cv.i_foldframeID <- h2o.getId(valid_H2Oframe_CV.i)

      dest_key_LIST <- vector(mode = "list", length = length(models_list))

      for (idx in seq_along(models_list)) {
        # h2o.predict(h2o.getModel(cv_models_IDs[[vfold_idx]]), newdata = h2o.getFrame(cv.i_foldframeID))
        # print("idx: "); print(idx); print("model: "); print(names(models_list)[idx])
        h2o_model <- models_list[[idx]]
        cv_models_IDs <- lapply(h2o_model@model$cross_validation_models, "[[", "name")
        ## Submit a job for prediction on a fold using internal REST API.
        ## Don't pull the prediction results until all of these jobs were submitted.
        url <- paste0('Predictions/models/', cv_models_IDs[[vfold_idx]], '/frames/',  cv.i_foldframeID)
        res <- h2o:::.h2o.__remoteSend(url, method = "POST", h2oRestApiVersion = 4)
        job_key <- res$key$name
        dest_key <- res$dest$name
        dest_key_LIST[[idx]] <- dest_key
      }

      newpreds_prev_CV_i <- NULL
      for (idx in seq_along(dest_key_LIST)) {
        newpreds <- h2o.getFrame(dest_key_LIST[[idx]])
        newpreds_prev_CV_i <- h2o.cbind(newpreds_prev_CV_i, newpreds)
      }
      newpreds_prev_CV_i <- h2o.cbind(h2o.which(fold_CV_i_logical), newpreds_prev_CV_i)
      pAoutMat_h2o <- h2o.rbind(pAoutMat_h2o, newpreds_prev_CV_i)
    }

    pAoutMat_h2o <- h2o.arrange(pAoutMat_h2o, "C1")
    pAoutMat_h2o <- pAoutMat_h2o[, 2:ncol(pAoutMat_h2o)]
    })

    print("CV_loop_t"); print(CV_loop_t)

    pAoutDT <- as.data.table(pAoutMat_h2o)
    setnames(pAoutDT, names(models_list))

    return(pAoutDT)
  }
}

## ----------------------------------------------------------------
## Prediction for h2ofit objects, predicts P(A = 1 | newXmat)
## ----------------------------------------------------------------
predictP1.H2Omodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
  return(predictP1.H2Ogridmodel(m.fit, ParentObject, DataStorageObject, subset_idx, ...))
}
predictP1.H2Ogridmodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict_model_names, ...) {
  H2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, DataStorageObject, subset_idx)
  models_list <- m.fit$fitted_models_all
  if (!missing(predict_model_names)) models_list <- models_list[predict_model_names]

  # pAoutMat <- matrix(gvars$misval, nrow = nrow(H2Oframe), ncol = length(models_list))
  # colnames(pAoutMat) <- names(models_list)

  pAoutDT <- rep.int(list(numeric()), length(models_list))
  names(pAoutDT) <- names(models_list)
  pAoutDT <- as.data.table(pAoutDT)

  if (nrow(H2Oframe) > 0) {
    pAout_h2o <- NULL
    for (idx in seq_along(models_list)) {
      # pAoutMat[, idx] <- predict_h2o_new(models_list[[idx]]@model_id, frame_id = h2o.getId(H2Oframe))
      pAout_h2o <- h2o.cbind(pAout_h2o,
                             predict_h2o_new(models_list[[idx]]@model_id, frame_id = h2o.getId(H2Oframe), returnVector = FALSE))
    }
    pAoutDT <- as.data.table(pAout_h2o)
    setnames(pAoutDT, names(models_list))
  }
  return(pAoutDT)
}

## take a list of args, take a function body and return only the args that belong to function signature
keep_only_fun_args <- function(Args, fun) {
  keepArgs <- intersect(names(Args), names(formals(fun))) # captures optional arguments given by user
  if (length(keepArgs) > 0) {
    Args <- Args[keepArgs]
  } else {
    Args <- NULL
  }
  return(Args)
}

## 1. replace any arg in mainArgs if it also appears in userArgs
## 2. add any arg from userArgs that also appears in formals(fun) of function
replace_add_user_args <- function(mainArgs, userArgs, fun) {
  replaceArgs <- intersect(names(mainArgs), names(userArgs)) # captures main arguments that were overridden by user
  if(length(replaceArgs) > 0) {
    mainArgs[replaceArgs] <- userArgs[replaceArgs]
    userArgs[replaceArgs] <- NULL
  }
  newArgs <- intersect(names(formals(fun)), names(userArgs)) # captures any additional args given by user that are not in mainArgs
  if (length(newArgs) > 0) {
    mainArgs <- c(mainArgs, userArgs[newArgs])
  }
  return(mainArgs)
}

## ---------------------------------------------------------------------------
## for running logistic regression with continuous outcome range [0-1]
## ---------------------------------------------------------------------------
## S3 method for fitting h2o GLM with binomial() family (logistic regression):
## use solver="L_BFGS" when doing classification and use "IRLSM" when not
fit.h2oglm <- function(fit.class, params, training_frame, y, x, fold_column, model_contrl, validation_frame  = NULL, ...) {
  # h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                  intercept = TRUE,
                  # family = "gaussian",
                  standardize = TRUE,
                  # standardize = FALSE,
                  # solver = "L_BFGS",
                  # solver = "IRLSM",
                  # solver = "COORDINATE_DESCENT",
                  # solver = "COORDINATE_DESCENT_NAIVE",
                  lambda = 0L,
                  max_iterations = 100,
                  ignore_const_cols = FALSE,
                  missing_values_handling = "Skip")

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.glm)

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs$validation_frame <- validation_frame

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }

  # h2o::h2o.glm(x = x, y = y, training_frame = training_frame, lambda = 0L, family = "gaussian")
  model.fit <- do.call(h2o::h2o.glm, mainArgs)

  # assign the fitted coefficients in correct order (same as predictor order in x)
  out_coef <- vector(mode = "numeric", length = length(x)+1)
  out_coef[] <- NA
  names(out_coef) <- c("Intercept", x)
  out_coef[names(model.fit@model$coefficients)] <- model.fit@model$coefficients
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]

  return(create_fit_object(model.fit, model_alg = "glm", fitfunname = "h2o.glm", params = params, coef = out_coef, nobs = nobs, model_contrl = model_contrl))
}

## S3 method for h2o randomForest fit (Random Forest):
fit.h2orandomForest <- function(fit.class, params, training_frame, y, x, fold_column, model_contrl, validation_frame  = NULL, ...) {
  # h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   ntrees = 100,
                   # balance_classes = TRUE,
                   ignore_const_cols = FALSE
                   )

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.randomForest)

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs$validation_frame <- validation_frame

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }

  model.fit <- do.call(h2o::h2o.randomForest, mainArgs)

  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  return(create_fit_object(model.fit, model_alg = "randomForest", fitfunname = "h2o.randomForest", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
}

## S3 method for h2o gbm fit, takes BinDat data object:
## use "bernoulli" when doing classification and use "gaussian" when not
fit.h2ogbm <- function(fit.class, params, training_frame, y, x, fold_column, model_contrl, validation_frame  = NULL, ...) {
  # h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   # distribution = "bernoulli",
                   # distribution = "gaussian",
                   ntrees = 100,
                   # balance_classes = TRUE,
                   ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.gbm)

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs$validation_frame <- validation_frame

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }

  model.fit <- do.call(h2o::h2o.gbm, mainArgs)
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  return(create_fit_object(model.fit, model_alg = "gbm", fitfunname = "h2o.gbm", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
}

## S3 method for h2o deeplearning fit, takes BinDat data object:
## use "bernoulli" when doing classification and use "gaussian" when doing regression
fit.h2odeeplearning <- function(fit.class, params, training_frame, y, x, fold_column, model_contrl, validation_frame  = NULL, ...) {
  # h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   # distribution = "bernoulli",
                   # distribution = "gaussian",
                   # balance_classes = TRUE,
                   ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.gbm)

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs$validation_frame <- validation_frame

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }

  model.fit <- do.call(h2o::h2o.deeplearning, mainArgs)
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  return(create_fit_object(model.fit, model_alg = "deeplearning", fitfunname = "h2o.deeplearning", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
}

h2oModelClass  <- R6Class(classname = "h2oModelClass",
  cloneable = TRUE,
  portable = TRUE,
  class = TRUE,
  public = list(
    reg = NULL,
    params = list(),
    outvar = character(),
    predvars = character(),
    runCV = logical(),
    fold_column = character(),
    model_contrl = list(),
    classify = FALSE,
    fit.class = c("glm", "randomForest", "gbm", "deeplearning", "GridLearner"),
    model.fit = list(),
    outfactors = NA,

    useH2Oframe = FALSE,

    initialize = function(fit.algorithm, fit.package, reg, useH2Oframe = FALSE, ...) {
      self$reg <- reg
      self$params <- create_fit_params(reg)
      self$outvar <- reg$outvar
      self$predvars <- reg$predvars
      self$runCV <- reg$runCV
      self$fold_column <- reg$fold_column
      self$model_contrl <- reg$model_contrl

      self$useH2Oframe <- useH2Oframe
      assert_that("h2o" %in% fit.package)

      self$fit.class <- fit.algorithm
      class(self$fit.class) <- c(class(self$fit.class), "h2o" %+% self$fit.class)
      invisible(self)
    },

    fit = function(data, subset_idx, validation_data = NULL, destination_frame, ...) {
      assert_that(is.DataStorageClass(data))
      if (missing(destination_frame)) destination_frame <- "train_H2Oframe"
      train_H2Oframe <- self$setdata(data, subset_idx, self$classify, destination_frame = destination_frame, ...)
      private$train_H2Oframe <- train_H2Oframe
      private$train_H2Oframe_ID <- h2o::h2o.getId(train_H2Oframe)

      if ((length(self$predvars) == 0L) || (length(subset_idx) == 0L) || (length(self$outfactors) < 2L)) {
        message("unable to run " %+% self$fit.class %+% " with h2o for: intercept only models or designmat with zero rows or  constant outcome (y) ...")
        class(self$model.fit) <- "try-error"
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }

      if (!is.null(validation_data)) {
        assert_that(is.DataStorageClass(validation_data))
        valid_H2Oframe <- self$setdata(validation_data, classify = self$classify, destination_frame = "valid_H2Oframe", ...)
        private$valid_H2Oframe <- valid_H2Oframe
        private$valid_H2Oframe_ID <- h2o::h2o.getId(valid_H2Oframe)

      } else {
        valid_H2Oframe = NULL
      }

      self$model.fit <- try(fit(self$fit.class, self$params, training_frame = train_H2Oframe, y = self$outvar, x = self$predvars,
                                model_contrl = self$model_contrl, fold_column = self$fold_column, validation_frame = valid_H2Oframe, ...),
                          silent = FALSE)

      if (inherits(self$model.fit, "try-error")) {
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }
      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx, predict_model_names) {
      P1_DT <- predictP1(self$model.fit, ParentObject = self, DataStorageObject = data, subset_idx = subset_idx, predict_model_names = predict_model_names)
      return(P1_DT)
    },

    # score_CV = function(validation_data) {
    predictP1_out_of_sample_cv = function(validation_data, subset_idx, predict_model_names) {
      P1_DT <- predict_out_of_sample_cv(self$model.fit, ParentObject = self, validation_data = validation_data, subset_idx = subset_idx, predict_model_names = predict_model_names)
      return(P1_DT)
    },

    getmodel_byname = function(model_names, model_IDs) {
      if (!missing(model_names)) {
        return(self$model.fit$fitted_models_all[model_names])
      } else {
        if (missing(model_IDs)) stop("Must provide either 'model_names' or 'model_IDs'.")
        return(lapply(model_IDs, h2o::h2o.getModel))
      }
    },

    get_best_model_params = function(model_names) {
      # model_obj <- self$model.fit$H2O.model.object
      model_obj <- self$getmodel_byname(model_names[1])[[1]]

      top_params <- list(fit.package = self$model_contrl$fit.package,
                         fit.algorithm = model_obj@algorithm)

      if (top_params$fit.algorithm %in% "drf") top_params$fit.algorithm <- "randomForest"

      # top_params_tmp <- model_obj@parameters ## only the user-set parameters are stored here
      top_params_tmp <- model_obj@allparameters ## alternative is to grab the exact params used in the model, including defaults
      top_params_tmp$model_id <- NULL
      top_params_tmp$training_frame <- NULL
      top_params_tmp$validation_frame <- NULL
      top_params_tmp$x <- NULL
      top_params_tmp$y <- NULL
      top_params_tmp$nfolds <- NULL
      top_params_tmp$fold_column <- NULL
      top_params_tmp$fold_assignment <- NULL

      top_params_tmp$score_each_iteration <- NULL # deeplearning fails otherwise
      top_params <- c(top_params, top_params_tmp)

      ## don't use all the rest of the parameters in model control that are specific to grid search:
      # best_params <- self$model_contrl
      # best_params$fit.algorithm <- model_obj@algorithm
      # top_params <- c(best_params, top_params)

      return(top_params)
    },

    setdata = function(data, subset_idx, classify = FALSE, destination_frame = "newH2Osubset", ...) {
      outvar <- self$outvar
      predvars <- self$predvars

      if (self$useH2Oframe) {

        if (missing(subset_idx)) {
          subsetH2Oframe <- data$H2Oframe
        } else {
          subset_t <- system.time(subsetH2Oframe <- data$H2Oframe[subset_idx, ])
          # if (gvars$verbose) {
          print("time to subset H2OFRAME: "); print(subset_t)
          # }
        }

      } else {

        load_var_names <- c(outvar, predvars)
        if (!is.null(data$fold_column)) load_var_names <- c(load_var_names, data$fold_column)
        if (!is.null(data$hold_column)) load_var_names <- c(load_var_names, data$hold_column)
        if (missing(subset_idx)) subset_idx <- (1:data$nobs)
        load_subset_t <- system.time(subsetH2Oframe <- fast.load.to.H2O(data$dat.sVar[subset_idx, load_var_names, with = FALSE], destination_frame = destination_frame))
        # if (gvars$verbose) {
          print("time to subset and load data into H2OFRAME: "); print(load_subset_t)
        # }
      }

      self$outfactors <- as.vector(h2o::h2o.unique(subsetH2Oframe[, outvar]))
      # Below being TRUE implies that the conversion to H2O.FRAME produced errors, since there should be no NAs in the source subset data
      if (any(is.na(self$outfactors))) stop("Found NA outcomes in h2oframe when there were not supposed to be any")
      if (classify && length(self$outfactors) > 2L) stop("Cannot run binary regression/classification for outcome with more than 2 categories")

      if (classify) subsetH2Oframe[, outvar] <- h2o::as.factor(subsetH2Oframe[, outvar])

      return(subsetH2Oframe)
    },

    show = function(all_fits = FALSE, ...) {
      model.fit <- self$model.fit
      grid_objects <- self$model.fit$grid_objects
      top_grid_models <- self$model.fit$top_grid_models

      if (!is.null(grid_objects)) {
        cat(" TOTAL NO. OF GRIDS: " %+% length(grid_objects) %+% "\n")
        cat(" ======================= \n")
        for (grid_nm in names(grid_objects)) {
          print(grid_objects[[grid_nm]])
          cat("\n TOP MODEL FOR THIS GRID: \n")
          cat(" ======================= \n")
          print(top_grid_models[[grid_nm]])
        }
      }

      if (all_fits) {
        cat("\n...Printing the summary fits of all models contained in this ensemble...\n")
        cat("==========================================================================\n")
        for (idx in seq_along(model.fit$fitted_models_all)) {
          cat("Model No. " %+% idx %+% "; ")
          print(model.fit$fitted_models_all[[idx]])
        }
      }
      return(invisible(NULL))
    },

    summary = function(all_fits = FALSE, ...) {
      print("...")
      return(invisible(self))
    }
  ),

  active = list( # 2 types of active bindings (w and wout args)
    emptydata = function() { },

    get_train_H2Oframe = function() {private$train_H2Oframe},
    get_train_H2Oframe_ID = function() {private$train_H2Oframe_ID},

    get_valid_H2Oframe = function() {private$valid_H2Oframe},
    get_valid_H2Oframe_ID = function() {private$valid_H2Oframe_ID},

    getmodel_ids = function() {
      if (is.null(self$model.fit$model_ids)) {
        return(assign_model_name_id(self$model.fit$fitted_models_all[[1]], self$model.fit$model_algorithms[[1]], self$model_contrl$name))
        # model_ids <- list(self$model.fit$H2O.model.object@model_id)
        # new_names <- self$model.fit$model_algorithms[[1]]
        # if (!is.null(self$model_contrl$name)) new_names <- new_names %+% "." %+% self$model_contrl$name
        # names(model_ids) <- new_names
        # return(model_ids)
      } else {
        return(self$model.fit$model_ids)
      }
    },

    getmodel_algorithms = function() { self$model.fit$model_algorithms }
  ),

  private = list(
    train_H2Oframe = NULL,
    train_H2Oframe_ID = NULL,
    valid_H2Oframe = NULL,
    valid_H2Oframe_ID = NULL
  )
)

h2oResidualModelClass  <- R6Class(classname = "h2oResidualModelClass",
  inherit = h2oModelClass,
  cloneable = TRUE,
  portable = TRUE,
  class = TRUE,
  public = list(
    firstGLMfit = NULL,

    # fit = function(data, outvar, predvars, subset_idx, validation_data = NULL, destination_frame, ...) {
    fit = function(data, subset_idx, validation_data = NULL, destination_frame, ...) {
      assert_that(is.DataStorageClass(data))
      nodes <- data$nodes

      if (missing(destination_frame)) destination_frame <- "train_H2Oframe"
      train_H2Oframe <- self$setdata(data, subset_idx, self$classify, destination_frame = destination_frame, ...)
      private$train_H2Oframe <- train_H2Oframe
      private$train_H2Oframe_ID <- h2o::h2o.getId(train_H2Oframe)

      if ((length(self$predvars) == 0L) || (length(subset_idx) == 0L) || (length(self$outfactors) < 2L)) {
        message("unable to run " %+% self$fit.class %+% " with h2o for: intercept only models or designmat with zero rows or  constant outcome (y) ...")
        class(self$model.fit) <- "try-error"
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }

      if (!is.null(validation_data)) {
        assert_that(is.DataStorageClass(validation_data))
        valid_H2Oframe <- self$setdata(validation_data, classify = self$classify, destination_frame = "valid_H2Oframe", ...)
        private$valid_H2Oframe <- valid_H2Oframe
        private$valid_H2Oframe_ID <- h2o::h2o.getId(valid_H2Oframe)
      } else {
        valid_H2Oframe = NULL
      }

      ## ------------------------------------------------------------------------------------------
      ## PART I. Fit the univariate glm on training set. Define new outcome as a residual of glm predictions for entire data (train + validation)
      ## ------------------------------------------------------------------------------------------
      ## will be made into a passable user-defined argument:
      firstGLM_params <- list(fit.package = "h2o", fit.algorithm = "glm", family = self$model_contrl$family)

      ## Option A (low level -- going directly for the residual glm fit)
      ## (x might be allowed later to include add'l user-def covars)
      firstGLMfit_class <- "glm"
      class(firstGLMfit_class) <- c(class(firstGLMfit_class), "h2o" %+% firstGLMfit_class)
      self$firstGLMfit <- try(fit(firstGLMfit_class, self$params, training_frame = train_H2Oframe, y = self$outvar, x = nodes$tnode,
                                  model_contrl = firstGLM_params, ...),
                              silent = FALSE)

      GLMmodelID <- self$firstGLMfit$fitted_models_all[[1]]@model_id
      firstGLM_preds_train <- predict_h2o_new(GLMmodelID, frame_id = h2o.getId(train_H2Oframe), returnVector = FALSE)
      firstGLM_preds_valid <- predict_h2o_new(GLMmodelID, frame_id = h2o.getId(valid_H2Oframe), returnVector = FALSE)

      ## save predictions from the first model (fit on training data, but predictions made for all data)
      train_H2Oframe[["firstGLM_preds"]] <- firstGLM_preds_train
      train_H2Oframe[["residual_y"]] <- train_H2Oframe[[self$outvar]] - train_H2Oframe[["firstGLM_preds"]]

      ## evaluate residuals and define them as new outcomes (to be used as outcomes for the next stage SL)
      if (!is.null(validation_data)) {
        valid_H2Oframe[["firstGLM_preds"]] <- firstGLM_preds_valid
        valid_H2Oframe[["residual_y"]] <- valid_H2Oframe[[self$outvar]] - valid_H2Oframe[["firstGLM_preds"]]
      }

      ## Option B (high level -- instantiating a daughter class that will be responsible for creating residual glm fits)
      # regFirstGLM <- RegressionClass$new(outvar = nodes$Ynode, predvars = nodes$tnode, model_contrl = firstGLM_params)
      # self$firstGLMfit <- h2oModelClass$new(fit.algorithm = "glm", fit.package = "h2o", reg = regFirstGLM, ...)$fit(data = data)
      # firstGLM_preds_train <- self$firstGLMfit$predict(newdata = data, MSE = FALSE)$getprobA1
      # firstGLM_preds_valid <- self$firstGLMfit$predict(newdata = validation_data, MSE = FALSE)$getprobA1

      # train_data[, ("firstGLM_preds") := predict_model(modelfit = modelfit_firstGLM, newdata = train_data, evalMSE = FALSE)]
      # valid_data[, ("firstGLM_preds") := predict_model(modelfit = modelfit_firstGLM, newdata = valid_data, evalMSE = FALSE)]
      # train_data[, ("residual_y") := (eval(as.name(y)) - firstGLM_preds)]
      # valid_data[, ("residual_y") := (eval(as.name(y)) - firstGLM_preds)]

      ## ------------------------------------------------------------------------------------------
      ## PART II. Fit the h2o grid or single h2o learner on training data with residual predictions as new outcomes
      ## ------------------------------------------------------------------------------------------
      y <- "residual_y" ## redefine the outcome
      mainfit_class <- "GridLearner"
      class(mainfit_class) <- c(class(mainfit_class), "h2o" %+% mainfit_class)
      self$model.fit <- try(fit(mainfit_class, self$params, training_frame = train_H2Oframe, y = "residual_y", x = self$predvars,
                            model_contrl = self$model_contrl, fold_column = data$fold_column, validation_frame = valid_H2Oframe, ...),
                        silent = FALSE)

      if (inherits(self$model.fit, "try-error")) {
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }
      return(self$model.fit)
    },

    ## PREDICTION IS MODIFIED INTO 2 STAGES (firstGLM prediction + second stage residual predictions for each learner)
    predictP1 = function(data, subset_idx, predict_model_names) {
      P1_firstGLM <- predictP1(self$firstGLMfit, ParentObject = self, DataStorageObject = data, subset_idx = subset_idx)
      P1_residSL <- predictP1(self$model.fit, ParentObject = self, DataStorageObject = data, subset_idx = subset_idx, predict_model_names = predict_model_names)

      if (ncol(P1_firstGLM) > 1) stop("initial glm fit for residuals must provide predictions with one column matrix only")
      for (model.fit in names(P1_residSL)) {
        P1_residSL[, (model.fit) := eval(as.name(model.fit)) + P1_firstGLM[[1]]]
      }
      # if (!missing(predict_model_names)) {
      #   colnames(P1_residSL) <- predict_model_names
      # } else {
      #   colnames(P1_residSL) <- names(self$getmodel_ids)
      # }
      return(P1_residSL)
    }
  )
)
