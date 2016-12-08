predict_h2o_new <- function(model_id, frame_id, returnVector = TRUE) {
  h2o.no_progress()
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

score_h2o_CV_models <- function(m.fit, validation_data, ...) {
  h2o.no_progress()

  if (missing(validation_data)) {
    ## Grab the internallly stored h2o cross-validation predictions (out of sample)
    pAoutMat <- sapply(m.fit$fitted_models_all, function(h2omodel) as.vector(h2o.cross_validation_holdout_predictions(h2omodel)))
    return(pAoutMat)

  } else {
    assert_that(data.table::is.data.table(validation_data))
    pAoutMat <- matrix(gvars$misval, nrow = nrow(validation_data), ncol = length(m.fit$fitted_models_all))
    colnames(pAoutMat) <- names(m.fit$model_ids)
    outvar <- m.fit$params$outvar
    predvars <- m.fit$params$predvars

    ## **** LOAD ALL DATA ONLY ONCE INTO h2o FRAME ****
    valid_H2Oframe <- fast.load.to.H2O(validation_data[, c(outvar, predvars), with = FALSE], destination_frame = "CV_valid_H2Oframe")

    ## Assumes folds were defined in exactly the same way across all models in fitted_models_all[] list
    h2o_model_1 <- m.fit$fitted_models_all[[1]]
    fold <- as.data.frame(h2o.cross_validation_fold_assignment(h2o_model_1))
    vfolds_cat <- sort(unique(fold$fold_assignment))

    for (vfold_idx in seq_along(vfolds_cat)) {
      fold_idx_cv.i <- which(fold$fold_assignment %in% vfolds_cat[vfold_idx])
      ## Define validation frame for this fold:
      subset_frame_t <- system.time(
        valid_H2Oframe_cv.i <- valid_H2Oframe[fold_idx_cv.i, ]
        # valid_H2Oframe_cv.i <- valid_H2Oframe[which(fold_idx_cv.i),]
        )
      print("subset_frame_t: "); print(subset_frame_t)

      cv.i_foldframeID <- h2o.getId(valid_H2Oframe_cv.i)

      dest_key_LIST <- vector(mode = "list", length = length(m.fit$fitted_models_all))

      for (idx in seq_along(m.fit$fitted_models_all)) {
        # print("idx: "); print(idx); print("model: "); print(names(m.fit$model_ids)[idx])
        h2o_model <- m.fit$fitted_models_all[[idx]]
        cv_models_IDs <- lapply(h2o_model@model$cross_validation_models, "[[", "name")

        ## Submit a job for prediction on a fold using internal REST API.
        ## Don't pull the prediction results until all of these jobs were submitted.
        url <- paste0('Predictions/models/', cv_models_IDs[[vfold_idx]], '/frames/',  cv.i_foldframeID)
        res <- h2o:::.h2o.__remoteSend(url, method = "POST", h2oRestApiVersion = 4)
        job_key <- res$key$name
        dest_key <- res$dest$name
        dest_key_LIST[[idx]] <- dest_key
      }

      h2o:::.h2o.__waitOnJob(job_key, pollInterval = 0.01)
      for (idx in seq_along(dest_key_LIST)) {
        newpreds <- h2o.getFrame(dest_key_LIST[[idx]])
        if ("p1" %in% colnames(newpreds)) {
          pAoutMat[fold_idx_cv.i, idx] <- as.vector(newpreds[,"p1"])
        } else {
          pAoutMat[fold_idx_cv.i, idx] <- as.vector(newpreds[,"predict"])
        }
      }
      ## remove the fold-specific frame from h2o cluster - this will cause a bug since it will remove parts of original frame valid_H2Oframe
      # h2o.rm(cv.i_foldframeID)
    }
    ## do not need to remove this manually, h2o will automatically clean this up when R's gc frees the variable
    # h2o.rm(valid_H2Oframe)
    return(pAoutMat)
  }
}

getPredictH2OFRAME <- function(m.fit, ParentObject, DataStorageObject, subset_idx) {
  assert_that(!is.null(subset_idx))
  if (!missing(DataStorageObject)) {
    # rows_subset <- which(subset_idx)
    data <- DataStorageObject
    outvar <- m.fit$params$outvar
    predvars <- m.fit$params$predvars
    prediction_H2Oframe <- fast.load.to.H2O(data$dat.sVar[subset_idx, c(outvar, predvars), with = FALSE], destination_frame = "prediction_H2Oframe")
  } else {
    prediction_H2Oframe <- h2o::h2o.getFrame(ParentObject$get_train_H2Oframe_ID)
  }
  return(prediction_H2Oframe)
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
fit.h2oglm <- function(fit.class, params, training_frame, y, x, model_contrl, validation_frame  = NULL, ...) {
# fit.h2oglm <- function(fit.class, fit, subsetH2Oframe, outvar, predvars, rows_subset, model_contrl, ...) {
  h2o.no_progress()
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
  if (!is.null(validation_frame)) {
    mainArgs$validation_frame <- validation_frame
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
fit.h2orandomForest <- function(fit.class, params, training_frame, y, x, model_contrl, validation_frame  = NULL, ...) {
  h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   ntrees = 100,
                   # balance_classes = TRUE,
                   ignore_const_cols = FALSE
                   )

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.randomForest)
  if (!is.null(validation_frame)) {
    mainArgs$validation_frame <- validation_frame
  }

  model.fit <- do.call(h2o::h2o.randomForest, mainArgs)

  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  return(create_fit_object(model.fit, model_alg = "randomForest", fitfunname = "h2o.randomForest", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
}

## S3 method for h2o gbm fit, takes BinDat data object:
## use "bernoulli" when doing classification and use "gaussian" when not
fit.h2ogbm <- function(fit.class, params, training_frame, y, x, model_contrl, validation_frame  = NULL, ...) {
  h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   # distribution = "bernoulli",
                   # distribution = "gaussian",
                   ntrees = 100,
                   # balance_classes = TRUE,
                   ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.gbm)
  if (!is.null(validation_frame)) {
    mainArgs$validation_frame <- validation_frame
  }
  model.fit <- do.call(h2o::h2o.gbm, mainArgs)
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  return(create_fit_object(model.fit, model_alg = "gbm", fitfunname = "h2o.gbm", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
}

## S3 method for h2o deeplearning fit, takes BinDat data object:
## use "bernoulli" when doing classification and use "gaussian" when doing regression
fit.h2odeeplearning <- function(fit.class, params, training_frame, y, x, model_contrl, validation_frame  = NULL, ...) {
  h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   # distribution = "bernoulli",
                   # distribution = "gaussian",
                   # balance_classes = TRUE,
                   ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.gbm)
  if (!is.null(validation_frame)) {
    mainArgs$validation_frame <- validation_frame
  }

  model.fit <- do.call(h2o::h2o.deeplearning, mainArgs)
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  return(create_fit_object(model.fit, model_alg = "deeplearning", fitfunname = "h2o.deeplearning", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
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

  pAoutMat <- matrix(gvars$misval, nrow = max(subset_idx), ncol = length(models_list))
  colnames(pAoutMat) <- names(models_list)

  if (length(subset_idx) > 0) {
    for (idx in seq_along(models_list)) {
      pAoutMat[subset_idx, idx] <- predict_h2o_new(models_list[[idx]]@model_id, frame_id = h2o.getId(H2Oframe))
      m.fit$fitted_models_all
    }
  }
  return(pAoutMat)
}

h2oGridModelClass  <- R6Class(classname = "h2oModelClass",
  inherit = h2oModelClass,
  cloneable = TRUE,
  portable = TRUE,
  class = TRUE,
  public = list(
    fit.class = c("GridLearner"),
    # Output info on the general type of regression being fitted:
    show = function(all_fits = FALSE, ...) {
      model.fit <- self$model.fit
      grid_objects <- self$model.fit$grid_objects
      top_grid_models <- self$model.fit$top_grid_models

      cat(" TOTAL NO. OF GRIDS: " %+% length(model.fit$grid_objects) %+% "\n")
      cat(" ======================= \n")

      for (grid_nm in names(grid_objects)) {
        print(grid_objects[[grid_nm]])
        cat("\n TOP MODEL FOR THIS GRID: \n")
        cat(" ======================= \n")
        print(top_grid_models[[grid_nm]])
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

    summary = function(all_fits = FALSE) {
      print("...")
      return(invisible(self))
    }
  )
)

# IMPLEMENTING NEW CLASS FOR BINARY REGRESSION THAT USES h2o
# NEEDS TO be able to pass on THE REGRESSION SETTINGS FOR h2o-specific functions
h2oModelClass  <- R6Class(classname = "h2oModelClass",
  cloneable = TRUE,
  portable = TRUE,
  class = TRUE,
  public = list(
    reg = NULL,
    params = list(),
    outvar = character(),
    predvars = character(),
    model_contrl = list(),
    classify = FALSE,
    fit.class = c("glm", "randomForest", "gbm", "deeplearning", "GridLearner"),
    # model.fit = list(coef = NA, fitfunname = NA, linkfun = NA, nobs = NA, params = NA, H2O.model.object = NA, model_algorithms = NA),
    model.fit = list(),
    outfactors = NA,
    nfolds = 5,

    initialize = function(fit.algorithm, fit.package, reg, ...) {
      self$reg <- reg
      self$params <- create_fit_params(reg)
      self$outvar <- reg$outvar
      self$predvars <- reg$predvars
      self$model_contrl <- reg$model_contrl

      assert_that("h2o" %in% fit.package)
      # if (fit.algorithm %in% "SuperLearner") {
      #   if (!"package:h2oEnsemble" %in% search()) stop("must load 'h2oEnsemble' package prior to using the SuperLearner: require('h2oEnsemble') or library('h2oEnsemble')")
      # }
      self$fit.class <- fit.algorithm
      class(self$fit.class) <- c(class(self$fit.class), "h2o" %+% self$fit.class)
      invisible(self)
    },

    # fit = function(data, outvar, predvars, subset_idx, validation_data = NULL, destination_frame, ...) {
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
                            model_contrl = self$model_contrl, fold_column = data$fold_column, validation_frame = valid_H2Oframe, ...),
                        silent = FALSE)

      if (inherits(self$model.fit, "try-error")) {
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }
      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx, predict_model_names) {
      P1 <- predictP1(self$model.fit, ParentObject = self, DataStorageObject = data, subset_idx = subset_idx, predict_model_names = predict_model_names)
      if (!is.matrix(P1)) {
        P1 <- matrix(P1, byrow = TRUE)
      }
      if (!missing(predict_model_names)) {
        colnames(P1) <- predict_model_names
      } else {
        colnames(P1) <- names(self$getmodel_ids)
      }
      return(P1)
    },

    score_CV = function(validation_data) {
      P1 <- score_h2o_CV_models(self$model.fit, validation_data)
      if (!is.matrix(P1)) {
        P1 <- matrix(P1, byrow = TRUE)
      }
      colnames(P1) <- names(self$getmodel_ids)
      return(P1)
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
      if (missing(subset_idx)) subset_idx <- 1:data$nobs

      load_var_names <- c(outvar, predvars)

      if (!is.null(data$fold_column)) load_var_names <- c(load_var_names, data$fold_column)
      if (!is.null(data$hold_column)) load_var_names <- c(load_var_names, data$hold_column)

      # 1. works on single core but fails in parallel:
      load_subset_t <- system.time(
        subsetH2Oframe <- fast.load.to.H2O(data$dat.sVar[subset_idx, load_var_names, with = FALSE], destination_frame = destination_frame)
      )
      if (gvars$verbose) {
        print("time to subset and load data into H2OFRAME: "); print(load_subset_t)
      }

      self$outfactors <- as.vector(h2o::h2o.unique(subsetH2Oframe[, outvar]))
      # Below being TRUE implies that the conversion to H2O.FRAME produced errors, since there should be no NAs in the source subset data
      if (any(is.na(self$outfactors))) stop("FOUND NA OUTCOMES IN H2OFRAME WHEN THERE WERE NOT SUPPOSED TO BE ANY")

      if (classify) {
        if (length(self$outfactors) > 2L) stop("cannot run binary regression/classification for outcome with more than 2 categories")
        subsetH2Oframe[, outvar] <- h2o::as.factor(subsetH2Oframe[, outvar])
      }
      return(subsetH2Oframe)
    },

    show = function(all_fits = FALSE, ...) {
      print(self$model.fit)
      return(invisible(NULL))
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
        return(assign_model_name_id(self$model.fit$H2O.model.object, self$model.fit$model_algorithms[[1]], self$model_contrl$name))
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

      for (i in seq.int(ncol(P1_residSL))) {
        P1_residSL[,i] <-  P1_residSL[,i] + P1_firstGLM[,1]
      }
      # P1 <- P1 + predict_h2o_new(self$firstGLMfit$fitted_models_all[[1]]@model_id, frame_id = h2o.getId(train_H2Oframe))
      # P1 <- P1 + predictP1(self$firstGLMfit, ParentObject = self, DataStorageObject = data, subset_idx = subset_idx)
      if (!missing(predict_model_names)) {
        colnames(P1_residSL) <- predict_model_names
      } else {
        colnames(P1_residSL) <- names(self$getmodel_ids)
      }
      return(P1_residSL)
    }
  )
)
