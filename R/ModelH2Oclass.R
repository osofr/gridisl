predict_h2o_new <- function(model_id, frame_id, convertResToDT = TRUE) {
  # predframe <- h2o::h2o.getFrame(frame_id)
  # h2omodel <- h2o::h2o.getModel(model_id)
  # str(h2omodel)
  # predict(h2omodel, predframe)
  # browser()

  # h2o::h2o.no_progress()
  # waitOnJob = FALSE,
  url <- paste0('Predictions/models/', model_id, '/frames/',  frame_id)
  res <- h2o:::.h2o.__remoteSend(url, method = "POST", h2oRestApiVersion = 4)
  job_key <- res$key$name
  dest_key <- res$dest$name

  h2o:::.h2o.__waitOnJob(job_key, pollInterval = 0.01)

  newpreds <- h2o::h2o.getFrame(dest_key)
  if (ncol(newpreds) > 1) newpreds <- newpreds[["p1"]]

  if (convertResToDT) {
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

## ----------------------------------------------------------------
## Prediction for h2ofit objects, predicts P(A = 1 | newXmat)
## ----------------------------------------------------------------
predictP1.H2Omodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
  return(predictP1.H2Ogrid(m.fit, ParentObject, DataStorageObject, subset_idx, ...))
}

predictP1.H2Ogrid <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict_model_names, ...) {
  H2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, DataStorageObject, subset_idx)

  models_list <- m.fit$modelfits_all
  if (!missing(predict_model_names) && !is.null(predict_model_names)) models_list <- models_list[predict_model_names]

  pAout_h2o <- rep.int(list(numeric()), length(models_list))
  names(pAout_h2o) <- names(models_list)
  pAout_h2o <- as.data.table(pAout_h2o)

  if (nrow(H2Oframe) > 0) {
    h2o::h2o.no_progress()
    old.exprs <- getOption("expressions")
    if (length(models_list) >= 400) options("expressions" = length(models_list)*50)
    pAout_h2o <- NULL

    for (idx in seq_along(models_list)) {
      # res <- lapply(models_list, function(model) predict_h2o_new(model@model_id, frame_id = h2o::h2o.getId(H2Oframe), convertResToDT = FALSE)[["predict"]])
      # pAout_h2o <- h2o::h2o.cbind(res)
      # browser()
      # predict(models_list[[idx]], H2Oframe[1:5, ])
      # predict(models_list[[idx]], H2Oframe[100, ])
      # predict(models_list[[idx]], H2Oframe[95, ])

      # str(models_list[[idx]])
      # alldata_H2Oframe
      # predict(models_list[[idx]], h2o.getFrame("alldata_H2Oframe"))
      # predict(models_list[[idx]], H2Oframe)
      # h2o.unique(H2Oframe[, "CVD"])
      # h2o.unique(H2Oframe[, "highA1c"])
      # h2o.unique(H2Oframe[, "N.tminus1"])
      # H2Oframe[1, "N.tminus1"] <- 1
      # H2Oframe[1, "TI"] <- 1
      # H2Oframe[, "TI"] <- 0
      # as.h2o(as.data.frame(cbind(H2Oframe, fold_ID = 0)))
      # predict(models_list[[idx]], as.h2o(as.data.frame(H2Oframe[4:10,]))[, c("CVD", "highA1c")])

      pAout_h2o <- h2o::h2o.cbind(pAout_h2o,
                        predict_h2o_new(models_list[[idx]]@model_id, frame_id = h2o::h2o.getId(H2Oframe), convertResToDT = FALSE)) # [["predict"]]
    }

    options("expressions" = old.exprs)
    h2o::h2o.show_progress()
    names(pAout_h2o) <- names(models_list)
  }
  return(pAout_h2o)
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
    fit.class = c("glm", "randomForest", "gbm", "deeplearning", "grid"),
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

      if (inherits(connectH2O <- try(h2o::h2o.getConnection(), silent = TRUE), "try-error")) {
        if (gvars$verbose) message("No active connection to an H2O cluster has been detected.
Will now attempt to initialize a local h2o cluster.
In the future, please run `h2o::h2o.init(nthreads=...)` prior to model training with h2o.")
        h2o::h2o.init(nthreads=2)
      }

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
        message("...unable to run " %+% self$fit.class %+% " with h2o.grid for either: 1) intercept only models or 2) training data with zero rows or 3) constant outcome (y) ...")
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

      self$model.fit <- try(fit(self$fit.class,
                                self$params,
                                training_frame = train_H2Oframe,
                                y = self$outvar,
                                x = self$predvars,
                                model_contrl = self$model_contrl,
                                fold_column = self$fold_column,
                                validation_frame = valid_H2Oframe, ...),
                          silent = FALSE)

    if (inherits(self$model.fit, "try-error"))
      message("All grid models for h2o.grid algorithm " %+% self$model_contrl[["grid.algorithm"]] %+% " have failed.
This can be either a result of an error in model parameters or the outcome variable type not matching the distribution family.
See https://0xdata.atlassian.net/browse/TN-2 for more info.
If the algorithm requested was different from 'glm', the next step will attempt to run h2o.glm as a backup, otherwise will return an error.")

      ## On first failure, try h2o.glm. This provides a chance to evalute the CV MSE (if using CV).
      ## This way other models in the ensemble that did not fail might still be selected.
      if (inherits(self$model.fit, "try-error") && (!self$model_contrl[["grid.algorithm"]] %in% "glm")) {
        self$model_contrl[["grid.algorithm"]] <- "glm"
        self$model.fit <- try(fit(self$fit.class,
                                  self$params,
                                  training_frame = train_H2Oframe,
                                  y = self$outvar,
                                  x = self$predvars,
                                  model_contrl = self$model_contrl,
                                  fold_column = self$fold_column,
                                  validation_frame = valid_H2Oframe, ...),
                          silent = FALSE)

        if (!inherits(self$model.fit, "try-error") && gvars$verbose) message("...h2o.glm backup has succeeded after the initial failed run of h2o.grid...")
      }

      ## When everything fails, clean up and return
      if (inherits(self$model.fit, "try-error")) {
        self$emptydata
        self$emptyY
      }

      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx, predict_model_names) {
      P1_DT <- predictP1(self$model.fit,
                         ParentObject = self,
                         DataStorageObject = data,
                         subset_idx = subset_idx,
                         predict_model_names = predict_model_names)
                         # convertResToDT = convertResToDT)
      # if (convertResToDT) P1_DT <- as.data.table(P1_DT)
      return(P1_DT)
    },

    # score_CV = function(validation_data) {
    predictP1_out_of_sample_cv = function(validation_data, subset_idx, predict_model_names) {
      P1_DT <- predict_out_of_sample_cv(self$model.fit,
                                        ParentObject = self,
                                        validation_data = validation_data,
                                        subset_idx = subset_idx,
                                        predict_model_names = predict_model_names)
                                        # convertResToDT = convertResToDT)
      # if (convertResToDT) P1_DT <- as.data.table(P1_DT)
      return(P1_DT)
    },

    getmodel_byname = function(model_names, model_IDs) {
      if (!missing(model_names)) {
        return(self$model.fit$modelfits_all[model_names])
      } else {
        if (missing(model_IDs)) stop("Must provide either 'model_names' or 'model_IDs'.")
        return(lapply(model_IDs, h2o::h2o.getModel))
      }
    },

    get_modelfits_grid = function(model_names, ...) {
      # if (!missing(model_names)) {
      #   Model_idx <- (self$model.fit$modelfits_grid[["model_names"]] %in% model_names)
      #   return(self$model.fit$modelfits_grid[Model_idx, ])
      # } else {
        return(self$model.fit$modelfits_grid)
        # if (missing(model_IDs)) stop("Must provide either 'model_names' or 'model_IDs'.")
        # return(lapply(model_IDs, h2o::h2o.getModel))
      # }
    },

    get_best_model_params = function(model_names) {
      # str(model_obj)
      # str(model_obj@model)
      # model_obj <- self$model.fit$H2O.model.object
      model_obj <- self$getmodel_byname(model_names[1])[[1]]

      top_params <- list(fit.package = self$model_contrl$fit.package,
                         fit.algorithm = model_obj@algorithm)

      if (top_params$fit.algorithm %in% "drf") top_params$fit.algorithm <- "randomForest"

      # top_params_tmp <- model_obj@parameters ## only the user-set parameters are stored here
      top_params_tmp <- model_obj@allparameters ## alternative is to grab the exact params used in the model, including defaults
      top_params_tmp$model_id <- NULL
      top_params_tmp$r2_stopping <- NULL
      top_params_tmp$training_frame <- NULL
      top_params_tmp$validation_frame <- NULL
      top_params_tmp$x <- NULL
      top_params_tmp$y <- NULL
      top_params_tmp$nfolds <- NULL
      top_params_tmp$fold_column <- NULL
      top_params_tmp$fold_assignment <- NULL

      if (!is.null(top_params_tmp[["lambda_search"]]) && top_params_tmp[["lambda_search"]]) {
        top_params_tmp[["lambda_search"]] <- FALSE
        top_params_tmp[["nlambdas"]] <- NULL
        top_params_tmp[["lambda_min_ratio"]] <- NULL
        top_params_tmp[["lambda"]] <- model_obj@model$lambda_best
      }

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
          if (gvars$verbose == 2) { print("time to subset H2OFRAME: "); print(subset_t) }
        }

      } else {

        load_var_names <- c(outvar, predvars)
        if (!is.null(data$fold_column)) load_var_names <- c(load_var_names, data$fold_column)

        # if (missing(subset_idx)) subset_idx <- (1:data$nobs)
        if (missing(subset_idx)) subset_idx <- TRUE

        subsetH2Oframe <- fast.load.to.H2O(data$get.dat.sVar(subset_idx, covars = load_var_names), destination_frame = destination_frame)
        # subsetH2Oframe <- fast.load.to.H2O(data$dat.sVar[subset_idx, load_var_names, with = FALSE], destination_frame = destination_frame)
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
        for (idx in seq_along(model.fit$modelfits_all)) {
          cat("Model No. " %+% idx %+% "; ")
          print(model.fit$modelfits_all[[idx]])
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
    wipe.allmodels = function() { },

    get_train_H2Oframe = function() {private$train_H2Oframe},
    get_train_H2Oframe_ID = function() {private$train_H2Oframe_ID},

    get_valid_H2Oframe = function() {private$valid_H2Oframe},
    get_valid_H2Oframe_ID = function() {private$valid_H2Oframe_ID},

    getmodel_ids = function() {
      if (is.null(self$model.fit$model_ids)) {
        return(assign_model_name_id(params = self$params, self$model.fit$modelfits_all[[1]], self$model.fit$model_algorithms[[1]], self$model_contrl$name))
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
        message("...unable to run " %+% self$fit.class %+% " with h2o.grid for either: 1) intercept only models or 2) training data with zero rows or 3) constant outcome (y) ...")
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
      ## Will be made into a passable user-defined argument:
      firstGLM_params <- list(fit.package = "h2o", fit.algorithm = "glm", family = self$model_contrl$family)

      ## Option A (low level -- going directly for the residual glm fit)
      ## (x might be allowed later to include add'l user-def covars)
      firstGLMfit_class <- "glm"
      class(firstGLMfit_class) <- c(class(firstGLMfit_class), "h2o" %+% firstGLMfit_class)
      self$firstGLMfit <- try(fit(firstGLMfit_class, self$params, training_frame = train_H2Oframe, y = self$outvar, x = nodes$tnode,
                                  model_contrl = firstGLM_params, ...),
                              silent = FALSE)

      GLMmodelID <- self$firstGLMfit$modelfits_all[[1]]@model_id
      firstGLM_preds_train <- predict_h2o_new(GLMmodelID, frame_id = h2o::h2o.getId(train_H2Oframe), convertResToDT = FALSE)
      firstGLM_preds_valid <- predict_h2o_new(GLMmodelID, frame_id = h2o::h2o.getId(valid_H2Oframe), convertResToDT = FALSE)

      ## Save predictions from the first model (fit on training data, but predictions made for all data)
      train_H2Oframe[["firstGLM_preds"]] <- firstGLM_preds_train
      train_H2Oframe[["residual_y"]] <- train_H2Oframe[[self$outvar]] - train_H2Oframe[["firstGLM_preds"]]

      ## Evaluate residuals and define them as new outcomes (to be used as outcomes for the next stage SL)
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
      mainfit_class <- "grid"
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

      # if (convertResToDT)
      # P1_firstGLM <- as.data.table(P1_firstGLM)
      # P1_residSL <- as.data.table(P1_residSL)

      if (ncol(P1_firstGLM) > 1) stop("initial glm fit for residuals must provide predictions with one column matrix only")
      for (model.fit in names(P1_residSL)) {
        # P1_residSL[, (model.fit) := eval(as.name(model.fit)) + P1_firstGLM[[1]]]
        P1_residSL[, model.fit] <-  P1_residSL[, model.fit] + P1_firstGLM
      }

      return(P1_residSL)
    }
  )
)
