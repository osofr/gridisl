## ******************************
## GLMs ('glm' / 'gblinear')
## ******************************
## For holdoutSL the test error is always assessed by predicting from LAST model iteration.
## However, for cvSL the test error is assessed by predicting from the BEST iteration.
## This is due to the fact that cvSL just grabs pre-saved holdout predictions (from xgboost), while holdoutSL does new holdout predictions (manual).
## The refit of best model for both (cvSL & holdoutSL) will be done based only on best iteration from validation data.
## This is not how GLMs are generally implemented (minimizing the train error instead).
## A more conventional approach is to perhaps NEVER use the test data when training GLMs, so only call xgb.train(), with watchlist containing only data_train.
## The model performance could then be assessed in the usual way.
## This still poses a problem for cvSL!!! No way to implement early stopping in xgb.cv so that its based on TRAINING DATA!

## ******
## TO DO: Implement out of sample CV prediction for best grid model (use ntreelimit as an arg to predict)
## TO DO: Automatically initiate the h2o cluster (h2o.init(2)) if its hasn't been started.
## TO DO fit.xgb.train(): Add validation_data to watchlist as watchlist <- list(train = dtrain, eval = dtest)?
## TO DO RFs: Add random forests to the grid (for RFs, the arg 'ntreelimit' might differ from 'best_iteration')
## TO DO RFs: Investigate difference between 'best_iteration' & 'best_ntreelimit' (different only for RFs).
##            Which one should be used for prediction with RFs?
## TO DO: Add support for booster 'dart'
## TO DO (xgb.grid): Add arg "nthread = value" to the main function call (will be passed on to params)
## TO DO: Determine optimal number of nthreads based on type of evaluation (parallel fits as in stremr or a single fit)
## TO DO: Implement an option for performing grid search in parallel (over nmodels)

## (DONE, no longer needed due to new interface):
##        Implement a grid for xgboost that may include several learners (e.g., grid-based glm, gbm, drf and individual learner (no grid)
##        In fit.xgb.grid we could then rbind the data.table that contains these learners
## (DONE, running parallel xgboost): Tested parallel grid searches with xgboost from stremr. Still need to test performance on real data.
## (DONE, retraining best model): Fix re-training of the best grid model (based on params, nrounds, ntreelimit, etc)
## (DONE, importing purr, dplyr in xgb.grid: Avoids loading the entire namespace of tidyverse package, imports %>%
## (DONE, custom metrics / objective): Allow passing optional metric(s), custom objective and eval funs
## (DONE, Validation Grid): Grid search can now be done without CV, just using holdout validation data and calling xgb.train instead of xgb.cv
## (DONE, CV): For CV need to convert the fold_column in data into a list of validation row indices.
##             Each validation fold rows (vector) are stored as a separate list item.
## (DONE): NEED TO BE ABLE TO TAKE PARAMS AS AN ARG AND MERGE WITH default params
## ******

# S3 method for xgboost model call
fit.xgb.train <- function(fit.class, params, train_data, model_contrl, ...) {
# fit.xgb.train <- function(fit.class, params, train_data, fold_column, model_contrl, validation_data  = NULL, ...) {
  if (gvars$verbose) print("calling xgboost::xgb.train...")

  mainArgs <- list(data = train_data)
  model_contrl['fit.package'] <- NULL
  model_contrl['fit.algorithm'] <- NULL

  # xbg.params <- model_contrl[["params"]]
  if (is.null(model_contrl[["params"]])) {
    message("separate list of params was not specified for xgboost, passing all arguments using globals ")
    # xbg.params <- model_contrl
  }

  # if (is.null(model_contrl$objective)) model_contrl$objective <- "reg:linear"
  # if (is.null(model_contrl$objective)) model_contrl$objective <- "reg:logistic"
  # if (is.null(model_contrl$booster)) model_contrl$booster <- "gbtree" ## gbtree or gblinear
  # if (is.null(model_contrl$nthreads)) model_contrl$nthreads <- 1
  ## additional parameters for 'gblinear': lambda = 0L, alpha = 0L ## regularization
  # if (is.null(model_contrl$silent)) model_contrl$silent <- ifelse(gvars$verbose, 0, 1)

  # nrounds <- xbg.params$nrounds
  # xbg.params['nrounds'] <- NULL
  # if (is.null(mainArgs[['callbacks']])) {
  #   mainArgs[['callbacks']] = list(xgboost::cb.evaluation.log())
  # } else {
  #   mainArgs[['callbacks']] <- c(xgboost::cb.evaluation.log(), mainArgs[['callbacks']])
  # }
  # mainArgs[['callbacks']] <- NULL

  # maximize <- xbg.params$maximize
  # if (is.null(maximize)) maximize <- FALSE
  # xbg.params['maximize'] <- NULL

  mainArgs <- c(mainArgs, model_contrl)
  mainArgs[["verbose"]] <- gvars$verbose

  if (is.null(mainArgs[['nrounds']]) && is.null(mainArgs[['params']][['nrounds']])) mainArgs[['nrounds']] <- 100
  if (is.null(mainArgs[['eval_metric']]) && is.null(mainArgs[['params']][['eval_metric']])) mainArgs[['eval_metric']] <- list("rmse")

  # mainArgs[["callbacks"]] <- c(list(xgboost::cb.evaluation.log()))
  # browser()

  # browser()
  # mainArgs[['objective']] <- "reg:logistic"

  if (nrow(train_data) == 0L) {
    model.fit <- list()
  } else {
    # SuppressGivenWarnings({
    # model.fit <- xgboost::xgb.train(xbg.params, train_data, nrounds = nrounds,
    #                                 maximize = maximize,
    #                                 callbacks = callbacks,
    #                                 eval_metric = metrics,
    #                                 maximize = maximize)
    model.fit <- do.call(xgboost::xgb.train, mainArgs)
    # }, GetWarningsToSuppress())
  }
  nobs <- nrow(train_data)
  return(create_fit_object(model.fit, model_alg = "gbm", fitfunname = "xgb.train",
                           params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl,
                           fitclass = "XGBoostmodel"))
}

#' @export
fit_single_xgboost_grid <- function(grid.algorithm, train_data, family = "binomial",
                                    model_contrl, fold_column = NULL, validation_data  = NULL, ...) {

  ## ********************************************************************************
  ## These defaults can be over-ridden in model_contrl
  # ********************************************************************************
  mainArgs <- list(data = train_data,
                   nrounds = 50,
                   # nrounds = 1000,
                   # early_stopping_rounds = 10,
                   # metrics = list(evalMSEerror),
                   # order_metric_name = "RMSE",
                   metrics = list("rmse"),
                   order_metric_name = "rmse",
                   maximize = FALSE,
                   verbose = gvars$verbose,
                   seed = model_contrl[['seed']])

  if (is.null(mainArgs[["objective"]])) {
    if (family %in% c("binomial", "quasibinomial")) {
      mainArgs[["objective"]] <- "reg:logistic"
    } else if (family %in% "gaussian") {
      mainArgs[["objective"]] <- "reg:linear"
    } else {
      stop("family values other than 'binomial' and 'gaussian' are not yet supported for modeling with xgboost package")
    }
  }

  if (is.null(grid.algorithm)) grid.algorithm <- "gbm"

  if (is.null(mainArgs[["booster"]])) {
    if (!is.character(grid.algorithm) || (!grid.algorithm %in% c("glm","gbm"))) stop("'grid.algorithm' must be either 'glm' or 'gbm'")
    if (grid.algorithm %in% "glm") {
      mainArgs[["booster"]] <- "gblinear"
    } else if (grid.algorithm %in% "gbm") {
      mainArgs[["booster"]] <- "gbtree"
    } else if (grid.algorithm %in% "drf") {
      stop("drf w/ xgboost is not implemented")
    } else {
      stop("the only algorithms allowed with xgboost are: glm, gbm and drf")
    }
  }

  # Is there a validation frame for model scoring?
  if (!is.null(validation_data)) mainArgs[["validation_data"]] <- validation_data

  ## Is there a fold_column for cross-validation based model scoring?
  ## ******** fold_column is already CONVERTED TO INTERNAL xgboost representation ********
  if (!is.null(fold_column)) {
      mainArgs[["folds"]] <- fold_column
      ## callback that saves the CV models (and out-of-sample / holdout predictions)
      mainArgs[["callbacks"]] = list(xgboost::cb.cv.predict(save_models = TRUE))
  }

  ## THE grid of hyper-parameters, can be either specified in a list with names: "glm"/"gbm"/"params"
  ## Disabling this due to possible ambiguity, unified interface always assumes that hyper_params come from 'params' item
  # grid_params <- model_contrl[[grid.algorithm]]

  # if (is.null(grid_params)) {
  grid_params <- model_contrl[["params"]]
  model_contrl[["params"]] <- NULL
  if (is.null(grid_params)) grid_params <- list()
    # stop("must specify hyper parameters for grid search with '" %+%
    #   algo_fun_name %+%
    #   "' by defining a SuperLearner params list item named '" %+% grid.algorithm %+% "'")

  # if (!is.null(grid_params[["search_criteria"]])) {
  #   search_criteria <- grid_params[["search_criteria"]]
  #   grid_params[["search_criteria"]] <- NULL
  # } else {
  search_criteria <- model_contrl[["search_criteria"]]
  # }

  mainArgs[["hyper_params"]] <- grid_params
  mainArgs[["search_criteria"]] <- search_criteria

  algo_fun_name <- "xgb.grid"
  algo_fun <- get0(algo_fun_name, mode = "function", inherits = TRUE)

  ## Keep only the relevant args in mainArgs list (DISABLING FOR NOW):
  # mainArgs <- keep_only_fun_args(mainArgs, fun = algo_fun)

  ## 1. Add all user args in model_contrl that also appear in args (signature) of the learner algo_fun
  ##    This will replace any default args predefined in mainArgs, but will also add new ones
  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = algo_fun)
  ## 2. Put the rest of the arguments that appear in mainArgs in mainArgs[["hyper_params"]]
  add_param_names <- names(model_contrl)[(!(names(model_contrl) %in% c(names(mainArgs), "fit.package", "fit.algorithm", "grid.algorithm", "family")))]
  new_params <- model_contrl[add_param_names]
  mainArgs[["hyper_params"]] <- c(mainArgs[["hyper_params"]], new_params)

  ## Remove any args from mainArgs that also appear in hyper_params:
  common_hyper_args <- intersect(names(mainArgs), names(mainArgs[["hyper_params"]]))
  if(length(common_hyper_args) > 0) mainArgs <- mainArgs[!(names(mainArgs) %in% common_hyper_args)]

  ## TO DO: What if 'params' is specified as part of model_contrl?
  ##        replace existing items in params with those in grid_params Intersect?

  ## Will put all fitted models in a single list for stacking:
  fitted_models_all <- NULL
  ngridmodels <- 0

  if (gvars$verbose) print("running xgb.grid with booster: " %+% mainArgs[["booster"]])
  model_fit <- do.call(xgb.grid, mainArgs)

  return(model_fit)
}

#' @export
fit.xgb.grid <- function(fit.class, params, train_data, model_contrl, fold_column, ...) {
  family <- model_contrl[["family"]]

  grid.algorithms <- model_contrl[["grid.algorithm"]]
  if (is.null(grid.algorithms)) grid.algorithms <- model_contrl[["fit.algorithm"]]

  ## no longer used:
  # learners <- model_contrl$learner
  if (is.null(family)) family <- "binomial"

  grid_model_fit <- fit_single_xgboost_grid(grid.algorithm = grid.algorithms[[1]], train_data = train_data, family = family,
                                            model_contrl = model_contrl, fold_column = fold_column, ...)

  fitted_models_all <- grid_model_fit[["xgb_fit"]]
  ngridmodels <- length(fitted_models_all)
  model_algorithms <- model_ids <- vector(mode = "list", length = length(fitted_models_all))
  model_algorithms[] <- "xgb"
  # model_ids <- lapply(fitted_models_all, function(model) model@model_id)
  # Assign names to each grid model, keep individual learner names intact (unless a $name arg was passed by the user):
  GRIDmodel_names <- "grid." %+% unlist(model_algorithms) %+% "." %+% (1:ngridmodels)

  if (!is.null(params$model_idx))
    GRIDmodel_names <- "m." %+% params$model_idx %+% "." %+% GRIDmodel_names

  model_names <- c(GRIDmodel_names)
  # learner_names <- names(fitted_models_all)[-(1:ngridmodels)]
  # model_names <- c(GRIDmodel_names, learner_names)

  if (!is.null(model_contrl$name))  model_names <- model_names %+% "." %+% model_contrl$name
  names(fitted_models_all) <- names(model_algorithms) <- names(model_ids) <- model_names
  grid_model_fit[, ("model_names") := model_names]

  fit <- list(
    params = params,
    fitted_gridDT = grid_model_fit,
    fitted_models_all = fitted_models_all,
    # grid_ids = lapply(fitted_gridDT, function(grids_object) grids_object@grid_id),
    ngridmodels = ngridmodels,
    model_algorithms = model_algorithms,
    model_ids = model_ids
    # top_grid_models = top_grid_models,
    )

  # ...
  # fit <- list(params = params,
  #             fitted_gridDT = fitted_gridDT,
  #             grid_ids = lapply(fitted_gridDT, function(grids_object) grids_object@grid_id),
  #             ngridmodels = ngridmodels,
  #             model_algorithms = model_algorithms,
  #             model_ids = model_ids,
  #             top_grid_models = top_grid_models,
  #             fitted_models_all = fitted_models_all)
  #   fit <- list(
  #   params = params,
  #   fitted_gridDT = fitted_gridDT,
  #   grid_ids = lapply(fitted_gridDT, function(grids_object) grids_object@grid_id),
  #   ngridmodels = ngridmodels,
  #   model_algorithms = model_algorithms,
  #   model_ids = model_ids,
  #   top_grid_models = top_grid_models,
  #   fitted_models_all = fitted_models_all
  #   )

  class(fit) <- c(class(fit)[1], c("XGBoostgrid"))
  return(fit)

}

## ----------------------------------------------------------------
## Obtain xgb.DMatrix to be used for prediction with xgboost
## ----------------------------------------------------------------
getPredictXGBDMat <- function(m.fit, ParentObject, DataStorageObject, subset_idx) {
  # assert_that(!is.null(subset_idx))
  if (missing(DataStorageObject)) {

    # return(h2o::h2o.getFrame(ParentObject$get_train_H2Oframe_ID))
    stop("prediction with missing input data is not implemented for xgboost")

  } else {

    if (ParentObject$useDMatrix) {

      stop("prediction with pre-saved DMatrix is not implemented for xgboost")
      # pred_dmat <- DataStorageObject$xgb.DMatrix[subset_idx, ]

    } else {
      fold_column <- ParentObject$fold_column

      newXmat <- as.matrix(DataStorageObject$dat.sVar[subset_idx, m.fit$params$predvars, with = FALSE])
      if (is.integer(newXmat)) newXmat[,1] <- as.numeric(newXmat[,1])
      pred_dmat <- xgboost::xgb.DMatrix(newXmat)

      ## fails if is.integer(mat)
      # pred_dmat <- xgboost::xgb.DMatrix(as.matrix(newXmat))

      # Yvals <- DataStorageObject$get.outvar(subset_idx, self$outvar) # Always a vector (or m.fit$params$outvar)
      # pred_dmat <- xgb.DMatrix(as.matrix(newXmat), label = Yvals)

    }
    return(pred_dmat)
  }
}

predictP1.XGBoostgrid <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
  return(predictP1.XGBoostmodel(m.fit, ParentObject, DataStorageObject, subset_idx, ...))
}

predictP1.XGBoostmodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict_model_names, ...) {
  pred_dmat <- getPredictXGBDMat(m.fit, ParentObject, DataStorageObject, subset_idx)

  models_list <- m.fit$fitted_models_all
  if (!missing(predict_model_names) && !is.null(predict_model_names)) models_list <- models_list[predict_model_names]

  pAoutDT <- rep.int(list(numeric()), length(models_list))
  names(pAoutDT) <- names(models_list)

  if (nrow(pred_dmat) > 0) {
    # pAoutDT <- NULL
    # idx <- 1
    # str(models_list[[1]])

    for (idx in seq_along(models_list)) {

      ## Use ntreelimit for prediction, if it was actually used during model training
      if (!is.null(models_list[[idx]]$best_ntreelimit)) ntreelimit <- models_list[[idx]]$best_ntreelimit else ntreelimit <- NULL

      ## temp fix to avoid the bug "GBLinear::Predict ntrees is only valid for gbtree predictor"
      ## for gblinear need to replace with this (set ntreelimit to 0)
      if ((models_list[[idx]]$params$booster %in% "gblinear") && (!is.null(ntreelimit))) {
        models_list[[idx]]$best_ntreelimit <- NULL
        ntreelimit <- 0
      }

      ## will generally return a vector, needs to be put into a corresponding column of a data.table
      pAoutDT[[names(models_list)[idx]]] <- predict(models_list[[idx]], newdata = pred_dmat, ntree_limit = ntreelimit)
    }

    setDT(pAoutDT)
    # pAoutDT <- as.data.table(pAoutDT)
    # names(pAoutDT) <- names(models_list)
  }
  return(pAoutDT)
}

## ---------------------------------------------------------------------
#' R6 class model fitting with xgboost R package
#'
#' This R6 class can request, store and manage the design matrix Xmat, as well as the binary outcome Bin for the
#'  logistic regression P(Bin|Xmat).
#'  Can also be used for converting data in wide format to long when requested,
#'  e.g., when pooling across binary indicators (fitting one pooled logistic regression model for several indicators)
#'  The class has methods that perform queries to data storage R6 class DataStorageClass to get appropriate data columns & row subsets
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' \itemize{
#' \item{ID} - Vector of observation IDs, \code{1:n}, used for pooling.
#' \item{outvar} - Outcome name.
#' \item{predvars} - Predictor names.
#' \item{subset_vars} - Defines the subset which would be used for fitting this model (logical, expression or indices).
#' \item{subset_idx} - Subset \code{subset_vars} converted to logical vector.
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{new(reg)}}{Uses \code{reg} R6 \code{\link{RegressionClass}} object to instantiate a new storage container for a
#'   design matrix and binary outcome.}
#'   \item{\code{setdata()}}{...}
#' }
#' @section Active Bindings:
#' \describe{
#'   \item{\code{emptydata}}{...}
#'   \item{\code{emptyY}}{...}
#'   \item{\code{emptySubset_idx}}{...}
#'   \item{\code{getXmat}}{...}
#'   \item{\code{getY}}{...}
#' }
#' @importFrom assertthat assert_that is.count is.string is.flag
#' @export
XGBoostClass <- R6Class(classname = "XGBoost",
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
    fit.class = c("glm", "gbm", "grid"),
    model.fit = list(),
    outfactors = NA,

    useDMatrix = FALSE,

    initialize = function(fit.algorithm, fit.package, reg, useDMatrix = FALSE, ...) {
      self$reg <- reg
      self$params <- create_fit_params(reg)
      self$outvar <- reg$outvar
      self$predvars <- reg$predvars
      self$runCV <- reg$runCV
      self$fold_column <- reg$fold_column
      self$model_contrl <- reg$model_contrl

      self$useDMatrix <- useDMatrix
      assert_that("xgboost" %in% fit.package)

      ## *** IN THE FUTURE THIS needs to be changed accordingly for running either single gbm model fit or for CV grid search (when implemented).
      self$fit.class <- fit.algorithm

      if (fit.algorithm %in% c("glm", "drf", "gbm", "grid")) {
        class(self$fit.class) <- c(class(self$fit.class), "xgb.grid")
      } else {
        class(self$fit.class) <- c(class(self$fit.class), self$fit.class)
      }

      # if (fit.algorithm %in% c("glm", "drf", "gbm")) {
      #   class(self$fit.class) <- c(class(self$fit.class), "xgb.train")
      # } else if (fit.algorithm %in% "grid") {
      #   class(self$fit.class) <- c(class(self$fit.class), "xgb.grid")
      # } else {
      #   class(self$fit.class) <- c(class(self$fit.class), self$fit.class)
      # }

      invisible(self)
    },

    fit = function(data, subset_idx, validation_data = NULL, ...) {
      assert_that(is.DataStorageClass(data))

      train_dmat <- self$setdata(data, subset_idx, ...)
      # train_dmat <- self$setdata(data, subset_idx, self$classify, destination_frame = destination_frame, ...)
      # private$train_H2Oframe <- train_H2Oframe
      # private$train_H2Oframe_ID <- h2o::h2o.getId(train_H2Oframe)

      if ( (length(self$predvars) == 0L) || (length(subset_idx) == 0L) ) {
      # if ((length(self$predvars) == 0L) || (length(subset_idx) == 0L) || (length(self$outfactors) < 2L)) {
        message("unable to run " %+%
          self$fit.class %+%
          " with h2o for: intercept only models or designmat with zero rows or  constant outcome (y) ...")
        class(self$model.fit) <- "try-error"
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }

      if (!is.null(validation_data)) {

        assert_that(is.DataStorageClass(validation_data))
        valid_dmat <- self$setdata(validation_data, ...)
        # valid_dmat <- self$setdata(validation_data, classify = self$classify, destination_frame = "valid_H2Oframe", ...)
        # private$valid_H2Oframe <- valid_H2Oframe
        # private$valid_H2Oframe_ID <- h2o::h2o.getId(valid_H2Oframe)

      } else {

        valid_dmat = NULL

      }

      if (!is.null(self$fold_column)) {
        Vfold_valid_rows <- data$dat.sVar[subset_idx, ][, .I, by = eval(self$fold_column)]
        setkeyv(Vfold_valid_rows, cols = self$fold_column)
        # Vfold_valid_rows[.(unique(fold)), mult = 'all']
        folds <- split(Vfold_valid_rows[["I"]], Vfold_valid_rows[[self$fold_column]])
      } else {
        folds <- NULL
      }

      self$model.fit <- try(fit(self$fit.class, self$params, train_data = train_dmat,
                                model_contrl = self$model_contrl, fold_column = folds,
                                validation_data = valid_dmat, ...),
                          silent = FALSE)

      if (inherits(self$model.fit, "try-error")) {
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }
      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx, predict_model_names) {
      P1_DT <- predictP1(self$model.fit,
                         ParentObject = self,
                         DataStorageObject = data,
                         subset_idx = subset_idx,
                         predict_model_names = predict_model_names)
      return(P1_DT)
    },

    predictP1_out_of_sample_cv = function(validation_data, subset_idx, predict_model_names) {
      P1_DT <- xgb_predict_out_of_sample_cv(self$model.fit,
                                            ParentObject = self,
                                            validation_data = validation_data,
                                            subset_idx = subset_idx,
                                            predict_model_names = predict_model_names)
      return(P1_DT)
    },

    getmodel_byname = function(model_names, model_IDs) {
      if (!missing(model_names)) {
        return(self$model.fit$fitted_models_all[model_names])
      } else stop("Can only use 'model_names' for retrieving xgboost models")
        # if (missing(model_IDs)) stop("Must provide either 'model_names' or 'model_IDs'.")
        # return(lapply(model_IDs, h2o::h2o.getModel))
      # }
    },

    getgridDT_byname = function(model_names, model_IDs) {
      if (!missing(model_names)) {
        model_idx <- (self$model.fit$fitted_gridDT[["model_names"]] %in% model_names)
        return(self$model.fit$fitted_gridDT[model_idx, ])
      } else {
        if (missing(model_IDs)) stop("Must provide either 'model_names' or 'model_IDs'.")
        # return(lapply(model_IDs, h2o::h2o.getModel))
      }
    },

    get_best_model_params = function(model_names) {
      ## actual model object returned by xgboost for the best performing model
      model_obj <- self$getmodel_byname(model_names[1])[[1]]
      ## data.table row from the grid with the best model
      gridmodel_obj <- self$getgridDT_byname(model_names[1])

      ## Top model is always going to be a call for training a single final xbg model (no CV /validation data)
      top_params <- list(fit.package = self$model_contrl$fit.package, fit.algorithm = "xgb.train")

      # str(model_obj)
      # model_obj$call
      ## params arg of xgb.train:
      top_params <- c(top_params, model_obj$params)

      ## should be equivalent:
      if (!all.equal(model_obj$params, gridmodel_obj[["params"]][[1]])) stop("fatal error, model parameters appear to be inconsistent")

      top_params[["nrounds"]] <- gridmodel_obj[["nrounds"]]
      top_params <- c(top_params, gridmodel_obj[["glob_params"]][[1]])

      # top_params$callbacks <- model_obj$callbacks
      # attributes(model_obj)
      # browser()

      return(top_params)
    },

    setdata = function(data, subset_idx, ...) {
      outvar <- self$outvar
      predvars <- self$predvars
      assert_that(is.DataStorageClass(data))
      # browser()

      if (self$useDMatrix) {

        stop("fitting with pre-saved DMatrix is not implemented for xgboost")
        # if (missing(subset_idx)) {
        #   fit_dmat <- data$H2Oframe
        # } else {
        #   subset_t <- system.time(fit_dmat <- data$H2Oframe[subset_idx, ])
        #   # if (gvars$verbose) {
        #   print("time to subset H2OFRAME: "); print(subset_t)
        #   # }
        # }

      } else {
        # load_var_names <- c(outvar, predvars)
        # if (!is.null(data$fold_column)) load_var_names <- c(load_var_names, data$fold_column)
        if (missing(subset_idx)) subset_idx <- (1:data$nobs)
        load_subset_t <- system.time({
          IDs <- data$get.outvar(subset_idx, data$nodes$IDnode)
          Yvals <- data$get.outvar(subset_idx, outvar) # Always a vector
          ## gives an error when all columns happen to be integer type:
          # fit_dmat <- xgboost::xgb.DMatrix(as.matrix(data$dat.sVar[subset_idx, predvars, with = FALSE]), label = Yvals)
          ## still gives the same error:
          # fit_dmat <- xgboost::xgb.DMatrix(data.matrix(data$dat.sVar[subset_idx, predvars, with = FALSE]), label = Yvals)
          # Another option is to stop at data.matrix and pass it directly to xgboost, passing Y's as separate arg
          Xmat <- as.matrix(data$dat.sVar[subset_idx, predvars, with = FALSE])
          if (is.integer(Xmat)) Xmat[,1] <- as.numeric(Xmat[,1])
          if (ncol(Xmat) == 0)
            stop("fitting intercept only model (with no covariates) is not supported with xgboost, use speedglm instead")
          fit_dmat <- xgboost::xgb.DMatrix(Xmat, label = Yvals)
          # attr(fit_dmat, 'ID') <- IDs
        })
        # if (gvars$verbose) {
          print("time to create xgb.DMatrix: "); print(load_subset_t)
        # }
      }

      return(fit_dmat)
    },

    show = function(all_fits = FALSE, ...) {
      model.fit <- self$model.fit
      fitted_gridDT <- self$model.fit$fitted_gridDT
      top_grid_models <- self$model.fit$top_grid_models

      # if (!is.null(fitted_gridDT)) {
      #   cat(" TOTAL NO. OF GRIDS: " %+% length(fitted_gridDT) %+% "\n")
      #   cat(" ======================= \n")
      #   for (grid_nm in names(fitted_gridDT)) {
      #     print(fitted_gridDT[[grid_nm]])
      #     cat("\n TOP MODEL FOR THIS GRID: \n")
      #     cat(" ======================= \n")
      #     print(top_grid_models[[grid_nm]])
      #   }
      # }

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
  )

)
