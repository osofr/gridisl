

## ******************************
## GLMs ('glm' / 'gblinear')
## ******************************
## For method = "holdout" the test error is always assessed by predicting from LAST model iteration.
## However, for method = "cv" the test error is assessed by predicting from the BEST iteration.
## This is due to the fact that cv just grabs pre-saved holdout predictions (from xgboost), while holdoutSL does new holdout predictions (manual).
## The refit of best model for both (cv & holdout) will be done based only on best iteration from validation data.
## This is not how GLMs are generally implemented (minimizing the train error instead).
## A more conventional approach is to perhaps NEVER use the test data when training GLMs, so only call xgb.train(), with watchlist containing only data_train.
## The model performance could then be assessed in the usual way.
## This still poses a problem for cv!!! No way to implement early stopping in xgb.cv so that its based on TRAINING DATA!

## ******
## TO DO: RFs -- Investigate difference between 'best_iteration' & 'best_ntreelimit' (different only for RFs).
##            Which one should be used for prediction with RFs?
## TO DO: Add support for booster 'dart'
## ******

## (DONE): Implement an option for performing grid search in parallel (over nmodels) -- see commented out section
## (DONE) RFs: Add random forests to the grid (for RFs, the arg 'ntreelimit' might differ from 'best_iteration')
## (DONE) fit.xgb.train(): Add validation_data to watchlist as watchlist <- list(train = dtrain, eval = dtest)?
## (DONE): Implement out of sample CV prediction for best grid model (use ntreelimit as an arg to predict)
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

fit_single_xgboost_grid <- function(grid.algorithm,
                                    train_data,
                                    model_contrl,
                                    fold_column = NULL,
                                    validation_data  = NULL, ...) {

  if ("interactions" %in% names(model_contrl)) model_contrl[["interactions"]] <- NULL
  if ("remove_const_cols" %in% names(model_contrl)) model_contrl[["remove_const_cols"]] <- NULL

  ## ********************************************************************************
  ## These defaults can be over-ridden in model_contrl
  # ********************************************************************************
  mainArgs <- list(data = train_data,
                   nrounds = 100,
                   # nrounds = 1000,
                   # early_stopping_rounds = 10,
                   # metrics = list(evalMSEerror),
                   metrics = list("rmse"),
                   # order_metric_name = "RMSE",
                   # order_metric_name = "rmse",
                   order_metric_name = NULL,
                   maximize = FALSE,
                   verbose = FALSE, # verbose = gvars$verbose,
                   seed = model_contrl[['seed']])

  mainArgs[["feature_names"]] <- attr(train_data, ".Dimnames")[[2]]

  if (is.null(mainArgs[["objective"]])) {

    family <- model_contrl[["family"]]
    if (is.null(family)) family <- "binomial"

    if (family %in% c("binomial", "quasibinomial")) {
      mainArgs[["objective"]] <- "reg:logistic"
    } else if (family %in% "gaussian") {
      mainArgs[["objective"]] <- "reg:linear"
    } else {
      stop("family values other than 'binomial' and 'gaussian' are not yet supported for modeling with xgboost package")
    }
  }

  ### *******************************************************
  ### ADD OFFSET (setinfo on design data)
  ### ADD WEIGHTS (setinfo on design data)
  ### *******************************************************

  if (is.null(grid.algorithm)) grid.algorithm <- "gbm"

  if (is.null(mainArgs[["booster"]])) {
    if (!is.character(grid.algorithm) || (!grid.algorithm %in% c("glm","gbm","drf"))) stop("'estimator' must be either 'glm', 'gbm' or 'drf'")
    if (grid.algorithm %in% "glm") {
      mainArgs[["booster"]] <- "gblinear"
      ## *** Enabling this callback implies that its no longer possible to use CV to find best params for regularized regression ***
      ## Call-back for early stopping based on training error, must be added before other callbacks
      ## h2o.glm defaults: [max_iterations = 100; score_each_iteration = FALSE; early_stopping = TRUE, obj_eps = 0.000001]
      # early_stop_ontrain <- xgboost::cb.early.stop(stopping_rounds = 5, maximize = FALSE, metric_name = "train-rmse", verbose = gvars$verbose)
      # mainArgs[["callbacks"]] <- early_stop_ontrain
    } else if (grid.algorithm %in% "gbm") {
      mainArgs[["booster"]] <- "gbtree"
    } else if (any(grid.algorithm %in% c("drf", "rf", "randomForest"))) {
      mainArgs[["booster"]] <- "gbtree"
      if (!is.null(model_contrl[["nrounds"]])) {
        mainArgs[["num_parallel_tree"]] <- model_contrl[["nrounds"]]
      } else {
        mainArgs[["num_parallel_tree"]] <- mainArgs[["nrounds"]]
      }
      ## Important to set nrounds to 1 for RandomForest
      model_contrl[["nrounds"]] <- 1
      mainArgs[["subsample"]] <- 0.632
      mainArgs[["colsample_bytree"]] <- 1/sqrt(ncol(train_data)) #/nrow(train_data)
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

    ## If validation data is also provided make sure its not passed to xgb.cv
    ## Default model scoring will be based on CV, validation data will be used later for model re-scoring
    mainArgs[["validation_data"]] <- NULL

    ## callback that saves the CV models (and out-of-sample / holdout predictions)
    ## -----------------------------------------------------------------
    ## ********* SAVING CV PREDICTIONS / MODELS -- IMPORTANT *********
    ## cb.cv.predict(save_models = FALSE)
    ## -----------------------------------------------------------------
    ## This callback function saves predictions for all of the test folds, and also allows to save the folds' models.
    ## It is a "finalizer" callback and it uses early stopping information whenever it is available,
    ## thus it must be run after the early stopping callback if the early stopping is used.
    ## Callback function expects the following values to be set in its calling frame:
    ## bst_folds, basket, data, end_iteration, params, num_parallel_tree, num_class.

    if (is.null(mainArgs[["callbacks"]])){
      mainArgs[["callbacks"]] <- list(xgboost::cb.cv.predict(save_models = TRUE))
      # mainArgs[["callbacks"]] <- list(xgboost::cb.cv.predict(save_models = FALSE))
    } else {
      mainArgs[["callbacks"]] <- c(mainArgs[["callbacks"]], list(xgboost::cb.cv.predict(save_models = TRUE)))
      # mainArgs[["callbacks"]] <- c(mainArgs[["callbacks"]], list(xgboost::cb.cv.predict(save_models = FALSE)))
    }
  }

  ## THE grid of hyper-parameters, if specified should be a named list of hyper-params
  param_grid <- model_contrl[["param_grid"]]
  if (is.null(param_grid)) param_grid <- list()
  mainArgs[["param_grid"]] <- param_grid
  model_contrl[["param_grid"]] <- NULL

  mainArgs[["search_criteria"]] <- model_contrl[["search_criteria"]]
  model_contrl[["search_criteria"]] <- NULL

  algo_fun <- get0("xgb.grid", mode = "function", inherits = TRUE)

  ## 1. Add all user args in model_contrl that also appear in args (signature) of the learner algo_fun
  ##    This will replace any default args predefined in mainArgs, but will also add new ones
  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = algo_fun)
  ## 2. Put the rest of the arguments that appear in mainArgs in mainArgs[["param_grid"]]
  add_param_names <- names(model_contrl)[(!(names(model_contrl) %in% c(names(mainArgs), "fit.package", "fit.algorithm", "grid.algorithm", "family")))]
  new_params <- model_contrl[add_param_names]
  mainArgs[["param_grid"]] <- c(mainArgs[["param_grid"]], new_params)

  ## Remove any args from mainArgs that also appear in param_grid:
  common_hyper_args <- intersect(names(mainArgs), names(mainArgs[["param_grid"]]))
  if(length(common_hyper_args) > 0) mainArgs <- mainArgs[!(names(mainArgs) %in% common_hyper_args)]

  ## Will put all fitted models in a single list for stacking:
  modelfits_all <- NULL
  ngridmodels <- 0

  if (gvars$verbose) print("running xgb.grid with booster: " %+% mainArgs[["booster"]])

  model_fit <- try(do.call(xgb.grid, mainArgs), silent = FALSE)

  if (inherits(model_fit, "try-error"))
    stop("All grid models for xgb.grid with " %+% grid.algorithm %+% " have failed. This suggests an error in model specification.")

  return(model_fit)
}

fit.xgb.grid <- function(fit.class, params, train_data, model_contrl, fold_column, ...) {
  grid.algorithm <- model_contrl[["grid.algorithm"]]
  if (is.null(grid.algorithm)) grid.algorithm <- model_contrl[["fit.algorithm"]]

  modelfits_grid <- fit_single_xgboost_grid(grid.algorithm = grid.algorithm[[1]],
                                            train_data = train_data,
                                            model_contrl = model_contrl,
                                            fold_column = fold_column, ...)

  modelfits_all <- modelfits_grid[["xgb_fit"]]
  ngridmodels <- length(modelfits_all)

  model_algorithms <- model_ids <- vector(mode = "list", length = length(modelfits_all))
  model_algorithms[] <- grid.algorithm[[1]]

  ## Assign names to each grid model, these are the names that are presented in the output tables
  model_names <- "xgb." %+% unlist(model_algorithms[1:ngridmodels])
  if (!is.null(params[["Model_idx"]])) model_names <- "m." %+% params[["Model_idx"]] %+% "." %+% model_names
  if (ngridmodels > 1) model_names <- model_names %+% ".grid." %+% (1:ngridmodels)
  if (!is.null(model_contrl$name))  model_names <- model_names %+% "." %+% model_contrl$name

  names(modelfits_all) <- names(model_algorithms) <- names(model_ids) <- model_names
  modelfits_grid[, ("model_names") := model_names]

  fit <- list(
    params = params,
    modelfits_grid = modelfits_grid,
    modelfits_all = modelfits_all,
    topmodel_grid = NULL,
    grid_id = NULL,
    ngridmodels = ngridmodels,
    model_algorithms = model_algorithms,
    model_ids = model_ids,
    model_names = model_names
    # top_grid_models = top_grid_models,
    )

  class(fit) <- c(class(fit)[1], c("XGBoostgrid"))
  return(fit)

}

## ----------------------------------------------------------------
## Obtain xgb.DMatrix to be used for prediction with xgboost
## ----------------------------------------------------------------
getPredictXGBDMat <- function(m.fit, ParentObject, DataStorageObject, subset_idx) {
  # assert_that(!is.null(subset_idx))
  if (missing(DataStorageObject)) stop("prediction with missing input data is not implemented for xgboost")

  if (ParentObject$useDMatrix) stop("prediction with pre-saved DMatrix is not implemented for xgboost")
  # pred_dmat <- DataStorageObject$xgb.DMatrix[subset_idx, ]
  # DataStorageObject$dat.sVar[subset_idx, ]

  pred_dmat <- ParentObject$setdata(DataStorageObject, subset_idx = subset_idx, getoutvar = FALSE)

  # browser()
  # newXmat <- as.matrix(DataStorageObject$dat.sVar[subset_idx, m.fit$params$predvars, with = FALSE])
  # if (is.integer(newXmat)) newXmat[,1] <- as.numeric(newXmat[,1])
  # pred_dmat <- xgboost::xgb.DMatrix(newXmat)

  ## fails if is.integer(mat)
  # pred_dmat <- xgboost::xgb.DMatrix(as.matrix(newXmat))
  # Yvals <- DataStorageObject$get.outvar(subset_idx, self$outvar) # Always a vector (or m.fit$params$outvar)
  # pred_dmat <- xgb.DMatrix(as.matrix(newXmat), label = Yvals)
  return(pred_dmat)
}

## S3 prediction function for cross-validated xgb object
predict.xgb.cv.synchronous <- function(xgbcvfit, newdata, ntreelimit, average = TRUE, ...) {
  xgbfits <- xgbcvfit[["models"]]
  pAoutDT <- rep.int(list(numeric()), length(xgbfits))
  names(pAoutDT) <- "xgbCV." %+% seq_along(xgbfits)
  for (idx in seq_along(xgbfits)) {
    pAoutDT[[idx]] <- predict(xgbfits[[idx]], newdata = newdata, ntreelimit = ntreelimit, ...)
  }

  if (average) {
    pAoutDT <- data.table(preds = data.table::as.data.table(pAoutDT)[, rowMeans(.SD, na.rm = TRUE)])
  } else {
    pAoutDT <-
      tibble::as_tibble(pAoutDT) %>%
      dplyr::mutate(ID = 1:length(pAoutDT[[1]])) %>%
      tidyr::nest(1:length(xgbfits), .key = "xgbCV")
    data.table::setDT(pAoutDT)
    pAoutDT[, ID := NULL]
    # pAoutDT2 %>% tidyr::unnest()
  }

  return(pAoutDT)
}

predictP1.XGBoostgrid <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
  return(predictP1.XGBoostmodel(m.fit, ParentObject, DataStorageObject, subset_idx, ...))
}

predictP1.XGBoostmodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict_model_names, ...) {
  pred_dmat <- getPredictXGBDMat(m.fit, ParentObject, DataStorageObject, subset_idx)

  models_list <- m.fit$modelfits_all
  if (!missing(predict_model_names) && !is.null(predict_model_names)) models_list <- models_list[predict_model_names]

  pAoutDT <- rep.int(list(numeric()), length(models_list))
  names(pAoutDT) <- names(models_list)

  if (nrow(pred_dmat) > 0) {
    for (idx in seq_along(models_list)) {
      ## Use ntreelimit for prediction, if it was actually used during model training.
      ## Use it only for gbtree (not for gblinear, i.e., glm, as it is not implemented)
      ntreelimit <- 0
      if (!is.null(models_list[[idx]][["best_ntreelimit"]]) && !(models_list[[idx]][["params"]][["booster"]] %in% "gblinear")) {
        ntreelimit <- models_list[[idx]][["best_ntreelimit"]]
      }
      ## will generally return a vector, needs to be put into a corresponding column of a data.table
      pAoutDT[[names(models_list)[idx]]] <- predict(models_list[[idx]], newdata = pred_dmat, ntreelimit = ntreelimit)
    }
    pAoutDT <- as.data.table(pAoutDT)
    names(pAoutDT) <- names(models_list)
    # setDT(pAoutDT)
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
#'   \item{\code{new(reg)}}{Uses \code{reg} R6 \code{RegressionClass} object to instantiate a new storage container for a
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

      invisible(self)
    },

    fit = function(data, subset_idx, validation_data = NULL, ...) {
      assert_that(is.DataStorageClass(data))

      if (!is.null(self$model_contrl[["remove_const_cols"]]) && self$model_contrl[["remove_const_cols"]]) {
        remove_const_cols <- TRUE
      } else {
        remove_const_cols <- FALSE
      }

      train_dmat <- self$setdata(data, subset_idx = subset_idx, remove_const_cols = remove_const_cols, ...)

      if ( (length(self$predvars) == 0L) || (length(subset_idx) == 0L) ) {
      # if ((length(self$predvars) == 0L) || (length(subset_idx) == 0L) || (length(self$outfactors) < 2L)) {
        message("unable to run " %+%
          self$fit.class %+%
          " with xgboost for: intercept only models or designmat with zero rows or  constant outcome (y) ...")
        class(self$model.fit) <- "try-error"
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }

      if (!is.null(validation_data)) {

        assert_that(is.DataStorageClass(validation_data))
        valid_dmat <- self$setdata(validation_data, ...)

      } else {

        valid_dmat = NULL

      }

      if (!is.null(self$fold_column)) {
        Vfold_valid_rows <- data$dat.sVar[subset_idx, ][, .I, by = eval(self$fold_column)]
        setkeyv(Vfold_valid_rows, cols = self$fold_column)
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

    getmodel_byname = function(model_names, ...) {
      if (!missing(model_names)) {
        return(self$model.fit$modelfits_all[model_names])
      } else stop("Can only use 'model_names' for retrieving xgboost models")
    },

    get_modelfits_grid = function(model_names, ...) {
      if (!missing(model_names)) {
        Model_idx <- (self$model.fit$modelfits_grid[["model_names"]] %in% model_names)
        return(self$model.fit$modelfits_grid[Model_idx, ])
      } else {
        return(self$model.fit$modelfits_grid)
      }
    },

    get_best_model_params = function(model_names) {
      ## actual model object returned by xgboost for the best performing model
      model_obj <- self$getmodel_byname(model_names[1])[[1]]

      ## data.table row from the grid with the best model
      gridmodelDT <- self$get_modelfits_grid(model_names[1])

      ## Top model is always going to be a call for training a single final xbg model (no CV /validation data)
      top_params <- list(fit.package = self$model_contrl$fit.package, fit.algorithm = "xgb.train")

      ## params arg of xgb.train:
      top_params <- c(top_params, model_obj$params)

      ## should be equivalent:
      if (!all.equal(model_obj$params, gridmodelDT[["params"]][[1]])) stop("fatal error, model parameters appear to be inconsistent")

      top_params[["nrounds"]] <- gridmodelDT[["nrounds"]]
      top_params <- c(top_params, gridmodelDT[["glob_params"]][[1]])
      top_params[["interactions"]] <- self$model_contrl[["interactions"]]
      top_params[["remove_const_cols"]] <- self$model_contrl[["remove_const_cols"]]

      return(top_params)
    },

    setdata = function(data, subset_idx, getoutvar = TRUE, remove_const_cols = FALSE, ...) {
      outvar <- self$outvar
      predvars <- self$predvars
      assert_that(is.DataStorageClass(data))

      if (self$useDMatrix) stop("fitting with pre-saved DMatrix is not implemented for xgboost")

      # if (missing(subset_idx)) subset_idx <- (1:data$nobs)
      if (missing(subset_idx)) subset_idx <- TRUE

      ## gives an error when all columns happen to be integer type:
      # fit_dmat <- xgboost::xgb.DMatrix(as.matrix(data$dat.sVar[subset_idx, predvars, with = FALSE]), label = Yvals)
      ## still gives the same error:
      # fit_dmat <- xgboost::xgb.DMatrix(data.matrix(data$dat.sVar[subset_idx, predvars, with = FALSE]), label = Yvals)
      # Another option is to stop at data.matrix and pass it directly to xgboost, passing Y's as separate arg

      # browser()

      Xmat <- data$get.dat.sVar(subset_idx, covars = predvars)

      if (ncol(Xmat) == 0)
        stop("fitting intercept only model (with no covariates) is not supported with xgboost, use speedglm instead")

      if (!is.null(self$model_contrl[["interactions"]])) {
        if (gvars$verbose == 2) {
          print("adding interactions in xgboost:")
          str(self$model_contrl[["interactions"]])
        }
        ## this is a hack to fix pointer allocation problem (so that Xmat can be modified by reference inside add_interactions_toDT())
        ## see this for more: http://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
        ## and this: http://stackoverflow.com/questions/36434717/adding-column-to-nested-r-data-table-by-reference
        data.table::setDF(Xmat)
        data.table::setDT(Xmat)
        add_interactions_toDT(Xmat, self$model_contrl[["interactions"]])
      }

      if (remove_const_cols) {
        data.table::setDF(Xmat)
        data.table::setDT(Xmat)
        const_cols <- test_remove_const_cols(Xmat)
        if (length(const_cols) > 0) {
          if (gvars$verbose) print("removing constant column(s) in xgboost: " %+% paste0(const_cols, collapse=","))
          self$predvars <- self$predvars[!(self$predvars %in% const_cols)]
        }
      }

      # browser()
      # self$predvars
      # Xmat[["TI.tminus1"]]

      Xmat <- as.matrix(Xmat)
      if (is.integer(Xmat)) Xmat[,1] <- as.numeric(Xmat[,1])

      if (getoutvar) {

        Yvals <- data$get.outvar(subset_idx, var = outvar) # Always a vector
        fit_dmat <- try(xgboost::xgb.DMatrix(Xmat, label = Yvals))

      } else {
        fit_dmat <- xgboost::xgb.DMatrix(Xmat)
      }
      # browser()
      # IDs <- data$get.outvar(subset_idx, data$nodes$IDnode)
      # attr(fit_dmat, 'ID') <- IDs
      # cat("writing the libsvm design matrix to file: ", file.path(getwd(), "xgb.DMatrix.dtrain"), "\n")
      # res <- xgboost::xgb.DMatrix.save(fit_dmat, file.path(getwd(), "xgb.DMatrix.dtrain"))
      # fit_dmat <- xgboost::xgb.DMatrix(file.path(getwd(), "xgb.DMatrix.dtrain") %+% '#dtrain.cache')

      return(fit_dmat)
    },

    show = function(all_fits = FALSE, ...) {
      model.fit <- self$model.fit
      topmodel_grid <- self$model.fit[["topmodel_grid"]]
      modelfits_grid <- self$model.fit[["modelfits_grid"]]

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

    wipe.allmodels = function() {
      self$model.fit[["modelfits_grid"]][, ("xgb_fit") := NULL]
      self$model.fit[["modelfits_all"]] <- NULL
      self$model.fit[["topmodel_grid"]] <- NULL
    },

    getmodel_ids = function() {
      if (is.null(self$model.fit$model_ids)) {
        return(assign_model_name_id(self$model.fit$modelfits_all[[1]], self$model.fit$model_algorithms[[1]], self$model_contrl$name))
      } else {
        return(self$model.fit$model_ids)
      }
    },

    getmodel_algorithms = function() { self$model.fit$model_algorithms }
  )

)
