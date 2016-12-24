# S3 method for glm binomial family fit, takes BinDat data object:
fit.xgb.train <- function(fit.class, params, train_data, fold_column, model_contrl, validation_data  = NULL, ...) {
  if (gvars$verbose) print("calling xgboost::xgb.train...")

  ## ******
  ## TO DO: NEED TO BE ABLE TO TAKE PARAMS AS AN ARG AND MERGE WITH default params
  ## TO DO: Determine optimal number of nthreads based on type of evaluation (parallel fits as in stremr or a single fit)
  ## TO DO: Need to be able to use validation_data in watchlist: watchlist <- list(eval = dtest, train = dtrain)
  ## TO DO: Allow passing optional metric(s), custom objective and eval funs
  ## TO DO: For CV need to convert the fold_column in data into a list of validation row indices. Each validation fold rows (vector) are stored as a separate list item.
  ## ******

  # browser()

  xbg.params <- model_contrl
  xbg.params['fit.package'] <- NULL
  xbg.params['fit.algorithm'] <- NULL

  if (is.null(xbg.params$objective)) xbg.params$objective <- "reg:linear"
  # if (is.null(xbg.params$objective)) xbg.params$objective <- "reg:logistic"

  if (is.null(xbg.params$booster)) xbg.params$booster <- "gbtree" ## gbtree or gblinear
  # if (is.null(xbg.params$nthreads)) xbg.params$nthreads <- 1
  ## additional parameters for 'gblinear': lambda = 0L, alpha = 0L ## regularization

  # if (is.null(xbg.params$silent)) xbg.params$silent <- ifelse(gvars$verbose, 0, 1)

  nrounds <- xbg.params$nrounds
  if (is.null(nrounds)) nrounds <- 200
  xbg.params['nrounds'] <- NULL

  ## NOT USED YET:
  callbacks <- xbg.params$callbacks
  xbg.params['callbacks'] <- NULL

  metrics <- xbg.params$metrics
  if (is.null(metrics)) metrics <- list("rmse")
  xbg.params['metrics'] <- NULL

  if (nrow(train_data) == 0L) {
    model.fit <- list()
    # model.fit$coef = rep.int(NA_real_, ncol(Xmat))
    # names(model.fit$coef) <- colnames(Xmat)
  } else {
    # SuppressGivenWarnings({
      model.fit <- xgboost::xgb.train(xbg.params, train_data,
                                      # , watchlist=list(train = train_data, test = validation_data),
                                      # early_stopping_rounds = 3,
                                      metrics = metrics,
                                      nrounds = nrounds,
                                      verbose = ifelse(gvars$verbose, 1, 0))
      # preds <- predict(logist_reg, dtrain)
    # }, GetWarningsToSuppress())
  }
  nobs <- nrow(train_data)

  return(create_fit_object(model.fit, model_alg = "gbm", fitfunname = "xgb.train",
                           params = params, coef = NULL, nobs = nobs, model_contrl = xbg.params,
                           fitclass = "XGBoostmodel"))
}

# # S3 method for glm binomial family fit, takes BinDat data object:
# fit.xgb.cv <- function(fit.class, params, Xmat, Yvals, model_contrl, ...) {
#   if (gvars$verbose) print("calling glm.fit...")
#   if (nrow(Xmat) == 0L) {
#     model.fit <- list()
#     model.fit$coef = rep.int(NA_real_, ncol(Xmat))
#     names(model.fit$coef) <- colnames(Xmat)
#   } else {
#     ctrl <- glm.control(trace = FALSE)
#     SuppressGivenWarnings({
#       model.fit <- stats::glm.fit(x = Xmat,
#                                   y = Yvals,
#                                   family = gaussian() ,
#                                   control = ctrl)
#     }, GetWarningsToSuppress())
#   }

#   # print(object.size(model.fit), units = "Kb")

#   return(create_fit_object(model.fit, model_alg = "gbm", fitfunname = "xgb.train",
#                            params = params, nobs = nobs, model_contrl = xbg.params,
#                            fitclass = "XGBoostmodel"))
# }

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


predictP1.XGBoostmodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict_model_names, ...) {
  pred_dmat <- getPredictXGBDMat(m.fit, ParentObject, DataStorageObject, subset_idx)

  models_list <- m.fit$fitted_models_all
  if (!missing(predict_model_names) && !is.null(predict_model_names)) models_list <- models_list[predict_model_names]

  pAoutDT <- rep.int(list(numeric()), length(models_list))
  names(pAoutDT) <- names(models_list)

  if (nrow(pred_dmat) > 0) {
    # pAoutDT <- NULL

    for (idx in seq_along(models_list)) {
      ## will generally return a vector, needs to be put into a corresponding column of a data.table
      # pAoutDT[, (names(models_list)[idx]) := predict(models_list[[idx]], pred_dmat)]

      pAoutDT[[names(models_list)[idx]]] <- predict(models_list[[idx]], pred_dmat)
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
    fit.class = c("gbm", "GridLearner"),
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
      class(self$fit.class) <- c(class(self$fit.class), "xgb.train")
      ## class(self$fit.class) <- c(class(self$fit.class), self$fit.class)



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
        message("unable to run " %+% self$fit.class %+% " with h2o for: intercept only models or designmat with zero rows or  constant outcome (y) ...")
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

      self$model.fit <- try(fit(self$fit.class, self$params, train_data = train_dmat,
                                model_contrl = self$model_contrl,
                                fold_column = self$fold_column,
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

    # predictP1_out_of_sample_cv = function(validation_data, subset_idx, predict_model_names) {
    #   P1_DT <- predict_out_of_sample_cv(self$model.fit,
    #                                     ParentObject = self,
    #                                     validation_data = validation_data,
    #                                     subset_idx = subset_idx,
    #                                     predict_model_names = predict_model_names)
    #   return(P1_DT)
    # },

    getmodel_byname = function(model_names, model_IDs) {
      if (!missing(model_names)) {
        return(self$model.fit$fitted_models_all[model_names])
      } else {
        if (missing(model_IDs)) stop("Must provide either 'model_names' or 'model_IDs'.")
        # return(lapply(model_IDs, h2o::h2o.getModel))
      }
    },

    get_best_model_params = function(model_names) {
      model_obj <- self$getmodel_byname(model_names[1])[[1]]

      # Top model is always going to be a call for training a single final gbm (no CV is needed)
      top_params <- list(fit.package = self$model_contrl$fit.package,
                         fit.algorithm = "xgb.train")

      # browser()
      # str(model_obj)
      # model_obj$call

      ## params arg of xgb.train:
      top_params <- c(top_params, model_obj$params)

      ## ntrees (nrounds) argument will need special treatment (with early stopping and CV / validation data):
      ## see model_obj$evaluation_log for model history by iteration/round and performance for each round
      top_params$nrounds <- model_obj$niter
      top_params$callbacks <- model_obj$callbacks

      # attributes(model_obj)


      ## -------------------------------------------------
      ## From h2o:
      ## -------------------------------------------------
      # top_params_tmp <- model_obj@allparameters ## alternative is to grab the exact params used in the model, including defaults
      # top_params_tmp$model_id <- NULL
      # top_params_tmp$training_frame <- NULL
      # top_params_tmp$validation_frame <- NULL
      # top_params_tmp$x <- NULL
      # top_params_tmp$y <- NULL
      # top_params_tmp$nfolds <- NULL
      # top_params_tmp$fold_column <- NULL
      # top_params_tmp$fold_assignment <- NULL

      # top_params_tmp$score_each_iteration <- NULL # deeplearning fails otherwise
      # top_params <- c(top_params, top_params_tmp)
      # stop("Not implemented")

      return(top_params)
    },

    setdata = function(data, subset_idx, ...) {
      outvar <- self$outvar
      predvars <- self$predvars
      assert_that(is.DataStorageClass(data))

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

        # browser()

        load_subset_t <- system.time({
          Yvals <- data$get.outvar(subset_idx, outvar) # Always a vector

          ## gives an error when all columns happen to be integer type:
          # fit_dmat <- xgboost::xgb.DMatrix(as.matrix(data$dat.sVar[subset_idx, predvars, with = FALSE]), label = Yvals)
          ## still gives the same error:
          # fit_dmat <- xgboost::xgb.DMatrix(data.matrix(data$dat.sVar[subset_idx, predvars, with = FALSE]), label = Yvals)

          # Another option is to stop at data.matrix and pass it directly to xgboost, passing Y's as separate arg

          Xmat <- as.matrix(data$dat.sVar[subset_idx, predvars, with = FALSE])
          if (is.integer(Xmat)) Xmat[,1] <- as.numeric(Xmat[,1])
          fit_dmat <- xgboost::xgb.DMatrix(Xmat, label = Yvals)

        })
        # if (gvars$verbose) {
          print("time to create xgb.DMatrix: "); print(load_subset_t)
        # }
      }

      return(fit_dmat)
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

    # get_train_H2Oframe = function() {private$train_H2Oframe},
    # get_train_H2Oframe_ID = function() {private$train_H2Oframe_ID},

    # get_valid_H2Oframe = function() {private$valid_H2Oframe},
    # get_valid_H2Oframe_ID = function() {private$valid_H2Oframe_ID},

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

  # private = list(
  #   train_H2Oframe = NULL,
  #   train_H2Oframe_ID = NULL,
  #   valid_H2Oframe = NULL,
  #   valid_H2Oframe_ID = NULL
  # )
)
