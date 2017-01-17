#' Generate model names / IDs
#'
#' @param params Original list of parametes for this model (outcome variable name, predictor names, stratification criteria and model id)
#' @param H2O.model.object H2O model object (if used)
#' @param model_algorithm The name of the modeling algorithm
#' @param name Additional name previously specified in \code{model_contrl} list
#' @export
assign_model_name_id <- function(params, H2O.model.object, model_algorithm, name = NULL) {
  if (!missing(H2O.model.object) && inherits(H2O.model.object, "H2OModel")) {
    model_ids <- list(H2O.model.object@model_id)
  } else {
    model_ids <- list(model_algorithm)
  }
  new_names <- model_algorithm

  if (!is.null(name)) new_names <- new_names %+% "." %+% name
  if (!is.null(params[["Model_idx"]])) new_names <- "m." %+% params[["Model_idx"]] %+% "." %+% new_names

  names(model_ids) <- new_names
  return(model_ids)
}

#' Create a model fit list
#'
#' @param model.fit Model fit object
#' @param model_alg Name of the model algorithm
#' @param fitfunname The name of the main function used for fitting (e.g., "glm")
#' @param params List of main model parameters
#' @param coef Fitted glm coefficients, if used
#' @param nobs Number of observations used for fitting the model
#' @param model_contrl Full list of model control parameters used
#' @param fitclass The name of the R6 object class used for fitting
#' @param ... Additional objects that will be included in the final fit list
#' @export
create_fit_object <- function(model.fit, model_alg, fitfunname, params, coef, nobs, model_contrl, fitclass = "H2Omodel", ...) {
  modelfits_all <- vector(mode = "list", length = 1)
  modelfits_all[[1]] <- model.fit

  model_ids <- assign_model_name_id(params, model.fit, model_alg, model_contrl$name)
  names(modelfits_all) <- names(model_ids)

  extra_params <- list(...)

  fit <- list(
    params = params,
    coef = coef,
    fitfunname = fitfunname,
    model_algorithms = list(model_alg),
    nobs =  nobs,
    model_ids = model_ids,
    modelfits_all = modelfits_all)

  if (length(extra_params) > 0) fit <- c(fit, extra_params)

  class(fit) <- c(class(fit)[1], fitclass)
  return(fit)
}

#' Create a list with main model parameters
#'
#' @param reg RegressionClass Object
#' @export
create_fit_params <- function(reg) {
  return(list(outvar = reg$outvar, predvars = reg$predvars, stratify = reg$subset_exprs[[1]], Model_idx = reg$Model_idx))
}

#----------------------------------------------------------------------------------
# Class that defines the same type of models for regression problem E[Y|X]
#----------------------------------------------------------------------------------
#' S3 methods for printing model fit summary for PredictionModel R6 class object
#'
#' Prints the modeling summaries
#' @param model The model fit object produced by functions \code{get_fit}.
#' @param model_stats Also print some model summaries?
#' @param all_fits Print all of the modeling fits contained in this object? Warning: this may produce a lot of output!
#' @param ... Additional options passed on to \code{print.PredictionModel}.
#' @export
print.PredictionModel <- function(model, model_stats = FALSE, all_fits = FALSE, ...) {
  model$show(model_stats = model_stats, all_fits = all_fits)
  return(invisible(NULL))
}

# summary.PredictionModel <- function(model, ...) {
#   return(model$summary())
#   # return(invisible(NULL))
# }

## ---------------------------------------------------------------------
#' R6 class for fitting and making predictions for a single or a grid of regression models E(outvar | predvars)
#'
#' This R6 class can request, store and manage the design matrix Xmat, as well as the outcome Y
#'  The class has methods that perform queries to data storage R6 class DataStorageClass to get appropriate data columns & row subsets
#'
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' \itemize{
#' \item{GLMpackage} - Controls which package will be used for performing model fits (\code{glm} or \code{speedglm}).
#' \item{ModelFitObject} - Pointer to an instance of \code{ModelFitObject} class that contains the data.
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{new(reg)}}{Uses \code{reg} R6 \code{\link{RegressionClass}} object to instantiate a new model for a
#'   logistic regression with binary outcome.}
#'   \item{\code{show()}}{Print information on outcome and predictor names used in this regression model}
#'   \item{\code{fit()}}{...}
#'   \item{\code{copy.fit()}}{...}
#'   \item{\code{predict()}}{...}
#'   \item{\code{copy.predict()}}{...}
#'   \item{\code{predictAeqa()}}{...}
#' }
#' @importFrom assertthat assert_that is.flag
#' @export
PredictionModel  <- R6Class(classname = "PredictionModel",
  cloneable = TRUE,
  portable = TRUE,
  class = TRUE,
  public = list(
    # classify = FALSE,
    reg = NULL,
    Model_idx = integer(),
    outvar = character(),   # outcome name(s)
    predvars = character(), # names of predictor vars
    runCV = logical(),
    is.fitted = FALSE,
    nodes = NULL,
    OData_train = NULL, # object of class DataStorageClass used for training
    OData_valid = NULL, # object of class DataStorageClass used for scoring models (contains validation data)
    ModelFitObject = NULL, # object of class ModelFitObject that is used in fitting / prediction
    # best_refit_only = FALSE,
    BestModelFitObject = NULL,
    fit.package = character(),
    fit.algorithm = character(),
    grid.algorithm = character(),

    method = NA,             # model selection method used
    n_obs_fit = NA_integer_, # total number of observations used for fitting the model
    model_contrl = list(),
    useH2Oframe = FALSE,

    # subset_vars = NULL,     # THE VAR NAMES WHICH WILL BE TESTED FOR MISSINGNESS AND WILL DEFINE SUBSETTING
    subset_exprs = NULL,      # THE LOGICAL EXPRESSION (ONE) TO self$subset WHICH WILL BE EVALUTED IN THE ENVIRONMENT OF THE data
    # subset_idx = NULL,      # Logical vector of length n (TRUE = include the obs)
    # subset_train = NULL,
    ReplMisVal0 = logical(),

    initialize = function(reg, useH2Oframe = FALSE, ...) {
      self$method <- gvars$method # record the model selection method used ("none", "cv", "holdout")

      self$model_contrl <- reg$model_contrl
      self$useH2Oframe <- useH2Oframe
      self$reg <- reg
      self$runCV <- reg$runCV
      self$Model_idx <- reg$Model_idx

      self$fit.package <- reg$model_contrl$fit.package[1L]
      self$fit.algorithm <- reg$model_contrl$fit.algorithm[1L]
      self$grid.algorithm <- reg$model_contrl$grid.algorithm[1L]

      assert_that(is.string(reg$outvar))
      assert_that(is.character(reg$predvars))

      if ("x" %in% names(self$model_contrl)) {
        new.x <- self$model_contrl[["x"]]
        message("over-riding default predictors with new ones: " %+% paste0(new.x, collapse=","))
        assert_that(is.character(new.x))
        reg$predvars <- new.x
      }

      self$outvar <- reg$outvar
      self$predvars <- reg$predvars

      # self$subset_vars <- reg$subset_vars[[1]]
      self$subset_exprs <- reg$subset_exprs[[1]]
      assert_that(length(self$subset_exprs) <= 1)
      self$ReplMisVal0 <- reg$ReplMisVal0

      # if (is.null(reg$subset_vars)) {self$subset_vars <- TRUE}
      # assert_that(is.logical(self$subset_vars) || is.character(self$subset_vars)) # is.call(self$subset_vars) ||

      self$ModelFitObject <- self$define_model_fit_object(self$fit.package, self$fit.algorithm, reg, self$useH2Oframe, ...)

      if (gvars$verbose) { print("New instance of " %+% class(self)[1] %+% " :"); self$show() }

      invisible(self)
    },

    define_model_fit_object = function(fit.package, fit.algorithm, reg, useH2Oframe, ...) {
      # ***************************************************************************
      # Add any additional options passed on to modeling functions as extra args
      # Calling the constructor for the fitting model class, dispatching on class name stored in fit.package
      # ***************************************************************************
      class(fit.package) <- fit.package
      ModelFitObject <- newFitModel(fit.package, fit.algorithm, reg, useH2Oframe, ...)

      return(ModelFitObject)
    },

    fit = function(overwrite = FALSE, data, validation_data = NULL, subset_exprs = NULL, ...) { # Move overwrite to a field? ... self$overwrite
      if (gvars$verbose) print("fitting the model: "); self$show()
      if (!overwrite) assert_that(!self$is.fitted) # do not allow overwrite of prev. fitted model unless explicitely asked

      ## save a pointer to training data class used for fitting
      self$OData_train <- data
      self$nodes <- self$OData_train$nodes

      ## save a pointer to validation data class used for scoring
      ## **** NOTE THAT AUTOMATIC SUBSETTING WILL NOT WORK FOR VALIDATION DATA ->
      ## **** VALIDATION DATA NEEDS TO BE ALREADY SPLIT APPROPRIATELY IF DOING SUBSETS
      if (!is.null(validation_data)) self$OData_valid <- validation_data

      if (is.null(subset_exprs)) subset_exprs <- self$subset_exprs

      subset_idx <- data$evalsubst(subset_exprs = subset_exprs)

      self$n_obs_fit <- length(subset_idx)
      model.fit <- self$ModelFitObject$fit(data, subset_idx = subset_idx, validation_data = validation_data, ...)

      # **********************************************************************
      # This should not be a fatal error, especially if we are doing a stack / ensemble and only some models have failed
      # Should be able to continue, after removing the failed models
      # **********************************************************************
      if (inherits(model.fit, "try-error")) {
        stop("running " %+% self$ModelFitObject$fit.class %+% " resulted in error...")
        # self$ModelFitObject <- glmModelClass$new(fit.algorithm = "GLM", fit.package = "speedglm", reg = reg, ...)
        # model.fit <- self$ModelFitObject$fit(data, subset_idx = subset_idx, ...)
      }

      self$is.fitted <- TRUE

      # **********************************************************************
      # to save RAM space when doing many stacked regressions wipe out all internal data:
      # **********************************************************************
      self$wipe.alldat
      return(invisible(self))
    },

    refit_best_model = function(data, subset_exprs = NULL, ...) {
      expand.dots <- list(...)

      if (gvars$verbose) print("refitting the best model: "); self$show()
      if (is.null(subset_exprs)) subset_exprs <- self$subset_exprs
      if ("subset_idx" %in% names(expand.dots)) {
        subset_idx <- data$evalsubst(subset_exprs = expand.dots[["subset_idx"]])
      } else {
        subset_idx <- data$evalsubst(subset_exprs = subset_exprs)
      }

      top_model_params <- self$get_best_model_params()
      top_model_name <- self$get_best_model_names(1)
      best_reg <- RegressionClass$new(outvar = self$outvar, predvars = self$predvars, model_contrl = top_model_params)
      self$BestModelFitObject <- self$define_model_fit_object(top_model_params$fit.package, top_model_params$fit.algorithm, best_reg, useH2Oframe = self$useH2Oframe)

      self$n_obs_fit <- length(subset_idx)
      model.fit <- self$BestModelFitObject$fit(data, subset_idx = subset_idx, destination_frame = "alldata_H2Oframe")

      if (inherits(model.fit, "try-error")) stop("refitting of the best model failed")
      # **********************************************************************
      # to save RAM space when doing many stacked regressions wipe out all internal data:
      # **********************************************************************
      self$wipe.alldat
      return(invisible(model.fit))
    },

    # Predict the response E[Y|newdata];
    predict = function(newdata, subset_exprs = NULL, predict_model_names = NULL, best_refit_only = FALSE, convertResToDT = TRUE, ...) {
      if (!self$is.fitted) stop("Please fit the model prior to attempting to make predictions.")
      if (is.null(subset_exprs) && !missing(newdata)) subset_exprs <- self$subset_exprs
      if (!missing(newdata)) subset_idx <- newdata$evalsubst(subset_exprs = subset_exprs)

      ## When missing newdata the predictions are for the training frame.
      ## No subset re-evaluation is needed (training frame was already subsetted by self$subset_exprs)
      if (best_refit_only && !is.null(self$BestModelFitObject)) {
        probA1 <- self$BestModelFitObject$predictP1(newdata, subset_idx = subset_idx)
      } else {
        probA1 <- self$ModelFitObject$predictP1(newdata, subset_idx = subset_idx, predict_model_names = predict_model_names)
      }

      if (convertResToDT) probA1 <- as.data.table(probA1)

      return(probA1)
    },

    # Predict the response E[Y|newdata] for out of sample observations  (validation set / holdouts);
    predict_out_of_sample = function(newdata, subset_exprs = NULL, predict_model_names, convertResToDT = TRUE, ...) {
      if (!self$is.fitted) stop("Please fit the model prior to attempting to make predictions.")
      if (is.null(subset_exprs)) subset_exprs <- self$subset_exprs

      if (missing(newdata) && self$runCV) {
        ## For CV with missing data use the default h2o/xgboost out-of-sample (holdout) predictions
        probA1 <- self$ModelFitObject$predictP1_out_of_sample_cv(predict_model_names = predict_model_names)

      } else if (missing(newdata) && !self$runCV) {
        ## For holdout validation use the validation data (if it is available)
        newdata <- self$OData_valid
        if (!is.null(newdata)) stop("Must supply the validation data for making holdout predictions")
        subset_idx <- newdata$evalsubst(subset_exprs = subset_exprs) # self$define.subset.idx(newdata)
        probA1 <- self$predict(newdata, subset_exprs, predict_model_names, ...)
      } else {
        subset_idx <- newdata$evalsubst(subset_exprs = subset_exprs) # self$define.subset.idx(newdata)
        if (self$runCV) {
          probA1 <- self$ModelFitObject$predictP1_out_of_sample_cv(validation_data = newdata, subset_idx = subset_idx, predict_model_names = predict_model_names)
        } else {
          probA1 <- self$ModelFitObject$predictP1(data = newdata, subset_idx = subset_idx, predict_model_names = predict_model_names)
        }
      }

      if (convertResToDT) probA1 <- as.data.table(probA1)

      return(probA1)
    },

    # Score models (so far only MSE) based on either out of sample CV model preds or validation data preds;
    score_models = function(validation_data, subset_exprs = NULL, OData_train = NULL, OData_valid = NULL, ...) {
      browser()

      if (!self$is.fitted) stop("Please fit the model prior to making predictions.")
      if (is.null(subset_exprs)) subset_exprs <- self$subset_exprs

      out_of_sample_preds_DT <- self$predict_out_of_sample(validation_data, subset_exprs, ...)

      if (!missing(validation_data)) {
        subset_idx <- validation_data$evalsubst(subset_exprs = subset_exprs)
        test_values <- validation_data$get.outvar(subset_idx, var = self$outvar)
        IDs <- validation_data$get.outvar(subset_idx, var = validation_data$nodes$IDnode)
      } else if (self$runCV) {
        subset_idx <- OData_train$evalsubst(subset_exprs = subset_exprs)
        test_values <- OData_train$get.outvar(subset_idx, var = self$outvar)
        IDs <- OData_train$get.outvar(subset_idx, var = OData_train$nodes$IDnode)
      } else if (!self$runCV) {
        if (is.null(OData_valid)) stop("Must either use CV or provide validation data for model scoring")
        subset_idx <- OData_valid$evalsubst(subset_exprs = subset_exprs)
        test_values <- OData_valid$get.outvar(subset_idx, var = self$outvar)
        IDs <- OData_valid$get.outvar(subset_idx, var = OData_valid$nodes$IDnode)
      }

      private$MSE <- self$evalMSE_byID(out_of_sample_preds_DT, test_values, IDs)

      ## save out of sample CV predictions for the best model
      private$out_of_sample_preds <- out_of_sample_preds_DT[, self$get_best_model_names(K = 1), with = FALSE]

      return(invisible(self))
    },

    # First evaluate the empirical loss by subject. Then average that loss across subjects
    evalMSE_byID = function(predsDT, test_values, IDs) {
      loss_fun_MSE <- function(yhat, y0) (yhat - y0)^2

      if (!self$is.fitted) stop("Please fit the model prior to evaluating MSE.")
      if (!is.vector(test_values)) stop("test_values must be a vector of outcomes.")


      ## 1. Evaluate the empirical loss at each person-time prediction (apply loss function to each row):
      resid_predsDT <- as.data.table(predsDT)[, lapply(.SD, loss_fun_MSE, test_values)][, ("subjID") := IDs]
      NA_predictions <- resid_predsDT[, lapply(.SD, function(x) any(is.na(x)))]
      nNA_predictions <- resid_predsDT[, lapply(.SD, function(x) sum(is.na(x)))]
      # system.time(resid_predsDT2 <- as.data.table(private$probA1[, ] - test_values)[, ("subjID") := IDs])
      setkeyv(resid_predsDT, cols = "subjID")


      ## 2. Evaluate the average loss for each person (average loss by rows within each subject)
      # str(self$ModelFitObject$model.fit$modelfits_all[[1]])
      # self$ModelFitObject$model.fit$grid_objects
      # fold_idx <- self$ModelFitObject$model.fit$modelfits_all[[1]]$folds
      # fold_idx2 <- self$ModelFitObject$model.fit$modelfits_all[[2]]$folds
      # fold_idx3 <- self$ModelFitObject$model.fit$modelfits_all[[3]]$folds
      # fold_idx4 <- self$ModelFitObject$model.fit$modelfits_all[[4]]$folds
      # for (fold_i in seq_along(fold_idx)) resid_predsDT[fold_idx[[fold_i]], ("fold") := fold_i]


      # 3A. Evaluate the mean, var, sd loss averaging at the subject level first, then averaging across subjects
      # mean_bysubj <- resid_predsDT[, lapply(.SD, mean, na.rm = TRUE), by = subjID]
      # mean_bysubj[, subjID := NULL]
      # n <- nrow(mean_bysubj)
      # MSE_mean <- as.list(mean_bysubj[, lapply(.SD, mean, na.rm = TRUE)])
      # RMSE_mean <- lapply(MSE_mean, sqrt)
      # MSE_var <- as.list(mean_bysubj[, lapply(.SD, var, na.rm = TRUE)])
      # MSE_sd <- as.list(mean_bysubj[, lapply(.SD, sd, na.rm = TRUE)] * (1 / sqrt(n)))


      # 3B. Evaluate the mean, var, SD loss averaging across all rows of the data
      resid_predsDT[, subjID := NULL]
      n <- nrow(resid_predsDT)
      MSE_mean <- as.list(resid_predsDT[, lapply(.SD, mean, na.rm = TRUE)])
      RMSE_mean <- lapply(MSE_mean, sqrt)
      MSE_var <- as.list(resid_predsDT[, lapply(.SD, var, na.rm = TRUE)])
      MSE_sd <- as.list(resid_predsDT[, lapply(.SD, sd, na.rm = TRUE)] * (1 / sqrt(n)))


      if (any(as.logical(NA_predictions)))
          warning("Some of the test set predictions of the following model fits were missing (NA) and hence were excluded from MSE evaluation.
  Note that this may lead to misleading & erroneous assessment of the model performance.
  These are the models with missing predictions: " %+%
                    paste0(names(NA_predictions)[as.logical(NA_predictions)], collapse = ",") %+% ".
  This is the number of missing (NA) test set predictions per model: " %+% paste0(as.integer(nNA_predictions)[as.logical(NA_predictions)], collapse = ",")
                   )

      return(list(MSE_mean = MSE_mean, RMSE_mean = RMSE_mean, MSE_var = MSE_var, MSE_sd = MSE_sd))
    },

    ## ------------------------------------------------------------------------------
    ## return a model object by name / ID
    ## ------------------------------------------------------------------------------
    getmodel_byname = function(model_names, model_IDs) {
      return(self$ModelFitObject$getmodel_byname(model_names, model_IDs))
    },

    ## ------------------------------------------------------------------------------
    ## return top K models based on smallest validation / test MSE
    ## ------------------------------------------------------------------------------
    get_best_MSEs = function(K = 1) {
      if (!self$is.fitted) stop("Please fit the model prior to calling get_best_models()")

      if (!is.integerish(K)) stop("K argument must be an integer <= the total number of models in this ensemble")
      if (K > length(self$getmodel_ids)) {
        message("K value exceeds the total number of models; K is being truncated to " %+% length(self$getmodel_ids))
        K <- length(self$getmodel_ids)
      }

      if (is.null(self$getMSE)) stop("The validation / holdout MSE has not been evaluated, making model model ranking impossible.
  Please call evalMSE_byID() and provide a vector of validation / test values.")

      ## ***********************************
      ## This throws everything off, since the model may not be uniquely idenfified.
      ## Need to use MSE as locate the model best K models adresses
      ## ***********************************
      return(sort(unlist(self$getMSE))[1:K])
    },

    ## ------------------------------------------------------------------------------
    ## return top K model object names
    ## ------------------------------------------------------------------------------
    get_best_model_names = function(K = 1) {
      if (length(self$getmodel_ids) == 1) {
        return(names(self$getmodel_ids))
      } else {
        return(names(self$get_best_MSEs(K)))
      }
    },

    ## ------------------------------------------------------------------------------
    ## return top K model objects ranked by prediction MSE on a holdout (CV) fold
    ## ------------------------------------------------------------------------------
    get_best_models = function(K = 1) {
      top_model_names <- self$get_best_model_names(K)
      # if (gvars$verbose) message("fetching top " %+% K %+% " models ranked by the smallest holdout / validation MSE")
      return(self$getmodel_byname(top_model_names))
    },

    # ------------------------------------------------------------------------------
    # return the parameters of the top K models as a list (ranked by prediction MSE on a holdout (CV) fold)
    # ------------------------------------------------------------------------------
    get_best_model_params = function(K = 1) {
      top_model_names <- self$get_best_model_names(K)
      return(self$ModelFitObject$get_best_model_params(top_model_names))
    },

    # ------------------------------------------------------------------------------
    # return a data.table with best mean MSEs, including SDs & corresponding model names
    # ------------------------------------------------------------------------------
    get_best_MSE_table = function(K = 1) {
      top_MSE_CV <- self$get_best_MSEs(K)
      top_model_names <- names(top_MSE_CV)
      top_model_pos <- unlist(lapply(top_model_names, function(model_n) which(names(self$getmodel_ids) %in% model_n)))
      top_model_ids <- unlist(self$getmodel_ids[top_model_names])
      if (is.null(top_model_ids)) top_model_ids <- rep.int(NA, length(top_MSE_CV))

      ## switch to data.table::data.table:
      datMSE <- data.table::data.table(model = names(self$getmodel_ids[top_model_pos]),
                           algorithm = unlist(self$getmodel_algorithms[top_model_pos]),
                           MSE = unlist(self$getMSE[top_model_pos]),
                           RMSE = unlist(self$getRMSE[top_model_pos]),
                           MSE.sd = unlist(self$getMSEsd[top_model_pos]),
                           model.id = top_model_ids,
                           model.pos = top_model_pos
                           )

      datMSE[["CIlow"]] <- datMSE[["MSE"]] - 1.96 * datMSE[["MSE.sd"]]
      datMSE[["CIhi"]] <- datMSE[["MSE"]] + 1.96 * datMSE[["MSE.sd"]]
      datMSE[["model"]] <- factor(datMSE[["model"]], levels = datMSE[["model"]][order(datMSE[["MSE"]])])
      # rownames(datMSE) <- NULL
      return(datMSE)
    },

    # ------------------------------------------------------------------------------
    # return a data.table of grid model fits with parameters, sorted by internal test MSE
    # ------------------------------------------------------------------------------
    get_modelfits_grid = function() {
      return(self$ModelFitObject$get_modelfits_grid())
    },

    # Output info on the general type of regression being fitted:
    show = function(print_format = TRUE, model_stats = FALSE, all_fits = FALSE) {
      if (print_format) {
        cat("model: E[" %+% self$outvar %+% "|" %+% paste(self$predvars, collapse=", ") %+% "]" %+% ";\\ Stratify: " %+% self$subset_exprs %+% "\n")
        cat("fit.package: " %+% self$fit.package %+% "\n")
        cat("fit.algorithm: " %+% self$fit.algorithm %+%"\n")
        cat("grid.algorithm: " %+% self$grid.algorithm %+%"\n")
        cat("N: " %+% self$n_obs_fit %+%"\n")
        cat("method: " %+% self$method %+%"\n")

        if (self$is.fitted && model_stats) self$ModelFitObject$show(all_fits = all_fits)

        return(invisible(NULL))

      } else {
        return(list(outvar = self$outvar, predvars = self$predvars, stratify = self$subset_exprs, fit.package = self$fit.package, fit.algorithm = self$fit.algorithm))
      }
    },

    summary = function(all_fits = FALSE) {
      return(self$ModelFitObject$summary(all_fits = all_fits))
    }
  ),

  active = list(
    wipe.alldat = function() {
      private$probA1 <- NULL
      self$ModelFitObject$emptydata
      self$ModelFitObject$emptyY
      if (!is.null(self$BestModelFitObject)) {
        self$BestModelFitObject$emptydata
        self$BestModelFitObject$emptyY
      }
      return(self)
    },

    wipe.allmodels = function() {
      self$ModelFitObject$wipe.allmodels
    },

    wipe.allOData = function() {
      self$OData_train <- NULL # object of class DataStorageClass used for training
      self$OData_valid <- NULL # object of class DataStorageClass used for scoring models (contains validation data)
    },

    emptymodelfit = function() { self$ModelFitObject$emptymodelfit },
    getprobA1 = function() { private$probA1 },
    get_out_of_sample_preds = function() { private$out_of_sample_preds },
    # getsubset = function() { self$subset_idx },
    getoutvarnm = function() { self$outvar },
    getoutvarval = function() { self$ModelFitObject$getY },

    getMSEtab = function() {
      MSE_list <- self$getMSE
      RMSE_list <- self$getRMSE
      MSE.sd_list <- self$getMSEsd

      if (length(MSE_list) == 0L) {
        warning("It looks like the CV/holdout MSEs have not been evaluated for the model calling order: " %+% self$Model_idx %+%
             ". Cannot make prediction or select the best model unless some model selection criteria is specified during fit() call.
             Please make sure the argument 'method' is set to either 'cv' or 'holdout'.")
        MSE_list <- MSE.sd_list <- RMSE_list <- rep.int(list(NA), length(self$getmodel_ids))
        names(MSE_list) <- names(MSE.sd_list) <- names(RMSE_list) <- names(self$getmodel_ids)
      }

      data.table::data.table(
        MSE = unlist(MSE_list),
        MSE.sd = unlist(MSE.sd_list),
        RMSE = unlist(RMSE_list),
        model = names(MSE_list),
        Model_idx = self$Model_idx,
        order = seq_along(MSE_list)
      )
    },

    getMSE = function() { private$MSE[["MSE_mean"]] },
    getRMSE = function() { private$MSE[["RMSE_mean"]] },
    getMSEvar = function() { private$MSE[["MSE_var"]] },
    getMSEsd = function() { private$MSE[["MSE_sd"]] },
    getfit = function() { self$ModelFitObject$model.fit },
    getRetrainedfit = function() { self$BestModelFitObject$model.fit },
    getmodel_ids = function() { self$ModelFitObject$getmodel_ids },
    getmodel_algorithms = function() { self$ModelFitObject$getmodel_algorithms }
  ),
  private = list(
    # model.fit = list(),   # the model fit (either coefficients or the model fit object)
    MSE = list(),
    probA1 = NULL,    # Predicted probA^s=1 conditional on Xmat
    out_of_sample_preds = NULL,
    probAeqa = NULL   # Likelihood of observing a particular value A^s=a^s conditional on Xmat
  )
)
