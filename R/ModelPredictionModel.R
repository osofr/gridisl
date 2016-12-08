assign_model_name_id <- function(H2O.model.object, model_algorithm, name = NULL) {
  if (!missing(H2O.model.object) && inherits(H2O.model.object, "H2OModel")) {
    model_ids <- list(H2O.model.object@model_id)
  } else {
    model_ids <- list(model_algorithm)
  }

  new_names <- model_algorithm
  if (!is.null(name)) new_names <- new_names %+% "." %+% name
  names(model_ids) <- new_names

  return(model_ids)
}

create_fit_object <- function(model.fit, model_alg, fitfunname, params, coef, nobs, model_contrl, fitclass = "H2Omodel", ...) {
  fitted_models_all <- vector(mode = "list", length = 1)
  fitted_models_all[[1]] <- model.fit

  model_ids <- assign_model_name_id(model.fit, model_alg, model_contrl$name)
  names(fitted_models_all) <- names(model_ids)

  extra_params <- list(...)

  fit <- list(
    params = params,
    coef = coef,
    fitfunname = fitfunname,
    model_algorithms = list(model_alg),
    nobs =  nobs,
    model_ids = model_ids,
    fitted_models_all = fitted_models_all)

  if (length(extra_params) > 0) fit <- c(fit, extra_params)

  class(fit) <- c(class(fit)[1], fitclass)
  return(fit)
}

create_fit_params <- function(reg) {
  return(list(outvar = reg$outvar, predvars = reg$predvars, stratify = reg$subset_exprs[[1]]))
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
    outvar = character(),   # outcome name(s)
    predvars = character(), # names of predictor vars
    is.fitted = FALSE,
    OData_train = NULL, # object of class DataStorageClass used for training
    OData_valid = NULL, # object of class DataStorageClass used for scoring models
    ModelFitObject = NULL, # object of class ModelFitObject that is used in fitting / prediction
    use_best_retrained_model = FALSE,
    BestModelFitObject = NULL,
    fit.package = character(),
    fit.algorithm = character(),
    model_contrl = list(),

    subset_vars = NULL,     # THE VAR NAMES WHICH WILL BE TESTED FOR MISSINGNESS AND WILL DEFINE SUBSETTING
    subset_exprs = NULL,     # THE LOGICAL EXPRESSION (ONE) TO self$subset WHICH WILL BE EVALUTED IN THE ENVIRONMENT OF THE data
    subset_idx = NULL,      # Logical vector of length n (TRUE = include the obs)
    subset_train = NULL,
    ReplMisVal0 = logical(),

    initialize = function(reg, ...) {
      self$model_contrl <- reg$model_contrl

      if ("fit.package" %in% names(self$model_contrl)) {
        self$fit.package <- self$model_contrl[['fit.package']]
        assert_that(is.character(self$fit.package))
      } else {
        self$fit.package <- reg$fit.package[1]
      }
      if (!(self$fit.package %in% allowed.fit.package)) stop("fit.package must be one of: " %+% paste0(allowed.fit.package, collapse=", "))

      if ("fit.algorithm" %in% names(self$model_contrl)) {
        self$fit.algorithm <- self$model_contrl[['fit.algorithm']]
        assert_that(is.character(self$fit.algorithm))
      } else {
        self$fit.algorithm <- reg$fit.algorithm[1]
      }
      if (!(self$fit.algorithm %in% allowed.fit.algorithm)) stop("fit.algorithm must be one of: " %+% paste0(allowed.fit.algorithm, collapse=", "))

      assert_that(is.string(reg$outvar))
      self$outvar <- reg$outvar
      assert_that(is.character(reg$predvars))
      self$predvars <- reg$predvars
      self$subset_vars <- reg$subset_vars[[1]]
      self$subset_exprs <- reg$subset_exprs[[1]]
      assert_that(length(self$subset_exprs) <= 1)
      self$ReplMisVal0 <- reg$ReplMisVal0

      if (is.null(reg$subset_vars)) {self$subset_vars <- TRUE}
      assert_that(is.logical(self$subset_vars) || is.character(self$subset_vars)) # is.call(self$subset_vars) ||

      self$ModelFitObject <- self$define_model_fit_object(self$fit.package, self$fit.algorithm, reg, ...)

      if (gvars$verbose) {
        print("New instance of " %+% class(self)[1] %+% " :"); self$show()
      }
      invisible(self)
    },

    define_model_fit_object = function(fit.package, fit.algorithm, reg, ...) {
      # ***************************************************************************
      # Add any additional options passed on to modeling functions as extra args
      # ****** NOTE: This needs to be changed to S3 dispatch for greater flexibility ******
      # ***************************************************************************
      if (fit.package %in% c("h2o", "h2oEnsemble")) {
        if (fit.algorithm %in% "GridLearner") {
          ModelFitObject <- h2oGridModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)
        } else if (fit.algorithm %in% "ResidGridLearner") {
          ModelFitObject <- h2oResidualModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)
        } else {
          ModelFitObject <- h2oModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)
        }
      } else if (fit.package %in% c("face")) {
        ModelFitObject <- faceModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)
        fit.algorithm <- NULL
        # fit.algorithm <- fit.package
      } else if (fit.package %in% c("brokenstick")) {
        ModelFitObject <- brokenstickModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)
        fit.algorithm <- NULL
        # fit.algorithm <- fit.package
      } else {
        ModelFitObject <- glmModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)
      }
      return(ModelFitObject)
    },

    fit = function(overwrite = FALSE, data, validation_data = NULL, ...) { # Move overwrite to a field? ... self$overwrite
      if (gvars$verbose) print("fitting the model: "); self$show()
      if (!overwrite) assert_that(!self$is.fitted) # do not allow overwrite of prev. fitted model unless explicitely asked

      ## save a pointer to training data class used for fitting
      self$OData_train <- data
      ## save a pointer to validation data class used for scoring
      if (!is.null(validation_data)) self$OData_valid <- validation_data

      self$define.subset.idx(data)
      self$subset_train <- self$subset_idx
      model.fit <- self$ModelFitObject$fit(data, subset_idx = self$subset_idx, validation_data = validation_data, ...)

      if (inherits(model.fit, "try-error")) {
        message("running " %+% self$ModelFitObject$fit.class %+% " with h2o has failed, trying speedglm as a backup...")
        self$ModelFitObject <- glmModelClass$new(fit.algorithm = "GLM", fit.package = "speedglm", reg = reg, ...)
        # self$outvar, self$predvars,
        model.fit <- self$ModelFitObject$fit(data, subset_idx = self$subset_idx, ...)
      }

      self$is.fitted <- TRUE

      # **********************************************************************
      # to save RAM space when doing many stacked regressions wipe out all internal data:
      # **********************************************************************
      self$wipe.alldat
      return(invisible(self))
    },

    refit_best_model = function(data, ...) {
      if (gvars$verbose) print("refitting the best model: "); self$show()
      self$define.subset.idx(data)

      ## reset the training data to be all data for future prediction with missing newdata
      # self$OData_train <- data
      ## reset the subset to include all data that was used for retraining (for automatic future prediction with missing newdata)
      # self$subset_train <- self$subset_idx

      top_model_params <- self$get_best_model_params()
      best_reg <- RegressionClass$new(outvar = self$outvar, predvars = self$predvars, model_contrl = top_model_params)
      self$BestModelFitObject <- self$define_model_fit_object(top_model_params$fit.package, top_model_params$fit.algorithm, best_reg)
      # self$outvar, self$predvars,
      model.fit <- self$BestModelFitObject$fit(data, subset_idx = self$subset_idx, destination_frame = "alldata_H2Oframe", ...)
      if (inherits(model.fit, "try-error")) stop("refitting of the best model failed")

      # **********************************************************************
      # to save RAM space when doing many stacked regressions wipe out all internal data:
      # **********************************************************************
      # self$wipe.alldat
      return(invisible(model.fit))
    },

    # Predict the response E[Y|newdata];
    predict = function(newdata, subset_vars, subset_exprs, predict_model_names, MSE = TRUE, ...) {
      if (!self$is.fitted) stop("Please fit the model prior to attempting to make predictions.")

      # if (missing(newdata) && is.null(private$probA1)) {
      if (missing(newdata)) {
        private$probA1 <- self$ModelFitObject$predictP1(subset_idx = self$subset_train, predict_model_names = predict_model_names)
      } else {
        if (!missing(subset_vars)) self$subset_vars <- subset_vars
        if (!missing(subset_exprs)) self$subset_exprs <- subset_exprs
        self$define.subset.idx(newdata)

        if (!self$use_best_retrained_model) {
          private$probA1 <- self$ModelFitObject$predictP1(data = newdata, subset_idx = self$subset_idx, predict_model_names = predict_model_names)
        } else {
          private$probA1 <- self$BestModelFitObject$predictP1(data = newdata, subset_idx = self$subset_idx)
        }
      }
      if (MSE && !self$use_best_retrained_model) {
        test_values <- newdata$get.outvar(self$subset_idx, var = self$outvar)
        private$MSE <- self$evalMSE(test_values)
      }
      return(invisible(self))
    },

    # Predict the response E[Y|newdata] based for CV fold models and using validation data;
    score_CV = function(validation_data, MSE = TRUE, yvals, ...) {
      if (!self$is.fitted) stop("Please fit the model prior to making predictions.")
      if (missing(validation_data)) {
        private$probA1 <- self$ModelFitObject$score_CV()
      } else {
        self$define.subset.idx(validation_data)
        self$OData_valid <- validation_data # save a pointer to last used validation data object
        private$probA1 <- self$ModelFitObject$score_CV(validation_data = validation_data$dat.sVar)
      }
      if (MSE) {
        if (!missing(yvals)) {
          test_values <- yvals
        } else if (!missing(validation_data)) {
          test_values <- validation_data$get.outvar(self$subset_idx, var = self$outvar)
        } else {
          stop("must provide either test values (yvals) or validation data with test values when evaluating MSE")
        }
        private$MSE <- self$evalMSE(test_values)
      }
      return(invisible(self))
    },

    evalMSE = function(test_values) {
      if (!self$is.fitted) stop("Please fit the model prior to evaluating MSE.")
      if (!is.vector(test_values)) stop("test_values must be a vector of outcomes.")

      n <- length(self$subset_idx)

      MSE_mean <- MSE_var <- MSE_sd <- var <- vector(mode = "list", length = ncol(private$probA1));
      names(MSE_mean) <- names(MSE_var) <- names(MSE_sd) <- names(var) <- colnames(private$probA1)

      for (idx in 1:ncol(private$probA1)) {
        predVals <- private$probA1[self$subset_idx, idx]
        MSE_mean[[idx]] <- mean((predVals - test_values)^2, na.rm = TRUE)
        MSE_var[[idx]] <- var((predVals - test_values)^2, na.rm = TRUE)
        MSE_sd[[idx]] <- 1 / sqrt(n) * sd((predVals - test_values)^2, na.rm = TRUE)
        var[[idx]] <- var(predVals, na.rm = TRUE)
        if (any(is.na(predVals)))
          warning("Some of the predictions of the model id " %+% idx %+% " were missing (NA); these were excluded from MSE evaluation")
      }
      return(list(MSE_mean = MSE_mean, MSE_var = MSE_var, MSE_sd = MSE_sd, var = var))
    },

    # ------------------------------------------------------------------------------
    # return a model object by name / ID
    # ------------------------------------------------------------------------------
    getmodel_byname = function(model_names, model_IDs) {
      return(self$ModelFitObject$getmodel_byname(model_names, model_IDs))
    },

    # ------------------------------------------------------------------------------
    # return top K models based on smallest validation / test MSE
    # ------------------------------------------------------------------------------
    get_best_MSEs = function(K = 1) {
      if (!self$is.fitted) stop("Please fit the model prior to calling get_best_models()")
      if (!is.integerish(K)) stop("K argument must be an integer <= the total number of models")
      if (K > length(self$getmodel_ids)) {
        message("K value exceeds the total number of models; K is being truncated to " %+% length(self$getmodel_ids))
        K <- length(self$getmodel_ids)
      }
      if (is.null(self$getMSE)) stop("The validation / holdout MSE has not been evaluated, making model model ranking impossible.
  Please call evalMSE() and provide a vector of validation / test values.")
      return(sort(unlist(self$getMSE))[1:K])
    },

    # ------------------------------------------------------------------------------
    # return top K model object names
    # ------------------------------------------------------------------------------
    get_best_model_names = function(K = 1) { return(names(self$get_best_MSEs(K))) },

    # ------------------------------------------------------------------------------
    # return top K model objects ranked by prediction MSE on a holdout (CV) fold
    # ------------------------------------------------------------------------------
    get_best_models = function(K = 1) {
      top_model_names <- self$get_best_model_names(K)
      message("fetching top " %+% K %+% " models ranked by the smallest holdout / validation MSE")
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
    # return a data.frame with best mean MSEs, including SDs & corresponding model names
    # ------------------------------------------------------------------------------
    get_best_MSE_table = function(K = 1) {
      top_MSE_CV <- self$get_best_MSEs(K)
      top_model_names <- names(top_MSE_CV)
      top_model_ids <- self$getmodel_ids[top_model_names]
      top_model_pos <- unlist(lapply(top_model_names, function(model_n) which(names(self$getmodel_ids) %in% model_n)))

      datMSE <- data.frame(model = names(self$getmodel_ids[top_model_pos]),
                           algorithm = unlist(self$getmodel_algorithms[top_model_pos]),
                           MSE.CV = unlist(self$getMSE[top_model_pos]),
                           MSE.sd = unlist(self$getMSEsd[top_model_pos]),
                           model.id = unlist(top_model_ids),
                           model.pos = top_model_pos
                           )

      datMSE$CIlow <- datMSE$MSE.CV - 1.96*datMSE$MSE.sd
      datMSE$CIhi <- datMSE$MSE.CV + 1.96*datMSE$MSE.sd
      datMSE$model <- factor(datMSE$model, levels = datMSE$model[order(datMSE$MSE.CV)])
      rownames(datMSE) <- NULL
      return(datMSE)
    },

    define.subset.idx = function(data) {
      if (is.logical(self$subset_vars)) {
        subset_idx <- self$subset_vars
      } else if (is.call(self$subset_vars)) {
        stop("calls aren't allowed in ModelFitObject$subset_vars")
      } else if (is.character(self$subset_vars)) {
        subset_idx <- data$evalsubst(subset_vars = self$subset_vars, subset_exprs = self$subset_exprs)
      } else if (is.null(self$subset_vars)) {
        subset_idx <- data$evalsubst(subset_exprs = self$subset_exprs)
      }
      assert_that(is.logical(subset_idx))

      if ((length(subset_idx) < data$nobs) && (length(subset_idx) > 1L)) {
        stop("ModelFitObject$define.subset.idx: subset_idx must be logical vector of length 1 or length nobs; current length:" %+% length(subset_idx))
      }

      if (length(subset_idx) == 1L) {
        subset_idx <- rep.int(subset_idx, data$nobs)
        if (gvars$verbose) message("subset_idx has length 1; repeating subset_idx nobs times, for nobs: " %+% data$nobs)
      }
      assert_that(length(subset_idx) == data$nobs)
      self$subset_idx <- which(subset_idx)

      return(invisible(self))
    },

    # Output info on the general type of regression being fitted:
    show = function(print_format = TRUE, model_stats = FALSE, all_fits = FALSE) {
      if (print_format) {
        cat("Model: E[" %+% self$outvar %+% "|" %+% paste(self$predvars, collapse=", ") %+% "]" %+% ";\\ Stratify: " %+% self$subset_exprs %+% "\n")
        cat("fit.package: " %+% self$fit.package %+% "\n")
        cat("fit.algorithm: " %+% self$fit.algorithm %+%"\n")

        if (self$is.fitted && model_stats) {
          self$ModelFitObject$show(all_fits = all_fits)
        }
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
      # private$probA1 <- NULL
      # private$probAeqa <- NULL
      # self$subset_idx <- NULL
      self$ModelFitObject$emptydata
      self$ModelFitObject$emptyY
      return(self)
    },
    emptymodelfit = function() { self$ModelFitObject$emptymodelfit },
    getprobA1 = function() { private$probA1 },
    getsubset = function() { self$subset_idx },
    getoutvarnm = function() { self$outvar },
    getoutvarval = function() { self$ModelFitObject$getY },
    getMSE = function() { private$MSE[["MSE_mean"]] },
    getMSEvar = function() { private$MSE[["MSE_var"]] },
    getMSEsd = function() { private$MSE[["MSE_sd"]] },
    getvar = function() { private$MSE[["var"]] },
    getfit = function() { self$ModelFitObject$model.fit },
    getmodel_ids = function() { self$ModelFitObject$getmodel_ids },
    getmodel_algorithms = function() { self$ModelFitObject$getmodel_algorithms }
  ),
  private = list(
    # model.fit = list(),   # the model fit (either coefficients or the model fit object)
    MSE = list(),
    probA1 = NULL,    # Predicted probA^s=1 conditional on Xmat
    probAeqa = NULL   # Likelihood of observing a particular value A^s=a^s conditional on Xmat
  )
)
