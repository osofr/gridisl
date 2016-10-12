#----------------------------------------------------------------------------------
# Classes for modelling regression models with outcome Y ~ Xmat
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
#' R6 class for fitting and making predictions for a single outcome regression model P(Y | PredVars)
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
#' @section Active Bindings:
#' \describe{
#'   \item{\code{getoutvarnm}}{...}
#'   \item{\code{getoutvarval}}{...}
#'   \item{\code{getsubset}}{...}
#'   \item{\code{getprobA1}}{...}
#'   \item{\code{getfit}}{...}
#'   \item{\code{wipe.alldat}}{...}
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
    cont.sVar.flag = logical(),
    bw.j = numeric(),
    is.fitted = FALSE,

    ModelFitObject = NULL, # object of class ModelFitObject that is used in fitting / prediction, never saved (need to be initialized with $new())
    fit.package = character(),
    fit.algorithm = character(),
    model_contrl = list(),

    n = NA_integer_,        # number of rows in the input data
    subset_vars = NULL,     # THE VAR NAMES WHICH WILL BE TESTED FOR MISSINGNESS AND WILL DEFINE SUBSETTING
    subset_exprs = NULL,     # THE LOGICAL EXPRESSION (ONE) TO self$subset WHICH WILL BE EVALUTED IN THE ENVIRONMENT OF THE data
    subset_idx = NULL,      # Logical vector of length n (TRUE = include the obs)

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

      # ***************************************************************************
      # Add any additional options passed on to modeling functions as extra args
      # ****** NOTE: This needs to be changed to S3 dispatch for greater flexibility ******
      # ***************************************************************************
      # class(self$fit.algorithm) <- c(class(self$fit.algorithm), self$fit.package)
      if (self$fit.package %in% c("h2o", "h2oEnsemble")) {
        if (self$fit.algorithm %in% "GridLearner") {
          self$ModelFitObject <- h2oGridModelClass$new(fit.algorithm = self$fit.algorithm, fit.package = self$fit.package, reg = reg, ...)
        } else {
          self$ModelFitObject <- h2oModelClass$new(fit.algorithm = self$fit.algorithm, fit.package = self$fit.package, reg = reg, ...)
        }
      } else if (self$fit.package %in% c("face")) {
        self$ModelFitObject <- faceModelClass$new(fit.algorithm = self$fit.algorithm, fit.package = self$fit.package, reg = reg, ...)
        self$fit.algorithm <- NULL
      } else if (self$fit.package %in% c("brokenstick")) {
        self$ModelFitObject <- brokenstickModelClass$new(fit.algorithm = self$fit.algorithm, fit.package = self$fit.package, reg = reg, ...)
        self$fit.algorithm <- NULL
      } else {
        self$ModelFitObject <- glmModelClass$new(fit.algorithm = self$fit.algorithm, fit.package = self$fit.package, reg = reg, ...)
      }

      self$ModelFitObject$params <- list(outvar = self$outvar, predvars = self$predvars, stratify = self$subset_exprs)

      if (gvars$verbose) {
        print("New instance of " %+% class(self)[1] %+% " :"); self$show()
      }
      invisible(self)
    },

    # if (predict) then use the same data to make predictions for all obs in self$subset_idx;
    # store these predictions in private$probA1 and private$probAeqa
    fit = function(overwrite = FALSE, data, predict = FALSE, validation_data = NULL, ...) { # Move overwrite to a field? ... self$overwrite
      if (gvars$verbose) print("fitting the model: "); self$show()
      if (!overwrite) assert_that(!self$is.fitted) # do not allow overwrite of prev. fitted model unless explicitely asked

      self$define.subset.idx(data)
      model.fit <- self$ModelFitObject$fit(data, self$outvar, self$predvars, self$subset_idx, validation_data = validation_data, ...)

      if (inherits(model.fit, "try-error")) {
        message("running " %+% self$ModelFitObject$fit.class %+% " with h2o has failed, trying to run speedglm as a backup...")
        self$ModelFitObject <- glmModelClass$new(fit.algorithm = "GLM", fit.package = "speedglm", reg = reg, ...)
        self$ModelFitObject$params <- list(outvar = self$outvar, predvars = self$predvars, stratify = self$subset_exprs)
        model.fit <- self$ModelFitObject$fit(data, self$outvar, self$predvars, self$subset_idx, ...)
      }

      private$model.fit <- model.fit
      # str(private$model.fit)
      # names(private$model.fit)
      # private$model.fit$fitted_models_all


      self$is.fitted <- TRUE
      # **********************************************************************
      # to save RAM space when doing many stacked regressions wipe out all internal data:
      # **********************************************************************
      self$wipe.alldat
      invisible(self)
    },

    # Predict the response P(Bin = 1|sW = sw);
    # uses private$model.fit to generate predictions for data:
    predict = function(newdata, subset_vars, subset_exprs, MSE = TRUE, ...) {
      assert_that(self$is.fitted)
      if (missing(newdata) && is.null(private$probA1)) {
        private$probA1 <- self$ModelFitObject$predictP1(subset_idx = self$subset_idx)
      } else {
        if (!missing(subset_vars)) self$subset_vars <- subset_vars
        if (!missing(subset_exprs)) self$subset_exprs <- subset_exprs
        self$define.subset.idx(newdata)
        private$probA1 <- self$ModelFitObject$predictP1(data = newdata, subset_idx = self$subset_idx)
      }
      if (MSE) {
        test_values <- newdata$get.outvar(self$subset_idx, var = self$outvar)
        private$MSE <- self$evalMSE(test_values)
      }
      return(invisible(self))
    },

    evalMSE = function(test_values) {
      assert_that(self$is.fitted)
      assert_that(is.vector(test_values))

      MSE_mean <- MSE_var <- vector(mode = "list", length = ncol(private$probA1));
      names(MSE_mean) <- names(MSE_var) <- colnames(private$probA1)

      for (idx in 1:ncol(private$probA1)) {
        MSE_mean[[idx]] <- mean((private$probA1[self$subset_idx, idx] - test_values)^2, na.rm = TRUE)
        MSE_var[[idx]] <- var((private$probA1[self$subset_idx, idx] - test_values)^2, na.rm = TRUE)
        if (any(is.na(private$probA1[self$subset_idx, idx])))
          warning("Some of the predictions of the model id " %+% idx %+% " were missing (NA); these were excluded from MSE evaluation")
      }
      # private$MSE <- MSE
      return(list(MSE_mean = MSE_mean, MSE_var = MSE_var))
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
      self$subset_idx <- subset_idx
      return(invisible(self))
    },

    # take fitted PredictionModel class object as an input and save the fits to itself
    copy.fit = function(bin.out.model) {
      assert_that("PredictionModel" %in% class(bin.out.model))
      private$model.fit <- bin.out.model$getfit
      self$is.fitted <- TRUE
      invisible(self)
    },

    # take PredictionModel class object that contains the predictions for P(A=1|sW) and save these predictions to self$
    copy.predict = function(bin.out.model) {
      assert_that("PredictionModel" %in% class(bin.out.model))
      assert_that(self$is.fitted)
      private$probA1 <- bin.out.model$getprobA1
    },

    # Returns the object that contains the actual model fits (itself)
    # get.fits = function() {
    #   model.fit <- self$getfit
    #   return(list(model.fit))
    # },

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
      self$subset_idx <- NULL
      self$ModelFitObject$emptydata
      self$ModelFitObject$emptyY
      return(self)
    },
    getfit = function() { private$model.fit },
    getmodelnames = function() { self$getfit$modelnames },
    getprobA1 = function() { private$probA1 },
    getsubset = function() { self$subset_idx },
    getoutvarnm = function() { self$outvar },
    getoutvarval = function() { self$ModelFitObject$getY },
    getMSE = function() { private$MSE[["MSE_mean"]] },
    getMSEvar = function() { private$MSE[["MSE_var"]] }
  ),
  private = list(
    model.fit = list(),   # the model fit (either coefficients or the model fit object)
    MSE = list(),
    probA1 = NULL,    # Predicted probA^s=1 conditional on Xmat
    probAeqa = NULL   # Likelihood of observing a particular value A^s=a^s conditional on Xmat
  )
)
