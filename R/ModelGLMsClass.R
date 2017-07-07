#' @import data.table
NULL

# Generic for fitting the logistic (binomial family) GLM model
fit <- function(fit, ...) UseMethod("fit")

# Generic for predicting P(A=1|...) from either logistic (binomial family) GLM model or H2O model
predictP1 <- function(m.fit, ...) UseMethod("predictP1")

# S3 method for glm binomial family fit, takes BinDat data object:
fit.glm <- function(fit.class, params, Xmat, Yvals, model_contrl, ...) {
  if ("family" %in% names(model_contrl)) {
    family <- model_contrl[["family"]]
    if (is.character(family)) {
      family <- get(family, mode = "function", envir = parent.frame())
      family <- family()
    }
    if (!class(family) %in% "family") stop("family arg must be of an object of class 'family'")
  } else {
    family <- stats::quasibinomial()
  }
  family_name <- family[["family"]]
  if (family_name %in% c("quasibinomial", "binomial")) {
    linkinv_fun <- logit_linkinv
  } else {
    linkinv_fun <- family[["linkinv"]]
  }

  if (gvars$verbose == 2) print("...calling glm.fit...")
  if (nrow(Xmat) == 0L) {
    model.fit <- list()
    model.fit$coef = rep.int(NA_real_, ncol(Xmat))
    names(model.fit$coef) <- colnames(Xmat)
  } else {
    ctrl <- glm.control(trace = FALSE)
    SuppressGivenWarnings({
      model.fit <- stats::glm.fit(x = Xmat,
                                  y = Yvals,
                                  family = family,
                                  control = ctrl)
    }, GetWarningsToSuppress())
  }

  model.fit$linear.predictors <- NULL
  model.fit$weights <- NULL
  model.fit$prior.weights <- NULL
  model.fit$y <- NULL
  model.fit$residuals <- NULL
  model.fit$fitted.values <- NULL
  model.fit$effects <- NULL
  model.fit$qr <- NULL

  # print(object.size(model.fit), units = "Kb")

  return(create_fit_object(model.fit, model_alg = "glm", fitfunname = "glm",
                           linkfun = "logit_linkinv",
                           linkinv_fun = linkinv_fun,
                           params = params, coef = model.fit$coef, nobs = nrow(Xmat), model_contrl = model_contrl,
                           fitclass = "GLMmodel"))
}

# S3 method for speedglm binomial family fit, takes BinDat data object:
fit.speedglm <- function(fit.class, params, Xmat, Yvals, model_contrl, ...) {
  if ("family" %in% names(model_contrl)) {
    family <- model_contrl[["family"]]
    if (is.character(family)) {
      family <- get(family, mode = "function", envir = parent.frame())
      family <- family()
    }
    if (!class(family) %in% "family") stop("family arg must be of an object of class 'family'")
  } else {
    family <- stats::quasibinomial()
  }

  family_name <- family[["family"]]
  if (family_name %in% c("quasibinomial", "binomial")) {
    linkinv_fun <- logit_linkinv
  } else {
    linkinv_fun <- family[["linkinv"]]
  }

  if (gvars$verbose == 2) print("...calling speedglm.wfit...")
  if (nrow(Xmat) == 0L) {
    model.fit <- list()
    model.fit$coef = rep.int(NA_real_, ncol(Xmat))
    names(model.fit$coef) <- colnames(Xmat)
  } else {
    # method = c('eigen','Cholesky','qr')
    # row.chunk=NULL
    SuppressGivenWarnings({
    model.fit <- try(speedglm::speedglm.wfit(X = Xmat,
                                             y = Yvals,
                                             method = 'Cholesky',
                                             family = family,
                                             trace = FALSE),
                    silent = TRUE)
    }, GetWarningsToSuppress())
  }

  if (inherits(model.fit, "try-error")) { # if failed, fall back on stats::glm
    if (gvars$verbose) message("speedglm::speedglm.wfit failed, falling back on stats:glm.fit; ", model.fit)
    return(fit.glm(fit.class, params, Xmat, Yvals, model_contrl, ...))
  }

  if (gvars$verbose == 2) { print("glm fit:"); print(model.fit$coef) }

  return(create_fit_object(model.fit, model_alg = "glm", fitfunname = "speedglm",
                           linkfun = "logit_linkinv",
                           linkinv_fun = linkinv_fun,
                           params = params, coef = model.fit$coef, nobs = nrow(Xmat), model_contrl = model_contrl,
                           fitclass = "GLMmodel"))

}

# Prediction for glmfit objects, predicts P(A = 1 | newXmat)
predictP1.GLMmodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
  # if (!missing(DataStorageObject)) {
    ParentObject$setdata(DataStorageObject, subset_idx = subset_idx, getoutvar = FALSE, getXmat = TRUE)
  # }
  Xmat <- ParentObject$getXmat
  assert_that(!is.null(Xmat)); assert_that(!is.null(subset_idx))
  # Set to default missing value for A[i] degenerate/degerministic/misval:
  # Alternative, set to default replacement val: pAout <- rep.int(gvars$misXreplace, newBinDatObject$n)
  # pAout <- rep.int(gvars$misval, max(subset_idx))
  # print(head(Xmat))

  pAout <- rep.int(gvars$misval, length(subset_idx))
  if (length(subset_idx) > 0) {
    if (!all(is.na(m.fit$coef))) {
      eta <- Xmat[,!is.na(m.fit$coef), drop = FALSE] %*% m.fit$coef[!is.na(m.fit$coef)]
      # pAout <- match.fun(FUN = m.fit$linkfun)(eta)
      pAout <- m.fit$linkinv_fun(eta)
    } else {
      pAout <- NaN
    }
  }
  return(pAout)
}

## ---------------------------------------------------------------------
#' R6 class for storing the design matrix and the binary outcome for a single GLM (logistic) regression
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
glmModelClass <- R6Class(classname = "glmModelClass",
  cloneable = TRUE, # changing to TRUE to make it easy to clone input h_g0/h_gstar model fits
  portable = TRUE,
  class = TRUE,
  public = list(
    reg = list(),
    params = list(),
    outvar = character(),
    predvars = character(),
    model_contrl = list(),
    ReplMisVal0 = logical(),
    fit.class = c("glm", "speedglm"),
    # model.fit = list(coef = NA, fitfunname = NA, linkfun = NA, nobs = NA, params = NA, model_algorithms = NA),
    model.fit = list(),

    initialize = function(fit.algorithm, fit.package, reg, ...) {
      self$reg <- reg
      self$params <- create_fit_params(reg)
      self$outvar <- reg$outvar
      self$predvars <- reg$predvars
      self$model_contrl <- reg$model_contrl
      self$ReplMisVal0 <- reg$ReplMisVal0
      assert_that(any(c("glm", "speedglm") %in% fit.package))
      self$fit.class <- fit.package
      class(self$fit.class) <- c(class(self$fit.class), self$fit.class)
      invisible(self)
    },

    fit = function(data, subset_idx, validation_data = NULL, ...) {
    # fit = function(data, outvar, predvars, subset_idx, validation_data = NULL, ...) {
      self$setdata(data, subset_idx = subset_idx, getXmat = TRUE, ...)
      self$model.fit <- fit(self$fit.class, self$params,
                               Xmat = private$Xmat,
                               Yvals = private$Yvals,
                               DataStorageObject = data,
                               subset_idx = subset_idx,
                               model_contrl = self$model_contrl, ...)
      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx, ...) {
      if (missing(data)) stop("to obtain predictions with glm must provide newdata")
      P1 <- predictP1(self$model.fit,
                      ParentObject = self,
                      DataStorageObject = data,
                      subset_idx = subset_idx
                      )
      # if (!is.matrix(P1)) P1 <- matrix(P1, nrow = length(P1), ncol = 1)
      if (!is.matrix(P1)) {
        P1 <- matrix(P1, byrow = TRUE)
      }
      colnames(P1) <- names(self$getmodel_ids)
      return(P1)
    },

    getmodel_byname = function(model_names, model_IDs) {
      res <- list(self$model.fit)
      names(res) <- model_names
      return(res)
    },

    get_best_model_params = function(model_names) {
      top_params <- list(fit.package = self$model_contrl$fit.package,
                         fit.algorithm = self$model_contrl$fit.algorithm)
      top_params[["interactions"]] <- self$model_contrl[["interactions"]]
      return(top_params)
    },

    get_modelfits_grid = function(...) {
      return(NULL)
    },

    # Sets Xmat, Yvals, evaluates subset and performs correct subseting of data
    # everything is performed using data$ methods (data is of class DataStorageClass)
    setdata = function(data, subset_idx, getoutvar = TRUE, getXmat = TRUE, ...) {
      assert_that(is.DataStorageClass(data))
      if (getoutvar) private$Yvals <- data$get.outvar(subset_idx, self$outvar) # Always a vector
      if (getXmat) self$define.Xmat(data, subset_idx)
      return(invisible(self))
    },

    define.Xmat = function(data, subset_idx) {
      predvars <- self$predvars
      if (length(subset_idx) == 0L) {  # When nrow(Xmat) == 0L avoids exception (when nrow == 0L => prob(A=a) = 1)
        Xmat <- matrix(, nrow = 0L, ncol = (length(predvars) + 1))
        colnames(Xmat) <- c("Intercept", predvars)
      } else {
        # *** THIS IS THE ONLY LOCATION IN THE PACKAGE WHERE CALL TO DataStorageClass$get.dat.sVar() IS MADE ***
        if (length(predvars)==0L) {
          Xmat <- data.table(Intercept = rep.int(1L, length(subset_idx)))
          # Xmat <- as.matrix(rep.int(1L, length(subset_idx)), ncol=1)
        } else {
          Xmat <- data$get.dat.sVar(subset_idx, predvars)
          Xmat[, ("Intercept") := 1L]
          data.table::setcolorder(Xmat, c("Intercept", predvars))
          # Xmat <- cbind(Intercept = 1L, data$get.dat.sVar(subset_idx, predvars))
          # Xmat <- as.matrix(cbind(Intercept = 1L, data$get.dat.sVar(subset_idx, predvars)))
        }
        colnames(Xmat)[1] <- "Intercept"

        if (!is.null(self$model_contrl[["interactions"]])) {
          if (gvars$verbose == 2) {
            print("adding interactions in GLM:")
            str(self$model_contrl[["interactions"]])
          }
          ## this is a hack to fix pointer allocation problem (so that Xmat can be modified by reference inside add_interactions_toDT())
          ## see this for more: http://stackoverflow.com/questions/28078640/adding-new-columns-to-a-data-table-by-reference-within-a-function-not-always-wor
          ## and this: http://stackoverflow.com/questions/36434717/adding-column-to-nested-r-data-table-by-reference
          data.table::setDF(Xmat)
          data.table::setDT(Xmat)

          add_interactions_toDT(Xmat, self$model_contrl[["interactions"]])
        }

        Xmat <- as.matrix(Xmat)
        # To find and replace misvals in Xmat:
        if (self$ReplMisVal0) Xmat[gvars$misfun(Xmat)] <- gvars$misXreplace
      }
      private$Xmat <- Xmat
      return(invisible(self))
    },

    show = function(all_fits = FALSE, ...) {
      print(self$model.fit)
      return(invisible(NULL))
    }
  ),

  active = list( # 2 types of active bindings (w and wout args)
    emptydata = function() { private$Xmat <- NULL},
    emptyY = function() { private$Yvals <- NULL},
    getXmat = function() {private$Xmat},
    getY = function() {private$Yvals},
    getmodel_ids = function() { return(assign_model_name_id(params = self$params, model_algorithm = self$model.fit$model_algorithms, name = self$model_contrl$name)) },
    getmodel_algorithms = function() { self$model.fit$model_algorithms }
  ),

  private = list(
    Xmat = NULL,
    Yvals = NULL
  )
)