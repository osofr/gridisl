

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("yhat", "y", "x", "subject", "new_vals_ind"))
}


#' S3 methods for printing model fit summary for brokenstickmodel class object
#'
#' Prints the modeling summary for the glm fit (\code{stats::glm.fit} or \code{speedglm::speedglm.wfit})
#' @param x The model fit object produced by functions stremr:::fit.glm or stremr:::fit.speedglm
#' @param ... Additional options passed on to \code{summary.GLMmodel}.
#' @return The output is printed with \code{cat}. To capture the markdown-formated model summary use \code{summary.GLMmodel}.
#' @export
print.brokenstickmodel <- function(x, ...) {
  cat(paste(summary(x, ...), collapse = '\n'))
}

#' S3 methods for getting model fit summary for glmfit class object
#'
#' Prints the modeling summary for the GLM fit (\code{stats::glm.fit} or \code{speedglm::speedglm.wfit})
#' @param object The model fit object produced by functions stremr:::glmfit.glm or stremr:::glmfit.speedglm
#' @param format_table Format the coefficients into a data.frame table?
#' @param ... Additional options (not used)
#' @return The markdown-formated model summary returned by \code{pander::pander_return}.
#' @export
summary.brokenstickmodel <- function(object, format_table = TRUE, ...) {
  makeModelCaption <- function(object) {
    return(
      "Model: " %+% object$params$outvar %+% " ~ " %+% paste0(object$params$predvars, collapse = " + ") %+% "; \\
       Stratify: " %+% object$params$stratify %+% "; \\
       N: " %+% prettyNum(object$nobs, big.mark = ",", scientific = FALSE) %+% "; \\
       Fit function: " %+% object$fitfunname
    )
  }
  nobs <- object$nobs
  coef_out <- object$coef
  if (format_table) {
    if (is.null(coef_out)) {
      coef_out <- "---"; names(coef_out) <- coef_out
    }
    coef_out <- data.frame(Terms = names(coef_out), Coefficients = as.vector(coef_out))
    # coef_out <- data.frame(Terms = object$params$predvars, Coefficients = as.vector(coef_out))
    rownames(coef_out) <- NULL
  }
  pander::set.caption(makeModelCaption(object))
  # S4 class: object$model.object
  m.summary <- utils::capture.output(print(object$model.object))
  out <- c(pander::pander_return(coef_out, justify = c('right', 'left')), m.summary)
  out
}

#' @export
fit.brokenstick <- function(fit.class, params, subject, x, Yvals, knots = NULL, mn = NULL, mx = NULL, model_contrl, ...) {
  if (gvars$verbose) print("calling brokenstick::brokenstick...")
  if (length(subject) == 0L) {
    model.fit <- list()
    model.fit$coef = rep.int(NA_real_, 1)
    names(model.fit$coef) <- "NA"
  } else {
    # unique number of ages after missing data removal
    nx <- length(unique(stats::na.omit(data.frame(x = x, y = Yvals))$x))
    if (is.null(knots)) knots <- min(6, nx)
    if (is.null(mn)) mn <- min(x, na.rm = TRUE)
    if (is.null(mx)) mx <- max(x, na.rm = TRUE)
    if (length(knots) == 1) knots <- seq(mn, mx, length = knots)[-knots]

    model.fit <- suppressMessages(
      brokenstick::brokenstick(
                              y = Yvals,
                              x = x,
                              subjid = subject,
                              knots = knots,
                              boundary = c(mn, mx))
              )
    model.fit <- brokenstick::export(model.fit)
  }

  return(create_fit_object(model.fit, model_alg = "brokenstick", fitfunname = "brokenstick",
                           params = params, coef = NULL, nobs = length(Yvals), model_contrl = model_contrl,
                           fitclass = "brokenstickmodel",
                           fitted.subject = subject, fitted.x = x, fitted.Yvals = Yvals
                           ))

}

# Prediction for glmfit objects, predicts P(A = 1 | newXmat)
predictP1.brokenstickmodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict.w.Y = FALSE, ...) {
  # if (!missing(DataStorageObject)) {
    # Will also obtain the outcomes of the prediction set:
  ParentObject$setdata(DataStorageObject, subset_idx = subset_idx, getoutvar = TRUE, getXmat = TRUE)
  # }

  # model.object <- m.fit$model.object
  model.object <- m.fit$modelfits_all[[1]]

  fitted.subject <- m.fit$fitted.subject
  fitted.x <- m.fit$fitted.x
  fitted.Yvals <- m.fit$fitted.Yvals

  # get new values (for prediction):
  new.x <- ParentObject$get.x
  new.subject <- ParentObject$get.subject

  # ----------------------------------------------------------------------------
  # Either include the new outcomes when predicting for new observations or not
  # ----------------------------------------------------------------------------
  new.Yvals <- rep.int(NA, length(new.subject))
  if (predict.w.Y) new.Yvals <- ParentObject$get.Y

  assert_that(!is.null(subset_idx))
  # Set to default missing value for A[i] degenerate/degerministic/misval:
  # Alternative, set to default replacement val: pAout <- rep.int(gvars$misXreplace, newBinDatObject$n)
  pAout <- rep.int(gvars$misval, max(subset_idx))
  if (length(subset_idx) > 0) {
    # x <- c(fitted.x, new.x)
    # y <- c(fitted.Yvals, new.Yvals)
    ## get fits at new data point, while also including fitted (training data points)
    new.dat <- data.table(
      subject = c(fitted.subject, new.subject),
      x = c(fitted.x, new.x),
      y = c(fitted.Yvals, new.Yvals),
      new_vals_ind = c(rep.int(FALSE, length(fitted.subject)), rep.int(TRUE, length(new.subject)))
    )

    new_vals_idx <- (1:nrow(new.dat))[-(1:length(fitted.x))]
    assert_that(length(new_vals_idx) == length(new.Yvals))
    assert_that(new_vals_idx[1] > length(fitted.Yvals))

    setkeyv(new.dat, cols = "subject")

    bs.predict <- utils::getFromNamespace("predict.brokenstick_export", "brokenstick")
    # bs.predict <- utils::getFromNamespace("predict.brokenstick", "brokenstick")

    # bs.predict <- function(object, y, x, output, ...) {
    #   bs.predict.fun <- utils::getFromNamespace("predict.brokenstick", "brokenstick")
    #   res <- bs.predict.fun(object, y = y, x = x, output = output, ...)
    #   return(res)
    # }

    new.dat[, yhat := bs.predict(model.object, y = y, x = x, output = "vector"), by = subject]
    new.preds <- new.dat[new_vals_ind == TRUE, yhat]
    pAout[subset_idx] <- as.vector(new.preds)

  }
  return(pAout)
}

#' @importFrom assertthat assert_that is.count is.string is.flag
brokenstickModelClass <- R6Class(classname = "brokenstickModelClass",
  cloneable = TRUE, # changing to TRUE to make it easy to clone input h_g0/h_gstar model fits
  portable = TRUE,
  class = TRUE,
  public = list(
    reg = list(),
    params = list(),
    outvar = character(),
    model_contrl = list(),
    fit.class = c("brokenstick"),
    # model.fit = list(fitfunname = NA, nobs = NA, params = NA, model_algorithms = NA),
    model.fit = list(),

    initialize = function(fit.algorithm, fit.package, reg, ...) {
      self$reg <- reg
      self$params <- create_fit_params(reg)
      self$outvar <- reg$outvar
      self$model_contrl <- reg$model_contrl
      # self$predvars <- reg$predvars

      assert_that(any(c("brokenstick") %in% fit.package))
      self$fit.class <- fit.package
      class(self$fit.class) <- c(class(self$fit.class), self$fit.class)
      invisible(self)
    },

    fit = function(data, subset_idx, validation_data = NULL, ...) {
    # fit = function(data, outvar, predvars, subset_idx, validation_data = NULL, ...) {
      self$setdata(data, subset_idx = subset_idx, getXmat = TRUE, ...)
      # self$model.fit$params <- self$params
      self$model.fit <- fit(self$fit.class, self$params,
                               subject = private$subject,
                               x = private$x,
                               Yvals = private$Yvals,
                               knots = self$model_contrl$knots,
                               mn = self$model_contrl$mn,
                               mx = self$model_contrl$mx,
                               model_contrl = self$model_contrl,
                               ...)
      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx, ...) {
      if (missing(data)) stop("to obtain predictions with brokenstick must provide newdata")
      predict.w.Y <- self$model_contrl$predict.w.Y
      if (is.null(predict.w.Y)) predict.w.Y <- FALSE
      P1 <- predictP1(self$model.fit,
                      ParentObject = self,
                      DataStorageObject = data,
                      subset_idx = subset_idx,
                      predict.w.Y = predict.w.Y)

      if (!is.matrix(P1)) {
        P1 <- matrix(P1, byrow = TRUE)
      }
      colnames(P1) <- names(self$getmodel_ids)
      return(P1)
    },

    getmodel_byname = function(model_names, model_IDs) {
      res <- list(self$model.fit$model.object)
      names(res) <- model_names
      return(res)
    },

    get_best_model_params = function(model_names) {
      top_params <- list(fit.package = self$model_contrl$fit.package,
                         fit.algorithm = self$model_contrl$fit.algorithm)
      return(top_params)
    },

    get_modelfits_grid = function(...) {
      return(NULL)
    },

    # Sets Xmat, Yvals, evaluates subset and performs correct subseting of data
    # everything is performed using data$ methods (data is of class DataStorageClass)
    setdata = function(data, subset_idx, getoutvar = TRUE, getXmat = TRUE, ...) {
      assert_that(is.DataStorageClass(data))
      nodes <- data$nodes
      IDnode <- nodes$IDnode
      tnode <- nodes$tnode

      if (getoutvar) {
        if (self$outvar %in% data$names.sVar) {
          private$Yvals <- data$get.outvar(subset_idx, self$outvar) # Always a vector
        } else {
          private$Yvals <- rep.int(NA, length(subset_idx))
        }
      }

      if (getXmat) {
        # self$define.Xmat(data, subset_idx) # design matrix in normal sense is not used in face
        private$subject <- data$get.outvar(subset_idx, IDnode)
        private$x <- data$get.outvar(subset_idx, tnode) # Always a vector
      }
      return(invisible(self))
    },

    show = function(all_fits = FALSE, ...) {
      # version A (uses pander and requires setting knit.asis=TRUE), but makes nice tables as a result:
      # print(self$model.fit)

      # version B not as pretty, but easier to maintain:
      model.fit <- self$model.fit
      # "Model: " %+% model.fit$params$outvar %+% " ~ " %+% paste0(model.fit$params$predvars, collapse = " + ") %+% "; \n" %+%
      cat("Stratify: " %+% model.fit$params$stratify %+% "; \n" %+%
          "N: " %+% prettyNum(model.fit$nobs, big.mark = ",", scientific = FALSE) %+% "; \n" %+%
          "Fit function: " %+% model.fit$fitfunname %+% "\n")
      print(model.fit$model.object)
      return(invisible(NULL))
    }
  ),

  active = list(
    # emptydata = function() { private$Xmat <- NULL },
    emptydata = function() { NULL },
    emptyY = function() { private$Yvals <- NULL },
    emptymodelfit = function() {self$model.fit$model.object <- NULL; return(invisible(NULL)) },
    get.subject = function() { private$subject },
    get.x = function() { private$x },
    get.Y = function() { private$Yvals },

    getmodel_ids = function() { return(assign_model_name_id(params = self$params, model_algorithm = self$model.fit$model_algorithms[[1]], name = self$model_contrl$name)) },
    getmodel_algorithms = function() { self$model.fit$model_algorithms }
  ),

  private = list(
    subject = NULL,
    x = NULL,
    Yvals = NULL
  )
)