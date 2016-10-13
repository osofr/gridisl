#' S3 methods for printing model fit summary for brokenstickmodel class object
#'
#' Prints the modeling summary for the glm fit (\code{stats::glm.fit} or \code{speedglm::speedglm.wfit})
#' @param model.fit The model fit object produced by functions stremr:::fit.glm or stremr:::fit.speedglm
#' @param ... Additional options passed on to \code{summary.GLMmodel}.
#' @return The output is printed with \code{cat}. To capture the markdown-formated model summary use \code{summary.GLMmodel}.
#' @export
print.brokenstickmodel <- function(model.fit, ...) {
  cat(paste(summary(model.fit, ...), collapse = '\n'))
}

#' S3 methods for getting model fit summary for glmfit class object
#'
#' Prints the modeling summary for the GLM fit (\code{stats::glm.fit} or \code{speedglm::speedglm.wfit})
#' @param model.fit The model fit object produced by functions stremr:::glmfit.glm or stremr:::glmfit.speedglm
#' @param format_table Format the coefficients into a data.frame table?
#' @param ... Additional options (not used)
#' @return The markdown-formated model summary returned by \code{pander::pander_return}.
#' @export
summary.brokenstickmodel <- function(model.fit, format_table = TRUE, ...) {
  makeModelCaption <- function(model.fit) {
    return(
      "Model: " %+% model.fit$params$outvar %+% " ~ " %+% paste0(model.fit$params$predvars, collapse = " + ") %+% "; \\
       Stratify: " %+% model.fit$params$stratify %+% "; \\
       N: " %+% prettyNum(model.fit$nobs, big.mark = ",", scientific = FALSE) %+% "; \\
       Fit function: " %+% model.fit$fitfunname
    )
  }
  nobs <- model.fit$nobs
  coef_out <- model.fit$coef
  if (format_table) {
    if (is.null(coef_out)) {
      coef_out <- "---"; names(coef_out) <- coef_out
    }
    coef_out <- data.frame(Terms = names(coef_out), Coefficients = as.vector(coef_out))
    # coef_out <- data.frame(Terms = model.fit$params$predvars, Coefficients = as.vector(coef_out))
    rownames(coef_out) <- NULL
  }
  pander::set.caption(makeModelCaption(model.fit))
  # S4 class: model.fit$model.object
  m.summary <- capture.output(print(model.fit$model.object))
  out <- c(pander::pander_return(coef_out, justify = c('right', 'left')), m.summary)
  out
}

fit.brokenstick <- function(fit.class, fit, subject, x, Yvals, knots = NULL, mn = NULL, mx = NULL, ...) {
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

    model.fit <- suppressMessages(brokenstick::brokenstick(
                  x = x,
                  y = Yvals,
                  subject = subject,
                  knots = knots,
                  Boundary.knots = c(mn, mx))
              )
  }

  fit$model.object <- model.fit
  # need to save the fitting data,
  # because face needs training data (including trained outcomes) to predict for new data points:
  fit$fitted.subject <- subject
  fit$fitted.x <- x
  fit$fitted.Yvals <- Yvals
  fit$model_algorithms <- list("brokenstick")
  fit$fitfunname <- "brokenstick"
  fit$nobs <- length(Yvals)
  class(fit)[2] <- "brokenstickmodel"
  return(fit)
}

# Prediction for glmfit objects, predicts P(A = 1 | newXmat)
predictP1.brokenstickmodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict.with.newYs = FALSE, ...) {
  if (!missing(DataStorageObject)) {
    # Will also obtain the outcomes of the prediction set:
    ParentObject$setdata(DataStorageObject, subset_idx = subset_idx, getoutvar = TRUE, getXmat = TRUE)
  }
  model.object <- m.fit$model.object
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
  if (predict.with.newYs) new.Yvals <- ParentObject$get.Y

  assert_that(!is.null(subset_idx))
  # Set to default missing value for A[i] degenerate/degerministic/misval:
  # Alternative, set to default replacement val: pAout <- rep.int(gvars$misXreplace, newBinDatObject$n)
  pAout <- rep.int(gvars$misval, length(subset_idx))
  if (sum(subset_idx) > 0) {
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
    bs.predict <- getFromNamespace("predict.brokenstick", "brokenstick")
    new.dat[, yhat := bs.predict(model.object, y = y, x = x, output = "vector"), by = subject]
    new.preds <- new.dat[new_vals_ind == TRUE, yhat]
    pAout[subset_idx] <- as.vector(new.preds)
  }
  return(pAout)
}

#' @importFrom assertthat assert_that is.count is.string is.flag
#' @export
brokenstickModelClass <- R6Class(classname = "brokenstickModelClass",
  cloneable = TRUE, # changing to TRUE to make it easy to clone input h_g0/h_gstar model fits
  portable = TRUE,
  class = TRUE,
  public = list(
    outvar = character(),
    model_contrl = list(),
    params = list(),
    fit.class = c("brokenstick"),
    model.fit = list(fitfunname = NA, nobs = NA, params = NA, model_algorithms = NA),

    initialize = function(fit.algorithm, fit.package, reg, ...) {
      self$model_contrl <- reg$model_contrl
      self$outvar <- reg$outvar
      assert_that(any(c("brokenstick") %in% fit.package))
      self$fit.class <- fit.package
      class(self$fit.class) <- c(class(self$fit.class), self$fit.class)
      invisible(self)
    },

    fit = function(data, outvar, predvars, subset_idx, validation_data = NULL, ...) {
      self$setdata(data, subset_idx = subset_idx, getXmat = TRUE, ...)
      self$model.fit$params <- self$params
      self$model.fit <- fit(self$fit.class, self$model.fit,
                               subject = private$subject,
                               x = private$x,
                               Yvals = private$Yvals,
                               knots = self$model_contrl$knots,
                               mn = self$model_contrl$mn,
                               mx = self$model_contrl$mx,
                               ...)
      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx) {
      predict.with.newYs <- self$model_contrl$predict.with.newYs
      if (is.null(predict.with.newYs)) predict.with.newYs <- FALSE
      P1 <- predictP1(self$model.fit,
                      ParentObject = self,
                      DataStorageObject = data,
                      subset_idx = subset_idx,
                      predict.with.newYs = predict.with.newYs)

      if (!is.matrix(P1)) {
        P1 <- matrix(P1, byrow = TRUE)
        colnames(P1) <- names(self$getmodel_ids)
      }
      return(P1)
    },

    getmodel_byname = function(model_names, model_IDs) {
      res <- list(self$model.fit$model.object)
      names(res) <- model_names
      return(res)
    },

    # Sets Xmat, Yvals, evaluates subset and performs correct subseting of data
    # everything is performed using data$ methods (data is of class DataStorageClass)
    setdata = function(data, subset_idx, getoutvar = TRUE, getXmat = TRUE, ...) {
      assert_that(is.DataStorageClass(data))
      nodes <- data$nodes
      IDnode <- nodes$IDnode
      tnode <- nodes$tnode
      if (getoutvar) private$Yvals <- data$get.outvar(subset_idx, self$outvar) # Always a vector
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

    getmodel_ids = function() { return(make_model_ID_name(self$model.fit$model_algorithms, self$model_contrl$name)) },
    getmodel_algorithms = function() { self$model.fit$model_algorithms }
  ),

  private = list(
    subject = NULL,
    x = NULL,
    Yvals = NULL
  )
)