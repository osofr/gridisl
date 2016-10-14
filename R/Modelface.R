fit.face <- function(fit.class, fit, subj, argvals, Yvals, knots = NULL, ...) {
  if (gvars$verbose) print("calling face::face.sparse...")
  if (length(subj) == 0L) {
    model.fit <- list()
    model.fit$coef = rep.int(NA_real_, ncol(Xmat))
    names(model.fit$coef) <- colnames(Xmat)
  } else {
    facedat <- data.frame(
      argvals = argvals,
      subj = subj,
      y = Yvals)
    # facedat <- facedat[complete.cases(facedat), ]
    if (is.null(knots)) knots <- face::select.knots(facedat$argvals, knots = 10)
    model.fit <- suppressMessages(face::face.sparse(facedat, knots = knots))
  }

  fit$model.object <- model.fit
  # need to save the fitting data,
  # because face needs training data (including trained outcomes) to predict for new data points:
  fit$fitted.Yvals <- Yvals
  fit$fitted.argvals <- argvals
  fit$fitted.subj <- subj
  fit$model_algorithms <- list("face")

  fit$fitfunname <- "face.sparse";
  fit$nobs <- length(Yvals)
  class(fit)[2] <- "facemodel"
  return(fit)
}

# Prediction for glmfit objects, predicts P(A = 1 | newXmat)
predictP1.facemodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict.w.Y = FALSE, knots  = NULL, ...) {
  if (!missing(DataStorageObject)) {
    # Will also obtain the outcomes of the prediction set:
    ParentObject$setdata(DataStorageObject, subset_idx = subset_idx, getoutvar = TRUE, getXmat = TRUE)
  }

  model.object <- m.fit$model.object
  fitted.argvals <- m.fit$fitted.argvals
  fitted.subj <- m.fit$fitted.subj
  fitted.Yvals <- m.fit$fitted.Yvals

  new.argvals <- ParentObject$get.argvals
  new.subj <- ParentObject$get.subj

  # ----------------------------------------------------------------------------
  # Either include the new outcomes when predicting for new observations or not
  # ----------------------------------------------------------------------------
  new.Yvals <- rep.int(NA, length(new.subj))
  if (predict.w.Y) new.Yvals <- ParentObject$get.Y
  # new.Yvals2 <- rep.int(NA, length(new.subj))

  assert_that(!is.null(subset_idx))

  # Set to default missing value for A[i] degenerate/degerministic/misval:
  # Alternative, set to default replacement val: pAout <- rep.int(gvars$misXreplace, newBinDatObject$n)
  pAout <- rep.int(gvars$misval, length(subset_idx))
  if (sum(subset_idx) > 0) {
    ## get fits at new data point, while also including fitted (training data points)

    new.face.dat <- data.frame(
      argvals = c(fitted.argvals, new.argvals),
      subj = c(fitted.subj, new.subj),
      y = c(fitted.Yvals, new.Yvals)
    )

    # new_vals_idx <- which(is.na(new.face.dat[,"y"]))
    new_vals_idx <- (1:nrow(new.face.dat))[-(1:length(fitted.argvals))]

    assert_that(length(new_vals_idx) == length(new.Yvals))
    assert_that(new_vals_idx[1] > length(fitted.Yvals))

    fpredict <- getFromNamespace("predict.face.sparse", "face")
    predict.res <- fpredict(model.object, new.face.dat)
    all.preds <- predict.res$y.pred
    assert_that(length(all.preds)==nrow(new.face.dat))
    new.preds <- all.preds[new_vals_idx]
    pAout[subset_idx] <- as.vector(new.preds)
  }
  return(pAout)
}

#' @importFrom assertthat assert_that is.count is.string is.flag
#' @export
faceModelClass <- R6Class(classname = "faceModelClass",
  cloneable = TRUE, # changing to TRUE to make it easy to clone input h_g0/h_gstar model fits
  portable = TRUE,
  class = TRUE,
  public = list(
    outvar = character(),
    nobs = integer(),
    model_contrl = list(),
    params = list(),
    fit.class = c("face"),
    model.fit = list(fitfunname = NA, nobs = NA, params = NA, model_algorithms = NA),

    initialize = function(fit.algorithm, fit.package, reg, ...) {
      self$model_contrl <- reg$model_contrl
      self$outvar <- reg$outvar
      assert_that(any(c("face") %in% fit.package))
      self$fit.class <- fit.package
      class(self$fit.class) <- c(class(self$fit.class), self$fit.class)
      invisible(self)
    },

    fit = function(data, outvar, predvars, subset_idx, validation_data = NULL, ...) {
      self$setdata(data, subset_idx = subset_idx, getXmat = TRUE, ...)
      self$model.fit$params <- self$params
      self$model.fit <- fit(self$fit.class, self$model.fit,
                               subj = private$subj,
                               argvals = private$argvals,
                               Yvals = private$Yvals,
                               knots = self$model_contrl$knots,
                               ...)

      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx) {
      predict.w.Y <- self$model_contrl$predict.w.Y
      if (is.null(predict.w.Y)) predict.w.Y <- FALSE
      P1 <- predictP1(self$model.fit,
                      ParentObject = self,
                      DataStorageObject = data,
                      subset_idx = subset_idx,
                      predict.w.Y = predict.w.Y)

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
        private$subj <- data$get.outvar(subset_idx, IDnode)
        private$argvals <- data$get.outvar(subset_idx, tnode) # Always a vector
      }
      return(invisible(self))
    },
    show = function(all_fits = FALSE, ...) {
      # print(self$model.fit)
      model.fit <- self$model.fit
      # "Model: " %+% model.fit$params$outvar %+% " ~ " %+% paste0(model.fit$params$predvars, collapse = " + ") %+% "; \n" %+%
      cat("Stratify: " %+% model.fit$params$stratify %+% "; \n" %+%
          "N: " %+% prettyNum(model.fit$nobs, big.mark = ",", scientific = FALSE) %+% "; \n" %+%
          "Fit function: " %+% model.fit$fitfunname %+% "\n")
      return(invisible(NULL))
    }
  ),

  active = list(
    # emptydata = function() { private$Xmat <- NULL },
    emptydata = function() { NULL },
    emptyY = function() { private$Yvals <- NULL },
    emptymodelfit = function() {self$model.fit$model.object <- NULL; return(invisible(NULL)) },
    get.subj = function() { private$subj },
    get.argvals = function() { private$argvals },
    get.Y = function() { private$Yvals },

    getmodel_ids = function() { return(make_model_ID_name(self$model.fit$model_algorithms, self$model_contrl$name)) },
    getmodel_algorithms = function() { self$model.fit$model_algorithms }
  ),

  private = list(
    subj = NULL,
    argvals = NULL,
    Yvals = NULL
  )
)