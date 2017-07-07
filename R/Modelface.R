#' @export
fit.face <- function(fit.class, params, subj, argvals, Yvals, knots = NULL, model_contrl, ...) {
  if (gvars$verbose) print("calling face::face.sparse...")
  if (length(subj) == 0L) {
    model.fit <- list()
  } else {
    facedat <- data.frame(
      argvals = argvals,
      subj = subj,
      y = Yvals)
    # facedat <- facedat[complete.cases(facedat), ]
    if (is.null(knots)) knots <- face::select.knots(facedat$argvals, knots = 10)
    model.fit <- suppressMessages(face::face.sparse(facedat, knots = knots))
  }

  return(create_fit_object(model.fit, model_alg = "face", fitfunname = "face.sparse",
                           params = params, coef = NULL, nobs = length(Yvals), model_contrl = model_contrl,
                           fitclass = "facemodel",
                           fitted.subj = subj, fitted.argvals = argvals, fitted.Yvals = Yvals
                           ))
}

# Prediction for glmfit objects, predicts P(A = 1 | newXmat)
#' @export
predictP1.facemodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, predict.w.Y = FALSE, knots  = NULL, ...) {
  # if (!missing(DataStorageObject)) {
    # Will also obtain the outcomes of the prediction set:
    ParentObject$setdata(DataStorageObject, subset_idx = subset_idx, getoutvar = TRUE, getXmat = TRUE)
  # }

  # model.object <- m.fit$model.object
  model.object <- m.fit$modelfits_all[[1]]

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
  pAout <- rep.int(gvars$misval, max(subset_idx))
  if (length(subset_idx) > 0) {
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

    fpredict <- utils::getFromNamespace("predict.face.sparse", "face")
    predict.res <- fpredict(model.object, new.face.dat)
    all.preds <- predict.res$y.pred
    assert_that(length(all.preds)==nrow(new.face.dat))
    new.preds <- all.preds[new_vals_idx]
    pAout[subset_idx] <- as.vector(new.preds)
  }
  return(pAout)
}

#' @importFrom assertthat assert_that is.count is.string is.flag
faceModelClass <- R6Class(classname = "faceModelClass",
  cloneable = TRUE, # changing to TRUE to make it easy to clone input h_g0/h_gstar model fits
  portable = TRUE,
  class = TRUE,
  public = list(
    reg = list(),
    params = list(),
    outvar = character(),
    nobs = integer(),
    model_contrl = list(),
    fit.class = c("face"),
    # model.fit = list(fitfunname = NA, nobs = NA, params = NA, model_algorithms = NA),
    model.fit = list(),

    initialize = function(fit.algorithm, fit.package, reg, ...) {
      self$reg <- reg
      self$params <- create_fit_params(reg)
      self$outvar <- reg$outvar
      self$model_contrl <- reg$model_contrl

      assert_that(any(c("face") %in% fit.package))
      self$fit.class <- fit.package
      class(self$fit.class) <- c(class(self$fit.class), self$fit.class)
      invisible(self)
    },

    fit = function(data, subset_idx, validation_data = NULL, ...) {
    # fit = function(data, outvar, predvars, subset_idx, validation_data = NULL, ...) {
      self$setdata(data, subset_idx = subset_idx, getXmat = TRUE, ...)
      # self$model.fit$params <- self$params
      self$model.fit <- fit(self$fit.class, self$params,
                               subj = private$subj,
                               argvals = private$argvals,
                               Yvals = private$Yvals,
                               knots = self$model_contrl$knots,
                               model_contrl = self$model_contrl,
                               ...)

      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx, ...) {
      if (missing(data)) stop("to obtain predictions with face must provide newdata")
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

    getmodel_ids = function() { return(assign_model_name_id(params = self$params, model_algorithm = self$model.fit$model_algorithms[[1]], name = self$model_contrl$name)) },
    getmodel_algorithms = function() { self$model.fit$model_algorithms }
  ),

  private = list(
    subj = NULL,
    argvals = NULL,
    Yvals = NULL
  )
)