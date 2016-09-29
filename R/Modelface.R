fit.face <- function(fit.class, fit, subj, argvals, Yvals, ...) {
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
    knots <- 10

    # facedat <- facedat[complete.cases(facedat), ]
    knots <- face::select.knots(facedat$argvals, knots = knots)
    model.fit <- suppressMessages(face::face.sparse(facedat, knots = knots))
  }

  fit$face.model.object <- model.fit
  # need to save the fitting data,
  # because face needs training data (including trained outcomes) to predict for new data points:
  fit$fitted.Yvals <- Yvals
  fit$fitted.argvals <- argvals
  fit$fitted.subj <- subj

  fit$modelnames <- "face.model"
  fit$fitfunname <- "face.sparse";
  fit$nobs <- length(Yvals)
  class(fit)[2] <- "facemodel"
  return(fit)
}

# Prediction for glmfit objects, predicts P(A = 1 | newXmat)
predictP1.facemodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, n, predict.with.newYs = FALSE, ...) {
  if (!missing(DataStorageObject)) {
    # Will also obtain the outcomes of the prediction set:
    ParentObject$setdata(DataStorageObject, subset_idx = subset_idx, getoutvar = TRUE, getXmat = TRUE)
    # ParentObject$setdata(DataStorageObject, subset_idx = subset_idx, getoutvar = TRUE, getXmat = TRUE)
  }

  face.model.object <- m.fit$face.model.object
  fitted.argvals <- m.fit$fitted.argvals
  fitted.subj <- m.fit$fitted.subj
  fitted.Yvals <- m.fit$fitted.Yvals

  new.argvals <- ParentObject$getargvals
  new.subj <- ParentObject$getsubj

  # ----------------------------------------------------------------------------
  # Either include the new outcomes when predicting for new observations or not
  # ----------------------------------------------------------------------------
  new.Yvals <- rep.int(NA, length(new.subj))
  if (predict.with.newYs) new.Yvals <- ParentObject$getY
  # new.Yvals2 <- rep.int(NA, length(new.subj))

  assert_that(!is.null(subset_idx))

  # Set to default missing value for A[i] degenerate/degerministic/misval:
  # Alternative, set to default replacement val: pAout <- rep.int(gvars$misXreplace, newBinDatObject$n)
  pAout <- rep.int(gvars$misval, n)
  if (sum(subset_idx) > 0) {
    ## get fits at new data point, while also including fitted (training data points)

    new.face.dat <- data.frame(
      argvals = c(fitted.argvals, new.argvals),
      subj = c(fitted.subj, new.subj),
      y = c(fitted.Yvals, new.Yvals)
    )
    # new.face.dat <- data.frame(
    #   argvals = c(fitted.argvals, new.argvals, new.argvals),
    #   subj = c(fitted.subj, new.subj, new.subj),
    #   y = c(fitted.Yvals, new.Yvals, new.Yvals2)
    # )

    # new_vals_idx <- which(is.na(new.face.dat[,"y"]))
    new_vals_idx <- (1:nrow(new.face.dat))[-(1:length(fitted.argvals))]

    assert_that(length(new_vals_idx) == length(new.Yvals))
    assert_that(new_vals_idx[1] > length(fitted.Yvals))

    fpredict <- getFromNamespace("predict.face.sparse", "face")
    predict.res <- fpredict(face.model.object, new.face.dat)
    all.preds <- predict.res$y.pred
    assert_that(length(all.preds)==nrow(new.face.dat))
    new.preds <- all.preds[new_vals_idx]
    pAout[subset_idx] <- as.vector(new.preds)
  }
  return(pAout)
}

#' @importFrom assertthat assert_that is.count is.string is.flag
#' @export
faceClass <- R6Class(classname = "faceClass",
  cloneable = TRUE, # changing to TRUE to make it easy to clone input h_g0/h_gstar model fits
  portable = TRUE,
  class = TRUE,
  public = list(
    ParentModel = NULL,
    model_contrl = list(),
    params = list(),
    fit.class = c("face"),
    model.fit = list(fitfunname = NA, nobs = NA, params = NA),

    initialize = function(fit.algorithm, fit.package, ParentModel, ...) {
      self$ParentModel <- ParentModel
      self$model_contrl <- ParentModel$model_contrl
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
                               Yvals = private$Yvals, ...)
      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx) {
      predict.with.newYs <- self$ParentModel$model_contrl$predict.with.newYs
      if (is.null(predict.with.newYs)) predict.with.newYs <- FALSE
      P1 <- predictP1(self$model.fit,
                      ParentObject = self,
                      DataStorageObject = data,
                      subset_idx = subset_idx,
                      n = self$ParentModel$n,
                      predict.with.newYs = predict.with.newYs)

      if (!is.matrix(P1)) {
        P1 <- matrix(P1, byrow = TRUE)
        colnames(P1) <- "PredModel"
      }
      return(P1)
    },

    # Sets Xmat, Yvals, evaluates subset and performs correct subseting of data
    # everything is performed using data$ methods (data is of class DataStorageClass)
    setdata = function(data, subset_idx, getoutvar = TRUE, getXmat = TRUE, ...) {
      assert_that(is.DataStorageClass(data))
      nodes <- data$nodes
      IDnode <- nodes$IDnode
      tnode <- nodes$tnode
      if (getoutvar) private$Yvals <- data$get.outvar(subset_idx, self$ParentModel$outvar) # Always a vector
      if (getXmat) {
        # self$define.Xmat(data, subset_idx) # design matrix in normal sense is not used in face
        private$subj <- data$get.outvar(subset_idx, IDnode)
        private$argvals <- data$get.outvar(subset_idx, tnode) # Always a vector
      }
      return(invisible(self))
    }

    # define design matrix:
    # define.Xmat = function(data, subset_idx) {
    #   predvars <- self$ParentModel$predvars
    #   if (sum(subset_idx) == 0L) {  # When nrow(Xmat) == 0L avoids exception (when nrow == 0L => prob(A=a) = 1)
    #     Xmat <- matrix(, nrow = 0L, ncol = (length(predvars) + 1))
    #     colnames(Xmat) <- c("Intercept", predvars)
    #   } else {
    #     # *** THIS IS THE ONLY LOCATION IN THE PACKAGE WHERE CALL TO DataStorageClass$get.dat.sVar() IS MADE ***
    #     if (length(predvars)==0L) {
    #       Xmat <- as.matrix(rep.int(1L, sum(subset_idx)), ncol=1)
    #     } else {
    #       Xmat <- as.matrix(cbind(Intercept = 1, data$get.dat.sVar(subset_idx, predvars)))
    #     }
    #     colnames(Xmat)[1] <- "Intercept"
    #     # To find and replace misvals in Xmat:
    #     if (self$ParentModel$ReplMisVal0) Xmat[gvars$misfun(Xmat)] <- gvars$misXreplace
    #   }
    #   private$Xmat <- Xmat
    #   return(invisible(self))
    # }
  ),

  active = list(
    emptydata = function() { private$Xmat <- NULL },
    emptyY = function() { private$Yvals <- NULL },
    getXmat = function() { private$Xmat },
    getY = function() { private$Yvals },
    getargvals = function() { private$argvals },
    getsubj = function() { private$subj }
  ),

  private = list(
    subj = NULL,
    argvals = NULL,
    Xmat = NULL,
    Yvals = NULL
  )
)

# fit_method.face <- function(dat, ...) {
#   # dots <- list(...)
#   # knots <- 10
#   # if (!is.null(dots$knots)) {
#   #   knots <- dots$knots
#   #   dots$knots <- NULL
#   # }

#   fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {
#     ## get fits at data
#     new.face.dat <- data.frame(
#       argvals = c(dat$x, dat$x),
#       subj = dat$subjid[1],
#       y = c(dat$y, rep(NA, nrow(dat)))
#     )

#     fpredict <- getFromNamespace("predict.face.sparse", "face")
#     aa <- fpredict(fit$fit_obj, new.face.dat)$y.pred
#     dfit <- tail(aa, nrow(dat))

#     ## get xgrid fits
#     ##---------------------------------------------------------

#     new.face.dat <- data.frame(
#       argvals = c(dat$x, xg),
#       subj = dat$subjid[1],
#       y = c(dat$y, rep(NA, length(xg)))
#     )

#     aa <- fpredict(fit$fit_obj, new.face.dat)$y.pred
#     yg <- tail(aa, length(xg))

#     ## get control point fits
#     ##---------------------------------------------------------

#     cpy <- NULL
#     if (!is.null(cpx)) {
#       new.face.dat <- data.frame(
#         argvals = c(dat$x, cpx),
#         subj = dat$subjid[1],
#         y = c(dat$y, rep(NA, length(cpx)))
#       )
#       aa <- fpredict(fit$fit_obj, new.face.dat)$y.pred
#       cpy <- tail(aa, length(cpx))
#     }

#     list(
#       xy = dat,
#       fit = dfit,
#       fitgrid = data.frame(x = xg, y = yg),
#       checkpoint = data.frame(x = cpx, y = cpy),
#       pars = NULL
#     )
#   }

#   list(
#     fit_obj = fit_obj,
#     fit_apply = fit_apply,
#     dots = dots
#   )
# }