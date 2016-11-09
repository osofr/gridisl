# ---------------------------------------------------------------------------
# TO DO: reformat single h2o model fit object so that its the same with H2O.grid model fit, i.e.,
#        1. Put model.fit into a named list entry called "fitted_models_all"
#        2. Then remove 'H2O.model.object'
# ---------------------------------------------------------------------------

# take a list of args, take a function body and return only the args that belong to function signature
keep_only_fun_args <- function(Args, fun) {
  keepArgs <- intersect(names(Args), names(formals(fun))) # captures optional arguments given by user
  if (length(keepArgs) > 0) {
    Args <- Args[keepArgs]
  } else {
    Args <- NULL
  }
  return(Args)
}

# 1. replace any arg in mainArgs if it also appears in userArgs
# 2. add any arg from userArgs that also appears in formals(fun) of function
replace_add_user_args <- function(mainArgs, userArgs, fun) {
  replaceArgs <- intersect(names(mainArgs), names(userArgs)) # captures main arguments that were overridden by user
  if(length(replaceArgs) > 0) {
    mainArgs[replaceArgs] <- userArgs[replaceArgs]
    userArgs[replaceArgs] <- NULL
  }
  newArgs <- intersect(names(formals(fun)), names(userArgs)) # captures any additional args given by user that are not in mainArgs
  if (length(newArgs) > 0) {
    mainArgs <- c(mainArgs, userArgs[newArgs])
  }
  return(mainArgs)
}

# ---------------------------------------------------------------------------
# for running logistic regression with continuous outcome range [0-1]
# ---------------------------------------------------------------------------
# S3 method for fitting h2o GLM with binomial() family (logistic regression):
# use solver="L_BFGS" when doing classification and use "IRLSM" when not
fit.h2oglm <- function(fit.class, fit, training_frame, y, x, model_contrl, validationH2Oframe  = NULL, ...) {
# fit.h2oglm <- function(fit.class, fit, subsetH2Oframe, outvar, predvars, rows_subset, model_contrl, ...) {
  h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                  intercept = TRUE,
                  family = "gaussian",
                  standardize = TRUE,
                  # standardize = FALSE,
                  # solver = "L_BFGS",
                  # solver = "IRLSM",
                  # solver = "COORDINATE_DESCENT",
                  # solver = "COORDINATE_DESCENT_NAIVE",
                  lambda = 0L,
                  max_iterations = 100,
                  ignore_const_cols = FALSE,
                  missing_values_handling = "Skip")

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.glm)
  if (!is.null(validationH2Oframe)) {
    mainArgs$validation_frame <- validationH2Oframe
  }

  # h2o::h2o.glm(x = x, y = y, training_frame = training_frame, lambda = 0L, family = "gaussian")
  model.fit <- do.call(h2o::h2o.glm, mainArgs)
  # assign the fitted coefficients in correct order (same as predictor order in x)
  out_coef <- vector(mode = "numeric", length = length(x)+1)
  out_coef[] <- NA
  names(out_coef) <- c("Intercept", x)
  out_coef[names(model.fit@model$coefficients)] <- model.fit@model$coefficients
  fit$coef <- out_coef;

  fit$linkfun <- "logit_linkinv";
  fit$fitfunname <- "h2o.glm";
  fit$model_algorithms <- list("glm")
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  fit$nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  fit$H2O.model.object <- model.fit

  if (gvars$verbose) {
    print("h2oglm fits:")
    print(fit$coef)
  }

  class(fit) <- c(class(fit)[1], c("H2Omodel"))
  return(fit)
}

# S3 method for h2o randomForest fit (Random Forest):
fit.h2orandomForest <- function(fit.class, fit, training_frame, y, x, model_contrl, validationH2Oframe  = NULL, ...) {
  h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   ntrees = 100,
                   balance_classes = TRUE,
                   ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.randomForest)
  if (!is.null(validationH2Oframe)) {
    mainArgs$validation_frame <- validationH2Oframe
  }

  model.fit <- do.call(h2o::h2o.randomForest, mainArgs)
  fit$coef <- NULL;

  fit$fitfunname <- "h2o.randomForest";
  fit$model_algorithms <- list("randomForest")
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  fit$nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  fit$H2O.model.object <- model.fit

  class(fit) <- c(class(fit)[1], c("H2Omodel"))
  return(fit)
}

# S3 method for h2o gbm fit, takes BinDat data object:
# use "bernoulli" when doing classification and use "gaussian" when not
fit.h2ogbm <- function(fit.class, fit, training_frame, y, x, model_contrl, validationH2Oframe  = NULL, ...) {
  h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   distribution = "bernoulli",
                   # distribution = "gaussian",
                   ntrees = 100,
                   balance_classes = TRUE,
                   ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.gbm)
  if (!is.null(validationH2Oframe)) {
    mainArgs$validation_frame <- validationH2Oframe
  }

  model.fit <- do.call(h2o::h2o.gbm, mainArgs)
  fit$coef <- NULL;

  fit$fitfunname <- "h2o.gbm";
  fit$model_algorithms <- list("gbm")
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  fit$nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  fit$H2O.model.object <- model.fit

  class(fit) <- c(class(fit)[1], c("H2Omodel"))
  return(fit)
}

# S3 method for h2o deeplearning fit, takes BinDat data object:
# use "bernoulli" when doing classification and use "gaussian" when doing regression
fit.h2odeeplearning <- function(fit.class, fit, training_frame, y, x, model_contrl, validationH2Oframe  = NULL, ...) {
  h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   distribution = "bernoulli",
                   # distribution = "gaussian",
                   balance_classes = TRUE,
                   ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.gbm)
  if (!is.null(validationH2Oframe)) {
    mainArgs$validation_frame <- validationH2Oframe
  }

  model.fit <- do.call(h2o::h2o.deeplearning, mainArgs)
  fit$coef <- NULL;

  fit$fitfunname <- "h2o.deeplearning";
  fit$model_algorithms <- list("deeplearning")
  confusionMat <- h2o::h2o.confusionMatrix(model.fit)
  fit$nobs <- confusionMat[["0"]][3]+confusionMat[["1"]][3]
  fit$H2O.model.object <- model.fit

  class(fit) <- c(class(fit)[1], c("H2Omodel"))
  return(fit)
}

# ----------------------------------------------------------------
# Prediction for h2ofit objects, predicts P(A = 1 | newXmat)
# ----------------------------------------------------------------
predictP1.H2Omodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
  subsetH2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, DataStorageObject, subset_idx)
  pAout <- rep.int(gvars$misval, length(subset_idx))
  if (sum(subset_idx) > 0) {
    predictFrame <- h2o::h2o.predict(m.fit$H2O.model.object, newdata = subsetH2Oframe)
    if ("p1" %in% colnames(predictFrame)) {
      pAout[subset_idx] <- as.vector(predictFrame[,"p1"])
    } else {
      pAout[subset_idx] <- as.vector(predictFrame[,"predict"])
    }
  }
  return(pAout)
}

# ----------------------------------------------------------------
# Prediction for h2ofit objects, predicts E(outvar | newXmat)
# ----------------------------------------------------------------
predictP1.H2Oensemblemodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
  subsetH2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, DataStorageObject, subset_idx)
  pAout <- rep.int(gvars$misval, length(subset_idx))
  if (sum(subset_idx) > 0) {
    predictObject <- predict(m.fit$H2O.model.object, newdata = subsetH2Oframe)
    predictFrame <- predictObject$pred
    if ("p1" %in% colnames(predictFrame)) {
      pAout[subset_idx] <- as.vector(predictFrame[,"p1"])
    } else {
      pAout[subset_idx] <- as.vector(predictFrame[,"predict"])
    }
  }
  return(pAout)
}

# TO DO: Add prediction only based on the subset of models (rather than predicting for all models)
predictP1.H2Ogridmodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
  subsetH2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, DataStorageObject, subset_idx)
  pAoutMat <- matrix(gvars$misval, nrow = length(subset_idx), ncol = length(m.fit$fitted_models_all))
  colnames(pAoutMat) <- names(ParentObject$getmodel_ids)
  # "PredModel" %+% (1:length(m.fit$fitted_models_all))
  if (sum(subset_idx) > 0) {
    for (idx in seq_along(m.fit$fitted_models_all)) {
      predictFrame <- predict(m.fit$fitted_models_all[[idx]], newdata = subsetH2Oframe)
      pAoutMat[subset_idx, idx] <- as.vector(predictFrame[,"predict"])
    }
  }
  return(pAoutMat)
}

getPredictH2OFRAME <- function(m.fit, ParentObject, DataStorageObject, subset_idx) {
  assert_that(!is.null(subset_idx))
  if (!missing(DataStorageObject)) {
    rows_subset <- which(subset_idx)
    data <- DataStorageObject
    outvar <- m.fit$params$outvar
    predvars <- m.fit$params$predvars
    subsetH2Oframe <- data$fast.load.to.H2O(data$dat.sVar[rows_subset, c(outvar, predvars), with = FALSE],
                                            saveH2O = FALSE,
                                            destination_frame = "subsetH2Oframe")
  } else {
    subsetH2Oframe <- ParentObject$getsubsetH2Oframe
  }
  return(subsetH2Oframe)
}

h2oGridModelClass  <- R6Class(classname = "h2oModelClass",
  inherit = h2oModelClass,
  cloneable = TRUE, # changing to TRUE to make it easy to clone input h_g0/h_gstar model fits
  portable = TRUE,
  class = TRUE,
  public = list(
    fit.class = c("GridLearner"),

    initialize = function(...) {
      super$initialize(...)
      invisible(self)
    },

    fit = function(...) {
      return(super$fit(...))
    },

    predictP1 = function(...) {
      return(super$predictP1(...))
    },

    getmodel_byname = function(model_names, model_IDs) {
      if (!missing(model_names)) {
        return(self$model.fit$fitted_models_all[model_names])
      } else {
        if (missing(model_IDs)) stop("Must provide either 'model_names' or 'model_IDs'.")
        return(lapply(model_IDs, h2o::h2o.getModel))
      }
    },

    # Output info on the general type of regression being fitted:
    show = function(all_fits = FALSE, ...) {
      model.fit <- self$model.fit
      grid_objects <- self$model.fit$grid_objects
      top_grid_models <- self$model.fit$top_grid_models

      cat(" TOTAL NO. OF GRIDS: " %+% length(model.fit$grid_objects) %+% "\n")
      cat(" ======================= \n")

      for (grid_nm in names(grid_objects)) {
        print(grid_objects[[grid_nm]])
        cat("\n TOP MODEL FOR THIS GRID: \n")
        cat(" ======================= \n")
        print(top_grid_models[[grid_nm]])
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

    summary = function(all_fits = FALSE) {
      # model.fit <- self$model.fit
      # grid_objects <- self$model.fit$grid_objects
      # top_grid_models <- self$model.fit$top_grid_models
      # no.grids <- "Total No. of Grids: " %+% length(model.fit$grid_objects)
      # cat(no.grids)
      # for (grid_nm in names(grid_objects)) {
      #   print(grid_objects[[grid_nm]])
      #   cat("top model for this grid: \n")
      #   print(top_grid_models[[grid_nm]])
      # }
      print("...")
      return(invisible(self))
    }
  )
)

# IMPLEMENTING NEW CLASS FOR BINARY REGRESSION THAT USES h2o
# NEEDS TO be able to pass on THE REGRESSION SETTINGS FOR h2o-specific functions
h2oModelClass  <- R6Class(classname = "h2oModelClass",
  cloneable = TRUE, # changing to TRUE to make it easy to clone input h_g0/h_gstar model fits
  portable = TRUE,
  class = TRUE,
  public = list(
    outvar = character(),
    predvars = character(),
    model_contrl = list(),
    params = list(),
    classify = FALSE,
    fit.class = c("glm", "randomForest", "gbm", "deeplearning", "SuperLearner", "GridLearner"),
    model.fit = list(coef = NA, fitfunname = NA, linkfun = NA, nobs = NA, params = NA, H2O.model.object = NA, model_algorithms = NA),
    outfactors = NA,
    nfolds = 5,

    initialize = function(fit.algorithm, fit.package, reg, ...) {
      self$outvar <- reg$outvar
      self$predvars <- reg$predvars
      self$model_contrl <- reg$model_contrl

      assert_that("h2o" %in% fit.package)
      if (fit.algorithm %in% "SuperLearner") {
        if (!"package:h2oEnsemble" %in% search()) stop("must load 'h2oEnsemble' package prior to using the SuperLearner: require('h2oEnsemble') or library('h2oEnsemble')")
      }
      self$fit.class <- fit.algorithm
      class(self$fit.class) <- c(class(self$fit.class), "h2o" %+% self$fit.class)
      invisible(self)
    },

    fit = function(data, outvar, predvars, subset_idx, validation_data = NULL, ...) {
      assert_that(is.DataStorageClass(data))
      subsetH2Oframe <- self$setdata(data, subset_idx, self$classify, ...)
      if ((length(predvars) == 0L) || (sum(subset_idx) == 0L) || (length(self$outfactors) < 2L)) {
        message("unable to run " %+% self$fit.class %+% " with h2o for: intercept only models or designmat with zero rows or  constant outcome (y) ...")
        class(self$model.fit) <- "try-error"
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }

      if (!is.null(validation_data)) {
        assert_that(is.DataStorageClass(validation_data))
        validationH2Oframe <- self$setdata(validation_data, classify = self$classify, destination_frame = "validationH2Oframe", ...)
      } else {
        validationH2Oframe = NULL
      }

      self$model.fit$params <- self$params
      self$model.fit <- try(fit(self$fit.class, self$model.fit, training_frame = subsetH2Oframe, y = outvar, x = predvars,
                            model_contrl = self$model_contrl, fold_column = data$fold_column, validationH2Oframe = validationH2Oframe, ...),
                        silent = FALSE)

      if (inherits(self$model.fit, "try-error")) {
        self$emptydata
        self$emptyY
        return(self$model.fit)
      }
      return(self$model.fit)
    },

    predictP1 = function(data, subset_idx) {
      P1 <- predictP1(self$model.fit, ParentObject = self, DataStorageObject = data, subset_idx = subset_idx)
      if (!is.matrix(P1)) {
        P1 <- matrix(P1, byrow = TRUE)
      }
      colnames(P1) <- names(self$getmodel_ids)
      return(P1)
    },

    getmodel_byname = function(model_names, model_IDs) {
      if (!missing(model_names)) {
        return(list(self$model.fit$H2O.model.object))
        # return(self$model.fit$fitted_models_all[model_names])
      } else {
        if (missing(model_IDs)) stop("Must provide either 'model_names' or 'model_IDs'.")
        return(lapply(model_IDs, h2o::h2o.getModel))
      }
    },

    setdata = function(data, subset_idx, classify = FALSE, destination_frame = "newH2Osubset", ...) {
      outvar <- self$outvar
      predvars <- self$predvars
      if (!missing(subset_idx)) {
        rows_subset <- which(subset_idx)
      } else {
        rows_subset <- 1:data$nobs
      }

      load_var_names <- c(outvar, predvars, data$fold_column)

      if (self$fit.class %in% "SuperLearner") {
        if (!is.null(self$model_contrl$nfolds)) {
          self$nfolds <- as.integer(self$model_contrl$nfolds)
        } else {
          self$model_contrl$nfolds <- self$nfolds
        }
        data$define_CVfolds(nfolds = self$nfolds, fold_column = "fold_id", seed = self$model_contrl$seed)
        load_var_names <- c(load_var_names, data$fold_column)
      }

      if (!is.null(data$hold_column)) load_var_names <- c(load_var_names, data$hold_column)

      # 1. works on single core but fails in parallel:
      load_subset_t <- system.time(
        subsetH2Oframe <- data$fast.load.to.H2O(data$dat.sVar[rows_subset, load_var_names, with = FALSE],
                                                saveH2O = FALSE,
                                                destination_frame = destination_frame)
      )
      if (gvars$verbose) {
        print("time to subset and load data into H2OFRAME: "); print(load_subset_t)
      }

      # **** WILL CREATE A TEMPORARY h2o FRAME ****
      self$outfactors <- as.vector(h2o::h2o.unique(subsetH2Oframe[, outvar]))
      # Below being TRUE implies that the conversion to H2O.FRAME produced errors, since there should be no NAs in the source subset data
      if (any(is.na(self$outfactors))) stop("FOUND NA OUTCOMES IN H2OFRAME WHEN THERE WERE NOT SUPPOSED TO BE ANY")

      if (classify) {
        if (length(self$outfactors) > 2L) stop("cannot run binary regression/classification for outcome with more than 2 categories")
        subsetH2Oframe[, outvar] <- h2o::as.factor(subsetH2Oframe[, outvar])
      }
      return(subsetH2Oframe)
    },
    show = function(all_fits = FALSE, ...) {
      print(self$model.fit)
      return(invisible(NULL))
    }
  ),

  active = list( # 2 types of active bindings (w and wout args)
    emptydata = function() { private$subsetH2Oframe <- NULL},
    getsubsetH2Oframe = function() {private$subsetH2Oframe},
    getmodel_ids = function() {
      if (is.null(self$model.fit$model_ids)) {
        model_ids <- list(self$model.fit$H2O.model.object@model_id)
        new_names <- self$model.fit$model_algorithms[[1]]
        if (!is.null(self$model_contrl$name)) new_names <- new_names %+% "." %+% self$model_contrl$name
        names(model_ids) <- new_names
        return(model_ids)
      } else {
        return(self$model.fit$model_ids)
      }
    },
    getmodel_algorithms = function() { self$model.fit$model_algorithms }
  ),

  private = list(
    subsetH2Oframe = NULL
  )
)
