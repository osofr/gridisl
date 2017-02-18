## take a list of args, take a function body and return only the args that belong to function signature
keep_only_fun_args <- function(Args, fun) {
  keepArgs <- intersect(names(Args), names(formals(fun))) # captures optional arguments given by user
  if (length(keepArgs) > 0) {
    Args <- Args[keepArgs]
  } else {
    Args <- NULL
  }
  return(Args)
}

## 1. replace any arg in mainArgs if it also appears in userArgs
## 2. add any arg from userArgs that also appears in formals(fun) of function
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

## ---------------------------------------------------------------------------
## for running logistic regression with continuous outcome range [0-1]
## ---------------------------------------------------------------------------
## S3 method for fitting h2o GLM with binomial() family (logistic regression):
## use solver="L_BFGS" when doing classification and use "IRLSM" when not
fit.h2oglm <- function(fit.class, params, training_frame, y, x, fold_column, model_contrl, validation_frame  = NULL, ...) {
  if (gvars$verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                  intercept = TRUE, standardize = TRUE,
                  # family = "gaussian", # standardize = FALSE,
                  # solver = "L_BFGS", # solver = "IRLSM", # solver = "COORDINATE_DESCENT", # solver = "COORDINATE_DESCENT_NAIVE",
                  lambda = 0L, max_iterations = 100,
                  ignore_const_cols = FALSE,
                  missing_values_handling = "Skip")

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.glm)

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs[["validation_frame"]] <- validation_frame

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }

  model.fit <- do.call(h2o::h2o.glm, mainArgs)
  # h2o::h2o.glm(x = x, y = y, training_frame = training_frame, lambda = 0L, family = "gaussian")
  # predict(model.fit, newdata = mainArgs[["training_frame"]])

  # assign the fitted coefficients in correct order (same as predictor order in x)
  out_coef <- vector(mode = "numeric", length = length(x)+1)
  out_coef[] <- NA
  names(out_coef) <- c("Intercept", x)
  out_coef[names(model.fit@model$coefficients)] <- model.fit@model$coefficients
  nobs <- nrow(training_frame)

  return(create_fit_object(model.fit, model_alg = "glm", fitfunname = "h2o.glm", params = params, coef = out_coef, nobs = nobs, model_contrl = model_contrl))
}

## S3 method for h2o randomForest fit (Random Forest):
fit.h2orandomForest <- function(fit.class, params, training_frame, y, x, fold_column, model_contrl, validation_frame  = NULL, ...) {
  if (gvars$verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame, # balance_classes = TRUE,
                   ntrees = 100, ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.randomForest)

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs[["validation_frame"]] <- validation_frame

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }

  if ("distribution" %in% names(model_contrl) && ("bernoulli" %in% model_contrl[["distribution"]])) {
    mainArgs[["training_frame"]][[y]] <- h2o::as.factor(mainArgs[["training_frame"]][[y]])
    if (!is.null(mainArgs[["validation_frame"]])) mainArgs[["validation_frame"]][[y]] <- h2o::as.factor(mainArgs[["validation_frame"]][[y]])
  }

  model.fit <- do.call(h2o::h2o.randomForest, mainArgs)
  nobs <- nrow(training_frame)
  return(create_fit_object(model.fit, model_alg = "randomForest", fitfunname = "h2o.randomForest", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
}

## S3 method for h2o gbm fit, takes BinDat data object:
## use "bernoulli" when doing classification and use "gaussian" when not
fit.h2ogbm <- function(fit.class, params, training_frame, y, x, fold_column, model_contrl, validation_frame  = NULL, ...) {
  if (gvars$verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   # distribution = "bernoulli", # distribution = "gaussian", # balance_classes = TRUE,
                   ntrees = 100, ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.gbm)

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs$validation_frame <- validation_frame

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }

  if ("distribution" %in% names(model_contrl) && ("bernoulli" %in% model_contrl[["distribution"]])) {
    mainArgs[["training_frame"]][[y]] <- h2o::as.factor(mainArgs[["training_frame"]][[y]])
    if (!is.null(mainArgs[["validation_frame"]])) mainArgs[["validation_frame"]][[y]] <- h2o::as.factor(mainArgs[["validation_frame"]][[y]])
  }

  model.fit <- do.call(h2o::h2o.gbm, mainArgs)
  nobs <- nrow(training_frame)
  return(create_fit_object(model.fit, model_alg = "gbm", fitfunname = "h2o.gbm", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
}

## S3 method for h2o deeplearning fit, takes BinDat data object:
## use "bernoulli" when doing classification and use "gaussian" when doing regression
fit.h2odeeplearning <- function(fit.class, params, training_frame, y, x, fold_column, model_contrl, validation_frame  = NULL, ...) {
  if (gvars$verbose) h2o::h2o.show_progress() else h2o::h2o.no_progress()
  mainArgs <- list(x = x, y = y, training_frame = training_frame,
                   # distribution = "bernoulli", # distribution = "gaussian", # balance_classes = TRUE,
                   ignore_const_cols = FALSE)

  mainArgs <- replace_add_user_args(mainArgs, model_contrl, fun = h2o::h2o.gbm)

  # Is there a validation frame for model scoring?
  if (!is.null(validation_frame)) mainArgs$validation_frame <- validation_frame

  # Is there a fold_column for cross-validation based model scoring?
  if (!missing(fold_column)) {
    if (!is.null(fold_column) && is.character(fold_column) && (fold_column != "")) {
      mainArgs$fold_column <- fold_column
    }
  }

  if ("distribution" %in% names(model_contrl) && ("bernoulli" %in% model_contrl[["distribution"]])) {
    mainArgs[["training_frame"]][[y]] <- h2o::as.factor(mainArgs[["training_frame"]][[y]])
    if (!is.null(mainArgs[["validation_frame"]])) mainArgs[["validation_frame"]][[y]] <- h2o::as.factor(mainArgs[["validation_frame"]][[y]])
  }

  model.fit <- do.call(h2o::h2o.deeplearning, mainArgs)
  nobs <- nrow(training_frame)
  return(create_fit_object(model.fit, model_alg = "deeplearning", fitfunname = "h2o.deeplearning", params = params, coef = NULL, nobs = nobs, model_contrl = model_contrl))
}