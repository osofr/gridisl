`[.DataStorageClass` <- function(var, indx, ...) {
  var$get.dat.sVar(indx)
}

## benefits over SuperLearner package from gencv: arbitrary CV schemes foreach parallelization from SL implementation, can
## return fold specific SL fits for smart sequential super learner

fitmods <- function(SL.library, ...) { UseMethod("fitmods") }

## do this robust to errors
fitmods.SL <- function(SL.library, Y, X, newX, family, obsWeights, id, ...) {
  fits <- future_lapply(SL.library, function(learner) {
      res <- NULL
      try({
          res <- do.call(learner, list(Y = Y, X = X, newX = newX, family = family, obsWeights = obsWeights, id = id, ...))
      }, silent = FALSE)

      res
  })

  names(fits) <- SL.library
  return(fits)
}

fitmods.ModelStack <- function(SL.library, X, newX, y_name, id_name, t_name, x_name, family, obsWeights, ...) {
  fits <- gridisl:::fit_model(ID = id_name,
                              t_name = t_name,
                              x = x_name,
                              y = y_name,
                              train_data = X,
                              # valid_data = newX,
                              models = SL.library)

  valid_preds <- gridisl:::predict_generic(fits,
                                           newdata = newX,
                                           add_subject_data = FALSE,
                                           best_only = FALSE)

  return(list(fits, preds = valid_preds))
}


## Split-specific predictions for validation sets
## Uses offsets that are also allowed to be split specific
cv_preds <- function(fold, foldFits, newdata, holdout = TRUE, ...) {
  ## These will be automatically defined in the calling frame of this function
  ## when the cross-validator that calls cv_split_preds()
  v <- origami::fold_index()
  train_idx <- origami::training()
  valid_idx <- origami::validation()
  pred <- predict_SL(foldFits[[v]], newdata)
  if (holdout) pred <- pred[valid_idx, ]
  list(pred = pred, valid_idx = valid_idx, train_idx = train_idx)
}

# @param fold a Fold to be passed to cv_SL.
# @export
# @rdname origamiSL
cv_SL <- function(fold, y_name, X, SL.library, family, obsWeights, id_name, t_name, x_name, fold_y_names, ...) {
  # training objects
  # if (!missing(Y)) {
  #     train_Y <- training(Y)
  # }

  v <- origami::fold_index()

  # browser()

  if (!missing(fold_y_names) && !is.null(fold_y_names)) {
    if (length(fold_y_names) < v) stop("the names of the split-specific folds in X must be equal to the total number of CV folds")
    # X[[y_name]] <- fold_y_names[[v]][[paste0(y_name, "_ss", v)]]
    X[[y_name]] <- X[[fold_y_names[[v]]]]
  }

  # browser()

  train_X <- origami::training(X)
  train_obsWeights <- origami::training(obsWeights)
  # train_id <- training(id)

  # validation objects
  valid_X <- origami::validation(X)
  valid_index <- origami::validation()

  # fit on training and predict on validation
  fits <- fitmods(SL.library,
                  # Y = train_Y,
                  X = train_X,
                  newX = valid_X,
                  family = family,
                  obsWeights = train_obsWeights,
                  id_name = id_name,
                  t_name = t_name,
                  x_name = x_name,
                  y_name = y_name,
                  ...)

  preds <- fits$preds
  fits$preds <- NULL

  fit_failed <- rep.int(FALSE, ncol(preds))
  names(fit_failed) <- colnames(preds)
  # fit_failed <- sapply(fits, is.null)
  # if (all(fit_failed)) {
  #     stop(sprintf("All learners failed for fold %d", fold_index()))
  # } else if (any(fit_failed)) {
  #     dummy_pred <- fits[[which(!fit_failed)[1]]]$pred * 0
  #     for (failed_index in which(fit_failed)) {
  #         fits[[failed_index]] <- list(pred = dummy_pred, fit = NA)
  #     }
  # }

  # browser()

  # extract and collapse predictions
  # preds <- lapply(fits, function(fit) drop(fit$pred))
  # Z <- do.call(abind::abind, c(preds, rev.along = 0))

  results <- list(Z = preds,
                  valid_index = valid_index,
                  valY = origami::validation(X[[y_name]]),
                  # valY = valid_X[[y_name]],
                  valWeights = origami::validation(obsWeights),
                  fits = fits,
                  fit_failed = fit_failed)

  return(results)
}

# @title origamiSL
# @description SuperLearner implemented using orgami cross-validation. Leverages a lot of code from Eric Polley's
# SuperLearner package. Because of it's based on origami, we get two features for free:
# foreach based parallelization, and support for arbitrary cross-validation schemes.
# @param Y vector of outcomes.
# @param X vector of covariates.
# @param newX currently ignored.
# @param SL.library character vector of learner names.
# @param family Either gaussian() or binomial() depending on if Y is binary or continuous.
# @param obsWeights vector of weights.
# @param id vector of ids.
# @param folds a list of Folds. See \code{\link{make_folds}}. If missing,
#        will be created based on Y and cluster_ids.
# @param method a combination method. Typically either method.NNLS or method.NNloglik.
# @param cvfun the function to be run at each cross-validation step. Changing this allows,
#        for example, dynamic creation of fold-specific data. Must have same prototype as cv_SL
# @param control Currently ignored.
# @param ... other arguments passed to the underlying call to \code{\link{cross_validate}}
#
# @seealso \code{\link{predict.origamiSL}}
#
# @export
# # Y,
origamiSL <- function(X,
                      newX = NULL,
                      SL.library,
                      family = gaussian(),
                      obsWeights = rep(1, nrow(newX)),
                      id = NULL,
                      folds = NULL,
                      method = SuperLearner::method.NNLS(),
                      cvfun = cv_SL,
                      control = list(),
                      id_name,
                      t_name,
                      x_name,
                      y_name,
                      fold_y_names = NULL,
                      ...) {

    if (!is.data.frame(X)) stop("X must be a data.frame")
    # if (!missing(Y)) stop("this version of SL must provide Y (outcome) as a character name of the column in X, using argument 'y_name'")

    if (is.null(folds)) {
      folds <- make_folds(X[[y_name]], cluster_ids = X[[id_name]])
    }

    if (is.null(newX)) {
      newX <- X
    }

    # if (is.null(id)) {
    #     id <- seq_along(Y)
    # }

    # fit algorithms to folds, get predictions
    results <- origami::cross_validate(cvfun,
                                       folds = folds,
                                       y_name = y_name,
                                       X = X,
                                       SL.library = SL.library,
                                       family = family,
                                       obsWeights = obsWeights,
                                       id_name = id_name,
                                       t_name = t_name,
                                       x_name = x_name,
                                       fold_y_names = fold_y_names,
                                       future.globals = FALSE)

    # unshuffle results
    # Z_tmp <- aorder(results$Z, order(results$valid_index))
    Z <- results$Z
    Z[, ("valY") := results$valY]
    Z[, ("valWeights") := results$valWeights]
    Z[, ("index") := results$valid_index]
    data.table::setkeyv(Z, cols = "index")

    valY <- Z[["valY"]]; Z[, ("valY") := NULL]
    valWeights <- Z[["valWeights"]]; Z[, ("valWeights") := NULL]
    Z[, ("index") := NULL]

    # valY <- aorder(results$valY, order(results$valid_index))
    # valY <- results$valY[order(results$valid_index)]
    # valWeights <- results$valWeights[order(results$valid_index)]

    # identify which algorithms failed and drop them
    failed_ever <- unique(names(which(results$fit_failed)))
    if (length(failed_ever) > 0) {
      warning(sprintf("The following learners failed on at least one fold and will be dropped: %s", paste(failed_ever, collapse = ", ")))
    }

    # good_ind <- match(setdiff(SL.library, failed_ever), SL.library)
    good_ind <- seq_along(Z)

    # drop bad algorithms from Z
    # last_dim <- length(safe_dim(Z))
    # Z <- index_dim(Z, good_ind, last_dim)
    # rownames(Z) <- NULL
    # SL.library <- SL.library[good_ind]

    # calculate coefficients
    # SLcontrol <- SuperLearner.control(control)
    getCoef <- method$computeCoef(Z = as.matrix(Z), Y = valY, obsWeights = valWeights, libraryNames = names(Z), verbose = TRUE)
    # getCoef <- method$computeCoef(Z = Z, Y = valY, obsWeights = valWeights, libraryNames = SL.library, verbose = FALSE, control = SLcontrol)

    # print("SL coefs"); print(getCoef)

    coef <- getCoef$coef
    cvRisk <- getCoef$cvRisk

    # browser()

    # refit models on full sample
    resub <- origami::make_folds(X, fold_fun = origami::folds_resubstitution)[[1]]
    full <- cvfun(resub, y_name = y_name,
                  X = X, SL.library = SL.library,
                  family = family, obsWeights = obsWeights,
                  id_name = id_name,
                  t_name = t_name,
                  x_name = x_name)

    fullSL_preds <- data.table::data.table(method$computePred(full$Z, coef))
    colnames(fullSL_preds) <- "preds"

    # fit object for predictions
    fitObj <- structure(list(library_fits = full$fits,
                             fullSL_preds = fullSL_preds,
                             coef = coef,
                             family = family,
                             method = method
                             # control = SLcontrol
                             ),
                             class = "origamiSL_fit",
                             folds = folds)

    # analogous objects but with learners fit only in a particular fold
    foldFits <- lapply(seq_along(folds), function(fold) {
        fitObj$library_fits <- results$fits[[fold]] # [good_ind]
        fitObj
    })

    # results
    out <- list(coef = coef,
                cvRisk = cvRisk,
                Z = Z,
                valY = valY,
                valWeights = valWeights,
                SL.library = SL.library,
                folds = folds,
                fullFit = fitObj,
                foldFits = foldFits)
    class(out) <- c("origamiSL", "SuperLearner")
    return(out)
}

fit_origamiSL <- function(ID,
                          t_name,
                          x,
                          y,
                          data,
                          models,
                          nfolds = 5,
                          fold_column = NULL,
                          subset_idx = NULL,
                          refit = TRUE,
                          seed = NULL,
                          fold_y_names,
                          verbose = getOption("gridisl.verbose"),
                          ...) {

  gvars$verbose <- verbose
  gvars$method <- "origamiSL"
  nodes <- list(Lnodes = x, Ynode = y, IDnode = ID, tnode = t_name)
  orig_colnames <- colnames(data)

  if (is.null(fold_column)) {
    fold_column <- "fold"
    data <- add_CVfolds_ind(data, ID, nfolds = nfolds, fold_column = fold_column, seed = seed)
  }

  if (!is.null(subset_idx)) {
    data <- data[subset_idx, ]
  }

  Vfolds <- make_kfold_from_column(data[[fold_column]])

  ## ------------------------------------------------------------------------------------------
  ## Perform SL fitting
  ## ------------------------------------------------------------------------------------------
  SLfit <- origamiSL(X = data,
                    y_name = y,
                    x_name = x,
                    id_name = ID,
                    t_name = t_name,
                    SL.library = models,
                    folds = Vfolds,
                    fold_y_names = fold_y_names, ...
                    )

  if (verbose) print(SLfit)

  return(SLfit)
}

# @export
predict_SL_byfold <- function(modelfit,
                              newdata,
                              add_subject_data = FALSE,
                              subset_idx = NULL,
                              verbose = getOption("gridisl.verbose"), ...) {

  Vfolds <- modelfit$folds
  # fold_column <- newdata$fold_column
  # Vfolds <- newdata$make_origami_fold_from_column(subset_idx)
  if (!is.null(subset_idx)) newdata <- newdata[subset_idx, ]
  # Vfolds2 <- make_kfold_from_column(newdata[[fold_column]])
  # identical(Vfolds, Vfolds2)
  # identical(Vfolds, modelfit$folds)

  ## by-fold / fold-specific / split-specific SL predictions for training/validation sets
  ## returns a list, each list item v consists of n predictions for v-fold trained SL
  byfold_preds <- origami::cross_validate(cv_preds,
                                          folds = Vfolds,
                                          foldFits = modelfit$foldFits,
                                          newdata = newdata,
                                          holdout = FALSE,
                                          .combine = FALSE,
                                          future.globals = FALSE)

  # byfold_preds$pred

  return(byfold_preds)
}

# @title predict.origamiSL
# @description prediction function for origamiSL. Note, while this is a working SuperLearner implementation, it is intended more as an example
# than production code. As such, it is subject to change in the future.
# @param object origamiSL fit.
# @param newdata matrix or data.frame of new covariate values. If newdata='cv-original' (default), it will return Z (the split-specific library predictions), and the Super Learner applied to Z (the split-specific Super Learner Predictions)
# @byfold Returning SuperLearner predictions by each fold (fold-specific or split-specific).
# Results in a list of n predictions, one for each fold among V folds.
# @param ... other arguments to the learners.
# @return A list with two elements: \code{library_pred} are the predictions from the library functions, and \code{pred} is the prediction from the SuperLearner.
# @seealso \code{\link{origamiSL}}
#
#' @export
predict_SL.origamiSL <- function(modelfit,
                                  newdata,
                                  add_subject_data = FALSE,
                                  subset_idx = NULL,
                                  holdout = FALSE,
                                  byfold = FALSE,
                                  verbose = getOption("gridisl.verbose"), ...) {
  # if (missing(newdata))
  #     (stop("newdata must be specified"))
  # if (identical(newdata, "cv-original")) {

  if (missing(newdata)) {
    if (holdout) {
      Z <- modelfit$Z
      # pred_obj <- list(
          pred = modelfit$fullFit$method$computePred(Z, modelfit$fullFit$coef, control = modelfit$fullFit$control)
          pred <- data.table::data.table(pred)
          colnames(pred) <- "preds"
          # library_pred = Z)
    } else if (byfold) {
      stop("... byfold prediction with missing newdata is not implemented...")
    } else if (!holdout) {
      pred <- modelfit$fullFit$fullSL_preds
    }

  } else {
    # browser()

    if (!is.null(subset_idx)) newdata <- newdata[subset_idx, ]

    if (holdout) {
      ## holdout SL predictions
      pred <- origami::cross_validate(cv_preds,
                                      folds = modelfit$folds,
                                      foldFits = modelfit$foldFits,
                                      newdata = newdata,
                                      future.globals = FALSE)
      pred <- data.table::data.table(pred$pred, index = pred$valid_idx)
      data.table::setkeyv(pred, cols = "index")
      pred[, "index" := NULL]
      # pred <- valid_preds$pred
    } else if (byfold) {
      pred <- predict_SL_byfold(modelfit, newdata, subset_idx = NULL, ...)
    } else if (!holdout) {
      pred <- predict_SL(modelfit$fullFit, newdata, ...)
    }
  }

  return(pred)
}


# @export
predict_SL.origamiSL_fit <- function(modelfit, newdata, ...) {
  if (missing(newdata))
      (stop("newdata must be specified"))
  # library_pred <- lapply(seq_along(modelfit$library_fits), function(index) {
  #     fitobj <- modelfit$library_fits[[index]]
  #     pred <- predict(fitobj$fit, newdata = newdata, family = modelfit$family, ...)
  #     drop(pred)
  # })
  library_pred <- gridisl:::predict_generic(modelfit$library_fits[[1]],
                                            newdata = newdata,
                                            add_subject_data = FALSE,
                                            best_only = FALSE)
  # Z <- do.call(abind::abind, c(library_pred, rev.along = 0))
  # dimnames(Z)[[length(dim(Z))]] <- names(modelfit$library_fits)
  pred <- modelfit$method$computePred(library_pred, modelfit$coef, control = modelfit$control)
  pred <- data.table::data.table(pred)
  colnames(pred) <- "preds"
  return(pred)
  # return(list(pred = pred, library_pred = library_pred))
}
