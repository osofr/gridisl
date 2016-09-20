#' @export
fit.h2oGridLearner <- function(fit.class, fit, training_frame, y, x, model_contrl, fold_column, ...) {
  # if (is.null(fold_column)) stop("must define the column of CV fold IDs using data$define_CVfolds()")

  family <- model_contrl$family
  grid.algorithms <- model_contrl$grid.algorithm
  learners <- model_contrl$learner
  if (is.null(family)) family <- "binomial"

  # Will put all fitted models in a single list for stacking:
  fitted_models_all <- NULL
  nfolds <- model_contrl$nfolds
  model_contrl$nfolds <- NULL

  if (is.null(grid.algorithms) && is.null(learners)) {
    stop("must specify either 'grid.algorithm' or 'learner' when performing estimation with GridLearner")
  }

  if (!is.null(grid.algorithms)) {
    if (!is.character(grid.algorithms)) stop("'grid.algorithm' must be a vector of strings naming the grid.algorithms to use in 'h2o.grid'")
    fitted_models <- vector(mode = "list", length = length(grid.algorithms))
    names(fitted_models) <- grid.algorithms
    for (grid.algorithm in grid.algorithms) {
      grid_model_fit <- SLfit.h2ogrid(grid.algorithm = grid.algorithm, training_frame = training_frame, y = y, x = x, family = family, fold_column = fold_column, model_contrl = model_contrl, ...)
      grid_model_H2O <- grid_model_fit$H2O.model.object
      fitted_models[[grid.algorithm]] <- lapply(grid_model_H2O@model_ids, function(model_id) h2o::h2o.getModel(model_id))
    }
    for (grid.algorithm in grid.algorithms) {
      fitted_models_all <- c(fitted_models_all, fitted_models[[grid.algorithm]])
    }
  }

  if (!is.null(learners)) {
    if (!is.character(learners)) stop("'learner' must be a vector of strings naming specific wrappers/learners")
    fitted_models_l <- vector(mode = "list", length = length(learners))
    names(fitted_models_l) <- learners
    for (learner in learners) {
      learner_fit <- SLfit.h2oLearner(learner = learner, training_frame = training_frame, y = y, x = x, family = family, fold_column = fold_column, model_contrl = model_contrl, ...)
      learner_model_H2O <- learner_fit$H2O.model.object
      fitted_models_l[[learner]] <- learner_model_H2O
    }
    fitted_models_all <- c(fitted_models_all, fitted_models_l)
  }

  # to by-pass error check in h2o.stack:
  for (idx in seq_along(fitted_models_all)) {
    fitted_models_all[[idx]]@allparameters$fold_assignment <- "Modulo"
    fitted_models_all[[idx]]@allparameters$nfolds <- nfolds
  }

  # ----------------------------------------------------------------------------------------------------
  # Evaluate the MSE based on the leave-one-out data point
  # ----------------------------------------------------------------------------------------------------
  # ....
  # browser()

  # # Specify a defalt GLM as the metalearner
  # metalearner <- model_contrl$metalearner
  # if (is.null(metalearner)) metalearner <- "h2o.glm_nn"
  # stacked.fit <- h2oEnsemble::h2o.stack(models = fitted_models_all, response_frame = training_frame[,y], metalearner = metalearner)

  # # ----------------------------------------------------------------------------------------------------
  # # Compute the final SL performance on the training set:
  # # ----------------------------------------------------------------------------------------------------
  # print("SuperLearner fit:"); print(stacked.fit$metafit)
  # perf <- h2oEnsemble::h2o.ensemble_performance(stacked.fit, newdata = training_frame, score_base_models = FALSE)
  # print("SuperLearner overall performance (AUC) on the training set: "); print(perf)
  # print("SuperLearner overall performance (MSE) on the training set: "); print(perf$ensemble@metrics$MSE)
  # # h2o.glm_nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)
  # # stacked.fit3 <- h2o.metalearn(stacked.fit, metalearner = "h2o.glm_nn")
  # # perf3 <- h2o.ensemble_performance(stacked.fit3, newdata = training_frame, score_base_models = FALSE)
  # # print(perf3)

  # # out_coef <- vector(mode = "numeric", length = length(fitted_models_all)+1)
  # out_coef <- vector(mode = "numeric", length = length(stacked.fit$learner)+1)
  # out_coef[] <- NA
  # names(out_coef) <- names(stacked.fit$metafit@model$coefficients)
  # out_coef[names(stacked.fit$metafit@model$coefficients)] <- stacked.fit$metafit@model$coefficients
  # names(out_coef)[which(!names(stacked.fit$learner) %in% "")+1] <- names(stacked.fit$learner)[!names(stacked.fit$learner) %in% ""]

  # fit$coef <- out_coef;
  # fit$linkfun <- NA
  # fit$nobs <- nrow(training_frame)

  # if (gvars$verbose) {
  #   print("SuperLearner fits:")
  #   print(fit$coef)
  # }

  # fit$fitfunname <- "h2oEnsemble::h2o.stack";
  # fit$H2O.model.object <- stacked.fit

  # TO DIRECTLY SAVE ALL MODEL FITS FROM GRID SEARCH (base-learners)
  fit$fitted_models_all <- fitted_models_all

  # class(fit) <- c(class(fit)[1], c("H2Oensemblemodel"))
  class(fit) <- c(class(fit)[1], c("H2Omodel"))
  return(fit)
}