#' @export
fit.h2oGridLearner <- function(fit.class, fit, training_frame, y, x, model_contrl, fold_column, ...) {
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
    fitted_models <- grid_objects <- top_grid_models <- vector(mode = "list", length = length(grid.algorithms));
    names(fitted_models) <- names(grid_objects) <- names(top_grid_models) <- grid.algorithms

    for (grid.algorithm in grid.algorithms) {
      grid_model_fit <- SLfit.h2ogrid(grid.algorithm = grid.algorithm, training_frame = training_frame, y = y, x = x, family = family,
                                      fold_column = fold_column, model_contrl = model_contrl, ...)
      top_grid_models[[grid.algorithm]] <- grid_model_fit$top.model
      grid_model_H2O <- grid_model_fit$H2O.model.object
      grid_objects[[grid.algorithm]] <- grid_model_H2O
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
      learner_fit <- SLfit.h2oLearner(learner = learner, training_frame = training_frame, y = y, x = x, family = family,
                                      fold_column = fold_column, model_contrl = model_contrl, ...)
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
  # Saving the fits:
  # ----------------------------------------------------------------------------------------------------
  if (!is.null(model_contrl$save.ensemble) && model_contrl$save.ensemble) {
    if (is.null(model_contrl$ensemble.dir.path) || (model_contrl$ensemble.dir.path %in% "")) {
      stop("when saving ensemble must specify the directory path with 'ensemble.dir.path' parameter")
    }
    saveres <- lapply(fitted_models_all, function(h2omodel) h2o::h2o.saveModel(object = h2omodel, path = model_contrl$ensemble.dir.path, force = TRUE))
    # h2oEnsemble::h2o.save_ensemble(stacked.fit, path = model_contrl$ensemble.dir.path, force = TRUE)
    # h2oEnsemble::h2o.save_ensemble(stacked.fit, path = model_contrl$ensemble.dir.path, force = TRUE, export_levelone = TRUE)
  }

  # fit$fitfunname <- "h2oEnsemble::h2o.stack";
  # fit$H2O.model.object <- stacked.fit

  fit$grid_objects <- grid_objects
  fit$grid_ids <- lapply(fit$grid_objects, function(grids_object) grids_object@grid_id)
  fit$top_grid_models <- top_grid_models

  # TO DIRECTLY SAVE ALL MODEL FITS FROM GRID SEARCH (base-learners)
  fit$fitted_models_all <- fitted_models_all
  fit$modelnames <- lapply(fit$fitted_models_all, function(model) model@model_id)

  class(fit) <- c(class(fit)[1], c("H2Ogridmodel"))
  return(fit)
}