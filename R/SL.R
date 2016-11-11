predictP1_externalCV <- function(m.fit, data_valid, DataStorageObject, ...) {
  h2o.no_progress()
  pAoutMat <- matrix(gvars$misval, nrow = nrow(data_valid), ncol = length(m.fit$fitted_models_all))
  colnames(pAoutMat) <- names(m.fit$model_ids)
  outvar <- m.fit$params$outvar
  predvars <- m.fit$params$predvars

  ## **** LOAD ALL DATA ONLY ONCE ****
  validH2Oframe <- DataStorageObject$fast.load.to.H2O(data_valid[, c(outvar, predvars), with = FALSE],
                                                      saveH2O = FALSE, destination_frame = "validH2Oframe")

  # Assumes folds were equivalent across all models
  h2o_model_1 <- m.fit$fitted_models_all[[1]]
  fold <- as.data.frame(h2o.cross_validation_fold_assignment(h2o_model_1))
  vfolds_cat <- sort(unique(fold$fold_assignment))

  for (vfold_idx in seq_along(vfolds_cat)) {
    fold_idx_cv.i <- (fold$fold_assignment %in% vfolds_cat[vfold_idx])
    ## Define validation frame for this fold:
    validH2Oframe_cv.i <- validH2Oframe[which(fold_idx_cv.i),]
    cv.i_foldframeID <- h2o.getId(validH2Oframe_cv.i)
    dest_key_LIST <- vector(mode = "list", length = length(m.fit$fitted_models_all))

    for (idx in seq_along(m.fit$fitted_models_all)) {
      # print("idx: "); print(idx)
      # print("model: "); print(names(m.fit$model_ids)[idx])
      h2o_model <- m.fit$fitted_models_all[[idx]]
      cv_models_IDs <- lapply(h2o_model@model$cross_validation_models, "[[", "name")

      ## Prediction for each CV (vfold) model with holdouts from new validation data (new summaries)
      # fold <- as.data.frame(h2o.cross_validation_fold_assignment(h2o_model))

      ## Submit a job for prediction on a fold using internal REST API.
      ## Don't pull the prediction results until all of these jobs were submitted.
      url <- paste0('Predictions/models/', cv_models_IDs[[vfold_idx]], '/frames/',  cv.i_foldframeID)
      res <- h2o:::.h2o.__remoteSend(url, method = "POST", h2oRestApiVersion = 4)
      job_key <- res$key$name
      dest_key <- res$dest$name
      dest_key_LIST[[idx]] <- dest_key

      # h2o:::.h2o.__waitOnJob(job_key, pollInterval = 0.01)
      # newpreds <- h2o.getFrame(dest_key)
      # browser()
      # if ("p1" %in% colnames(newpreds)) {
      #   pAoutMat[fold_idx_cv.i, idx] <- as.vector(newpreds[,"p1"])
      # } else {
      #   # sum(fold_idx_cv.i)
      #   # length(as.vector(newpreds[,"predict"]))
      #   pAoutMat[fold_idx_cv.i, idx] <- as.vector(newpreds[,"predict"])
      # }
    }

    h2o:::.h2o.__waitOnJob(job_key, pollInterval = 0.01)
    for (idx in seq_along(dest_key_LIST)) {
      newpreds <- h2o.getFrame(dest_key_LIST[[idx]])
      if ("p1" %in% colnames(newpreds)) {
        pAoutMat[fold_idx_cv.i, idx] <- as.vector(newpreds[,"p1"])
      } else {
        pAoutMat[fold_idx_cv.i, idx] <- as.vector(newpreds[,"predict"])
      }
    }

    # remove the fold-specific frame from h2o cluster
    # h2o.rm(cv.i_foldframeID)
  }

  return(pAoutMat)
}

# ## ----------------------------------------------------------------
# ## Prediction for h2ofit objects, predicts E(outvar | newXmat)
# ## ----------------------------------------------------------------
# predictP1.H2Oensemblemodel <- function(m.fit, ParentObject, DataStorageObject, subset_idx, ...) {
#   subsetH2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, DataStorageObject, subset_idx)
#   pAout <- rep.int(gvars$misval, length(subset_idx))
#   if (sum(subset_idx) > 0) {
#     predictObject <- predict(m.fit$H2O.model.object, newdata = subsetH2Oframe)
#     predictFrame <- predictObject$pred
#     if ("p1" %in% colnames(predictFrame)) {
#       pAout[subset_idx] <- as.vector(predictFrame[,"p1"])
#     } else {
#       pAout[subset_idx] <- as.vector(predictFrame[,"predict"])
#     }
#   }
#   return(pAout)
# }

# #' @export
# fit.h2oSuperLearner <- function(fit.class, fit, training_frame, y, x, model_contrl, fold_column, validationH2Oframe, ...) {
#   if (is.null(fold_column)) stop("must define the column of CV fold IDs using data$define_CVfolds()")

#   if (!is.null(model_contrl$load.ensemble) && model_contrl$load.ensemble) {
#     if (is.null(model_contrl$ensemble.dir.path) || (model_contrl$ensemble.dir.path %in% "")) {
#       stop("when loading ensemble must specify the directory path with 'ensemble.dir.path' parameter")
#     }
#     stacked.fit <- h2oEnsemble::h2o.load_ensemble(path = model_contrl$ensemble.dir.path, import_levelone = FALSE)
#     # stacked.fit <- h2oEnsemble::h2o.load_ensemble(path = model_contrl$ensemble.dir.path, import_levelone = TRUE)

#   } else {
#     family <- model_contrl$family
#     grid.algorithms <- model_contrl$grid.algorithm
#     learners <- model_contrl$learner
#     if (is.null(family)) family <- "binomial"
#     # Will put all fitted models in a single list for stacking
#     fitted_models_all <- NULL
#     nfolds <- model_contrl$nfolds
#     model_contrl$nfolds <- NULL
#     model_contrl$fold_column <- fold_column
#     model_contrl$keep_cross_validation_predictions <- TRUE

#     if (is.null(grid.algorithms) && is.null(learners)) {
#       stop("must specify either 'grid.algorithm' or 'learner' when performing estimation with SuperLearner")
#     }

#     if (!is.null(grid.algorithms)) {
#       if (!is.character(grid.algorithms)) stop("'grid.algorithm' must be a vector of strings naming the grid.algorithms to use in 'h2o.grid'")
#       fitted_models <- vector(mode = "list", length = length(grid.algorithms))
#       names(fitted_models) <- grid.algorithms
#       for (grid.algorithm in grid.algorithms) {
#         grid_model_fit <- SLfit.h2ogrid(grid.algorithm = grid.algorithm, training_frame = training_frame, y = y, x = x, family = family, fold_column = fold_column, model_contrl = model_contrl, ...)
#         grid_model_H2O <- grid_model_fit$H2O.model.object
#         fitted_models[[grid.algorithm]] <- lapply(grid_model_H2O@model_ids, function(model_id) h2o::h2o.getModel(model_id))
#       }
#       for (grid.algorithm in grid.algorithms) {
#         fitted_models_all <- c(fitted_models_all, fitted_models[[grid.algorithm]])
#       }
#     }

#     if (!is.null(learners)) {
#       if (!is.character(learners)) stop("'learner' must be a vector of strings naming specific wrappers/learners")
#       fitted_models_l <- vector(mode = "list", length = length(learners))
#       names(fitted_models_l) <- learners
#       for (learner in learners) {
#         learner_fit <- SLfit.h2oLearner(learner = learner, training_frame = training_frame, y = y, x = x, family = family, fold_column = fold_column, model_contrl = model_contrl, ...)
#         learner_model_H2O <- learner_fit$H2O.model.object
#         fitted_models_l[[learner]] <- learner_model_H2O
#       }
#       fitted_models_all <- c(fitted_models_all, fitted_models_l)
#     }

#     # to by-pass error check in h2o.stack:
#     for (idx in seq_along(fitted_models_all)) {
#       fitted_models_all[[idx]]@allparameters$fold_assignment <- "Modulo"
#       fitted_models_all[[idx]]@allparameters$nfolds <- nfolds
#     }

#     # Specify a defalt GLM as the metalearner
#     metalearner <- model_contrl$metalearner
#     if (is.null(metalearner)) metalearner <- "h2o.glm_nn"
#     stacked.fit <- h2oEnsemble::h2o.stack(models = fitted_models_all, response_frame = training_frame[,y], metalearner = metalearner)

#     # ----------------------------------------------------------------------------------------------------
#     # Saving the fits:
#     # ----------------------------------------------------------------------------------------------------
#     if (!is.null(model_contrl$save.ensemble) && model_contrl$save.ensemble) {
#       if (is.null(model_contrl$ensemble.dir.path) || (model_contrl$ensemble.dir.path %in% "")) {
#         stop("when saving ensemble must specify the directory path with 'ensemble.dir.path' parameter")
#       }
#       h2oEnsemble::h2o.save_ensemble(stacked.fit, path = model_contrl$ensemble.dir.path, force = TRUE)
#       # h2oEnsemble::h2o.save_ensemble(stacked.fit, path = model_contrl$ensemble.dir.path, force = TRUE, export_levelone = TRUE)
#     }
#   }

#   # ----------------------------------------------------------------------------------------------------
#   # Compute the final SL performance on the training set:
#   # ----------------------------------------------------------------------------------------------------
#   print("SuperLearner fit:"); print(stacked.fit$metafit)
#   perf <- h2oEnsemble::h2o.ensemble_performance(stacked.fit, newdata = training_frame, score_base_models = FALSE)
#   print("SuperLearner overall performance (AUC) on the training set: "); print(perf)
#   print("SuperLearner overall performance (MSE) on the training set: "); print(perf$ensemble@metrics$MSE)
#   # h2o.glm_nn <- function(..., non_negative = TRUE) h2o.glm.wrapper(..., non_negative = non_negative)
#   # stacked.fit3 <- h2o.metalearn(stacked.fit, metalearner = "h2o.glm_nn")
#   # perf3 <- h2o.ensemble_performance(stacked.fit3, newdata = training_frame, score_base_models = FALSE)
#   # print(perf3)

#   # out_coef <- vector(mode = "numeric", length = length(fitted_models_all)+1)
#   out_coef <- vector(mode = "numeric", length = length(stacked.fit$learner)+1)
#   out_coef[] <- NA
#   names(out_coef) <- names(stacked.fit$metafit@model$coefficients)
#   out_coef[names(stacked.fit$metafit@model$coefficients)] <- stacked.fit$metafit@model$coefficients
#   names(out_coef)[which(!names(stacked.fit$learner) %in% "")+1] <- names(stacked.fit$learner)[!names(stacked.fit$learner) %in% ""]

#   fit$coef <- out_coef;
#   fit$linkfun <- NA
#   fit$nobs <- nrow(training_frame)

#   if (gvars$verbose) {
#     print("SuperLearner fits:")
#     print(fit$coef)
#   }

#   fit$fitfunname <- "h2oEnsemble::h2o.stack";
#   fit$H2O.model.object <- stacked.fit
#   # fit$modelnames <- "SL.model"
#   fit$model_algorithms <- "SuperLearner"
#   fit$model_ids <- lapply(fitted_models_all, function(model) model@model_id)

#   # TO DIRECTLY SAVE ALL MODEL FITS FROM GRID SEARCH (base-learners)
#   # fit$fitted_models_all <- fitted_models_all

#   class(fit) <- c(class(fit)[1], c("H2Oensemblemodel"))
#   return(fit)
# }
