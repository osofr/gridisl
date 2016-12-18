check_out_of_sample_consistency <- function(models_list, valid_H2Oframe, predvars, fold_column) {
  all_folds_h2o <- lapply(models_list, h2o.cross_validation_fold_assignment)
  train_frame_ID_1 <- models_list[[1]]@parameters$training_frame

  if (length(all_folds_h2o) > 1) {
    ## 1. Test that the exactly the same fold assignments were used by all CV models in the ensemble.
    for (idx in 2:length(all_folds_h2o) ) {
      if (!h2o.all(all_folds_h2o[[1]]==all_folds_h2o[[idx]])  )
        stop("Out-of-sample (holdout) predictions for new data has failed. The fold assignmets of the following CV model do not match to others: " %+% names(models_list)[idx])
    }

    ## 2. Test that same training h2oFrame was used for all models in the ensemble (just in case).
    for (idx in 2:length(all_folds_h2o) ) {
      if (!all.equal(train_frame_ID_1, models_list[[idx]]@parameters$training_frame))
        stop("Out-of-sample (holdout) predictions for new data has failed. It appears that some of the CV models in ensemble used different training frames.")
    }
  }

  ## 3. Test that the validation and training data have exactly the same fold assignments (in h2oFrame)
  if (!all(valid_H2Oframe[[fold_column]] == all_folds_h2o[[1]]))
    stop("Out-of-sample (holdout) predictions for new data has failed. The fold assignments in new data (validation_data) and training data appear to be different.")

  ## 4a. Test that the new validation data (in h2oFrame) has the same number of observations as the training data
  if (!(nrow(valid_H2Oframe) == nrow(h2o.getFrame(train_frame_ID_1))))
    stop("Out-of-sample (holdout) predictions for new data has failed. The number of rows in new data (validation_data) does not match that of the training data.")

  ## 4b. Test that all predictors are present in the validation data (in h2oFrame)
  if (!all(c(predvars,fold_column) %in% colnames(valid_H2Oframe)))
    stop("Out-of-sample (holdout) predictions for new data has failed. Some of the predictors were not found in new data (validation_data).")
  return(invisible(TRUE))
}

## ----------------------------------------------------------------------------------------------------------------------------------
## Evaluate out-of-sample predictions from V cross-validation models, based on new validation_data.
## Can be useful for re-scoring the models when the validation data has to change from the training data in V-fold cross-validation.
## Will only perform the out-sample predictions for each model V_i
## (i.e., predictions in validation_data will be only made for rows that were not used for training the model V_i)
## In the end we generate a vector of n=nrow(validation_data) predictions by combining predictions from all models V=(V_1,...,V_v)
## This procedure is repeated for each cross-validated model in the ensemble, resulting in a matrix of predictions (n,k),
## where k is the total number of models trained by this ensemble (with h2o.grid, etc) and is equal to length(models_list)
## ----------------------------------------------------------------------------------------------------------------------------------
predict_out_of_sample_cv <- function(m.fit, ParentObject, validation_data, subset_idx, predict_model_names, ...) {
  # h2o.no_progress()
  models_list <- m.fit$fitted_models_all
  if (!missing(predict_model_names) && !is.null(predict_model_names)) models_list <- models_list[predict_model_names]

  ## Grab the internallly stored h2o out of sample predictions for each CV model (cross-validation predictions are combined into a single vector of length n)
  if (missing(validation_data)) {

    message("Obtaining the out-of-sample CV predictions for h2o-stored training data")
    # pAoutDT <- sapply(m.fit$fitted_models_all, function(h2omodel) as.vector(h2o.cross_validation_holdout_predictions(h2omodel)))
    pAoutDT <- lapply(models_list, function(h2omodel) h2o.cross_validation_holdout_predictions(h2omodel))
    pAoutDT <- h2o.cbind(pAoutDT)
    names(pAoutDT) <- names(models_list)
    # setnames(pAoutDT, names(models_list))
    # if (convertResToDT) pAoutDT <- as.data.table(pAoutDT)
    return(pAoutDT)

  } else {

    outvar <- m.fit$params$outvar
    predvars <- m.fit$params$predvars
    fold_column <- ParentObject$fold_column

    ## **** Allows re-using existing h2oFrame is it was already pre-loaded in validation_data object ****
    valid_H2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, validation_data, subset_idx)

    message("Obtaining the out-of-sample CV predictions for new data")
    res <- check_out_of_sample_consistency(models_list, valid_H2Oframe, predvars, fold_column)

    ## Get the fold assignments for the 1st model in ensemble:
    h2o_model_1 <- models_list[[1]]
    fold_h2o <- h2o.cross_validation_fold_assignment(h2o_model_1)
    vfolds_cat_h2o <- sort(h2o.levels(fold_h2o)) # # vfolds_ncat_h2o <- h2o.nlevels(fold_h2o)

    pAoutMat_h2o <- NULL
    CV_loop_t <- system.time({
    for (vfold_idx in seq_along(vfolds_cat_h2o)) {

      message("Obtaining out-of-sample CV predictions for all models and validation fold: " %+% vfolds_cat_h2o[vfold_idx])
      fold_CV_i_logical <- fold_h2o == vfolds_cat_h2o[vfold_idx]

      ## Define validation frame for this fold:
      valid_H2Oframe_CV.i <- valid_H2Oframe[fold_CV_i_logical, ]
      cv.i_foldframeID <- h2o.getId(valid_H2Oframe_CV.i)

      dest_key_LIST <- vector(mode = "list", length = length(models_list))

      for (idx in seq_along(models_list)) {
        # h2o.predict(h2o.getModel(cv_models_IDs[[vfold_idx]]), newdata = h2o.getFrame(cv.i_foldframeID))
        # print("idx: "); print(idx); print("model: "); print(names(models_list)[idx])
        h2o_model <- models_list[[idx]]
        cv_models_IDs <- lapply(h2o_model@model$cross_validation_models, "[[", "name")
        ## Submit a job for prediction on a fold using internal REST API.
        ## Don't pull the prediction results until all of these jobs were submitted.
        url <- paste0('Predictions/models/', cv_models_IDs[[vfold_idx]], '/frames/',  cv.i_foldframeID)
        res <- h2o:::.h2o.__remoteSend(url, method = "POST", h2oRestApiVersion = 4)
        job_key <- res$key$name
        dest_key <- res$dest$name
        dest_key_LIST[[idx]] <- dest_key
      }

      newpreds_prev_CV_i <- NULL
      for (idx in seq_along(dest_key_LIST)) {
        newpreds <- h2o.getFrame(dest_key_LIST[[idx]])
        newpreds_prev_CV_i <- h2o.cbind(newpreds_prev_CV_i, newpreds)
      }
      newpreds_prev_CV_i <- h2o.cbind(h2o.which(fold_CV_i_logical), newpreds_prev_CV_i)
      pAoutMat_h2o <- h2o.rbind(pAoutMat_h2o, newpreds_prev_CV_i)
    }

    pAoutMat_h2o <- h2o.arrange(pAoutMat_h2o, "C1")
    pAoutDT <- pAoutMat_h2o[, 2:ncol(pAoutMat_h2o)]
    })
    print("CV_loop_t"); print(CV_loop_t)
    names(pAoutDT) <- names(models_list)
    # if (convertResToDT) pAoutDT <- as.data.table(pAoutDT)
    # setnames(pAoutDT, names(models_list))
    return(pAoutDT)
  }
}