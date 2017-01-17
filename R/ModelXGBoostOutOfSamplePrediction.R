# check_out_of_sample_consistency <- function(models_list, valid_H2Oframe, predvars, fold_column) {
#   all_folds_h2o <- lapply(models_list, h2o.cross_validation_fold_assignment)
#   train_frame_ID_1 <- models_list[[1]]@parameters$training_frame

#   if (length(all_folds_h2o) > 1) {
#     ## 1. Test that the exactly the same fold assignments were used by all CV models in the ensemble.
#     for (idx in 2:length(all_folds_h2o) ) {
#       if (!h2o::h2o.all(all_folds_h2o[[1]]==all_folds_h2o[[idx]])  )
#         stop("Out-of-sample (holdout) predictions for new data has failed. The fold assignmets of the following CV model do not match to others: " %+% names(models_list)[idx])
#     }

#     ## 2. Test that same training h2oFrame was used for all models in the ensemble (just in case).
#     for (idx in 2:length(all_folds_h2o) ) {
#       if (!all.equal(train_frame_ID_1, models_list[[idx]]@parameters$training_frame))
#         stop("Out-of-sample (holdout) predictions for new data has failed. It appears that some of the CV models in ensemble used different training frames.")
#     }
#   }

#   ## 3. Test that the validation and training data have exactly the same fold assignments (in h2oFrame)
#   if (!all(valid_H2Oframe[[fold_column]] == all_folds_h2o[[1]]))
#     stop("Out-of-sample (holdout) predictions for new data has failed. The fold assignments in new data (validation_data) and training data appear to be different.")

#   ## 4a. Test that the new validation data (in h2oFrame) has the same number of observations as the training data
#   if (!(nrow(valid_H2Oframe) == nrow(h2o::h2o.getFrame(train_frame_ID_1))))
#     stop("Out-of-sample (holdout) predictions for new data has failed. The number of rows in new data (validation_data) does not match that of the training data.")

#   ## 4b. Test that all predictors are present in the validation data (in h2oFrame)
#   if (!all(c(predvars,fold_column) %in% colnames(valid_H2Oframe)))
#     stop("Out-of-sample (holdout) predictions for new data has failed. Some of the predictors were not found in new data (validation_data).")
#   return(invisible(TRUE))
# }

## ----------------------------------------------------------------------------------------------------------------------------------
## Evaluate out-of-sample predictions from V cross-validation models, based on new validation_data.
## Can be useful for re-scoring the models when the validation data has to change from the training data in V-fold cross-validation.
## Will only perform the out-sample predictions for each model V_i
## (i.e., predictions in validation_data will be only made for rows that were not used for training the model V_i)
## In the end we generate a vector of n=nrow(validation_data) predictions by combining predictions from all models V=(V_1,...,V_v)
## This procedure is repeated for each cross-validated model in the ensemble, resulting in a matrix of predictions (n,k),
## where k is the total number of models trained by this ensemble (with h2o.grid, etc) and is equal to length(models_list)
## ----------------------------------------------------------------------------------------------------------------------------------
xgb_predict_out_of_sample_cv <- function(m.fit, ParentObject, validation_data, subset_idx, predict_model_names, ...) {
  models_list <- m.fit$modelfits_all
  if (!missing(predict_model_names) && !is.null(predict_model_names)) models_list <- models_list[predict_model_names]

  ## Grab the internallly stored h2o out of sample predictions for each CV model (cross-validation predictions are combined into a single vector of length n)
  if (missing(validation_data)) {
    message("Obtaining pre-saved out-of-sample/holdout CV predictions for xgboost")
    pAoutDT <- lapply(models_list, function(cv_xgb_model) cv_xgb_model[["pred"]])
    pAoutDT <- as.data.table(pAoutDT)
    names(pAoutDT) <- names(models_list)
    return(pAoutDT)

  } else {
    message("Obtaining out-of-sample/holdout CV predictions for xgboost with newdata")
    # ## pre-saved out-of-sample preds for validation folds
    # models_list[[1]][["pred"]]
    # ## list of validation fold indices (rows in input data)
    # models_list[[1]][["folds"]]
    # nfold <- length(models_list[[1]][["folds"]])
    # ## validation fold indices for fold 1:
    # models_list[[1]][["folds"]][[1]]
    # ## pre-saved list of cv training-fold model objects
    # models_list[[1]][["models"]]
    # fold_model <- models_list[[1]][["models"]][[1]]

    # ## **** Allows re-using existing h2oFrame is it was already pre-loaded in validation_data object ****
    # valid_H2Oframe <- getPredictH2OFRAME(m.fit, ParentObject, validation_data, subset_idx)
    valid_dmat <- getPredictXGBDMat (m.fit, ParentObject, validation_data, subset_idx)

    # outvar <- m.fit$params$outvar
    # predvars <- m.fit$params$predvars
    # fold_column <- ParentObject$fold_column
    # res <- check_out_of_sample_consistency(models_list, valid_dmat, predvars, fold_column)

    # ## Get the fold assignments for the 1st model in ensemble:
    xgb_model_1 <- models_list[[1]]

    fold_xgb <- xgb_model_1[["folds"]]
    vfolds_cat_xgb <- names(fold_xgb)

    pAoutDT <- matrix(data = NA_real_, nrow = nrow(valid_dmat), ncol = length(models_list))
    colnames(pAoutDT) <- names(models_list)
    pAoutDT <- data.table::data.table(pAoutDT)

    CV_loop_t <- system.time({
      for (vfold_idx in seq_along(vfolds_cat_xgb)) {

        message("Obtaining out-of-sample CV predictions for all models and validation fold: " %+% vfolds_cat_xgb[vfold_idx])

        ## validation row indices for the current fold:
        fold_CV_i_idx <- fold_xgb[[vfolds_cat_xgb[vfold_idx]]]

        ## subset the validation data by the current validation fold indices (will do predict for this data)
        valid_dmat_CV.i <- valid_dmat[fold_CV_i_idx, ]
        attributes(valid_dmat)
        attr(valid_dmat_CV.i, ".Dimnames") <- attr(valid_dmat, ".Dimnames")

        for (idx in seq_along(models_list)) {
          ## grab the model among the list of all fits, by its index
          xgb_model <- models_list[[idx]]

          ## Use ntreelimit for prediction, if it was actually used during model training.
          ## Use it only for gbtree (not for gblinear, i.e., glm, as it is not implemented)
          ntreelimit <- 0
          if (!is.null(xgb_model[["best_ntreelimit"]]) && !(xgb_model[["params"]][["booster"]] %in% "gblinear"))
            ntreelimit <- xgb_model[["best_ntreelimit"]]

          ## grab all the V-fold trained models and grab the relevant one for current fold index
          xgb_model_fold <- xgb_model[["models"]][[vfold_idx]]

          ## 1. obtain predictions for new validation data (fold-specific)
          ## 2. save in the appropriate fold ID rows and appropriate model name column in final data.table:
          pAoutDT[fold_CV_i_idx, names(models_list)[idx] := predict(xgb_model_fold, newdata = valid_dmat_CV.i, ntreelimit = ntreelimit)]
        }
      }
    })

    print("CV_loop_t_xgb"); print(CV_loop_t)

    return(pAoutDT)
  }
}