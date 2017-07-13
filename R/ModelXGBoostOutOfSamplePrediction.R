## ----------------------------------------------------------------------------------------------------------------------------------
## Evaluate out-of-sample predictions from V cross-validation models, based on new validation_data.
## Can be useful for re-scoring the models when the validation data has to change from the training data in V-fold cross-validation.
## Will only perform the out-sample predictions for each model V_i
## (i.e., predictions in validation_data will be only made for rows that were not used for training the model V_i)
## In the end we generate a vector of n=nrow(validation_data) predictions by combining predictions from all models V=(V_1,...,V_v)
## This procedure is repeated for each cross-validated model in the ensemble, resulting in a matrix of predictions (n,k),
## where k is the total number of models trained by this ensemble (with xgb.grid, etc) and is equal to length(models_list)
## ----------------------------------------------------------------------------------------------------------------------------------
xgb_predict_out_of_sample_cv <- function(m.fit, ParentObject, validation_data, subset_idx, predict_model_names, ...) {
  models_list <- m.fit$modelfits_all
  if (!missing(predict_model_names) && !is.null(predict_model_names)) models_list <- models_list[predict_model_names]

  ## Grab the internallly stored h2o out of sample predictions for each CV model (cross-validation predictions are combined into a single vector of length n)
  if (missing(validation_data)) {
    if (gvars$verbose == 2) message("Obtaining pre-saved out-of-sample/holdout CV predictions for xgboost")
    pAoutDT <- lapply(models_list, function(cv_xgb_model) cv_xgb_model[["pred"]])
    pAoutDT <- as.data.table(pAoutDT)
    names(pAoutDT) <- names(models_list)
    return(pAoutDT)

  } else {
    if (gvars$verbose == 2) message("Obtaining out-of-sample/holdout CV predictions for xgboost with newdata")
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

    valid_dmat <- getPredictXGBDMat(m.fit, ParentObject, validation_data, subset_idx)
    fold_column <- validation_data$fold_column
    if (is.null(fold_column)) fold_column <- ParentObject$fold_column
    valid_folds <- validation_data$dat.sVar[subset_idx, ][[fold_column]]

    if (!is.null(valid_folds)) {
      fold_origami <- make_kfold_from_column(valid_folds)
      fold_valid <- lapply(fold_origami, '[[', "validation_set")
      names(fold_valid) <- levels(valid_folds)
    } else {
      # ## Get the fold assignments for the 1st model in ensemble:
      xgb_model_1 <- models_list[[1]]
      fold_valid <- xgb_model_1[["folds"]]
    }

    vfolds_cat <- names(fold_valid)

    pAoutDT <- matrix(data = NA_real_, nrow = nrow(valid_dmat), ncol = length(models_list))
    colnames(pAoutDT) <- names(models_list)
    pAoutDT <- data.table::data.table(pAoutDT)

    for (vfold_idx in seq_along(vfolds_cat)) {

      if (gvars$verbose == 2) message("Obtaining out-of-sample CV predictions for all models and validation fold: " %+% vfolds_cat[vfold_idx])

      ## validation row indices for the current fold:
      fold_CV_i_idx <- fold_valid[[vfolds_cat[vfold_idx]]]

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

    return(pAoutDT)
  }
}