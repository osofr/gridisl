test.SL <- function() {
  # library("growthcurveSL")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  # h2o::h2o.init(nthreads = 32, max_mem_size = "40G")
  # h2o::h2o.shutdown(prompt = FALSE)
  options(growthcurveSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  # define holdout col:
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  holdout_col <- cpp_holdout[["hold"]]
  ## ----------------------------------------------------------------
  ## Define learners (glm, grid glm and grid gbm)
  ## ----------------------------------------------------------------
  ## glm grid learner:
  alpha_opt <- c(0,1,seq(0.1,0.9,0.1))
  lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)
  glm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 3), alpha = alpha_opt, lambda = lambda_opt)
  ## gbm grid learner:
  gbm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 2, max_runtime_secs = 60*60),
                           ntrees = c(100, 200, 300, 500),
                           learn_rate = c(0.005, 0.01, 0.03, 0.06),
                           max_depth = c(3, 4, 5, 6, 9),
                           sample_rate = c(0.7, 0.8, 0.9, 1.0),
                           col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
                           balance_classes = c(TRUE, FALSE))

  ## ----------------------------------------------------------------
  ## Perform fitting on a grid of algorithms w/ model scoring based on user-spec'ed validation data (valid_data)
  ## ----------------------------------------------------------------
  ## single glm learner:
  h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)
  GRIDparams = list(fit.package = "h2o", fit.algorithm = "GridLearner", family = "gaussian",
                    grid.algorithm = c("glm", "gbm"), seed = 23,
                    glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  grid_mfit_val <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                             train_data = cpp[!holdout_col, ], valid_data = cpp[holdout_col, ],
                             params  = GRIDparams)

  # return predictions for all models in the ensemble
  fit_preds <- predict_model(grid_mfit_val, newdata = cpp)
  head(fit_preds[])
  # + add subject-level features to predictions
  fit_preds <- predict_model(grid_mfit_val, newdata = cpp, add_subject_data = TRUE)
  fit_preds[]
  # Without newdata predictions for training data are returned:
  fit_preds_nonew <- predict_model(grid_mfit_val)
  head(fit_preds_nonew); nrow(fit_preds_nonew)
  # + subject spec data:
  fit_preds_nonew <- predict_model(grid_mfit_val, add_subject_data = TRUE)
  fit_preds_nonew[]

  # just return a K column matrix of predictions for top K models (ranked by validation MSE)
  fit_preds_best <- predict_model(grid_mfit_val, newdata = cpp, predict_only_bestK_models = 1)
  head(fit_preds_best[])
  # + subj-spec data
  fit_preds_best <- predict_model(grid_mfit_val, newdata = cpp, predict_only_bestK_models = 1, add_subject_data = TRUE)
  fit_preds_best[]

  # Same without newdata, predict for training data
  fit_preds_best_nonew <- predict_model(grid_mfit_val, predict_only_bestK_models = 1)
  head(fit_preds_best_nonew[])
  # + subj-spec data
  fit_preds_best_nonew <- predict_model(grid_mfit_val, predict_only_bestK_models = 1, add_subject_data = TRUE)
  fit_preds_best_nonew[]

  # get the model objects for top K models:
  top_model <- grid_mfit_val$get_best_models(K = 1)

  grid_mfit_val$show(model_stats = TRUE, all_fits = TRUE)

  # fetch the model fits directly:
  model_1 <- grid_mfit_val$getfit$fitted_models_all[[1]]

  ## ----------------------------------------------------------------
  ## Perform fitting on a grid of algorithms w/ Vfold CV (can still specify validation data, model scoring will be based on CV though)
  ## ----------------------------------------------------------------
  grid_mfit_cv <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                            train_data = cpp, params  = GRIDparams, nfolds = 3, seed = 12345)

  fit_preds_all <- predict_model(grid_mfit_cv, newdata = cpp)
  head(fit_preds_all[])

  fit_preds_best <- predict_model(grid_mfit_cv, newdata = cpp, predict_only_bestK_model = 1)
  head(fit_preds_best[])

  fit_preds_best <- predict_model(grid_mfit_cv, predict_only_bestK_model = 1)
  head(fit_preds_best[]); nrow(fit_preds_best)

  grid_mfit_cv$show(model_stats = TRUE, all_fits = TRUE)
  model_cv <- grid_mfit_cv$getfit$fitted_models_all[[1]]

  eval_MSE_CV(grid_mfit_cv, yvals = cpp[,"haz"])

  ## ----------------------------------------------------------------
  ## Perform fitting on a grid of algorithms w/ Vfold CV based on user-spec'ed fold_column
  ## ----------------------------------------------------------------
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  grid_mfit_cv <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                            train_data = cpp_folds, params  = GRIDparams, fold_column = "fold")

  fit_preds_all <- predict_model(grid_mfit_cv, newdata = cpp)
  head(fit_preds_all[])

  fit_preds_best <- predict_model(grid_mfit_cv, newdata = cpp, predict_only_bestK_model = 1)
  head(fit_preds_best[])

  # eval_MSE_CV(grid_mfit_cv, yvals = cpp[,"haz"])

  grid_mfit_cv$getmodel_ids

  grid_mfit_cv$show(model_stats = TRUE, all_fits = TRUE)
  # model_cv <- grid_mfit_cv$getfit$fitted_models_all$h2o.glm.reg03
  model_cv <- grid_mfit_cv$getfit$fitted_models_all[[1]]
}

test.growthSL <- function() {
  # library("growthcurveSL")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  # h2o::h2o.init(nthreads = 32, max_mem_size = "40G")
  # h2o::h2o.shutdown(prompt = FALSE)
  options(growthcurveSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ------------------------------------------------------------------------------------
  ## Fitting with model scoring based on random holdouts
  ## ------------------------------------------------------------------------------------
  GRIDparams = list(fit.package = "h2o",
                   fit.algorithm = "GridLearner",
                   family = "gaussian",
                   grid.algorithm = c("glm", "gbm"), seed = 23,
                   glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                   stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  # add holdout indicator column
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  # fit, training on non-holdouts and using holdouts as validation set (for scoring only)
  mfit_hold <- fit_growthSL_holdout(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                    data = cpp_holdout, params = GRIDparams,
                                    hold_column = "hold")

  # predict for previously used holdout / validation set:
  preds_holdout_all <- predict_holdouts(mfit_hold)
  head(preds_holdout_all[])
  # predict for best model only (based on prev. holdouts)
  preds_holdout_best <- predict_holdouts(mfit_hold, predict_only_bestK_models = 1)
  head(preds_holdout_best[])
  # add subject-level data to prediction data
  preds_holdout_best <- predict_holdouts(mfit_hold, predict_only_bestK_models = 1, add_subject_data = TRUE)
  head(preds_holdout_best[])
  # Obtain predictions from the best holdout model for all training data points:
  preds_holdout_train <- predict_model(mfit_hold, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_holdout_train[]
  # Obtain predictions from the best holdout model for all data:
  preds_holdout_cpp <- predict_model(mfit_hold, newdata = cpp_holdout, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_holdout_cpp[]

  # fit the model based on additional special features (summaries) of the outcomes:
  mfit_hold2 <- fit_growthSL_holdout(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                     data = cpp_holdout, params = GRIDparams,
                                     hold_column = "hold", use_new_features = TRUE)

  preds_holdout2 <- predict_holdouts(mfit_hold2, add_subject_data = TRUE)
  preds_holdout2[]

  # DOES NOT WORK RIGHT NOW, SINCE IT DOESN'T KNOW HOW TO DEFINE THE SPECIAL FEATURES!!!!!
  preds_holdout2_alldat <- predict_model(mfit_hold2, newdata = cpp, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_holdout2_alldat[]
  # HAVE TO FIRST MANUALLY DEFINE FEATURES FOR ENTIRE DATASET:
  cpp_plus <- define_features(cpp_holdout, nodes = mfit_hold2$modelfit$OData_train$nodes, train_set = TRUE, holdout = FALSE)
  preds_holdout2_alldat <- predict_model(mfit_hold2, newdata = cpp_plus, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_holdout2_alldat[]

  # ask the function to create the random holdouts internally:
  mfit_hold3 <- fit_growthSL_holdout(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                     data = cpp_holdout, params = GRIDparams,
                                     random = TRUE, use_new_features = TRUE)
  preds_holdout3 <- predict_holdouts(mfit_hold3$modelfit)
  head(preds_holdout3[])

  modelfit <- mfit_hold3$modelfit
  # get 3 top performing models:
  models <- modelfit$get_best_models(K = 1)

  ## ------------------------------------------------------------------------------------
  ## Fitting with model scoring based on full V-FOLD CROSS-VALIDATION
  ## ------------------------------------------------------------------------------------
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  # CV with manually defined fold column
  mfit_cv1 <- fit_growthSL_CV(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_folds, params = GRIDparams, fold_column = "fold")
  # No re-scoring (use previous validation data)
  MSE_cv1_a <- score_CVgrowthcurveSL(modelfit)

  # ****** DOESN'T WORK ******
  # Re-score on new validation data:
  MSE_cv1_b <- score_CVgrowthcurveSL(modelfit, mfit_cv1$valid_data)

  # USE SPECIAL FEATURES WHEN MODEL FITTING
  mfit_cv2 <- fit_growthSL_CV(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_folds, params = GRIDparams,
                              fold_column = "fold", use_new_features = TRUE)

  # WORKS
  preds_cv2_a <- score_CVgrowthcurveSL(mfit_cv2$modelfit)
  # WORKS
  preds_cv2_b <- score_CVgrowthcurveSL(mfit_cv2$modelfit, mfit_cv2$valid_data)
  # WORKS
  preds_cv2_check <- score_CVgrowthcurveSL(mfit_cv2$modelfit, mfit_cv2$train_data)

  # Internally define folds for CV
  mfit_cv3 <- fit_growthSL_CV(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_holdout, params = GRIDparams,
                              nfolds = 5, use_new_features = TRUE)

  preds_cv3_a <- score_CVgrowthcurveSL(mfit_cv3$modelfit)
  preds_cv3_b <- score_CVgrowthcurveSL(mfit_cv3$modelfit, mfit_cv3$valid_data)

  modelfit <- mfit_cv3$modelfit
  models <- modelfit$get_best_models(K = 3)


  # CV SCORING that uses the exact training data with features based on holdout observation outcome
  # THESE predictions should be equivalent to internal h2o prediction for out-of-sample CV folds
  preds_cv3_check <- score_CVgrowthcurveSL(mfit_cv3$modelfit, mfit_cv3$train_data)


  ## ------------------------------------------------------------------------------------
  ## CHECKING CV IMPLEMENTATION VS. INTERNAL H2O CV
  ## ------------------------------------------------------------------------------------
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit_growthSL_CV(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                             data = cpp_folds, params = GRIDparams, fold_column = "fold", use_new_features = TRUE)

  # Re-score on training data (should be equivalent to h2o-based CV metrics):
  preds_cv_check <- score_CVgrowthcurveSL(mfit_cv$modelfit, mfit_cv$train_data)
  # Internal CV MSE and CV evaluated on train_data should be the same:
  mfit_cv$modelfit$getmodel_ids

  for (model_name in names(mfit_cv$modelfit$getmodel_ids)) {
    check_model <- mfit_cv$modelfit$getmodel_byname(model_name)
    # Internal H2O holdout CV predictions by fold:
    # cv_preds_fold <- h2o.cross_validation_predictions(check_model[[1]])
    # List of individual holdout predictions:
    # Holdout (out of sample) predictions for all of training data:
    cv_preds_all <- as.vector(h2o.cross_validation_holdout_predictions(check_model[[1]])) - preds_cv_check[[model_name]]

    # MUST BE TRUE FOR ALL MODELS
    print(paste0("CV validity for model: ", model_name))
    print(mfit_cv$modelfit$getMSE[[model_name]] - check_model[[1]]@model$cross_validation_metrics@metrics$MSE < (10^-6))
    print(sum(cv_preds_all, na.rm = TRUE) < 10^-8)
  }


  # ## -----------------------------------------------------------------------------
  # ## Getting cv folds and cv models by fold:
  # ## -----------------------------------------------------------------------------
  # str(model_cv)
  # model_cv@model$cross_validation_fold_assignment_frame_id
  # model_cv@model$cross_validation_fold_assignment_frame_id
  # model_cv@model$cross_validation_metrics
  # model_cv@model$cross_validation_metrics_summary

  # fold <- as.data.frame(h2o.cross_validation_fold_assignment(model_cv))

  # # List of holdout cv predictions (by fold & cv model):
  # cv_predictions <- h2o.cross_validation_predictions(model_cv)

  # # List of individual holdout predictions:
  # model_cv@model$cross_validation_predictions[[1]]
  # hframe <- as.data.frame(h2o.getFrame(model_cv@model$cross_validation_predictions[[1]]$name))
  # cbind(as.data.frame(cv_predictions[[1]]), hframe, fold)

  # # Holdout (out of sample) predictions for all of training data:
  # predictions <- h2o.cross_validation_holdout_predictions(model_cv)
  # model_cv@model$cross_validation_holdout_predictions_frame_id
  # h2o.getFrame(model_cv@model$cross_validation_holdout_predictions_frame_id$name)

  # # Individual fold models (cv models).
  # # These would need to be re-scored based on new holdout predictors:
  # cv_models <- h2o.cross_validation_models(model_cv)
  # model_cv_1 <- h2o.getModel(model_cv@model$cross_validation_models[[1]]$name)
  # model_cv_1@model$training_metrics
  # model_cv_1@model$validation_metrics

  # # DO PREDICTION FOR EACH cv MODEL BASED ON A NEW HOLDOUT SET (new summaries)
  # fold <- as.data.frame(h2o.cross_validation_fold_assignment(model_cv))
  # cv_models <- h2o.cross_validation_models(model_cv)
  # finalpreds <- vector(mode = "numeric", length = nrow(cpp_folds))
  # vfolds_cat <- sort(unique(fold$fold_assignment))
  # for (vfold_idx in seq_along(vfolds_cat)) {
  #     fold_idx <- fold$fold_assignment %in% vfolds_cat[vfold_idx]
  #     # REPLACE WITH TEST / VALIDATION DATA:
  #     newframe <- as.h2o(cpp_folds[fold_idx,])
  #     # newpreds <- h2o.predict(cv_models[[vfold_idx]], newdata = newframe)
  #     newpreds <- predict(cv_models[[vfold_idx]], newdata = newframe)
  #     finalpreds[fold_idx] <- as.vector(newpreds[, "predict"])
  # }

  # sum(finalpreds - as.vector(predictions[, "predict"]))
  # cbind(finalpreds, as.vector(predictions[, "predict"]), finalpreds - as.vector(predictions[, "predict"]))
}
