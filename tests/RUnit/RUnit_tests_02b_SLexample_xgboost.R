## ------------------------------------------------------------------------------------
## Holdout Growth Curve SL with model scoring based on random holdouts
## ------------------------------------------------------------------------------------

test.glm.XGBoost <- function() {
  # library("longGriDiSL")
  options(longGriDiSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  # lambda = 0L, alpha = 0L
  alpha_opt <- c(0,1.0,seq(0.1,0.9,0.1))
  lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1, 0.5, 0.9, 1.1, 1.5, 2)

 GRIDparams = list(fit.package = "xgboost",
                   fit.algorithm = "grid",
                   grid.algorithm = c("glm"),
                   family = "gaussian",
                   search_criteria = list(strategy = "RandomDiscrete", max_models = 100),
                   params = list(
                                 alpha = alpha_opt,
                                 lambda = lambda_opt
                                 # eta = c(0.3, 0.1,0.01),
                                 # max_depth = c(4,6,8,10),
                                 # max_delta_step = c(0,1),
                                 # subsample = 1,
                                 # scale_pos_weight = 1
                                 ),
                   seed = 123456
                   # glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                   # stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10
                   )

  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  grid_mfit_xgboost_cv1 <- fit_cvSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                    data = cpp_folds, params = GRIDparams, fold_column = "fold")
  pred_alldat_cv <- predict_SL(grid_mfit_xgboost_cv1, newdata = cpp_folds, add_subject_data = FALSE)
  head(pred_alldat_cv[])

#     eta max_depth max_delta_step subsample scale_pos_weight              xgb_fit glob_params niter
# 1: 0.10         8              1         1                1 <xgb.cv.synchronous>      <list>    47
# 2: 0.10         8              0         1                1 <xgb.cv.synchronous>      <list>    47
# 3: 0.01         6              1         1                1 <xgb.cv.synchronous>      <list>   399
# 4: 0.01         8              0         1                1 <xgb.cv.synchronous>      <list>   399
#    nrounds ntreelimit params iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
# 1:      37         37 <list>   37        1.240311     0.01824292       1.245224    0.07221765
# 2:      37         37 <list>   37        1.240311     0.01824292       1.245224    0.07221765
# 3:     389        389 <list>  389        1.240320     0.01830807       1.245259    0.07237535
# 4:     389        389 <list>  389        1.240320     0.01830807       1.245259    0.07237535

  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  grid_mfit_xgboost_holdout <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                              data = cpp_holdout, params = GRIDparams, hold_column = "hold")
  pred_alldat_hold <- predict_SL(grid_mfit_xgboost_holdout, newdata = cpp_folds, add_subject_data = FALSE)
  head(pred_alldat_hold[])

#     eta max_depth max_delta_step subsample scale_pos_weight       xgb_fit glob_params niter nrounds
# 1: 0.10         8              1         1                1 <xgb.Booster>      <list>    57      47
# 2: 0.10         8              0         1                1 <xgb.Booster>      <list>    57      47
# 3: 0.01         6              1         1                1 <xgb.Booster>      <list>   499     489
# 4: 0.01         8              0         1                1 <xgb.Booster>      <list>   499     489
#    ntreelimit params iter train_rmse test_rmse
# 1:         47 <list>   47   1.226035  1.271081
# 2:         47 <list>   47   1.226035  1.271081
# 3:        489 <list>  489   1.226041  1.271265
# 4:        489 <list>  489   1.226041  1.271265

}

test.holdoutSL.XGBoost <- function() {
  # library("longGriDiSL");
  options(longGriDiSL.verbose = FALSE)
  options(longGriDiSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ----------------------------------------------------------------
  ## Define learners (glm, grid glm and grid gbm)
  ## ----------------------------------------------------------------
  ## glm grid learner:
  # alpha_opt <- c(0,1,seq(0.1,0.9,0.1))
  # lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)
  # glm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 3), alpha = alpha_opt, lambda = lambda_opt)
  ## gbm grid learner:
  # gbm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 2, max_runtime_secs = 60*60),
  #                          ntrees = c(100, 200, 300, 500),
  #                          learn_rate = c(0.005, 0.01, 0.03, 0.06),
  #                          max_depth = c(3, 4, 5, 6, 9),
  #                          sample_rate = c(0.7, 0.8, 0.9, 1.0),
  #                          col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
  #                          balance_classes = c(TRUE, FALSE))
  # h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)

  hyper_params = list(eta = c(0.3, 0.1,0.01),
                      max_depth = c(4,6,8,10),
                      max_delta_step = c(0,1),
                      subsample = 1,
                      scale_pos_weight = 1)

  GRIDparams = list(fit.package = "xgboost",
                   fit.algorithm = "grid",
                   grid.algorithm = c("gbm"),
                   family = "gaussian",
                   search_criteria = list(strategy = "RandomDiscrete", max_models = 4),
                   params = hyper_params,
                   seed = 123456
                   # glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                   # stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10
                   )

  GRIDparams2 <- defLearner(estimator = "xgboost_glm", family = "gaussian") +
                 defGrid(estimator = "xgboost_gbm",
                         search_criteria = list(strategy = "RandomDiscrete", max_models = 4),
                         param_grid = hyper_params, family = "gaussian", seed = 123456)

  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

  grid_mfit_xgboost_cv1 <- fit_cvSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                    data = cpp_folds, models = GRIDparams2, fold_column = "fold")

  grid_mfit_xgboost_cv2 <- fit(GRIDparams2, method = "cv",
                               ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                               data = cpp_folds, fold_column = "fold")

#     eta max_depth max_delta_step subsample scale_pos_weight              xgb_fit glob_params niter
# 1: 0.01         6              1         1                1 <xgb.cv.synchronous>      <list>   160
# 2: 0.10         8              1         1                1 <xgb.cv.synchronous>      <list>    21
# 3: 0.10         8              0         1                1 <xgb.cv.synchronous>      <list>    19
# 4: 0.01         8              0         1                1 <xgb.cv.synchronous>      <list>   110
#    nrounds ntreelimit params iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
# 1:     150        150 <list>  150       1.0388152     0.01169493       1.219476    0.06126740
# 2:      11         11 <list>   11       0.9829254     0.01858946       1.231881    0.05634943
# 3:       9          9 <list>    9       0.9733750     0.01398356       1.239929    0.05497296
# 4:     100        100 <list>  100       0.9578688     0.01304842       1.242598    0.05493737

  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  grid_mfit_xgboost_holdout <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                              data = cpp_holdout, models = GRIDparams, hold_column = "hold")

  grid_mfit_xgboost_holdout2 <- fit(GRIDparams2, method = "holdout",
                                    ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                    data = cpp_holdout, hold_column = "hold")

  str(grid_mfit_xgboost_holdout)
  grid_mfit_xgboost_holdout$getfit
  grid_mfit_xgboost_holdout$getmodel_ids
  grid_mfit_xgboost_holdout$get_best_model_names()
  grid_mfit_xgboost_holdout$get_best_models()
  grid_mfit_xgboost_holdout$get_best_model_params()

  ## add holdout indicator column
  # cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  # --------------------------------------------------------------------------------------------
  ## Fit the model based on additional special features (summaries) of the outcomes:
  # --------------------------------------------------------------------------------------------
  mfit_hold2 <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_holdout, params = GRIDparams, hold_column = "hold")

  train_dat <- get_train_data(mfit_hold2)
  print(train_dat)
  checkTrue(nrow(train_dat)==sum(!cpp_holdout[["hold"]]))
  val_dat <- get_validation_data(mfit_hold2)
  print(val_dat)
  checkTrue(nrow(val_dat)==sum(cpp_holdout[["hold"]]))

  print("Holdout MSE, using the holdout Y for prediction"); print(mfit_hold2$getMSE)
  ## Predictions for new data based on best SL model trained on all data:
  preds_alldat <- predict_SL(mfit_hold2, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_alldat[]
  ## Predict for best model trained on non-holdouts only:
  preds_train_top2 <- predict_SL(mfit_hold2, add_subject_data = TRUE, use_best_retrained_model = FALSE)
  preds_train_top2[]

  ## Predictions for training data from all models trained on non-holdout data (default):
  ## not implemented
  # preds_train <- longGriDiSL:::predict_model(mfit_hold2, add_subject_data = TRUE)
  # preds_train[]
  ## Same but only for the top model
  ## not implemented
  # preds_train_top <- longGriDiSL:::predict_model(mfit_hold2, predict_only_bestK_models = 1, add_subject_data = TRUE)
  # preds_train_top[]
  # checkTrue(all.equal(preds_train_top, preds_train_top2))

  ## Predictions for holdouts for all models trained on non-holdout data:
  preds_holdout <- longGriDiSL:::predict_holdout(mfit_hold2, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_holdout[]
  ## Same with predict_SL
  preds_holdout2 <- predict_SL(mfit_hold2, add_subject_data = TRUE, pred_holdout = TRUE)
  preds_holdout2[]

  ## Predictions of the best trained model for new data
  preds_newdat_alldat <- predict_SL(mfit_hold2, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_newdat_alldat[]
  ## Same but for model trained only on non-holdouts
  preds_newdat <- predict_SL(mfit_hold2, newdata = cpp_holdout, add_subject_data = TRUE, use_best_retrained_model = FALSE)
  preds_newdat[]
  ## Same as above
  preds_newdat2 <- longGriDiSL:::predict_model(mfit_hold2, newdata = cpp_holdout, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_newdat2[]
  checkTrue(all.equal(preds_newdat, preds_newdat2))

  holdPredDT <- longGriDiSL:::predict_holdout(mfit_hold2, predict_only_bestK_models = 5, add_subject_data = TRUE)
  holdPredDT[]

  print("10 best MSEs among all learners: "); print(mfit_hold2$get_best_MSEs(K = 5))
  models <- mfit_hold2$get_best_models(K = 5)
  print("Top 5 models: "); print(models)
  res_tab <- mfit_hold2$get_best_MSE_table(K = 5)
  print("5 best models among all learners: "); print(res_tab)
  make_report_rmd(mfit_hold2, data = cpp_holdout, K = 10, format = "html", openFile = FALSE)

  ## Save the best performing h2o model fit to disk:
  # not implemented
  # save_best_h2o_model(mfit_hold2, file.path = "/Users/olegsofrygin/GoogleDrive/HBGDki/ImputationSL/sofware")
}

## ------------------------------------------------------------------------------------
## Growth Curve SL with model scoring based on full V-FOLD CROSS-VALIDATION
## ------------------------------------------------------------------------------------
test.CV.SL.XGBoost <- function() {
  # library("longGriDiSL")
  require("data.table")
  options(longGriDiSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ------------------------------------------------------------------------------------------------
  ## Define learners (glm, grid glm and grid gbm)
  ## ------------------------------------------------------------------------------------------------
  # ## glm grid learner:
  # alpha_opt <- c(0,1,seq(0.1,0.9,0.1))
  # lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)
  # glm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 3), alpha = alpha_opt, lambda = lambda_opt)
  # ## gbm grid learner:
  # gbm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 2, max_runtime_secs = 60*60),
  #                          ntrees = c(100, 200, 300, 500),
  #                          learn_rate = c(0.005, 0.01, 0.03, 0.06),
  #                          max_depth = c(3, 4, 5, 6, 9),
  #                          sample_rate = c(0.7, 0.8, 0.9, 1.0),
  #                          col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
  #                          balance_classes = c(TRUE, FALSE))

  # h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas)
  # ## this fails on latest build (lambda_search)
  # # h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)

  # GRIDparams = list(fit.package = "h2o",
  #                  fit.algorithm = "grid",
  #                  family = "gaussian",
  #                  grid.algorithm = c("gbm"), seed = 23,
  #                  # "glm",
  #                  glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
  #                  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  GRIDparams = list(fit.package = "xgboost",
                   fit.algorithm = "grid",
                   grid.algorithm = c("gbm"),
                   family = "gaussian",
                   search_criteria = list(strategy = "RandomDiscrete", max_models = 4),
                   params = list(eta = c(0.3, 0.1,0.01),
                                 max_depth = c(4,6,8,10),
                                 max_delta_step = c(0,1),
                                 subsample = 1,
                                 scale_pos_weight = 1),
                   seed = 123456
                   # glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                   # stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10
                   )

  ## define CV folds (respecting that multiple observations per subject must fall within the same fold)
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

  ## ------------------------------------------------------------------------------------------------
  ## CV with manually defined fold column, no curve summary features are used as predictors
  ## -> Internal CV metrics must match all manual model scoring results
  ## ------------------------------------------------------------------------------------------------
  mfit_cv1 <- fit_cvSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                       data = cpp_folds, params = GRIDparams, fold_column = "fold")

  ## Best (re-trained) model predictions on data used for CV training (default):
  preds_alldat1 <- predict_SL(mfit_cv1, add_subject_data = FALSE)
  head(preds_alldat1[])

  ## Best model predictions for new data, must match:
  preds_alldat2 <- predict_SL(mfit_cv1, newdata = cpp_folds, add_subject_data = FALSE)
  head(preds_alldat2[])
  checkTrue(all.equal(preds_alldat1, preds_alldat2))

  ## Predictions for best CV model (not re-trained, trained only on non-holdouts), must match:
  ## NOT IMPLEMENTED
  # preds_best_CV <- longGriDiSL:::predict_model(mfit_cv1, add_subject_data = FALSE)
  # preds_best_CV[]
  # preds_best_CV <- longGriDiSL:::predict_model(mfit_cv1, add_subject_data = TRUE)
  # preds_best_CV[]
  # preds_best_CV <- longGriDiSL:::predict_model(mfit_cv1, predict_only_bestK_models = 1, add_subject_data = FALSE)
  # preds_best_CV[]
  # checkTrue(all.equal(as.vector(preds_alldat1[[1]]), as.vector(preds_best_CV[[1]])))

  # Same with predict_SL:
  preds_best_CV2 <- predict_SL(mfit_cv1, use_best_retrained_model = FALSE, add_subject_data = FALSE)
  preds_best_CV2[]
  # Same but with newdata:
  preds_best_CV3 <- predict_SL(mfit_cv1, newdata = cpp_folds, use_best_retrained_model = FALSE, add_subject_data = FALSE)
  preds_best_CV3[]
  checkTrue(all.equal(preds_best_CV2, preds_best_CV3))

  ## Save out of sample CV predictions from the best model.
  ## THESE WILL USE INTERNAL-h2o pre-saved out-of-sample predictions (NO NEW MODEL RESCORING)
  # cv_valid_preds <- get_validation_data(mfit_cv1)
  # cv_valid_preds[, ("out_of_sample_cv_preds") := get_out_of_sample_CV_predictions(mfit_cv1)]
  cv_valid_preds <- get_out_of_sample_predictions(mfit_cv1)
  cv_valid_preds[]
  cv_valid_preds_2 <- longGriDiSL:::predict_holdout(mfit_cv1)
  cv_valid_preds_2[]
  cv_valid_preds_2 <- longGriDiSL:::predict_holdout(mfit_cv1, predict_only_bestK_models = 1, add_subject_data = FALSE)
  cv_valid_preds_2[]
  checkTrue(all.equal(cv_valid_preds, cv_valid_preds_2))
  cv_valid_preds_2 <- longGriDiSL:::predict_holdout(mfit_cv1, predict_only_bestK_models = 1, add_subject_data = TRUE)
  cv_valid_preds_2[]

  ## SAME BUT WITH predict_SL, NEW RESCORING ON TRAINING DATA:
  cv_valid_preds_rescore <- predict_SL(mfit_cv1, add_subject_data = TRUE, pred_holdout = TRUE)
  cv_valid_preds_rescore[]
  checkTrue(all.equal(cv_valid_preds_2, cv_valid_preds_rescore))
  ## SAME BUT WITH predict_SL, RESCORING ON newdata:
  cv_valid_preds_newdata <- predict_SL(mfit_cv1, newdata = cpp_folds, add_subject_data = TRUE, pred_holdout = TRUE)
  cv_valid_preds_newdata[]
  checkTrue(all.equal(cv_valid_preds_rescore, cv_valid_preds_newdata))

  # Return training data used (as data.table)
  train_dat <- get_train_data(mfit_cv1)
  train_dat[]
  # Return validation data used (as data.table)
  valid_data <- get_validation_data(mfit_cv1)
  valid_data[]
  # Obtain out of sample CV predictions for all training data-points
  cv_preds <- get_out_of_sample_predictions(mfit_cv1)
  cv_preds[]

  ## Make report, save grid predictions and out of sample predictions
  # fname <- paste0(data.name, "_", "CV_gridSL_")
  make_report_rmd(mfit_cv1, K = 10, data = cpp_folds,
                  # file.name = paste0(fname, getOption("longGriDiSL.file.name")),
                  title = paste0("Growth Curve Imputation with cpp Data"),
                  format = "html", keep_md = TRUE, openFile = FALSE)

  ## Must all match:
  (MSE_1 <- unlist(eval_MSE(mfit_cv1))) ## Use internally h2o-evaluated CV MSE
  # (MSE_2 <- unlist(eval_MSE(mfit_cv1, get_validation_data(mfit_cv1)))) ## Rescore on validation data, but use old saved y values
  (MSE_4 <- unlist(eval_MSE(mfit_cv1, get_train_data(mfit_cv1)))) # rescore on training data
  # checkTrue(abs(sum(MSE_1 - MSE_2)) <= 10^-5)
  checkTrue(abs(sum(MSE_1 - MSE_4)) <= 10^-5)

  ## ------------------------------------------------------------------------------------------------
  ## Define folds for CV internally
  ## ------------------------------------------------------------------------------------------------
  mfit_cv3 <- fit_cvSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                       data = cpp_folds, params = GRIDparams,
                       nfolds = 5)
  (models <- mfit_cv3$get_best_models(K = 1))

  ## ------------------------------------------------------------------------------------
  ## CHECKING CV IMPLEMENTATION VS. INTERNAL H2O CV
  ## ------------------------------------------------------------------------------------
  # cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 10, seed = 23)
  mfit_cv <- fit_cvSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                      data = cpp_folds, params = GRIDparams, fold_column = "fold")

  train_dat <- get_train_data(mfit_cv)
  valid_data <- get_validation_data(mfit_cv)

  # Re-score on training data (should be equivalent to h2o-based CV metrics):
  preds_cv_check <- eval_MSE(mfit_cv, get_train_data(mfit_cv))
  # Internal CV MSE and CV evaluated on train_data should be the same:
  mfit_cv$getmodel_ids

  for (model_name in names(mfit_cv$getmodel_ids)) {
    check_model <- mfit_cv$getmodel_byname(model_name)
    # Internal H2O holdout CV predictions by fold:
    # cv_preds_fold <- h2o.cross_validation_predictions(check_model[[1]])
    # List of individual holdout predictions:
    # Holdout (out-of-sample) predictions for all of training data:
    cv_preds_all <- as.vector(h2o.cross_validation_holdout_predictions(check_model[[1]])) - preds_cv_check[[model_name]]

    # MUST BE TRUE FOR ALL MODELS
    print(paste0("CV validity for model: ", model_name))
    test1 <- mfit_cv$getMSE[[model_name]] - check_model[[1]]@model$cross_validation_metrics@metrics$MSE < (10^-6)
    print(test1); checkTrue(test1)
    test2 <- sum(cv_preds_all, na.rm = TRUE) < 10^-8
    print(test2); checkTrue(test1)
  }

}


## ------------------------------------------------------------------------------------
## Holdout Growth Curve SL based on residuals from initial glm regression (model scoring based on random holdouts)
## ------------------------------------------------------------------------------------
test.residual.holdoutSL <- function() {
  # library("longGriDiSL")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  options(longGriDiSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

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

  h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)
  GRIDparams = list(fit.package = "h2o",
                   fit.algorithm = "resid_grid",
                   family = "gaussian",
                   grid.algorithm = c("glm", "gbm"), seed = 23,
                   glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                   stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  ## add holdout indicator column
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  ## fit the model based on additional special features (summaries) of the outcomes:
  mfit_resid_hold <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                    data = cpp_holdout, params = GRIDparams,
                                    hold_column = "hold", use_new_features = TRUE)
  print("Holdout MSE, using the residual holdout Y prediction"); print(mfit_resid_hold$getMSE)

  ## Predictions for all holdout data points for all models trained on non-holdout data only:
  preds_holdout_all <- longGriDiSL:::predict_holdout(mfit_resid_hold, add_subject_data = TRUE)
  preds_holdout_all[]
  ## Predictions for new data based on best SL model re-trained on all data:
  preds_alldat <- predict_SL(mfit_resid_hold, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_alldat[]
}
