test.xgb.grid.printing <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  # data.table::setDTthreads(1)
  # library("doParallel")
  # registerDoParallel(cores = 2)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  hyper_params = list(eta = c(0.3, 0.1, 0.01),
                      max_depth = c(4, 6, 8, 10),
                      max_delta_step = c(0,1),
                      subsample = 1,
                      scale_pos_weight = 1)

  GRIDparams2 <- defModel(estimator = "xgboost__glm", family = "gaussian", nthread = 1) +
                 defModel(estimator = "xgboost__gbm", family = "gaussian", nrounds = 5, nthread = 1,
                         search_criteria = list(strategy = "RandomDiscrete", max_models = 3),
                         param_grid = hyper_params, seed = 123456)

  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  xgboost_holdout <- fit(GRIDparams2, method = "holdout",
                         ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                         data = cpp_holdout, hold_column = "hold")

  models <- xgboost_holdout$get_best_models(K = 5)
  for (model_idx in seq_along(models)) {
    if (!is.null(models[[model_idx]])) {
      print_tables(models[[model_idx]])
    }
  }

  plotMSEs(xgboost_holdout, interactive = TRUE)

  make_model_report(xgboost_holdout, data = cpp_holdout,
                    K = 10,
                    file.name = paste0("GLMs_", getOption("gridisl.file.name")),
                    title = paste0("Growth Curve Imputation with GLM"),
                    format = "html", keep_md = TRUE,
                    # openFile = TRUE)
                    openFile = FALSE)


}

## ------------------------------------------------------------------------------------
## test xgboost GLM learner / GBM Grid, no model scoring (no cv or holdout), just fit all models
## ------------------------------------------------------------------------------------
test.XGBoost.simple <- function() {
  options(gridisl.verbose = TRUE)
  # options(gridisl.verbose = FALSE)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  cpp <- data.table::data.table(cpp)
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  params <- defModel(estimator = "xgboost__glm", family = "gaussian", nthread = 1)
  mfit_xgb1 <- fit(params, method = "none", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                  data = cpp)

  GRIDparams2 <- defModel(estimator = "xgboost__glm", family = "gaussian", nthread = 1) +
                 defModel(estimator = "xgboost__gbm", family = "gaussian",
                          nthread = 1,
                          nrounds = 5, early_stopping_rounds = 2,
                          seed = 123456,
                          search_criteria = list(
                            strategy = "RandomDiscrete",
                            max_models = 2),
                          param_grid = list(
                            eta = c(0.3, 0.1, 0.01),
                            max_depth = c(4, 6, 8, 10),
                            max_delta_step = c(0,1),
                            subsample = 1,
                            scale_pos_weight = 1
                            )
                          )

  # cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_xgb <- fit(GRIDparams2, method = "none", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                  data = cpp)

  ## can't predict from best model since no model selection method was specified (method = "none")
  checkException(pred_alldat_best <- predict_SL(mfit_xgb, newdata = cpp_folds, add_subject_data = FALSE))
  ## can't predict with xgboost when newdata is missing:
  checkException(preds <- predict_generic(mfit_xgb, add_subject_data = TRUE, best_only = FALSE))

  preds <- predict_generic(mfit_xgb, newdata = cpp, add_subject_data = TRUE, best_only = FALSE)
  preds[]

  ## out-of-sample / holdout predictions --- NEED A MORE INFORMATIVE ERROR
  ## CURRENT ERROR: Error in model_obj$predict_out_of_sample(...) : attempt to apply non-function
  checkException(preds <- predict_generic(mfit_xgb, add_subject_data = TRUE, best_only = FALSE, holdout = TRUE))
}

## ------------------------------------------------------------------------------------
## test xgboost GBM, with CV
## ------------------------------------------------------------------------------------
test.XGBoost.simpleCV <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  cpp <- data.table::data.table(cpp)
  cpp <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
  GRIDparams2 <-  defModel(estimator = "xgboost__gbm", family = "gaussian",
                          nthread = 1,
                          nrounds = 5, early_stopping_rounds = 2,
                          seed = 123456,
                          search_criteria = list(
                            strategy = "RandomDiscrete",
                            max_models = 2),
                          param_grid = list(
                            eta = c(0.3, 0.1, 0.01),
                            max_depth = c(4, 6, 8, 10),
                            max_delta_step = c(0,1),
                            subsample = 1,
                            scale_pos_weight = 1
                            )
                          )
  # cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_xgb <- fit(GRIDparams2, method = "cv", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                  data = cpp, fold_column = "fold")

  ## can't predict from best model since no model selection method was specified (method = "none")
  checkException(pred_alldat_best <- predict_SL(mfit_xgb, newdata = cpp_folds, add_subject_data = FALSE))
  ## can't predict with xgboost when newdata is missing:
  checkException(preds <- predict_generic(mfit_xgb, add_subject_data = TRUE, best_only = FALSE))

  ## PREDICTIONS from each CV model (stored as nested tables)
  preds_all <- predict_generic(mfit_xgb, newdata = cpp, add_subject_data = TRUE, best_only = FALSE)
  preds_all[1, ][["m.1.xgb.gbm.grid.1"]]

  ## out-of-sample / holdout predictions (for each model):
  preds_holdout <- predict_generic(mfit_xgb, add_subject_data = TRUE, best_only = FALSE, holdout = TRUE)

}


## ------------------------------------------------------------------------------------
## test xgboost glm, model scoring with CV
## ------------------------------------------------------------------------------------
test.XGBoost.GLM <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  require("h2o")
  Sys.sleep(1)
  h2o::h2o.init(nthreads = 1)
  Sys.sleep(1)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  params_glm <- defModel(estimator = "xgboost__glm", family = "gaussian",
                         nthread = 1,
                         eta = 1.7,
                         early_stopping_rounds = 2,
                         nrounds = 50,
                         alpha = 0.3,
                         lambda = 0.1)

  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(params_glm, method = "cv", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                 data = cpp_folds, fold_column = "fold")

  params_glm <- defModel(estimator = "xgboost__glm", family = "gaussian",
                         nthread = 1,
                         eta = 1.7,
                           alpha = 0.3, lambda = 0.1,
                           seed = 123456) +
                defModel(estimator = "h2o__glm", family = "gaussian",
                           alpha = 0, lambda = 0.1,
                           seed = 123456)
                            # +
                # defModel(estimator = "h2o__glm", family = "gaussian",
                #            alpha = 0.3, lambda_search = TRUE,
                #            seed = 123456)

  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(params_glm, method = "cv", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                 data = cpp_folds, fold_column = "fold")

  pred_alldat_cv <- predict_SL(mfit_cv, newdata = cpp_folds, add_subject_data = FALSE)
  head(pred_alldat_cv[])

  mfit_cv$get_best_MSE_table()
  mfit_cv$getMSEtab
  mfit_cv$get_modelfits_grid()

  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  mfit_hold <- fit(params_glm, method = "holdout", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                 data = cpp_holdout, hold_column = "hold")

  pred_alldat_hold <- predict_SL(mfit_hold, newdata = cpp_holdout, add_subject_data = FALSE)
  head(pred_alldat_hold[])

  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(1)
}

## ------------------------------------------------------------------------------------
## test xgboost glm, model scoring with CV
## ------------------------------------------------------------------------------------
test.XGBoost.regularizedGLM_grid <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  require("h2o")
  Sys.sleep(1)
  h2o::h2o.init(nthreads = 1)
  Sys.sleep(1)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  # alpha_opt <- c(0,1.0,seq(0.1,0.9,0.1))
  alpha_opt <- 0
  lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1, 0.5, 0.9, 1.1, 1.5, 2)

  params_glm <- defModel(estimator = "xgboost__glm", family = "gaussian",
                         nthread = 1,
                         eta = 1.3,
                         search_criteria = list(strategy = "RandomDiscrete", max_models = 3),
                         param_grid = list(alpha = alpha_opt,
                                          lambda = lambda_opt),
                        seed = 123456) +
                defModel(estimator = "h2o__glm", family = "gaussian",
                         search_criteria = list(strategy = "RandomDiscrete", max_models = 3),
                         param_grid = list(alpha = alpha_opt,
                                          lambda = lambda_opt),
                         seed = 123456)

  # params_glm[[1]][["fit.algorithm"]] <- "glm"

  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(params_glm, method = "cv", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                 data = cpp_folds, fold_column = "fold")
  pred_alldat_cv <- predict_SL(mfit_cv, newdata = cpp_folds, add_subject_data = FALSE)
  head(pred_alldat_cv[])

  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  mfit_hold <- fit(params_glm, method = "holdout", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                 data = cpp_holdout, hold_column = "hold")
  # grid_mfit_xgboost_holdout <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
  #                                             data = cpp_holdout, params = GRIDparams, hold_column = "hold")
  pred_alldat_hold <- predict_SL(mfit_hold, newdata = cpp_holdout, add_subject_data = FALSE)
  head(pred_alldat_hold[])

  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(1)
}

## ------------------------------------------------------------------------------------
## test xgboost random forest, model scoring with CV
## ------------------------------------------------------------------------------------
test.XGBoost.drfs <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  require("h2o")
  Sys.sleep(1)
  h2o::h2o.init(nthreads = 1)
  Sys.sleep(1)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  params_drf <- defModel(estimator = "xgboost__drf", family = "gaussian",
                         ntrhead = 1,
                         eta = 1.3,
                         nrounds = 10,
                         seed = 123456) +
                defModel(estimator = "h2o__randomForest", distribution = "gaussian",
                           ntrees = 10,
                           seed = 123456)

  # params_drf[[1]][["fit.algorithm"]] <- "glm"

  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(params_drf, method = "cv", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                 data = cpp_folds, fold_column = "fold")
  pred_alldat_cv <- predict_SL(mfit_cv, newdata = cpp_folds, add_subject_data = FALSE)
  head(pred_alldat_cv[])

  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  mfit_hold <- fit(params_drf, method = "holdout", ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                   data = cpp_holdout, hold_column = "hold")

  pred_alldat_hold <- predict_SL(mfit_hold, newdata = cpp_holdout, add_subject_data = FALSE, holdout = TRUE)
  head(pred_alldat_hold[])

  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(1)
}

test.holdout.XGBoost <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  hyper_params = list(eta = c(0.3, 0.1,0.01),
                      max_depth = c(4,6,8,10),
                      max_delta_step = c(0,1),
                      subsample = 1,
                      scale_pos_weight = 1)

  GRIDparams2 <- defModel(estimator = "xgboost__glm", family = "gaussian", nthreads = 1) +
                 defModel(estimator = "xgboost__gbm", family = "gaussian", nrounds = 5, nthreads = 1,
                         search_criteria = list(strategy = "RandomDiscrete", max_models = 2),
                         param_grid = hyper_params, seed = 123456)

  # --------------------------------------------------------------------------------------------
  ## Fit xgboost model with model scoring via random holdout point for each observation:
  # --------------------------------------------------------------------------------------------
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  xgboost_holdout <- fit(GRIDparams2, method = "holdout",
                         ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                         data = cpp_holdout, hold_column = "hold")

  train_dat <- get_train_data(xgboost_holdout)
  print(train_dat)
  checkTrue(nrow(train_dat)==sum(!cpp_holdout[["hold"]]))
  val_dat <- get_validation_data(xgboost_holdout)
  print(val_dat)
  checkTrue(nrow(val_dat)==sum(cpp_holdout[["hold"]]))

  holdout_preds <- get_out_of_sample_predictions(xgboost_holdout)
  print("Holdout MSE, using the holdout Y for prediction"); print(xgboost_holdout$getMSE)
  print("Holdout MSE, using the holdout Y for prediction"); print(xgboost_holdout$getMSEtab)

  ## Predictions for new data based on best SL model trained on all data:
  preds_alldat <- predict_SL(xgboost_holdout, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_alldat[]

  ## ***** NOT IMPLEMENTED ****
  ## Predict for best model trained on non-holdouts only
  ## NOTE: The class for this already exists: xgboost_holdout$predict_within_sample()
  ##       The only difficulty is figuring out the interface, i.e., what argument / function should be used
  checkException(preds_train_top2 <- predict_SL(xgboost_holdout, add_subject_data = TRUE, best_refit_only = FALSE))
  # preds_train_top2[]

  print("10 best MSEs among all learners: "); print(xgboost_holdout$get_best_MSEs(K = 5))
  models <- xgboost_holdout$get_best_models(K = 5)
  print("Top 5 models: "); print(models)
  res_tab <- xgboost_holdout$get_best_MSE_table(K = 5)
  print("5 best models among all learners: "); print(res_tab)
  make_model_report(xgboost_holdout, data = cpp_holdout, K = 10, format = "html",
                    # openFile = TRUE)
                    openFile = FALSE)

  ## Save the best performing h2o model fit to disk:
  # not implemented
  # save_best_model(xgboost_holdout, file.path = "/Users/olegsofrygin/GoogleDrive/HBGDki/ImputationSL/sofware")
}


## ------------------------------------------------------------------------------------
## xgboost with model scoring based on full V-FOLD CROSS-VALIDATION
## ------------------------------------------------------------------------------------
test.CV.SL.XGBoost <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  hyper_params = list(eta = c(0.3, 0.1,0.01),
                      max_depth = c(4,6,8,10),
                      max_delta_step = c(0,1),
                      subsample = 1,
                      scale_pos_weight = 1)

  GRIDparams <- defModel(estimator = "xgboost__glm", family = "gaussian", nthread = 1) +

                defModel(estimator = "xgboost__gbm", family = "gaussian", nrounds = 5,
                         nthread = 1,
                         early_stopping_rounds = 2,
                         search_criteria = list(strategy = "RandomDiscrete", max_models = 2),
                         param_grid = hyper_params, seed = 123456)

  ## --------------------------------------------------------------------------------------------
  ## Fit xgboost model with model scoring via 5 fold CV:
  ## --------------------------------------------------------------------------------------------
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(models = GRIDparams, method = "cv", data = cpp_folds,
                 ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                 fold_column = "fold")

  ## Predictions for new data based on best SL model trained on all data:
  preds_alldat <- predict_SL(mfit_cv, newdata = cpp_folds, add_subject_data = TRUE)
  preds_alldat[]
  preds_alldat <- predict_SL(mfit_cv, add_subject_data = TRUE)
  preds_alldat[]

  ## out-of-sample / holdout predictions:
  preds_hold_best <- predict_generic(mfit_cv, add_subject_data = TRUE, best_only = TRUE, holdout = TRUE)
  preds_hold_best[]

  ## out-of-sample / holdout predictions for all models:
  preds_hold_all <- predict_generic(mfit_cv, best_only = FALSE, holdout = TRUE) # add_subject_data = TRUE,
  preds_hold_all[]

  holdout_preds <- get_out_of_sample_predictions(mfit_cv)
  preds_hold_best <- predict_generic(mfit_cv, add_subject_data = FALSE, best_only = TRUE, holdout = TRUE)
  checkTrue(ncol(preds_hold_best)==1)
  checkTrue(all.equal(holdout_preds[[1]], preds_hold_best[[1]]))


  models <- mfit_cv$get_best_models(K = 5)
  for (model_idx in seq_along(models)) {
    if (!is.null(models[[model_idx]])) {
      print_tables(models[[model_idx]])
    }
  }

  make_model_report(mfit_cv, data = cpp_folds, K = 10,
                    file.name = paste0("GLMs_", getOption("gridisl.file.name")),
                    title = paste0("Growth Curve Imputation with GLM"),
                    format = "html", keep_md = TRUE,
                    openFile = FALSE)
                    # openFile = TRUE)

  ## --------------------------------------------------------------------------------------------
  ## PERFORM OUT-OF-SAMPLE PREDICTIONS FOR CV MODELS using newdata
  ## --------------------------------------------------------------------------------------------
  cv_valid_preds_newdata <- predict_SL(mfit_cv, newdata = cpp_folds, holdout = TRUE) # add_subject_data = TRUE,
  cv_valid_preds_newdata[]

  print(holdout_preds - cv_valid_preds_newdata)
  checkTrue(max(abs(holdout_preds - cv_valid_preds_newdata)) < 10^-5)

  ## SAME BUT FOR ALL MODELS (not just the best scoring)
  cv_valid_preds_newdata_all <- predict_generic(mfit_cv, newdata = cpp_folds, best_only = FALSE, holdout = TRUE) # add_subject_data = TRUE,
  cv_valid_preds_newdata_all[]
  print(preds_hold_all - cv_valid_preds_newdata_all)
  checkTrue(all(unlist(lapply(abs(preds_hold_all - cv_valid_preds_newdata_all), max)) < 10^-5))

  ## Make report, save grid predictions and out-of-sample predictions
  # fname <- paste0(data.name, "_", "CV_gridSL_")
  make_model_report(mfit_cv, K = 10, data = cpp_folds,
                    # file.name = paste0(fname, getOption("gridisl.file.name")),
                    title = paste0("Growth Curve Imputation with cpp Data"),
                    format = "html", keep_md = TRUE,
                    openFile = FALSE)
                    # openFile = TRUE)

  mfit_cv$get_modelfits_grid()[[1]]

  ## Must all match:
  (MSE_1 <- unlist(eval_MSE(mfit_cv))) ## Use internally h2o-evaluated CV MSE
   # M.1.xgb.glm M.2.xgb.gbm.grid.1 M.2.xgb.gbm.grid.2 M.2.xgb.gbm.grid.3 M.2.xgb.gbm.grid.4
   #    1.558494           1.502805           1.519974           1.535077           1.541958

  (MSE_2 <- unlist(eval_MSE(mfit_cv, get_train_data(mfit_cv)))) # rescore on training data
   # M.1.xgb.glm M.2.xgb.gbm.grid.1 M.2.xgb.gbm.grid.2 M.2.xgb.gbm.grid.3 M.2.xgb.gbm.grid.4
   #    1.558494           1.502805           1.519974           1.535077           1.541958

  checkTrue(abs(sum(MSE_1 - MSE_2)) <= 10^-5)

  ## Predict for all models trained on non-holdouts only with newdata:
  ## Since with xgboost, each model is technically nfolds different models (fit on training folds),
  ## the returned predictions are nested as:
  ##   (xgbmodel_name) ---> (xgbCV.1, xgbCV.2, ..., xgbCV.nfolds)
  preds_train_all2 <- predict_generic(mfit_cv, newdata = cpp_folds, add_subject_data = TRUE, best_only = FALSE, holdout = FALSE)

  ## --------------------------------------------------------------------------------------------
  ## ********** NEED MORE INFORMATIVE ERROR: "prediction for all models is not possible when doing cv with xgboost:" *****************
  ## --------------------------------------------------------------------------------------------
  ## Predict for all models trained on non-holdouts only (should give error for xgboost, but should work for h2o):
  checkException(preds_train_all <- predict_generic(mfit_cv, add_subject_data = TRUE, best_only = FALSE, holdout = FALSE))

  p <- plotMSEs(mfit_cv, K = 10, interactive = FALSE)
  p <- plotMSEs(mfit_cv, K = 10, interactive = TRUE)

  ## --------------------------------------------------------------------------------------------
  ## ****** (NOT IMPLEMENTED) *******
  ## --------------------------------------------------------------------------------------------
  ## Predictions for best CV model (not re-trained, trained only on non-holdouts), must match:
  # preds_best_CV <- gridisl:::predict_model(mfit_cv, add_subject_data = FALSE)
  # preds_best_CV[]
  # preds_best_CV <- gridisl:::predict_model(mfit_cv, add_subject_data = TRUE)
  # preds_best_CV[]
  # preds_best_CV <- gridisl:::predict_model(mfit_cv, bestK_only = 1, add_subject_data = FALSE)
  # preds_best_CV[]
  # checkTrue(all.equal(as.vector(preds_alldat1[[1]]), as.vector(preds_best_CV[[1]])))
}

## --------------------------------------------------------------------------------------------
## ****** (NOT IMPLEMENTED) *******
## --------------------------------------------------------------------------------------------
## Holdout Growth Curve SL based on residuals from initial glm regression (model scoring based on random holdouts)
NOtest.residual.holdoutSL.xgboost <- function() {
  # # options(gridisl.verbose = TRUE)
  # options(gridisl.verbose = FALSE)

  # library("h2o")
  # Sys.sleep(1)
  # h2o::h2o.init(nthreads = 2)
  # Sys.sleep(1)

  # data(cpp)
  # cpp <- cpp[!is.na(cpp[, "haz"]), ]
  # covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  # ## ----------------------------------------------------------------
  # ## Define learners (glm, grid glm and grid gbm)
  # ## ----------------------------------------------------------------
  # ## glm grid learner:
  # alpha_opt <- c(0,1,seq(0.1,0.9,0.1))
  # lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)
  # glm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 3), alpha = alpha_opt, lambda = lambda_opt)
  # ## gbm grid learner:
  # gbm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 2, max_runtime_secs = 60*60),
  #                          ntrees = c(10, 20, 30, 50),
  #                          learn_rate = c(0.005, 0.01, 0.03, 0.06),
  #                          max_depth = c(3, 4, 5, 6, 9),
  #                          sample_rate = c(0.7, 0.8, 0.9, 1.0),
  #                          col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
  #                          balance_classes = c(TRUE, FALSE))

  # h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 5, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)
  # GRIDparams = list(fit.package = "h2o",
  #                  fit.algorithm = "resid_grid",
  #                  family = "gaussian",
  #                  grid.algorithm = c("glm", "gbm"), seed = 23,
  #                  glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
  #                  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE")

  # ## add holdout indicator column
  # cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  # ## fit the model based on additional special features (summaries) of the outcomes:
  # mfit_resid_hold <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
  #                                   data = cpp_holdout, params = GRIDparams,
  #                                   hold_column = "hold", use_new_features = TRUE)
  # print("Holdout MSE, using the residual holdout Y prediction"); print(mfit_resid_hold$getMSE)

  # ## Predictions for all holdout data points for all models trained on non-holdout data only:
  # preds_holdout_all <- gridisl:::predict_holdout(mfit_resid_hold, add_subject_data = TRUE)
  # preds_holdout_all[]
  # ## Predictions for new data based on best SL model re-trained on all data:
  # preds_alldat <- predict_SL(mfit_resid_hold, newdata = cpp_holdout, add_subject_data = TRUE)
  # preds_alldat[]

  # h2o::h2o.shutdown(prompt = FALSE)
  # Sys.sleep(1)
}
