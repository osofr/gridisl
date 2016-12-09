## ------------------------------------------------------------------------------------
## face / brokenstick without random holdouts
## ------------------------------------------------------------------------------------
test.genericfit_FACE_BS <- function() {
  # library("growthcurveSL")
  options(growthcurveSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  # covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
  # define holdout col:
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  holdout_col <- cpp_holdout[["hold"]]

  run_algo <- function(fit.package, fit.algorithm) {
    mfit_useY <- fit_model(ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
                               train_data = cpp[!holdout_col, ], valid_data = cpp[holdout_col, ],
                               params  = list(fit.package = fit.package, fit.algorithm = fit.algorithm, predict.w.Y = TRUE, name = "useY"))
    fit_preds_1 <- predict_model(mfit_useY, newdata = cpp)
    # print(head(fit_preds_1[]))
    print(mfit_useY$getMSE)

    mfit_cor <- fit_model(ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
                               train_data = cpp[!holdout_col, ], valid_data = cpp[holdout_col, ],
                               params  = list(fit.package = fit.package, fit.algorithm = fit.algorithm, predict.w.Y = FALSE, name = "correct"))
    print(mfit_cor$getMSE)
    fit_preds_2 <- predict_model(mfit_cor, newdata = cpp)
    # print(head(fit_preds_2[]))
  }

  run_algo("face", "face")
  run_algo("brokenstick", "brokenstick")
  run_algo("speedglm", "glm")

}

## ------------------------------------------------------------------------------------
## face / brokenstick based on random holdouts
## ------------------------------------------------------------------------------------
test.holdoutfit_FACE_BS_h2o <- function() {
  # library("growthcurveSL")
  options(growthcurveSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  # covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
  # define holdout col:
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  holdout_col <- cpp_holdout[["hold"]]

  run_algo <- function(fit.package, fit.algorithm) {

    # Fit, training on non-holdouts and using holdouts as validation set (for scoring only)
    mfit_useY_hold <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
                                     data = cpp_holdout, hold_column = "hold",
                                     params = list(fit.package = fit.package, fit.algorithm = fit.algorithm, predict.w.Y = TRUE, name = "useY"))
    print("Holdout MSE, using the holdout Y for prediction"); print(mfit_useY_hold$modelfit$getMSE)
    # FACE MSE: [1] 0.2271609
    # BS MSE: [1] 0.02650036
    # predict for previously used holdout / validation set:
    preds_holdout_1 <- growthcurveSL:::predict_holdouts_only(mfit_useY_hold)
    print(nrow(preds_holdout_1))     # [1] 453
    print(head(preds_holdout_1[]))
    #       face.useY
    # [1,]  1.1349727
    # [2,] -0.9447051
    # [3,]  0.4762184
    # [4,]  0.6611348
    # [5,]  1.3357934
    # [6,]  0.6251740
    #      brokenstick.useY
    # [1,]        1.2869520
    # [2,]       -1.3341364
    # [3,]        0.6166304
    # [4,]        0.8225809
    # [5,]        1.1366634
    # [6,]        0.5201101

    ## Obtain predictions for model trained on non-holdout obs:
    preds_holdout_train <- predict_model(mfit_useY_hold, newdata = cpp_holdout, add_subject_data = TRUE)
    preds_holdout_train[]
    ## Obtain predictions for a model trained on all data:
    preds_alldat_train <- predict_SL(mfit_useY_hold, newdata = cpp_holdout, add_subject_data = TRUE)
    preds_alldat_train[]

    mfit_cor_hold <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
                                    data = cpp_holdout, hold_column = "hold",
                                    params = list(fit.package = fit.package, fit.algorithm = fit.algorithm, predict.w.Y = FALSE, name = "correct"))
    print("Holdout MSE, hiding the holdout Y for prediction"); print(mfit_cor_hold$modelfit$getMSE)
    # FACE MSE: [1] 1.211989
    # BS MSE: [1] 1.186241
    # speed or reg GLM MSE: [1] 1.813257
    # h2o GLM MSE: [1] 1.619442
    # GBM MSE [1] 1.531809
    # DRF MSE [1] 1.531855
    # deeplearning MSE [1] 1.543277

    # predict for previously used holdout / validation set:
    preds_holdout_2 <- growthcurveSL:::predict_holdouts_only(mfit_cor_hold)
    print(nrow(preds_holdout_2)) # [1] 453
    print(head(preds_holdout_2[]))
    #      face.correct
    # [1,]    0.8130066
    # [2,]   -0.5080113
    # [3,]    0.3607981
    # [4,]    0.6898970
    # [5,]    1.4985014
    # [6,]    0.8654325
    #          brokenstick.correct
    # [1,]           0.5584825
    # [2,]          -0.7488577
    # [3,]           0.6277059
    # [4,]           1.0331509
    # [5,]           1.0495770
    # [6,]           0.8009358

    ## Obtain predictions for model trained on non-holdout obs:
    preds_holdout_train <- predict_model(mfit_cor_hold, newdata = cpp_holdout, add_subject_data = TRUE)
    preds_holdout_train[]
    ## Obtain predictions for a model trained on all data:
    preds_alldat_train <- predict_SL(mfit_cor_hold, newdata = cpp_holdout, add_subject_data = TRUE)
    preds_alldat_train[]

    return(list(mfit_useY_hold =  mfit_useY_hold, mfit_cor_hold =  mfit_cor_hold))
  }

  res_FACE <- run_algo("face", "face")
  res_BS <- run_algo("brokenstick", "brokenstick")
  res_GLM1 <- run_algo("speedglm", "glm")
  res_GLM2 <- run_algo("glm", "glm")
  res_GLM3 <- run_algo("h2o", "glm")
  res_GBM <- run_algo("h2o", "gbm")
  res_DRF <- run_algo("h2o", "randomForest")
  res_DP <- run_algo("h2o", "deeplearning")

  mfits_stack <- make_PredictionStack(res_BS$mfit_useY_hold$modelfit, res_BS$mfit_cor_hold$modelfit,
                                      res_FACE$mfit_useY_hold$modelfit, res_FACE$mfit_cor_hold$modelfit,
                                      res_GLM3$mfit_cor_hold$modelfit, res_GBM$mfit_cor_hold$modelfit,
                                      res_DRF$mfit_cor_hold$modelfit, res_DP$mfit_cor_hold$modelfit
                                      )

  print(mfits_stack$get_best_MSEs(K = 2))
  print(mfits_stack$get_best_MSE_table(K = 2))
  make_report_rmd(mfits_stack, K = 2, file.name = paste0("BS_ALL_", getOption("growthcurveSL.file.name")), format = "html", openFile = TRUE)

  # get the model objects for top K models:
  top_model <- mfits_stack$get_best_models(K = 1)
  mfits_stack$show(model_stats = TRUE, all_fits = TRUE)
  # fetch the model fits directly:
  res_BS$mfit_useY_hold$modelfit$getfit

}

## ------------------------------------------------------------------------------------
## generic functions for fitting / predicting using either single test set / validation set
## or using internal h2o cross-validation
## ------------------------------------------------------------------------------------
test.generic.h2oSL <- function() {
  library("growthcurveSL")
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

  ## return predictions for all models in the ensemble
  fit_preds <- predict_model(grid_mfit_val, newdata = cpp)
  head(fit_preds[])
  ## + add subject-level features to predictions
  fit_preds <- predict_model(grid_mfit_val, newdata = cpp, add_subject_data = TRUE)
  fit_preds[]
  ## Without newdata predictions for training data are returned:
  fit_preds_nonew <- predict_model(grid_mfit_val)
  head(fit_preds_nonew); nrow(fit_preds_nonew)
  ## + subject spec data:
  fit_preds_nonew <- predict_model(grid_mfit_val, add_subject_data = TRUE)
  fit_preds_nonew[]

  ## just return a K column matrix of predictions for top K models (ranked by validation MSE)
  fit_preds_best <- predict_model(grid_mfit_val, newdata = cpp, predict_only_bestK_models = 1)
  head(fit_preds_best[])
  ## + subj-spec data
  fit_preds_best <- predict_model(grid_mfit_val, newdata = cpp, predict_only_bestK_models = 1, add_subject_data = TRUE)
  fit_preds_best[]

  ## Same without newdata, predict for training data
  fit_preds_best_nonew <- predict_model(grid_mfit_val, predict_only_bestK_models = 1)
  head(fit_preds_best_nonew[])
  ## + subj-spec data
  fit_preds_best_nonew <- predict_model(grid_mfit_val, predict_only_bestK_models = 1, add_subject_data = TRUE)
  fit_preds_best_nonew[]

  ## get the model objects for top K models:
  top_model <- grid_mfit_val$get_best_models(K = 1)

  grid_mfit_val$show(model_stats = TRUE, all_fits = TRUE)

  ## fetch the model fits directly:
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

test.holdoutSL.GLM <- function() {
  # library("growthcurveSL")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  options(growthcurveSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  # ----------------------------------------------------------------
  # Perform fitting with regularlized GLMs using h2o.grid
  # ----------------------------------------------------------------
  alpha_opt <- c(0,1,seq(0.1,0.9,0.01))
  lambda_opt <- c(seq(0.001, 0.009, by = 0.001), seq(0.01, 0.09, by = 0.01)) # , seq(0.1, 0.9, by = 0.02)
  glm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 10),
                           alpha = alpha_opt, lambda = lambda_opt)

  GLM.GRIDparams = list(name = "GLM",
                       fit.package = "h2o", fit.algorithm = "GridLearner",
                       grid.algorithm = c("glm"), seed = 23, glm = glm_hyper_params, family = "gaussian")
  ## add holdout indicator column
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  mfit_holdGLM <- fit_holdoutSL(ID = "subjid", t_name = "agedays",
                              y = "haz",
                              # x = c("agedays", covars),
                              x = c("agedays", covars, "nY", "meanY", "medianY", "minY", "maxY"),
                              data = cpp_holdout, params = GLM.GRIDparams,
                              hold_column = "hold") # , use_new_features = TRUE

    # print("Holdout MSE, using the holdout Y for prediction"); print(mfit_holdGLM$modelfit$getMSE)
    holdPredDT <- growthcurveSL:::predict_holdouts_only(mfit_holdGLM, predict_only_bestK_models = 5, add_subject_data = TRUE)
    print("GLM holdPredDT"); print(holdPredDT[])
    preds_best_train <- predict_model(mfit_holdGLM, predict_only_bestK_models = 1, add_subject_data = TRUE)
    print("GLM preds_best_train"); print(preds_best_train[])
    preds_best_all <- predict_SL(mfit_holdGLM, newdata = cpp_holdout, add_subject_data = TRUE)
    print("GLM preds_best_all"); print(preds_best_all[])

    print("TOP MSE: "); print(min(unlist(mfit_holdGLM$modelfit$getMSE)), na.rm = TRUE)
    print(mfit_holdGLM$modelfit$get_best_MSEs(5))
    print(mfit_holdGLM$modelfit$get_best_MSEs(15))
    BEST_GLM_model <- mfit_holdGLM$modelfit$get_best_models(K = 1)[[1]]

    make_report_rmd(mfit_holdGLM$modelfit,
                    K = 10,
                    file.name = paste0("GLMs_", getOption("growthcurveSL.file.name")),
                    title = paste0("Growth Curve Imputation with GLM"),
                    format = "html", keep_md = TRUE, openFile = FALSE)

}

## ------------------------------------------------------------------------------------
## Holdout Growth Curve SL with model scoring based on random holdouts
## ------------------------------------------------------------------------------------
test.holdoutSL.GLM.GBM <- function() {
  # library("growthcurveSL")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  options(growthcurveSL.verbose = TRUE)
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
  gbm_hyper_params <- list(search_criteria = list(strategy = "RandomDiscrete", max_models = 5, max_runtime_secs = 60*60),
                           ntrees = c(100, 200, 300, 500),
                           learn_rate = c(0.005, 0.01, 0.03, 0.06),
                           max_depth = c(3, 4, 5, 6, 9),
                           sample_rate = c(0.7, 0.8, 0.9, 1.0),
                           col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
                           balance_classes = c(TRUE, FALSE))

  h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)
  GRIDparams = list(fit.package = "h2o",
                   fit.algorithm = "GridLearner",
                   family = "gaussian",
                   grid.algorithm = c("glm", "gbm"), seed = 23,
                   glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                   stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  ## add holdout indicator column
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  # --------------------------------------------------------------------------------------------
  ## Fit the model based on additional special features (summaries) of the outcomes:
  # --------------------------------------------------------------------------------------------
  mfit_hold2 <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_holdout, params = GRIDparams,
                              hold_column = "hold", use_new_features = TRUE)

  print("Holdout MSE, using the holdout Y for prediction"); print(mfit_hold2$modelfit$getMSE)
  # $grid.glm.1
  # [1] 1.797792
  # $grid.glm.2
  # [1] 1.976214
  # $grid.glm.3
  # [1] 1.976399
  # $grid.gbm.4
  # [1] 1.597701
  # $grid.gbm.5
  # [1] 1.700594
  # $h2o.glm.reg03
  # [1] 1.776226

  ## Predictions for new data based on best SL model trained on all data:
  preds_alldat <- predict_SL(mfit_hold2, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_alldat[]

  ## Predictions for all holdout data points for all models trained on non-holdout data:
  preds_holdout2 <- growthcurveSL:::predict_holdouts_only(mfit_hold2, add_subject_data = TRUE)
  preds_holdout2[]

  ## Predictions for training data from models trained on non-holdout data (default):
  preds_holdout_train <- predict_model(mfit_hold2, add_subject_data = TRUE)
  preds_holdout_train[]
  preds_holdout_train <- predict_model(mfit_hold2, predict_only_bestK_models = 2, add_subject_data = TRUE)
  preds_holdout_train[]

  ## does not work right now, since it doesn't know how to define the special features!!!!!
  # preds_holdout2_alldat <- predict_model(mfit_hold2, newdata = cpp_holdout, predict_only_bestK_models = 1, add_subject_data = TRUE)
  ## Instead, have to first manually define features for entire dataset - predictions will be for a model trained on non-holdouts only!
  # cpp_plus <- define_features(cpp_holdout, nodes = mfit_hold2$modelfit$OData_train$nodes, train_set = TRUE, holdout = FALSE)
  cpp_plus <- define_features_drop(cpp_holdout, ID = "subjid", t_name = "agedays", y = "haz", train_set = TRUE)
  preds_holdout2_alldat <- predict_model(mfit_hold2, newdata = cpp_plus, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_holdout2_alldat[]

  ## Ask the fit function to determine random holdouts internally:
  mfit_hold3 <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_holdout, params = GRIDparams,
                              random = TRUE, use_new_features = TRUE)

  preds_holdout3 <- growthcurveSL:::predict_holdouts_only(mfit_hold3$modelfit)
  head(preds_holdout3[])

  modelfit <- mfit_hold3$modelfit
  ## get 3 top performing models:
  models <- modelfit$get_best_models(K = 1)

  ## Fit, training on non-holdouts and using holdouts as validation set (for scoring only). No additional summaries are used
  mfit_hold <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                    data = cpp_holdout, params = GRIDparams,
                                    hold_column = "hold")

  ## Obtain predictions from the best holdout model for all data (re-trained on all observations):
  preds_best_all <- predict_SL(mfit_hold, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_best_all[]

  ## predict for previously used holdout / validation set:
  preds_holdout_all <- growthcurveSL:::predict_holdouts_only(mfit_hold)
  head(preds_holdout_all[])
  ## Predict for best model only (based on prev. holdouts)
  preds_holdout_best <- growthcurveSL:::predict_holdouts_only(mfit_hold, predict_only_bestK_models = 1)
  head(preds_holdout_best[])
  ## add subject-level data to prediction data
  preds_holdout_best <- growthcurveSL:::predict_holdouts_only(mfit_hold, predict_only_bestK_models = 1, add_subject_data = TRUE)
  head(preds_holdout_best[])

  ## Obtain predictions from the best holdout model for training data only (default in predict_model when newdata is missing):
  preds_holdout_train <- predict_model(mfit_hold, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_holdout_train[]

  ## Obtain predictions from the best holdout model for all data (trained on non-holdouts only):
  preds_best_train <- predict_model(mfit_hold, newdata = cpp_holdout, predict_only_bestK_models = 1, add_subject_data = TRUE)
  preds_best_train[]

  holdPredDT <- growthcurveSL:::predict_holdouts_only(mfit_hold, predict_only_bestK_models = 5, add_subject_data = TRUE)
  print("10 best MSEs among all learners: "); print(mfit_hold2$modelfit$get_best_MSEs(K = 5))
  models <- mfit_hold2$modelfit$get_best_models(K = 5)
  print("Top 5 models: "); print(models)
  res_tab <- mfit_hold2$modelfit$get_best_MSE_table(K = 5)
  print("5 best models among all learners: "); print(res_tab)
  make_report_rmd(mfit_hold2$modelfit, K = 10, format = "html", openFile = FALSE)

}

## ------------------------------------------------------------------------------------
## Holdout Growth Curve SL based on residuals from initial glm regression (model scoring based on random holdouts)
## ------------------------------------------------------------------------------------
test.residual.holdoutSL <- function() {
  # library("growthcurveSL")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  options(growthcurveSL.verbose = TRUE)
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
                   fit.algorithm = "ResidGridLearner",
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
  print("Holdout MSE, using the residual holdout Y prediction"); print(mfit_resid_hold$modelfit$getMSE)
  # [1] "Holdout MSE, using the holdout Y for prediction"
  # $grid.glm.1
  # [1] 1.977161
  # $grid.glm.2
  # [1] 1.977161
  # $grid.glm.3
  # [1] 1.977161
  # $grid.gbm.4
  # [1] 1.435757
  # $grid.gbm.5
  # [1] 1.495895
  # $h2o.glm.reg03
  # [1] 1.776226

  ## Predictions for all holdout data points for all models trained on non-holdout data only:
  preds_holdout_all <- growthcurveSL:::predict_holdouts_only(mfit_resid_hold, add_subject_data = TRUE)
  preds_holdout_all[]
  ## Predictions for new data based on best SL model re-trained on all data:
  preds_alldat <- predict_SL(mfit_resid_hold, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_alldat[]
}

## ------------------------------------------------------------------------------------
## Growth Curve SL with model scoring based on full V-FOLD CROSS-VALIDATION
## ------------------------------------------------------------------------------------
test.CV.SL <- function() {
  # library("growthcurveSL")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  options(growthcurveSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ------------------------------------------------------------------------------------------------
  ## Define learners (glm, grid glm and grid gbm)
  ## ------------------------------------------------------------------------------------------------
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
                   fit.algorithm = "GridLearner",
                   family = "gaussian",
                   grid.algorithm = c("glm", "gbm"), seed = 23,
                   glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                   stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  ## define CV folds (respecting that multiple observations per subject must fall within the same fold)
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

  ## ------------------------------------------------------------------------------------------------
  ## CV with manually defined fold column, no curve summary features are used as predictors
  ## -> Internal CV metrics must match all manual model scoring results
  ## ------------------------------------------------------------------------------------------------
  mfit_cv1 <- fit_curveSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                          data = cpp_folds, params = GRIDparams, fold_column = "fold")

  ## Best (re-trained) model predictions on data used for CV training (default):
  preds_alldat1 <- predict_SL(mfit_cv1, add_subject_data = FALSE)
  head(preds_alldat1[])
  ## Best model predictions for new data, must match:
  preds_alldat2 <- predict_SL(mfit_cv1, newdata = cpp_folds, add_subject_data = FALSE)
  head(preds_alldat2[])
  checkTrue(all.equal(preds_alldat1, preds_alldat2))
  ## Predictions for best CV model (not re-trained), must match:
  preds_best_CV <- predict_model(mfit_cv1, predict_only_bestK_models = 1, add_subject_data = FALSE)
  head(preds_best_CV[])
  checkTrue(all.equal(as.vector(preds_alldat1), as.vector(preds_best_CV)))
  ## Predict with new data (ONLY WORKS WHEN NO SPECIAL CURVE SUMMARIES ARE USED FOR PREDICTION)
  preds_best_CV_2 <- predict_model(mfit_cv1, newdata = cpp_folds, predict_only_bestK_models = 1, add_subject_data = FALSE)
  head(preds_best_CV_2[])
  checkTrue(all.equal(as.vector(preds_best_CV_2), as.vector(preds_best_CV)))

  ## Must all match:
  (MSE_1 <- unlist(eval_MSE_CV(mfit_cv1))) ## Use internally h2o-evaluated CV MSE
  (MSE_2 <- unlist(eval_MSE_CV(mfit_cv1, mfit_cv1$valid_data))) ## Rescore on validation data, but use old saved y values
  (MSE_3 <- unlist(eval_MSE_CV(mfit_cv1, mfit_cv1$valid_data, yvals = mfit_cv1$valid_data[["haz"]])))
  (MSE_4 <- unlist(eval_MSE_CV(mfit_cv1, mfit_cv1$train_data, yvals = mfit_cv1$train_data[["haz"]]))) # rescore on training data
  checkTrue(abs(sum(MSE_1 - MSE_2)) <= 10^-5)
  checkTrue(abs(sum(MSE_1 - MSE_3)) <= 10^-5)
  checkTrue(abs(sum(MSE_1 - MSE_4)) <= 10^-5)

  ## ------------------------------------------------------------------------------------------------
  ## ADD SPECIAL curve FEATURES / summaries as predictors to CV evaluation
  ## ------------------------------------------------------------------------------------------------
  mfit_cv2 <- fit_curveSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_folds, params = GRIDparams,
                              fold_column = "fold", use_new_features = TRUE)

  ## Best (re-trained) model predictions on data used for CV training (default):
  preds_alldat1 <- predict_SL(mfit_cv2, add_subject_data = FALSE)
  head(preds_alldat1[])
  ## Best model predictions for new data, must match:
  preds_alldat2 <- predict_SL(mfit_cv2, newdata = cpp_folds, add_subject_data = FALSE)
  head(preds_alldat2[])
  checkTrue(all.equal(preds_alldat1, preds_alldat2))
  ## Predictions for best CV model (not re-trained), must match:
  preds_best_CV <- predict_model(mfit_cv2, predict_only_bestK_models = 1, add_subject_data = FALSE)
  head(preds_best_CV[])
  checkTrue(all.equal(as.vector(preds_alldat1), as.vector(preds_best_CV)))

  ## Not supposed to work anymore, since cpp_folds doesn't contain special curve summaries
  #preds_best_CV_2 <- predict_model(mfit_cv2, newdata = cpp_folds, predict_only_bestK_models = 1, add_subject_data = FALSE)

  ## NO LONGER NEED TO MATCH SINCE THE MANUAL SCORING IS BASED ON SUMMARIES THAT LEAVE OUT 1 OBS AT A TIME:
  ## 1a. Use internally h2o-evaluated CV MSE (WILL LEAD TO INCORRECT MSE).
  (MSE_1 <- unlist(eval_MSE_CV(mfit_cv2)))
  ## 1b. Use training data to score CV models (should be the same as 1a)
  (MSE_4 <- unlist(eval_MSE_CV(mfit_cv2, mfit_cv2$train_data, yvals = mfit_cv2$train_data[["haz"]]))) # rescore on training data
  checkTrue(abs(sum(MSE_1 - MSE_4)) <= 10^-5)
  ## 2. Use validation data for scoring that build curve summaries by leaving one obs out at a time (CORRECT)
  (MSE_2 <- unlist(eval_MSE_CV(mfit_cv2, mfit_cv2$valid_data))) ## Rescore on validation data, but use old saved y values
  (MSE_3 <- unlist(eval_MSE_CV(mfit_cv2, mfit_cv2$valid_data, yvals = mfit_cv2$valid_data[["haz"]])))
  checkTrue(abs(sum(MSE_2 - MSE_3)) <= 10^-5)

  ## ------------------------------------------------------------------------------------------------
  ## Internally define folds for CV
  ## ------------------------------------------------------------------------------------------------
  mfit_cv3 <- fit_curveSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_holdout, params = GRIDparams,
                              nfolds = 5, use_new_features = TRUE)
  modelfit <- mfit_cv3$modelfit
  models <- modelfit$get_best_models(K = 1)

  ## ------------------------------------------------------------------------------------
  ## CHECKING CV IMPLEMENTATION VS. INTERNAL H2O CV
  ## ------------------------------------------------------------------------------------
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit_curveSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                             data = cpp_folds, params = GRIDparams, fold_column = "fold", use_new_features = TRUE)

  # Re-score on training data (should be equivalent to h2o-based CV metrics):
  preds_cv_check <- eval_MSE_CV(mfit_cv, mfit_cv$train_data)
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
    test1 <- mfit_cv$modelfit$getMSE[[model_name]] - check_model[[1]]@model$cross_validation_metrics@metrics$MSE < (10^-6)
    print(test1); checkTrue(test1)
    test2 <- sum(cv_preds_all, na.rm = TRUE) < 10^-8
    print(test2); checkTrue(test1)
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
