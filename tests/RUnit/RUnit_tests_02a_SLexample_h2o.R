test.H2O_holdoutSL_GLM_old_syntax <- function() {
  # require("h2o")
  # library("data.table")
  h2o::h2o.init(nthreads = 1)
  options(gridisl.verbose = TRUE)
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
                       fit.package = "h2o", fit.algorithm = "grid",
                       grid.algorithm = c("glm"), seed = 23, glm = glm_hyper_params, family = "gaussian")

  ## add holdout indicator column
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  mfit_holdGLM <- gridisl:::fit_holdoutSL(ID = "subjid", t_name = "agedays",
                                y = "haz",
                                x = c("agedays", covars),
                                data = cpp_holdout, models = list(GLM.GRIDparams),
                                hold_column = "hold") # , use_new_features = TRUE

    make_model_report(mfit_holdGLM, data = cpp_holdout,
                    K = 10,
                    file.name = paste0("GLMs_", getOption("gridisl.file.name")),
                    title = paste0("Growth Curve Imputation with GLM"),
                    format = "html", keep_md = TRUE,
                    # openFile = TRUE)
                    openFile = FALSE)

    ## PREDICT FOR BEST MODEL RE-TRAINED ON ALL DATA:
    preds_best_all <- predict_SL(mfit_holdGLM, newdata = cpp_holdout, add_subject_data = TRUE)
    print("GLM preds_best_all"); print(preds_best_all[])

    print("Holdout MSE, using the holdout Y for prediction"); print(mfit_holdGLM$getMSEtab)
    print("TOP MSE: "); print(min(unlist(mfit_holdGLM$getMSE)), na.rm = TRUE)
    print(mfit_holdGLM$get_best_MSEs(5))
    BEST_GLM_model <- mfit_holdGLM$get_best_models(K = 1)[[1]]

    ## ------------------------------------------------------------------------------------
    ## ***** Prediction on holdout obs, for models trained on non-holdouts only *****
    ## ------------------------------------------------------------------------------------
    holdPredDT <- gridisl:::predict_holdout(mfit_holdGLM, best_only = FALSE, add_subject_data = TRUE)
    print("GLM holdPredDT"); print(holdPredDT[])
    holdPredDT <- gridisl:::predict_holdout(mfit_holdGLM, best_only = TRUE, add_subject_data = TRUE)
    print("GLM holdPredDT"); print(holdPredDT[])

    ## ------------------------------------------------------------------------------------
    ## ***** Prediction on non-holdout obs, for models trained on non-holdouts only *****
    ## ------------------------------------------------------------------------------------
    preds_best_train <- gridisl:::predict_nonholdouts(mfit_holdGLM, best_only = TRUE, add_subject_data = FALSE)
    print("GLM preds_best_train"); print(preds_best_train[])
    preds_best_train <- gridisl:::predict_nonholdouts(mfit_holdGLM, best_only = TRUE, add_subject_data = TRUE)
    print("GLM preds_best_train"); print(preds_best_train[])
    preds_best_train <- gridisl:::predict_nonholdouts(mfit_holdGLM, best_only = FALSE, add_subject_data = TRUE)
    print("GLM preds_best_train"); print(preds_best_train[])

    h2o::h2o.shutdown(prompt = FALSE)
}


## ------------------------------------------------------------------------------------
## Holdout Growth Curve SL with model scoring based on random holdouts
## ------------------------------------------------------------------------------------
test.H2O_holdoutSL_GRID_GBM_old_syntax <- function() {
  require("h2o")
  h2o::h2o.init(nthreads = 1)
  options(gridisl.verbose = TRUE)
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
                   fit.algorithm = "grid",
                   family = "gaussian",
                   grid.algorithm = c("glm", "gbm"), seed = 23,
                   glm = glm_hyper_params, gbm = gbm_hyper_params, learner = "h2o.glm.reg03",
                   stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  ## add holdout indicator column
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  # --------------------------------------------------------------------------------------------
  ## Fit the model based on additional special features (summaries) of the outcomes:
  # --------------------------------------------------------------------------------------------
  mfit_hold2 <- gridisl:::fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                              data = cpp_holdout, models = list(GRIDparams), hold_column = "hold")

  train_dat <- get_train_data(mfit_hold2)
  print(train_dat)
  checkTrue(nrow(train_dat)==sum(!cpp_holdout[["hold"]]))
  val_dat <- get_validation_data(mfit_hold2)
  print(val_dat)
  checkTrue(nrow(val_dat)==sum(cpp_holdout[["hold"]]))

  print("Holdout MSE, using the holdout Y for prediction"); print(mfit_hold2$getMSE)
  print("Holdout MSE, using the holdout Y for prediction"); print(mfit_hold2$getMSEtab)
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

  ## Predictions for training data from all models trained on non-holdout data (default):
  preds_train <- gridisl:::predict_nonholdouts(mfit_hold2, add_subject_data = TRUE)
  preds_train[]
  ## Same but only for the top model
  preds_train_top <- gridisl:::predict_nonholdouts(mfit_hold2, best_only = TRUE, add_subject_data = TRUE)
  preds_train_top[]

  ## Predictions for all holdout data points for best models trained on non-holdout data:
  preds_holdout <- gridisl:::predict_holdout(mfit_hold2, best_only = TRUE, add_subject_data = TRUE)
  preds_holdout[]
  ## Same with predict_generic
  preds_holdout2 <- predict_SL(mfit_hold2, add_subject_data = TRUE, holdout = TRUE)
  preds_holdout2[]
  checkTrue(all.equal(preds_holdout, preds_holdout2))

  ## Predictions of the best trained model for new data
  preds_newdat_alldat <- predict_SL(mfit_hold2, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_newdat_alldat[]
  ## Same but for model trained only on non-holdouts
  preds_newdat <- predict_nonholdouts(mfit_hold2, newdata = cpp_holdout, best_only = TRUE, add_subject_data = TRUE)
  preds_newdat[]

  print("10 best MSEs among all learners: "); print(mfit_hold2$get_best_MSEs(K = 5))
  models <- mfit_hold2$get_best_models(K = 5)
  print("Top 5 models: "); print(models)
  res_tab <- mfit_hold2$get_best_MSE_table(K = 5)
  print("5 best models among all learners: "); print(res_tab)
  make_model_report(mfit_hold2, data = cpp_holdout, K = 10, format = "html", openFile = FALSE)

  ## Save the best performing h2o model fit to disk:
  # save_best_h2o_model(mfit_hold2, file.path = "/Users/olegsofrygin/GoogleDrive/HBGDki/ImputationSL/sofware")
  h2o::h2o.shutdown(prompt = FALSE)
}

## ------------------------------------------------------------------------------------
## Growth Curve SL with model scoring based on full V-FOLD CROSS-VALIDATION
## ------------------------------------------------------------------------------------
test.H2O_cvSL_GRID_GBM_new_syntax <- function() {
  # library("gridisl")
  # require("data.table")
  require("h2o")
  h2o::h2o.init(nthreads = 1)
  options(gridisl.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ------------------------------------------------------------------------------------------------
  ## Define learners (glm, grid glm and grid gbm)
  ## ------------------------------------------------------------------------------------------------
  ## gbm grid learner:
  gbm_hyper <- list(
    ntrees = c(100, 200, 300, 500),
    learn_rate = c(0.005, 0.01, 0.03, 0.06),
    max_depth = c(3, 4, 5, 6, 9),
    sample_rate = c(0.7, 0.8, 0.9, 1.0),
    col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
    balance_classes = c(TRUE, FALSE))

  params <- defModel(estimator = "h2o__glm",
                    family = "gaussian",
                    seed = 23,
                    alpha = 0.3, nlambdas = 50, lambda_search = TRUE) +

            defModel(estimator = "h2o__glm",
                    search_criteria = list(strategy = "RandomDiscrete", max_models = 3),
                    family = "gaussian",
                    seed = 23,
                    param_grid = list(
                      alpha = c(0,1,seq(0.1,0.9,0.1)),
                      lambda = c(0,1e-7,1e-5,1e-3,1e-1)
                      )
                    ) +

            defModel(estimator = "h2o__gbm",
                    search_criteria = list(strategy = "RandomDiscrete", max_models = 2, max_runtime_secs = 60*60),
                    family = "gaussian",
                    seed = 23,
                    param_grid = gbm_hyper,
                    stopping_rounds = 5,
                    stopping_tolerance = 1e-4,
                    stopping_metric = "MSE",
                    score_tree_interval = 10)

  ## define CV folds (respecting that multiple observations per subject must fall within the same fold)
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

  ## ------------------------------------------------------------------------------------------------
  ## CV with manually defined fold column, no curve summary features are used as predictors
  ## -> Internal CV metrics must match all manual model scoring results
  ## ------------------------------------------------------------------------------------------------
  mfit_cv1 <- fit(models = params, method = "cv", data = cpp_folds,
                  ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                  fold_column = "fold")

  ## Best (re-trained) model predictions on data used for CV training (default):
  preds_alldat1 <- predict_SL(mfit_cv1, add_subject_data = FALSE)
  head(preds_alldat1[])

  ## Best model predictions for new data, must match:
  preds_alldat2 <- predict_SL(mfit_cv1, newdata = cpp_folds, add_subject_data = FALSE)
  head(preds_alldat2[])
  checkTrue(all.equal(preds_alldat1, preds_alldat2))

  ## Predictions for best CV model (not re-trained, trained only on non-holdouts), must match:
  preds_best_CV <- gridisl:::predict_nonholdouts(mfit_cv1, add_subject_data = FALSE)
  # preds_best_CV[]
  preds_best_CV <- gridisl:::predict_nonholdouts(mfit_cv1, add_subject_data = TRUE)
  # preds_best_CV[]
  # checkTrue(all.equal(as.vector(preds_alldat1[[1]]), as.vector(preds_best_CV[[1]])))

  # Same with predict_SL:
  preds_best_CV2 <- predict_SL(mfit_cv1, add_subject_data = FALSE)
  preds_best_CV2[]
  # Same but with newdata:
  preds_best_CV3 <- predict_SL(mfit_cv1, newdata = cpp_folds, add_subject_data = FALSE)
  preds_best_CV3[]
  checkTrue(all.equal(preds_best_CV2, preds_best_CV3))

  ## Save out of sample CV predictions from the best model.
  ## THESE WILL USE INTERNAL-h2o pre-saved out-of-sample predictions (NO NEW MODEL RESCORING)
  # cv_valid_preds <- get_validation_data(mfit_cv1)
  # cv_valid_preds[, ("out_of_sample_cv_preds") := get_out_of_sample_CV_predictions(mfit_cv1)]
  cv_valid_preds <- get_out_of_sample_predictions(mfit_cv1)
  cv_valid_preds[]
  cv_valid_preds_2 <- gridisl:::predict_holdout(mfit_cv1)
  cv_valid_preds_2[]
  cv_valid_preds_2 <- gridisl:::predict_holdout(mfit_cv1, best_only = TRUE, add_subject_data = FALSE)
  cv_valid_preds_2[]
  checkTrue(all.equal(cv_valid_preds[[1]], cv_valid_preds_2[[1]]))

  ## SAME BUT WITH predict_SL, NEW RESCORING ON TRAINING DATA:
  cv_valid_preds_rescore <- predict_SL(mfit_cv1, add_subject_data = FALSE, holdout = TRUE)
  cv_valid_preds_rescore[]
  checkTrue(all.equal(cv_valid_preds_2, cv_valid_preds_rescore))
  ## SAME BUT WITH predict_SL, RESCORING ON newdata:
  cv_valid_preds_newdata <- predict_SL(mfit_cv1, newdata = cpp_folds, add_subject_data = FALSE, holdout = TRUE)
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
  make_model_report(mfit_cv1, K = 10, data = cpp_folds,
                  # file.name = paste0(fname, getOption("gridisl.file.name")),
                  title = paste0("Growth Curve Imputation with cpp Data"),
                  format = "html", keep_md = TRUE,
                  # openFile = TRUE)
                  openFile = FALSE)

  ## Must all match:
  ## Internally evaluated CV MSE (evaluted by h2o and stored in h2o object)
  (MSE_1 <- unlist(eval_MSE(mfit_cv1)))
  # Manually re-score MSE based on h2o fold models and out-of-sample predictions for validaiton folds:
  (MSE_4 <- unlist(eval_MSE(mfit_cv1, get_train_data(mfit_cv1))))
  # checkTrue(abs(sum(MSE_1 - MSE_2)) <= 10^-5)
  checkTrue(abs(sum(MSE_1 - MSE_4)) <= 10^-5)

  ## ------------------------------------------------------------------------------------------------
  ## Define folds for CV internally
  ## ------------------------------------------------------------------------------------------------
  mfit_cv3 <- fit( method = "cv", data = cpp_folds,
                   ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz", nfolds = 5)

  (models <- mfit_cv3$get_best_models(K = 1))

  ## ------------------------------------------------------------------------------------
  ## CHECKING CV IMPLEMENTATION VS. INTERNAL H2O CV
  ## ------------------------------------------------------------------------------------
  # cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 10, seed = 23)
  mfit_cv <- fit(models = params, method = "cv", data = cpp_folds,
                 ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                 fold_column = "fold")

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
test.H2O_residual_holdoutSL_old_syntax <- function() {
  # library("gridisl")
  require("h2o")
  h2o::h2o.init(nthreads = 1)
  options(gridisl.verbose = TRUE)
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
  mfit_resid_hold <- gridisl:::fit_holdoutSL(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                    data = cpp_holdout, params = GRIDparams,
                                    hold_column = "hold", use_new_features = TRUE)
  print("Holdout MSE, using the residual holdout Y prediction"); print(mfit_resid_hold$getMSE)
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
  preds_holdout_all <- gridisl:::predict_holdout(mfit_resid_hold, add_subject_data = TRUE)
  preds_holdout_all[]
  ## Predictions for new data based on best SL model re-trained on all data:
  preds_alldat <- predict_SL(mfit_resid_hold, newdata = cpp_holdout, add_subject_data = TRUE)
  preds_alldat[]
}
