## ----------------------------------------------------------------
## Perform fitting using a single shared h2oFrame (loaded only once and shared by all models)
## ----------------------------------------------------------------
test.shared_h2o_database <- function() {
  library("longGriDiSL")
  library("data.table")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  # h2o::h2o.init(nthreads = 32, max_mem_size = "40G")
  # h2o::h2o.shutdown(prompt = FALSE)
  options(longGriDiSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  # define holdout col:
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  holdout_col <- cpp_holdout[["hold"]]

  h2oFRAME_test <- fast.load.to.H2O(cpp_holdout, destination_frame = "")
  h2oFRAME_ID <- h2o::h2o.getId(h2oFRAME_test)

  ## ----------------------------------------------------------------
  ## Defining h2o plogis and qlogis functions
  ## ----------------------------------------------------------------
  x <- h2oFRAME_test[, "whz"]

  # plogis(x) <=> exp(x)/(1+exp(x))
  h2o_exp_x <- h2o.exp(x)
  plogis_h2o <- h2o_exp_x / (1 + h2o_exp_x)
  all.equal(as.vector(plogis_h2o) , plogis(as.vector(x)))

  # qlogis(x) <=> log(x/(1-x))
  qlogis_h2o <-h2o::h2o.log(x / (1 - x))
  all.equal(as.vector(qlogis_h2o), qlogis(as.vector(x)))

  ## ----------------------------------------------------------------
  ## Defining new column (pre-assigned) values. Then assigning new values to a subset of rows of h2o.FRAME
  ## ----------------------------------------------------------------
  h2oFRAME_test[, "test_col"] <- 1
  h2oFRAME_test[, "test_col_copy"] <- h2oFRAME_test[, "wtkg"]

  subset_idx <- sort(sample(1:nrow(h2oFRAME_test), 20))
  h2oFRAME_test[subset_idx, "test_col"]
  h2oFRAME_test[subset_idx, "test_col"] <- h2oFRAME_test[subset_idx, "wtkg"]

  h2oFRAME_test[1:3, "test_col_copy"] <- 1

  ## THIS DOES NOT WORK. THE VECTOR HAS TO BE CONVERTED TO h2o FRAME work
  # h2oFRAME_test[1:3, "test_col_copy"] <- c(1:3)
  h2oFRAME_test[1:3, "test_col_copy"] <- as.h2o(c(1:3))

  ## SUBSETTING BY LOGICAL EXPRESSION ON ANOTHER h2o FRAME
  h2oFRAME_test[h2oFRAME_test[, "test_col_copy"] < 2, ]

  ## CONVERTING INTEGER TO NUMERIC
  class(as.vector(h2oFRAME_test[, "agedays"]))
  h2oFRAME_test[["agedays"]] <- h2o.asnumeric(h2oFRAME_test[["agedays"]]) + 0.0000000
  class(as.vector(h2oFRAME_test[["agedays"]]))
  # as.numeric(h2oFRAME_test[["agedays"]])





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
  ## single glm learner:
  # h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)
  GRIDparams = list(fit.package = "h2o", fit.algorithm = "grid", family = "gaussian",
                    grid.algorithm = c("glm", "gbm"), seed = 23,
                    glm = glm_hyper_params, gbm = gbm_hyper_params,
                    # learner = "h2o.glm.reg03",
                    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  ## ----------------------------------------------------------------
  ## Fit with validation data (valid_data)
  ## ----------------------------------------------------------------
  grid_mfit_val_h2o <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                 train_data = cpp[!holdout_col, ], valid_data = cpp[holdout_col, ],
                                 params  = GRIDparams, useH2Oframe = TRUE)

  ## return predictions for all models in the ensemble for training data
  fit_preds <- predict_model(grid_mfit_val_h2o); head(fit_preds[])
  ## Same without newdata, predict for training data
  fit_preds_best_nonew <- predict_model(grid_mfit_val_h2o, bestK_only = 1); head(fit_preds_best_nonew[])
  ## add subj-spec data
  fit_preds_best_nonew <- predict_model(grid_mfit_val_h2o, bestK_only = 1, add_subject_data = TRUE); fit_preds_best_nonew[]
  ## return predictions for all models in the ensemble
  fit_preds <- predict_model(grid_mfit_val_h2o, newdata = cpp); head(fit_preds[])
  ## + add subject-level features to predictions
  fit_preds <- predict_model(grid_mfit_val_h2o, newdata = cpp, add_subject_data = TRUE); fit_preds[]

  ## ----------------------------------------------------------------
  ## Cross-validation
  ## ----------------------------------------------------------------
  grid_mfit_cv <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                                train_data = cpp, params  = GRIDparams, nfolds = 3, seed = 12345, useH2Oframe = TRUE)
  ## Perform fitting on a grid of algorithms w/ Vfold CV based on user-spec'ed fold_column
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  grid_mfit_cv <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                            train_data = cpp_folds, params  = GRIDparams, fold_column = "fold", useH2Oframe = TRUE)

  # get_train_data(grid_mfit_cv)

  ## ----------------------------------------------------------------
  ## Passing existing DataStorage Object (No loading of the data should take place)
  ## ----------------------------------------------------------------
  OData_train <- validate_convert_input_data(cpp_folds, ID = "subjid",
                                             t_name = "agedays",
                                             x = c("agedays", covars),
                                             y = "haz",
                                             useH2Oframe = TRUE,
                                             dest_frame = "all_train_H2Oframe")

  grid_mfit_cv <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                            train_data = OData_train, params  = GRIDparams, fold_column = "fold", seed = 12345, useH2Oframe = TRUE)
  MSE_1 <- eval_MSE(grid_mfit_cv)
  MSE_2 <- eval_MSE(grid_mfit_cv, OData_train)
  checkTrue(all.equal(MSE_1, MSE_2))

  fit_preds <- predict_model(grid_mfit_cv, newdata = OData_train, bestK_only = 1, add_subject_data = TRUE)
  fit_preds[]
}

test.genericSL.subset <- function() {
  library("longGriDiSL")
  library("data.table")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  options(longGriDiSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
  ## Perform fitting on a grid of algorithms w/ Vfold CV based on user-spec'ed fold_column
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

  ## ----------------------------------------------------------------
  ## Passing existing DataStorage Object (No loading of the data should take place)
  ## ----------------------------------------------------------------
  # h2oFRAME_test <- fast.load.to.H2O(cpp_holdout, destination_frame = "")
  # h2oFRAME_ID <- h2o::h2o.getId(h2oFRAME_test)
  OData_train <- validate_convert_input_data(cpp_folds, ID = "subjid",
                                             t_name = "agedays",
                                             x = c("agedays", covars),
                                             y = "haz",
                                             useH2Oframe = TRUE,
                                             dest_frame = "all_train_H2Oframe")

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
  ## single glm learner:
  # h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)
  GRIDparams = list(fit.package = "h2o", fit.algorithm = "grid", family = "gaussian",
                    grid.algorithm = c("glm", "gbm"), seed = 23,
                    glm = glm_hyper_params, gbm = gbm_hyper_params,
                    # learner = "h2o.glm.reg03",
                    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  ## ----------------------------------------------------------------
  ## Cross-validated modeling on a subset of training data (subset specified via subsetting expression)
  ## ----------------------------------------------------------------
  grid_mfit_cv <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                            train_data = OData_train, params  = GRIDparams, fold_column = "fold", seed = 12345,
                            useH2Oframe = TRUE, subset_exprs = list("agedays == 1"))
  # [1] "Mean CV MSE (for out of sample predictions) as evaluated by h2o: "
  #   grid.glm.1 grid.glm.2 grid.glm.3 grid.gbm.4 grid.gbm.5
  # 1   1.727191   1.727197   1.727594   2.035656   2.107039
  MSE_1 <- eval_MSE(grid_mfit_cv)
  MSE_2 <- eval_MSE(grid_mfit_cv, OData_train)
  checkTrue(all.equal(MSE_1, MSE_2))

  fit_preds <- predict_model(grid_mfit_cv, newdata = OData_train, bestK_only = 1, add_subject_data = FALSE)
  fit_preds[]
  fit_holdout <- predict_holdout(grid_mfit_cv, newdata = OData_train, bestK_only = 1, add_subject_data = FALSE)
  fit_holdout[]

  ## ----------------------------------------------------------------
  ## Cross-validated modeling on a subset of training data (subset specified via row index)
  ## ----------------------------------------------------------------
  subset_idx <- OData_train$evalsubst(subset_exprs = "agedays == 1")
  grid_mfit_cv_b <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                            train_data = OData_train, params  = GRIDparams, fold_column = "fold", seed = 12345,
                            useH2Oframe = TRUE, subset_idx = subset_idx)

  MSE_1b <- eval_MSE(grid_mfit_cv_b, subset_idx = subset_idx)
  MSE_2b <- eval_MSE(grid_mfit_cv_b, OData_train, subset_idx = subset_idx)
  checkTrue(all.equal(MSE_1b, MSE_2b))

  fit_preds_b <- predict_model(grid_mfit_cv_b, newdata = OData_train, bestK_only = 1, add_subject_data = FALSE, subset_idx = subset_idx)
  fit_preds_b[]

  fit_preds_b <- predict_model(grid_mfit_cv_b, newdata = OData_train, bestK_only = 1, add_subject_data = TRUE, subset_idx = subset_idx)
  fit_preds_b[]

  fit_holdout_b <- predict_holdout(grid_mfit_cv_b, newdata = OData_train, bestK_only = 1, add_subject_data = FALSE, subset_idx = subset_idx)
  fit_holdout_b[]

  fit_holdout_b <- predict_holdout(grid_mfit_cv_b, newdata = OData_train, bestK_only = 1, add_subject_data = TRUE, subset_idx = subset_idx)
  fit_holdout_b[]
}


## ------------------------------------------------------------------------------------
## generic functions for fitting / predicting using either single test set / validation set
## or using internal h2o cross-validation
## ------------------------------------------------------------------------------------
test.generic.h2oSL <- function() {
  library("longGriDiSL")
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  # h2o::h2o.init(nthreads = 32, max_mem_size = "40G")
  # h2o::h2o.shutdown(prompt = FALSE)
  options(longGriDiSL.verbose = TRUE)
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
  GRIDparams = list(fit.package = "h2o", fit.algorithm = "grid", family = "gaussian",
                    grid.algorithm = c("gbm"), seed = 23, # "glm",
                    glm = glm_hyper_params,
                    gbm = gbm_hyper_params,
                    # , learner = "h2o.glm.reg03",
                    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10)

  grid_mfit_val <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                             train_data = cpp[!holdout_col, ], valid_data = cpp[holdout_col, ],
                             params  = GRIDparams)

  ## return predictions for all models in the ensemble
  fit_preds <- predict_model(grid_mfit_val, newdata = cpp)
  fit_preds[]

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
  fit_preds_best <- predict_model(grid_mfit_val, newdata = cpp, bestK_only = 1)
  head(fit_preds_best[])
  ## + subj-spec data
  fit_preds_best <- predict_model(grid_mfit_val, newdata = cpp, bestK_only = 1, add_subject_data = TRUE)
  fit_preds_best[]

  ## Same without newdata, predict for training data
  fit_preds_best_nonew <- predict_model(grid_mfit_val, bestK_only = 1)
  head(fit_preds_best_nonew[])
  ## + subj-spec data
  fit_preds_best_nonew <- predict_model(grid_mfit_val, bestK_only = 1, add_subject_data = TRUE)
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
  fit_preds_all[]
  head(cpp)

  fit_preds_best <- predict_model(grid_mfit_cv, newdata = cpp, bestK_only = 1)
  head(fit_preds_best[])

  fit_preds_best <- predict_model(grid_mfit_cv, bestK_only = 1)
  head(fit_preds_best[]); nrow(fit_preds_best)

  grid_mfit_cv$show(model_stats = TRUE, all_fits = TRUE)
  model_cv <- grid_mfit_cv$getfit$fitted_models_all[[1]]

  eval_MSE_cv(grid_mfit_cv)

  ## ----------------------------------------------------------------
  ## Perform fitting on a grid of algorithms w/ Vfold CV based on user-spec'ed fold_column
  ## ----------------------------------------------------------------
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  grid_mfit_cv <- fit_model(ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                            train_data = cpp_folds, params  = GRIDparams, fold_column = "fold")

  fit_preds_all <- predict_model(grid_mfit_cv, newdata = cpp)
  head(fit_preds_all[])

  fit_preds_best <- predict_model(grid_mfit_cv, newdata = cpp, bestK_only = 1)
  head(fit_preds_best[])

  # eval_MSE_cv(grid_mfit_cv)

  grid_mfit_cv$getmodel_ids

  grid_mfit_cv$show(model_stats = TRUE, all_fits = TRUE)
  # model_cv <- grid_mfit_cv$getfit$fitted_models_all$h2o.glm.reg03
  model_cv <- grid_mfit_cv$getfit$fitted_models_all[[1]]
}
