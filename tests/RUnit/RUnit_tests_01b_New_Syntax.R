test.SL.H2O.GLM_GBM_change_covars <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  library("h2o")
  Sys.sleep(3)
  h2o::h2o.init(nthreads = 1)
  Sys.sleep(3)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ------------------------------------------------------------------------------------
  ## SL with random holdout:
  ## ------------------------------------------------------------------------------------
  GRIDparams <-
              # defModel(estimator = "speedglm__glm", family = "gaussian") +

              #  defModel(estimator = "speedglm__glm", family = "gaussian",
              #             x = c("agedays", "apgar1", "apgar5", "parity")) +

               defModel(estimator = "h2o__glm", family = "gaussian",
                          x = c("agedays", "apgar1", "apgar5", "parity")) +

                defModel(estimator = "h2o__gbm", family = "gaussian", ntrees = 5,
                         search_criteria = list(strategy = "Cartesian"),
                         stopping_rounds = 2, stopping_metric = "MSE", # score_each_iteration = TRUE, score_tree_interval = 1,
                         param_grid = list(learn_rate = c(0.5),
                                           max_depth = c(5, 6),
                                           sample_rate = 0.8,
                                           col_sample_rate = 0.5),
                         x = c("agedays", "apgar1", "apgar5", "parity"),
                         seed = 23)

  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  mfit_hold <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_holdout, method = "holdout", hold_column = "hold")

  ## ------------------------------------------------------------------------------------
  ## SL with CV (CANNOT USE speedglm or glm with V-fold CV yet):
  ## ------------------------------------------------------------------------------------
  GRIDparams <- defModel(estimator = "h2o__glm", family = "gaussian",
                           x = c("agedays", "apgar1", "apgar5", "parity")) +
                defModel(estimator = "h2o__gbm", family = "gaussian", ntrees = 5,
                         search_criteria = list(strategy = "Cartesian"),
                         stopping_rounds = 2, stopping_metric = "MSE", # score_each_iteration = TRUE, score_tree_interval = 1,
                         param_grid = list(learn_rate = c(0.5),
                                           max_depth = c(5, 6),
                                           sample_rate = 0.8,
                                           col_sample_rate = 0.5),
                         x = c("agedays", "apgar1", "apgar5", "parity"),
                         seed = 23)

  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

  mfit_cv <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_folds, method = "cv", fold_column = "fold")

  # str(mfit_cv)
  # mfit_cv$getMSE
  # mfit_cv$getMSEtab
  # mfit_cv$get_best_MSE_table()
  # mfit_cv$getMSE_bysubj

  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(2)
}

test.GBM_xgboost_onelearner <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  lapply(cpp[, covars], class)

  ## ------------------------------------------------------------------------------------------------------
  ## Single GBM w/ h2o vs. xgboost
  ## ------------------------------------------------------------------------------------------------------
  GRIDparams <- defModel(estimator = "xgboost__gbm", family = "gaussian",
                          nthread = 1,
                          nrounds = 5,
                           eta = 0.01, # (learning_rate alias)
                           # min_split_loss = , # default 0 (gamma alias)
                           max_depth = 5,
                           min_child_weight = 10, # [default=1, range: [0,∞]]
                            #  In linear regression mode, this simply corresponds to minimum number of instances needed to be in each node.
                            # The larger, the more conservative the algorithm will be.
                           # max_delta_step = , # [default=0]
                            # Maximum delta step we allow each tree's weight estimation to be.
                            # If the value is set to 0, it means there is no constraint.
                            # If it is set to a positive value, it can help making the update step more conservative.
                            # Usually this parameter is not needed, but it might help in logistic regression when class is extremely imbalanced.
                            # Set it to value of 1-10 might help control the update
                           subsample = 0.8,
                           colsample_bytree = 0.3,
                           # colsample_bylevel = , # [default=1]
                           # lambda = , #  [default=1] L2 regularization term on weights, increase this value will make model more conservative.
                           # alpha =  ,# [default=0, alias: reg_alpha] L1 regularization term on weights, increase this value will make model more conservative.
                           # early_stopping_rounds = 50,
                           seed = 23)

  ## SuperLearner with random holdout:
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  mfit_hold <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_holdout, method = "holdout", hold_column = "hold")

  ## SuperLearner with CV:
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_folds, method = "cv", fold_column = "fold")


}

## ------------------------------------------------------------------------------------------------------
## New syntax for learners. Compare gbm w/ xgboost vs. h2o
## ------------------------------------------------------------------------------------------------------
test.GBM_xgboost_vs_H2O <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  require("h2o")
  Sys.sleep(3)
  h2o::h2o.init(nthreads = 1)
  Sys.sleep(3)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  # lapply(cpp[, covars], class)

  ## ------------------------------------------------------------------------------------------------------
  ## Single GBM w/ h2o vs. xgboost with (roughly) equivalent parameter settings as evaluated by holdout MSE & CV-MSE
  ## ------------------------------------------------------------------------------------------------------
  GRIDparams <- defModel(estimator = "h2o__gbm", family = "gaussian",
                           ntrees = 5,
                           learn_rate = 0.5,
                           max_depth = 5,
                           min_rows = 10, # [default=10]
                           col_sample_rate_per_tree = 0.3,  # [default=1]
                           stopping_rounds = 2, stopping_metric = "MSE",
                           seed = 23) +

                defModel(estimator = "xgboost__gbm", family = "gaussian",
                         nthread = 1,
                         nrounds = 5,
                           learning_rate = 0.5, # (alias eta), try around 5 in range (0.01-0.1)
                           max_depth = 5,
                           min_child_weight = 10, # [default=1, range: [0,∞]]
                           colsample_bytree = 0.3, # [default=1, range: (0,1]], good values: (0.3-0.5)
                           early_stopping_rounds = 2,
                           seed = 23)

  ## SuperLearner with random holdout:
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  mfit_hold <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_holdout, method = "holdout", hold_column = "hold")

  ## SuperLearner with CV:
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_folds, method = "cv", fold_column = "fold")

  ## ------------------------------------------------------------------------------------------------------
  ## Grid GBM h2o vs xgboost
  ## ------------------------------------------------------------------------------------------------------
  ## Note: when ntrees (or nrounds) is part of param_grid, the param_grid values over-ride any other.
  GRIDparams <- defModel(estimator = "h2o__gbm", family = "gaussian",
                          ntrees = 5,
                          param_grid = list(
                            # ntrees = c(10, 20),
                            learn_rate = c(0.5),
                            max_depth = 5,
                            sample_rate = c(0.9, 1),
                            col_sample_rate_per_tree = c(0.5, 1.0)
                          ),
                          # stopping_rounds = 10, stopping_metric = "MSE",
                          seed = 23) +

                defModel(estimator = "xgboost__gbm", family = "gaussian",
                         nthread = 1,
                         nrounds = 5,
                          param_grid = list(
                            # nrounds = c(10, 20),
                            eta = c(0.3, 0.5),
                            max_depth = 5,
                            max_delta_step = c(0,1),
                            subsample = c(0.9, 1),
                            colsample_bytree = c(0.5, 1.0)
                            ),
                          # early_stopping_rounds = 50,
                          seed = 23)

  ## SL with random holdout:
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  mfit_hold <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_holdout, method = "holdout", hold_column = "hold")

  ## SL with CV:
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_folds, method = "cv", fold_column = "fold")

  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(1)
}

test.H2O_GRID_GBM_GLM <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)

  require("h2o")
  Sys.sleep(3)
  h2o::h2o.init(nthreads = 1)
  Sys.sleep(3)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ------------------------------------------------------------------------------------------------------
  ## New syntax for learners. Define learners (gbm grid, glm grid and glm learner)
  ## ------------------------------------------------------------------------------------------------------
  alpha_opt <- c(0,1,seq(0.1,0.9,0.1))
  lambda_opt <- c(0,1e-7,1e-5,1e-3,1e-1)

  gbm_hyper_params <- list(ntrees = c(10, 20, 30, 50),
                           learn_rate = c(0.1, 0.3),
                           max_depth = c(3, 4, 5, 6, 9),
                           sample_rate = c(0.7, 0.8, 0.9, 1.0),
                           col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
                           balance_classes = c(TRUE, FALSE))

  GRIDparams <- defModel(estimator = "h2o__gbm", family = "gaussian", ntrees = 5,
                         search_criteria = list(strategy = "RandomDiscrete", max_models = 2, max_runtime_secs = 60*60),
                         stopping_rounds = 2, stopping_metric = "MSE",
                         param_grid = gbm_hyper_params, seed = 23) +

                defModel(estimator = "h2o__glm", family = "gaussian",
                          alpha = 0, nlambdas = 5, lambda_search = TRUE)
                #            +

                # defModel(estimator = "h2o__glm", family = "gaussian",
                #          search_criteria = list(strategy = "Cartesian"),
                #          param_grid = list(alpha = seq(0,1,0.1)),
                #          nlambdas = 5, lambda_search = TRUE) +

                # defModel(estimator = "h2o__glm", family = "gaussian",
                #          search_criteria = list(strategy = "RandomDiscrete", max_models = 2),
                #          param_grid = list(alpha = alpha_opt, lambda = lambda_opt), seed = 23)
  # old early stop params for gbm:
  # stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "MSE", score_tree_interval = 10,

  ## ------------------------------------------------------------------------------------
  ## SL with random holdout:
  ## ------------------------------------------------------------------------------------
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  mfit_hold <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_holdout, method = "holdout", hold_column = "hold")

  ## ------------------------------------------------------------------------------------
  ## SL with CV:
  ## ------------------------------------------------------------------------------------
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  mfit_cv <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                    data = cpp_folds, method = "cv", fold_column = "fold")

  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(1)
}