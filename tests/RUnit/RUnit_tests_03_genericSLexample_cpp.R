# ## ----------------------------------------------------------------
# ## Perform fitting using some subset of rows of intput data
# ## ----------------------------------------------------------------
# test.genericSL.subset <- function() {
#   # options(gridisl.verbose = TRUE)
#   options(gridisl.verbose = FALSE)

#   require("h2o")
#   Sys.sleep(1)
#   h2o::h2o.init(nthreads = 1)
#   Sys.sleep(1)

#   data(cpp)
#   cpp <- cpp[!is.na(cpp[, "haz"]), ]
#   covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
#   ## Perform fitting on a grid of algorithms w/ Vfold CV based on user-spec'ed fold_column
#   cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

#   ## ----------------------------------------------------------------
#   ## Passing existing DataStorage Object (No loading of the data should take place)
#   ## ----------------------------------------------------------------
#   # h2oFRAME_test <- fast.load.to.H2O(cpp_holdout, destination_frame = "")
#   # h2oFRAME_ID <- h2o::h2o.getId(h2oFRAME_test)
#   OData_train <- gridisl:::validate_convert_input_data(cpp_folds, ID = "subjid",
#                                                        t_name = "agedays",
#                                                        x = c("agedays", covars),
#                                                        y = "haz",
#                                                        # useH2Oframe = TRUE,
#                                                        # dest_frame = "all_train_H2Oframe"
#                                                        )

#   ## ----------------------------------------------------------------
#   ## Define learners (glm, grid glm and grid gbm)
#   ## ----------------------------------------------------------------
#   ## gbm grid learner:
#   gbm_hyper_params <- list(ntrees = c(10, 20, 30, 50),
#                            learn_rate = c(.1, .3),
#                            max_depth = c(3, 4, 5, 6, 9),
#                            sample_rate = c(0.7, 0.8, 0.9, 1.0),
#                            col_sample_rate = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
#                            balance_classes = c(TRUE, FALSE))
#   ## single glm learner:
#   # h2o.glm.reg03 <- function(..., alpha = 0.3, nlambdas = 50, lambda_search = TRUE) h2o.glm.wrapper(..., alpha = alpha, nlambdas = nlambdas, lambda_search = lambda_search)
#   params <- defModel(estimator = "h2o__glm", family = "gaussian",
#                      search_criteria = list(strategy = "RandomDiscrete", max_models = 2),
#                      param_grid = list(
#                       alpha = c(0,1,seq(0.1,0.9,0.1)),
#                       lambda = c(0,1e-7,1e-5,1e-3,1e-1)),
#                      seed = 23) +
#             defModel(estimator = "h2o__gbm", family = "gaussian",
#                      search_criteria = list(strategy = "RandomDiscrete", max_models = 2, max_runtime_secs = 60*60),
#                      param_grid = gbm_hyper_params,
#                      stopping_rounds = 2, stopping_tolerance = 1e-4, stopping_metric = "MSE",
#                      seed = 23)

#   ## ----------------------------------------------------------------
#   ## Cross-validated modeling on a subset of training data (subset specified via subsetting expression)
#   ## ----------------------------------------------------------------
#   grid_mfit_cv <- fit(params, data = OData_train, method = "cv",
#                       ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
#                       fold_column = "fold", seed = 12345,
#                       useH2Oframe = TRUE, subset_exprs = list("agedays == 1"))

#   # [1] "Mean CV MSE (for out of sample predictions) as evaluated by h2o: "
#   #   grid.glm.1 grid.glm.2 grid.glm.3 grid.gbm.4 grid.gbm.5
#   # 1   1.727191   1.727197   1.727594   2.035656   2.107039
#   MSE_1 <- eval_MSE(grid_mfit_cv)
#   MSE_2 <- eval_MSE(grid_mfit_cv, OData_train)
#   checkTrue(all.equal(MSE_1, MSE_2))

#   fit_preds <- predict_SL(grid_mfit_cv, newdata = OData_train, add_subject_data = FALSE)
#   fit_preds[]
#   fit_holdout <- predict_SL(grid_mfit_cv, newdata = OData_train, add_subject_data = FALSE, holdout = TRUE)
#   fit_holdout[]

#   ## ----------------------------------------------------------------
#   ## Cross-validated modeling on a subset of training data (subset specified via row index)
#   ## ----------------------------------------------------------------
#   subset_idx <- OData_train$evalsubst(subset_exprs = "agedays == 1")
#   grid_mfit_cv_b <- fit(params, data = OData_train, method = "cv",
#                         ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
#                         fold_column = "fold", seed = 12345,
#                         useH2Oframe = TRUE, subset_idx = subset_idx)

#   MSE_1b <- eval_MSE(grid_mfit_cv_b, subset_idx = subset_idx)
#   MSE_2b <- eval_MSE(grid_mfit_cv_b, OData_train, subset_idx = subset_idx)
#   checkTrue(all.equal(MSE_1b, MSE_2b))

#   fit_preds_b <- predict_SL(grid_mfit_cv_b, newdata = OData_train, add_subject_data = FALSE, subset_idx = subset_idx)
#   fit_preds_b[]

#   fit_preds_b <- predict_SL(grid_mfit_cv_b, newdata = OData_train, add_subject_data = TRUE, subset_idx = subset_idx)
#   fit_preds_b[]

#   fit_holdout_b <- predict_SL(grid_mfit_cv_b, newdata = OData_train, holdout = TRUE, add_subject_data = FALSE, subset_idx = subset_idx)
#   fit_holdout_b[]

#   fit_holdout_b <- predict_SL(grid_mfit_cv_b, newdata = OData_train, holdout = TRUE, add_subject_data = TRUE, subset_idx = subset_idx)
#   fit_holdout_b[]


#   h2o::h2o.shutdown(prompt = FALSE)
#   Sys.sleep(1)
# }

