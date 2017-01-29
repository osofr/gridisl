## ------------------------------------------------------------------------------------
## face / brokenstick based on random holdouts
## ------------------------------------------------------------------------------------
test.holdoutfit_all <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  # covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
  # define holdout col:
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  holdout_col <- cpp_holdout[["hold"]]

  ID <- "subjid"
  t_name <- "agedays"
  x <- "agedays"
  y <- "haz"

  run_algo <- function(fit.package, fit.algorithm) {
    mfit_cor_hold <- fit_holdoutSL(ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
                                    data = cpp_holdout, hold_column = "hold",
                                    models = list(
                                              list(fit.package = fit.package,
                                                fit.algorithm = fit.algorithm,
                                                family = "gaussian",
                                                predict.w.Y = FALSE, name = "correct")
                                              )
                                    )
    print("Holdout MSE, hiding the holdout Y for prediction"); print(mfit_cor_hold$getMSE)
    # speed or reg GLM MSE: [1] 1.813257
    # h2o GLM MSE: [1] 1.619442
    # xgboost glm MSE: [1] 1.619442
    # DRF MSE: [1] 1.531855
    # deeplearning MSE: [1] 1.543277
    # GBM MSE: [1] 1.531809
    # xgboost gbm MSE: [1] 1.531563

    # predict for previously used holdout / validation set:
    preds_hold <- gridisl:::predict_holdout(mfit_cor_hold)
    print(nrow(preds_hold)) # [1] 453
    print(preds_hold[])

    ## Obtain predictions for model trained on non-holdout obs (***** NOT WORKING *****):
    # preds_train <- predict_nonholdouts(mfit_cor_hold, newdata = cpp_holdout, add_subject_data = TRUE)
    # print(preds_train[])

    ## Obtain predictions for a model trained on all data:
    preds_alldat_train <- predict_SL(mfit_cor_hold, newdata = cpp_holdout, add_subject_data = TRUE)
    print(preds_alldat_train[])

    return(list(mfit_cor_hold =  mfit_cor_hold))
  }

  res_GLM1 <- run_algo("speedglm", "glm")
  res_GLM2 <- run_algo("glm", "glm")

  h2o::h2o.init(nthreads = 1)
  res_GLM3 <- run_algo("h2o", "glm")
  res_GBM <- run_algo("h2o", "gbm")
  res_DRF <- run_algo("h2o", "randomForest")
  res_DP <- run_algo("h2o", "deeplearning")

  res_XGBM <- run_algo("xgboost", "gbm")
  res_XGLM <- run_algo("xgboost", "glm")

  mfits_stack <- make_PredictionStack(res_GLM3$mfit_cor_hold, res_GBM$mfit_cor_hold,
                                      res_DRF$mfit_cor_hold, res_DP$mfit_cor_hold,
                                      res_XGBM$mfit_cor_hold, res_XGLM$mfit_cor_hold
                                      )

  print(mfits_stack$get_best_MSEs(K = 1))
  print(mfits_stack$get_best_MSE_table(K = 1))
  checkException(print(mfits_stack$getMSEtab(K = 1)))
  make_model_report(mfits_stack, data = cpp_holdout, K = 2,
                  file.name = paste0("BS_ALL_", getOption("gridisl.file.name")),
                  format = "html", openFile = FALSE)

  # get the model objects for top K models:
  top_model <- mfits_stack$get_best_models(K = 1)
  mfits_stack$show(model_stats = TRUE, all_fits = TRUE)

  train_dat <- get_train_data(res_GBM$mfit_cor_hold)
  val_dat <- get_validation_data(res_GBM$mfit_cor_hold)

  h2o::h2o.shutdown(prompt = FALSE)
}