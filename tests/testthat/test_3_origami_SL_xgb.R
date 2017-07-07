context("CV brokenstick")

  require('h2o')
  require("gridisl")
  options(width = 100)
  options(gridisl.verbose = TRUE)
  # options(gridisl.verbose = FALSE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  cpp <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  cpp <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

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
                            max_depth = 5
                            # max_delta_step = c(0,1),
                            # subsample = c(0.9, 1),
                            # colsample_bytree = c(0.5, 1.0)
                            ),
                          # early_stopping_rounds = 50,
                          seed = 23) +
                defModel(estimator = "speedglm__glm", family = "gaussian", x = "agedays")

cv_origami <- function(models, data, fold_column, hold_column, ID, t_name, x, y) {
  cv_1fold <- function(fold) {
    v <- origami::fold_index()
    # train_idx <- origami::training()
    train_data <- origami::training(data)
    # valid_idx <- origami::validation()
    valid_data <- origami::validation(data)

    ## fit brokenstick model on training fold
    mfit <- gridisl:::fit_model(ID = ID, t_name = t_name, x = x, y = y, train_data, valid_data, models = models)
    ## save holdout outcomes:
    valid_y <- valid_data[[y]]
    ## hide holdout outcomes in validation data
    valid_data[, (y) := NULL]
    ## matrix (data.table) of validation fold prediction from ALL models in the grid
    valid_preds <- gridisl:::predict_generic(mfit, newdata = valid_data, add_subject_data = FALSE, best_only = FALSE)
    valid_preds[, ("y") := valid_y]
    valid_MSE <- valid_preds[, lapply(.SD, function(x) (x - y)^2), .SDcols = names(valid_preds)[-ncol(valid_preds)]]
    # ncol(MSE)
    # ncol(preds)

    valid_mean_MSE <- sort(unlist(lapply(valid_MSE, mean, na.rm = TRUE)))
    valid_mean_MSEdf <- data.frame(model = names(valid_mean_MSE), MSE = valid_mean_MSE, row.names = NULL)

    print(paste0("mean CV-MSE for fold ", v, ":"))
    print(valid_mean_MSEdf)

    list(fold = v, valid_preds = valid_preds, valid_MSE = valid_MSE)

    ## predictions for training data:
    ## these two should be always identical:
    # preds_train <- predict_SL(mfit)
    # preds_train <- predict_SL(mfit, newdata = train_data)
    ## this will obtain predictions from previously supplied validation data:
    # preds <- predict_SL(mfit, holdout = TRUE)

    # mfit <- fit(models, ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
    #             data = train_data, method = "none")
    # data[, ("hold") := FALSE]
    # data[valid_idx, ("hold") := TRUE]
    # mfit <- fit(models, ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
    #             data = data, method = "holdout", hold_column = "hold")
  }

  Vfolds <- make_kfold_from_column(data[[fold_column]])
  results <- origami::cross_validate(cv_1fold, Vfolds)
  # browser()
  # str(lapply(results$valid_MSE, mean, na.rm = TRUE))
}

  x = c("agedays", covars)
  models <- GRIDparams
  ## Convert folds to origami format (performed inside cv_origami)
  # cv_origami(models, cpp, "fold", "hold", ID = "subjid", t_name = "agedays", x = x, y = "haz")

  Vfolds <- make_kfold_from_column(cpp[["fold"]])
  SLfit <- origami_SuperLearner(X = cpp, y_name = "haz", x_name = x, id_name = "subjid", t_name = "agedays",
                       SL.library = models,
                       folds = Vfolds
                       )
  # class(SLfit)
#                        Risk       Coef
# m.1.h2o.gbm.grid.1 1.479386 0.42243010
# m.1.h2o.gbm.grid.2 1.505647 0.17311494
# m.1.h2o.gbm.grid.3 1.528869 0.09132913
# m.1.h2o.gbm.grid.4 1.606154 0.00000000
# m.2.xgb.gbm.grid.1 1.497424 0.09265465
# m.2.xgb.gbm.grid.2 1.588657 0.00000000
# m.3.h2o.glm        1.537484 0.22047118


  ## SL holdout predictions from original data:
  Z1 <- predict_SL(SLfit, holdout = TRUE)
  Z1
  ## SL holdout predictions for new data:
  Z2_holdout <- predict_SL(SLfit, cpp, holdout = TRUE)
  Z2_holdout[]
  ## SL prediction for new data:
  Z2 <- predict_SL(SLfit, cpp)
  Z2

  split_preds <- predict_SL_splitspec.origami_SuperLearner(SLfit, cpp)

  SLfit_SS <- origami_SuperLearner(X = cpp, y_name = "haz", x_name = x, id_name = "subjid", t_name = "agedays",
                       SL.library = models,
                       folds = Vfolds,
                       splitspec_y = split_preds
                       )
#                          Risk       Coef
# m.1.h2o.gbm.grid.1 0.01692271 0.36455182
# m.1.h2o.gbm.grid.2 0.01763421 0.17718720
# m.1.h2o.gbm.grid.3 0.02048878 0.20380576
# m.1.h2o.gbm.grid.4 0.02120910 0.02848721
# m.2.xgb.gbm.grid.1 0.03038491 0.00000000
# m.2.xgb.gbm.grid.2 0.01758235 0.22596801
# m.3.glm            0.10645430 0.00000000

  cpp2 <- cpp
  cpp2[["haz"]] <- Z2[["pred"]]
  SLfit_noSS <- origami_SuperLearner(X = cpp2, y_name = "haz", x_name = x, id_name = "subjid", t_name = "agedays",
                       SL.library = models,
                       folds = Vfolds
                       )

  #                          Risk      Coef
  # m.1.h2o.gbm.grid.1 0.03357588 0.4333617
  # m.1.h2o.gbm.grid.2 0.03620003 0.1335599
  # m.1.h2o.gbm.grid.3 0.04452229 0.1239840
  # m.1.h2o.gbm.grid.4 0.04669002 0.0000000
  # m.2.xgb.gbm.grid.1 0.05067947 0.0000000
  # m.2.xgb.gbm.grid.2 0.03532525 0.3090944
  # m.3.h2o.glm        0.10704434 0.0000000

