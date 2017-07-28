## ------------------------------------------------------------------------------------
## Growth Curve SL with model scoring based on full V-FOLD CROSS-VALIDATION
## ------------------------------------------------------------------------------------
test.convexSL_GRIDs <- function() {
  options(gridisl.verbose = TRUE)
  # options(gridisl.verbose = FALSE)
  require("gridisl")

  library("h2o")
  Sys.sleep(3)
  h2o::h2o.init(nthreads = 1)
  Sys.sleep(3)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ------------------------------------------------------------------------------------------------
  ## Define learners (glm, grid glm and grid gbm)
  ## ------------------------------------------------------------------------------------------------
  ## gbm grid learner:
  h2ogbm_hyper <- list(
    ntrees = c(20, 40),
    learn_rate = c(0.1, 0.3),
    max_depth = c(3, 9),
    # sample_rate = c(0.7, 0.8),
    sample_rate = 1, # for replicability of results with different CV approaches
    # col_sample_rate = c(0.5, 0.8)
    col_sample_rate = 1
  )
  # balance_classes = c(TRUE, FALSE))

  xgb_hyper = list(eta = c(0.3, 0.1, 0.01),
                  max_depth = c(4, 6, 8), # , 10
                  max_delta_step = c(0, 1),
                  subsample = 1,
                  scale_pos_weight = 1)

  params <-
    # defModel(estimator = "xgboost__glm", family = "gaussian", nrounds = 200, nthread = 1) +
    # defModel(estimator = "xgboost__gbm", family = "gaussian", nrounds = 20,
    #          nthread = 1,
    #          param_grid = xgb_hyper,
    #          seed = 123456)
    # defModel(estimator = "h2o__glm",
    #       # search_criteria = list(strategy = "RandomDiscrete", max_models = 5),
    #       family = "gaussian",
    #       seed = 23,
    #       param_grid = list(
    #         alpha = c(0,.5,1),
    #         # alpha = c(0,1,seq(0.1,0.9,0.1)),
    #         lambda = c(0,1e-7,1e-5,1e-3,1e-1)
    #         )) +
    defModel(estimator = "h2o__gbm",
            # search_criteria = list(strategy = "RandomDiscrete", max_models = 5, max_runtime_secs = 60*60),
            family = "gaussian",
            seed = 12345,
            param_grid = h2ogbm_hyper
            # stopping_rounds = 2,
            # stopping_tolerance = 1e-4,
            # stopping_metric = "MSE"
            )

  ## define CV folds (respecting that multiple observations per subject must fall within the same fold)
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 10, seed = 23)
  # cpp_folds_10 <- data.table::rbindlist(lapply(1:10, function(x) cpp_folds))


  ## ------------------------------------------------------------------------------------------------
  ## use internal CV with manually defined fold column, then fit the SL on Z matrix
  ## ------------------------------------------------------------------------------------------------
  # set.seed(12345)
  system.time(
Rprof(tmp <- tempfile())
# example(glm)
    mfit_origamiSL <- fit(models = params, method = "origamiSL", data = cpp_folds,
                          ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                          fold_column = "fold"
                          # subset_idx = 1:1200
                          )
Rprof()
summaryRprof(tmp)
unlink(tmp)

  )
   #   user  system elapsed
   # 38.590   0.281  38.717

  # set.seed(12345)
  system.time(
    mfit_internalSL <- fit(models = params, method = "internalSL", data = cpp_folds,
                            ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                            fold_column = "fold",
                            refit = FALSE
                            # subset_idx = 1:1200
                            )
  )
  #  user  system elapsed
  # 8.014   0.076   8.077

  print(mfit_origamiSL)

  print(mfit_internalSL)

  ## Super learner predictions for training data:
  predsSL_train <- predict_SL(mfit_internalSL)
  # predsSL_train[]

  predsSL_train_all <- predict_SL(mfit_internalSL, stack = FALSE)

  ## Super learner predictions for new data:
  predsSL_train2 <- predict_SL(mfit_internalSL, newdata = cpp_folds)
  # predsSL_train2[]

  ## Super learner predictions for holdout observations (SL for Z matrix):
  predsSL_holdout <- predict_SL(mfit_internalSL, holdout = TRUE)
  # predsSL_holdout[]

  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(3)
}


test.convexSL_manual <- function() {
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)
  require("gridisl")

  library("h2o")
  Sys.sleep(3)
  h2o::h2o.init(nthreads = 1)
  Sys.sleep(3)

  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

  ## ------------------------------------------------------------------------------------------------
  ## Define learners (glm, grid glm and grid gbm)
  ## ------------------------------------------------------------------------------------------------
  xgb_hyper = list(eta = c(0.3, 0.01),
                  max_depth = c(4, 10),
                  max_delta_step = c(0, 1),
                  subsample = 1,
                  scale_pos_weight = 1)

  params <-
    defModel(estimator = "xgboost__gbm", family = "gaussian", nrounds = 20,
             nthread = 1,
             param_grid = xgb_hyper,
             seed = 123456)

  ## define CV folds (respecting that multiple observations per subject must fall within the same fold)
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 10, seed = 23)

  ## ------------------------------------------------------------------------------------------------
  ## do the regular V-fold CV, then manually do the super learner NNLS
  ## ------------------------------------------------------------------------------------------------
  mfit_cv1 <- fit(models = params, method = "cv", data = cpp_folds,
                  ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                  fold_column = "fold",
                  refit = FALSE, subset_idx = 1:500)
  ## Best model predictions, in-sample and holdout:
  # preds_alldat1 <- predict_SL(mfit_cv1, add_subject_data = FALSE, holdout = FALSE)
  # preds_alldat1 <- predict_SL(mfit_cv1, add_subject_data = FALSE, holdout = TRUE)
  # head(preds_alldat1[])
  ## training fold predictions for new data
  # trainpreds <- gridisl:::predict_generic(mfit_cv1, mfit_cv1$OData_train, add_subject_data = FALSE, best_only = FALSE, holdout = FALSE)
  # trainpreds[1, 1][[1]]
  ## holdout (Z) predictions for validation folds from input data and for new data:
  Zpreds <- gridisl:::predict_generic(mfit_cv1, add_subject_data = FALSE, best_only = FALSE, holdout = TRUE)
  # Zpreds2 <- gridisl:::predict_generic(mfit_cv1, mfit_cv1$OData_train, add_subject_data = FALSE, best_only = FALSE, holdout = TRUE)
  # all.equal(Zpreds, Zpreds2) ## not exactly equal due to some internal rounding in xgboost

  ## NNLS Super-Learner:
  Yvals <- cpp_folds[["haz"]]
  Wtsvals <- rep.int(1L, length(Yvals))
  method <- SuperLearner::method.NNLS()
  getCoef <- method$computeCoef(Z = as.matrix(Zpreds), Y = Yvals, obsWeights = Wtsvals, libraryNames = names(Zpreds), verbose = TRUE)
  data.frame(cvRisk = getCoef$cvRisk, coef = getCoef$coef)
  library_pred <- gridisl:::predict_generic(mfit_cv1, newdata = cpp_folds, add_subject_data = FALSE, best_only = FALSE, holdout = FALSE)
  pred <- data.table::data.table(preds = method$computePred(library_pred, getCoef$coef))

  ## LASSO-RIDGE SL with h2o:
  Zpreds2 <- Zpreds
  Zpreds2[, "Yvals" := Yvals]
  ## Super Learner with non-negative LS h2o

  sum(h2o.coef(SLh2o_1))
  sum(h2o.coef(SLh2o_2))
  Zpreds2[, "fold" := NULL]
  SLh2o_1 <- h2o.glm(y = "Yvals",
          training_frame = as.h2o(Zpreds2),
          standardize = FALSE,
          intercept = FALSE,
          non_negative = FALSE,
          )

  Zpreds2[, "fold" := cpp_folds[["fold"]]]
  ## Super Learner with non-negative regularlized h2o elestic net
  SLh2o_2 <- h2o.glm(y = "Yvals",
          training_frame = as.h2o(Zpreds2),
          standardize = FALSE,
          intercept = FALSE,
          non_negative = FALSE,
          # lambda = 0.1,
          lambda_search = TRUE,
          nlambdas = 20,
          fold_column = "fold"
          )


  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(3)
}
