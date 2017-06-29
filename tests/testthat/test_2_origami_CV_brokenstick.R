context("CV brokenstick")

require("gridisl")
options(width = 100)
# options(gridisl.verbose = TRUE)
options(gridisl.verbose = FALSE)
data(cpp)

cpp <- cpp[!is.na(cpp[, "haz"]), ]
cpp <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
cpp <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

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
    cv_heldout_y <- valid_data[valid_data[[hold_column]], ][[y]]

    ## hide holdout outcomes in validation data
    valid_data[valid_data[[hold_column]], (y) := NA]

    ## predict for new modified holdout / validation data:
    preds <- predict_SL(mfit, newdata = valid_data)

    ## get preds for holdout (hidden) outcomes only and evaluate MSE
    cv_heldout_yhat <- preds[valid_data[[hold_column]], ][["preds"]]
    MSE <- (cv_heldout_y - cv_heldout_yhat)^2
    cat("mean CV-MSE for fold ", v, ":", mean(MSE), "\n")
    list(fold = v, MSE = MSE)
  }

  ## Convert folds to origami format
  Vfolds <- make_kfold_from_column(data[[fold_column]])
  results <- origami::cross_validate(cv_1fold, Vfolds)
  print("mean CV-MSE across all holdouts:"); print(mean(results$MSE, na.rm = TRUE))
  return(list(MSE = mean(results$MSE, na.rm = TRUE), results = results))
}

test_that("Trying V-fold CV and random holdout within validation fold with brokenstick", {
  models <- defModel(estimator = "brokenstick__brokenstick", predict.w.Y = TRUE)
  res <- cv_origami(models, cpp, fold_column = "fold", hold_column = "hold", ID = "subjid", t_name = "agedays", x = "agedays", y = "haz")
  expect_true(all.equal(round(res$MSE,4), 1.2140))

  # mean CV-MSE for fold  1 : 0.6816121
  # mean CV-MSE for fold  2 : 0.8580636
  # mean CV-MSE for fold  3 : 1.171489
  # mean CV-MSE for fold  4 : 1.422196
  # mean CV-MSE for fold  5 : 1.885165
  # [1] "mean CV-MSE across all holdouts:"
  # [1] 1.214012
})

