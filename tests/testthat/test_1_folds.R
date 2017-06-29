context("folds")

  require("gridisl")
  options(width = 100)
  # options(gridisl.verbose = TRUE)
  options(gridisl.verbose = FALSE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")

test_that("Cross-validation and folds definitions", {
  cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)

  ## Convert folds to origami format
  Vfolds <- make_kfold_from_column(cpp_folds[["fold"]])
  Vfolds[[1]]$validation_set

  ## Same with origami
  Vfolds <- origami::make_folds(fold_fun = origami::folds_vfold,
                                cluster_ids = cpp_folds[["subjid"]],
                                V = 5)
})

test_that("Two ways of calling brokenstick with random holdout", {
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)

  mfit_hold1 <- gridisl:::fit_holdoutSL(
        ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
        data = cpp_holdout, hold_column = "hold",
        models = list(list(fit.package = "brokenstick", fit.algorithm = "brokenstick"))
      )

    modeldef <- defModel(estimator = "brokenstick__brokenstick")
    mfit_hold2 <- fit(modeldef, ID = "subjid", t_name = "agedays", x = "agedays", y = "haz",
                     data = cpp_holdout, method = "holdout", hold_column = "hold")

})







