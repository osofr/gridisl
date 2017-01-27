gridisl
==========
<!-- 
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/gridisl)](http://cran.r-project.org/package=gridisl)
[![](http://cranlogs.r-pkg.org/badges/gridisl)](http://cran.rstudio.com/web/packages/gridisl/index.html)
[![Travis-CI Build Status](https://travis-ci.org/osofr/gridisl.svg?branch=master)](https://travis-ci.org/osofr/gridisl)
[![Coverage Status](https://coveralls.io/repos/osofr/gridisl/badge.svg?branch=master&service=github)](https://coveralls.io/github/osofr/gridisl?branch=master)
 -->


### Installation

<!-- To install the CRAN release version of `gridisl`: 

```R
install.packages('gridisl')
```
 -->

To install the development version (requires the `devtools` package):

```R
devtools::install_github('osofr/gridisl', build_vignettes = FALSE)
```


### Perform Simultaneuous Model Selection with xgboost and h2o 

Initialize h2o cluster:

```R
  require("h2o")
  h2o::h2o.init(nthreads = -1)
  options(gridisl.verbose = TRUE)
  # options(gridisl.verbose = FALSE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
```

Specify the models:

```R
## Single GBM w/ h2o vs. xgboost with (roughly) equivalent parameter settings as evaluated by holdout MSE & CV-MSE

GRIDparams <- 
    defModel(estimator = "h2o__gbm", family = "gaussian",
               ntrees = 500,
               learn_rate = 0.01,
               max_depth = 5,
               min_rows = 10, # [default=10]
               col_sample_rate_per_tree = 0.3,  # [default=1]
               stopping_rounds = 10, stopping_metric = "MSE", score_each_iteration = TRUE, score_tree_interval = 1,
               seed = 23) +

    defModel(estimator = "xgboost__gbm", family = "gaussian",
               nrounds = 500,
               learning_rate = 0.01, 
               max_depth = 5,
               min_child_weight = 10,
               colsample_bytree = 0.3,
               alpha = 0.5
               early_stopping_rounds = 10,
               seed = 23)
```

Fit all models at once and evaluate perfomance based on random holdout observations: 

```R
## SuperLearner with random holdout:
cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
mfit_hold <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                data = cpp_holdout, method = "holdout", hold_column = "hold")
```

Fit all models at once and evaluate perfomance based on V-fold cross-validation:

```R
## SuperLearner with CV:
cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
mfit_cv <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                data = cpp_folds, method = "cv", fold_column = "fold")

```

Specifying large ensembles of models with grids of hyper-parameters:

```R
  GRIDparams <- 
    defModel(estimator = "h2o__gbm", family = "gaussian",
              ntrees = 500,
              param_grid = list(
                learn_rate = c(0.01, 0.02, 0.5, 0.3),
                max_depth = 5,
                sample_rate = c(0.3, 0.5, 0.8, 0.9, 1),
                col_sample_rate_per_tree = c(0.3, 0.4, 0.5, 0.7, 0.9, 1.0)
              ),
              stopping_rounds = 10, stopping_metric = "MSE", score_each_iteration = TRUE, score_tree_interval = 1,
              seed = 23) +
    defModel(estimator = "xgboost__gbm", family = "gaussian",
              nrounds = 500,
              param_grid = list(
                eta = c(0.01, 0.02, 0.5, 0.3),
                max_depth = 5,
                max_delta_step = c(0,1),
                subsample = c(0.3, 0.5, 0.8, 0.9, 1),
                colsample_bytree = c(0.3, 0.4, 0.5, 0.7, 0.9, 1.0)
                ),
              early_stopping_rounds = 50,
              seed = 23)
```


```R
## SL with random holdout:
cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
mfit_hold <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                data = cpp_holdout, method = "holdout", hold_column = "hold")
```

```R
## SL with CV:
cpp_folds <- add_CVfolds_ind(cpp, ID = "subjid", nfolds = 5, seed = 23)
mfit_cv <- fit(GRIDparams, ID = "subjid", t_name = "agedays", x = c("agedays", covars), y = "haz",
                data = cpp_folds, method = "cv", fold_column = "fold")
```

### Copyright
This software is distributed under the GPL-2 license.
