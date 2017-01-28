## ---------------------------------------------
## THIS runs fine on AWS linux
## ---------------------------------------------
NOtest.xgb_parallel <- function() {
    library('foreach')
    library('doParallel')
    library('xgboost')
    Models <- function(seed){
        data(agaricus.train, package='xgboost')
        data(agaricus.test, package='xgboost')

        dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
        dtest <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)

        watchlist <- list(eval = dtest, train = dtrain)
        param <- list(max_depth = 5, eta = 0.02, nthread = 1, silent = 1,
                      objective = "binary:logistic", eval_metric = "auc")
        bst <- xgb.train(param, dtrain, nrounds = 2, watchlist)
        return(bst)
    }
    registerDoParallel(cores = 20)
    r <- foreach(n=seq.int(200), .packages=c('xgboost')) %dopar% {
        Models(n)
    }
    stopCluster(cl)
    stopImplicitCluster()
    # cl <- makeCluster(20)
    # registerDoParallel(cl)
    # cl <- makeForkCluster(32)
    # registerDoParallel(cl)
}

## -----------------------------------------------------------------
## The bug with xgboost has been narrowed down to data.table / xgboost interactions.
## Only appears on systems where data.table has been compliled with OMP nthreads.
## -----------------------------------------------------------------
NOtest.xgb_parallel_2 <- function() {
    library('foreach')
    library('doParallel')
    library('xgboost')
    BootStrappedModels <- function(seed){
        data.table::setDTthreads(2)
        x = data.table(x1 = rnorm(500), x2 = rnorm(500))
        # x = matrix(rnorm(1:1000),200,5)
        target = sample(0:2,200,replace = TRUE)

        xgb_dat <- xgb.DMatrix(x, label = target)

        param <- list("objective" = "multi:softprob",
                      "eval_metric" = "mlogloss",
                      "num_class" = 3,
                      "nthread" = 1)
        tempModel <- xgb.train(params=param, data=xgb_dat, nrounds=5)
        return (tempModel)
    }
    registerDoParallel(cores = 4)
    # cl <- makeCluster(20)
    # cl <- makeForkCluster(32)
    # registerDoParallel(cl)
    # registerDoSEQ()
    models <- foreach (i=(1:100), .packages=c('xgboost')) %dopar% {
      BootStrappedModels(i)
    }
    stopCluster(cl)
    stopImplicitCluster()
    preds <- predict(models[[1]],x) ####This is where failure occurs
}
