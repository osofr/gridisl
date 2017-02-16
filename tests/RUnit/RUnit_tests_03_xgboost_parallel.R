## ---------------------------------------------
## THIS runs fine on AWS linux
## ---------------------------------------------
NOtest.xgb_parallel <- function() {
    library('foreach')
    library('doParallel')
    library('xgboost')

    run_test_xgb_Models <- function(seed){
        data(agaricus.train, package='xgboost')
        data(agaricus.test, package='xgboost')
        # browser()
        dtrain <- xgb.DMatrix(agaricus.train$data, label = agaricus.train$label)
        dtest <- xgb.DMatrix(agaricus.test$data, label = agaricus.test$label)
        # xgboost::xgb.DMatrix.save(dtrain, file.path(getwd(), "xgb.DMatrix.dtrain"))
        # fit_dmat_external <- xgb.DMatrix(paste0(file.path(getwd(), "xgb.DMatrix.dtrain"), '#dtrain.cache'))
        watchlist <- list(eval = dtest, train = dtrain)
        param <- list(max_depth = 5, eta = 0.02, nthread = 1, silent = 1,
                      objective = "binary:logistic", eval_metric = "auc")
        bst <- xgb.train(param, dtrain, nrounds = 500, watchlist)
        return(bst)
    }

    # registerDoParallel(cores = 4)
    cl <- makeForkCluster(4, outfile = "")
    registerDoParallel(cl)

    r <- foreach(n=seq.int(10), .packages=c('xgboost')) %dopar% {
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
        set.seed(123456)
        x <- as.matrix(data.table(x1 = rnorm(500), x2 = rnorm(500)))
        # x = matrix(rnorm(1:1000),200,5)
        target <- sample(0:1,500,replace = TRUE)
        # target = sample(0:2,200,replace = TRUE)

        offset <- runif(500)

        xgb_dat <- xgb.DMatrix(x, label = target)

        # weight: to do a weight rescale ;
        # setinfo(xgtrain, "weight", log(d$exposure))
        # "base_margin: base margin is the base prediction Xgboost will boost from ;"
        setinfo(xgb_dat, "base_margin", qlogis(offset))

        param <- list("objective" = "reg:logistic", "booster" = "gbtree",
                      # "objective" = "multi:softprob", "eval_metric" = "mlogloss", "num_class" = 3,
                      "nthread" = 1)
        tempModel <- xgb.train(params=param,
                               data=xgb_dat, nrounds=50, verbose = 1,
                               watchlist = list(train = xgb_dat)
                               # callbacks = list(cb.print.evaluation(period = 1))
                               )

        p1 <- predict(tempModel, xgb_dat)
        p1_logodds <- predict(tempModel, xgb_dat, outputmargin=TRUE)
        sum(p1 - plogis(p1_logodds))


        # [1]   train-rmse:0.546849
        # [50]  train-rmse:0.331395
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
