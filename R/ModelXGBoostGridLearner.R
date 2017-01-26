## custom MSE error evaluation function. Averages the subject level MSE first, then averages across subjects
evalMSEerror_byID <- function(preds, data) {
  labels <- getinfo(data, "label")
  # The usual RMSE (based on rows in the dataset)
  # err = sqrt(as.numeric(sum((labels - preds)^2))/length(labels))
  # RMSE based on the average MSE across subjects first
  IDs <- attr(data, "ID")
  MSEerr_row = (labels - preds)^2
  err <- sqrt(mean(tapply(MSEerr_row, IDs, mean)))
  # datDT <- data.table::data.table(MSEerr = (labels - preds)^2, IDs = IDs)
  # data.table::setkeyv(datDT, cols = "IDs")
  # err <- sqrt(mean(datDT[, mean(MSEerr, na.rm = TRUE), by = IDs][, IDs := NULL][[1]], na.rm = TRUE))
  # classification error
  # err <- as.numeric(sum(labels != (preds > 0)))/length(labels)
  return(list(metric = "RMSE", value = err))
}

## Peforming simple hyperparameter grid search for xgboost with cross-validation
## Relies on tidyverse syntax and borrowed from: https://drsimonj.svbtle.com/grid-search-in-the-tidyverse
#' @export
# print_every_n = 1L,
xgb.grid <- function(param_grid, data, nrounds, nfold, label = NULL, missing = NA,
                    prediction = FALSE, showsd = TRUE, metrics = list(), obj = NULL,
                    feval = NULL, stratified = TRUE, folds = NULL, verbose = TRUE,
                    early_stopping_rounds = NULL, maximize = NULL,
                    callbacks = list(), search_criteria, seed = NULL, order_metric_name = NULL,
                    validation_data = NULL, ...) {

  add_args <- list(...)

  if (!missing(nfold)) stop("For model evaluation via cross-validation please specify the argument 'folds'; use of 'nfold' argument is not allowed here.")
  if (!is.null(validation_data) && !is.null(folds)) stop("Cannot use validation_data and folds at the same time.")

  runCV <- FALSE ## by default runs xgb.train, if validation data is provided it will be used as a test metric
  if (is.null(validation_data) && !is.null(folds)) runCV <- TRUE

  if ( missing(search_criteria) || is.null(search_criteria) ) search_criteria <- list(strategy = 'Cartesian')
  strategy <- search_criteria[["strategy"]]

  if ( is.null(strategy) || strategy %in% "Cartesian" ) {
    random <- FALSE
  } else if ( strategy %in% "RandomDiscrete" ) {
    random <- TRUE
  } else {
    stop("Strategy must be either 'Cartesian' or 'RandomDiscrete'.")
  }

  if ( !runCV ) {
    ## Test models based on holdout validation data
    if (!is.null(validation_data)) {
      watchlist <- list(train = data, test = validation_data)
      order_metric_type <- "test"
    } else {
      watchlist <- list(train = data)
      order_metric_type <- "train"
    }
  } else {
    order_metric_type <- "test"
  }

  run_singe_model <- function(...) {
    params <- list(...)
    if (length(add_args) > 0) params <- c(params, add_args)

    ## 1. Explicit use of the cb.evaluation.log callback allows to run xgb.train / xgb.cv silently but still storing the evaluation results
    ## 2. Early stopping: Can directly specify which validation metric should be used for early stopping.
    ##    When omitted, but 'early_stopping_rounds' is specified the LAST dataset in watchlist() is used for early stopping
    #     if (!is.null(early_stopping_rounds))
    #       early_stop_call_back <- xgboost::cb.early.stop(early_stopping_rounds, maximize = maximize,
    #                                                  metric_name = "test_"%+%order_metric_name,
    #                                                  verbose = verbose)

    if (!runCV) {
      model_fit <- xgboost::xgb.train(params = params,
                                      data = data,
                                      nrounds = as.integer(nrounds),
                                      watchlist = watchlist,
                                      obj = obj,
                                      feval = feval,
                                      verbose = verbose,
                                      # print_every_n,
                                      early_stopping_rounds = early_stopping_rounds,
                                      maximize = maximize,
                                      callbacks = c(list(xgboost::cb.evaluation.log()), callbacks),
                                      eval_metric = metrics)
    } else {
      ## Test models via V-fold cross-validation
      model_fit <- xgboost::xgb.cv(params = params,
                                   data = data,
                                   nrounds = as.integer(nrounds),
                                   nfold = nfold,
                                   label = label,
                                   missing = missing,
                                   prediction = prediction,
                                   showsd = showsd,
                                   obj = obj,
                                   feval = feval,
                                   stratified = stratified,
                                   folds = folds,
                                   verbose = verbose,
                                   # print_every_n,
                                   early_stopping_rounds = early_stopping_rounds,
                                   maximize = maximize,
                                   callbacks = c(list(xgboost::cb.evaluation.log()), callbacks),
                                   metrics = metrics
                                   )
    }
    ## cv raw models:
    raw_models_cv <- lapply(model_fit[["models"]], '[[', "raw")
    reloaded_models <- lapply(raw_models_cv, function(raw) xgboost::xgb.load(raw))
    model_fit[["models"]] <- NULL
    gc(verbose = FALSE)
    model_fit[["models"]] <- reloaded_models
    return(model_fit)
  }

  ## Convert to data frame grid
  gs <- param_grid %>% purrr::cross_d()
  if (nrow(gs) == 0L) gs <- data.frame(placeholder = TRUE)

  ## Shuffle the rows to obtain random ordering of hyper-parameters
  if (random) {
    set.seed(seed)
    gs <- gs[sample.int(nrow(gs)), ]
  }

  ## Select the max number of models for the grid
  max_models <- search_criteria[["max_models"]]
  if (!is.null(max_models) && nrow(gs) > max_models) gs <- gs[1:max_models, ]

  ## ------------------------------------------------------------
  ## Sequentially fit each model from the grid
  ## ------------------------------------------------------------
  data.table::setDT(gs)
  for (i in 1:nrow(gs)) {
    gs[i, xgb_fit := list(list(purrr::lift(run_singe_model)(gs[i, ])))]
  }
  # # gs <- gs %>% dplyr::mutate(xgb_fit = purrr::pmap(gs, run_singe_model))

  ## ------------------------------------------------------------
  ## TO RUN GRID MODELS IN PARALLEL
  ## ------------------------------------------------------------
  # # gs <- data.table::data.table(gs)
  # gs <- data.frame(gs)
  # # parallel <- TRUE
  # # if (parallel) {
  #   #     mcoptions <- list(preschedule = FALSE)
  #   # '%dopar%' <- foreach::'%dopar%'
  #   # xgb_fit <- foreach::foreach(i = 1:nrow(gs), .options.multicore = mcoptions) %dopar% {
  #   #   purrr::lift(run_singe_model)(gs[i, ])
  #   # }
  #   #   , .options.multicore = mcoptions
  # xgb_fit <- foreach::foreach(i = 1:nrow(gs)) %dopar% {
  #   do.call(run_singe_model, gs[i, , drop = FALSE])
  # }
  # gs <- data.table::data.table(gs)
  # for (i in 1:nrow(gs)) {
  #   gs[i, xgb_fit := list(xgb_fit[i])]
  # }

  glob_params <- list(missing = missing, obj = obj, feval = feval, maximize = maximize)
  gs[["glob_params"]] <- rep.int(list(glob_params), nrow(gs))

  gs <- gs %>%
    dplyr::mutate(niter = purrr::map_dbl(xgb_fit, "niter", .null = NA)) %>%
    dplyr::mutate(nrounds = purrr::map_dbl(xgb_fit, "best_iteration", .null = NA)) %>%
    dplyr::mutate(nrounds = ifelse(is.na(nrounds), niter, nrounds)) %>%
    dplyr::mutate(ntreelimit = purrr::map_dbl(xgb_fit, "best_ntreelimit", .null = NA)) %>%
    dplyr::mutate(params = purrr::map(xgb_fit, "params")) %>%
    dplyr::mutate(metrics = purrr::map2(xgb_fit, nrounds, function(fit, nrounds) fit$evaluation_log[nrounds,])) %>%
    tidyr::unnest(metrics) %>%
    data.table::data.table()

  ## Sort the data.table with grid model fits by user-supplied test/train metric value (lowest to highest)
  if (!is.null(order_metric_name)) data.table::setkeyv(gs, cols = order_metric_type %+% "_" %+% order_metric_name %+% ifelse(runCV, "_mean", ""))

  print("grid fits ordered by test metric (lowest to highest):"); print(gs)

  return(gs)

}
