## ----------------------------------------------------------------
## TO DO TO FINISH external CV for using with stremr:
##
## 1) Need to handle subsets correctly:
##    Only the subset data will be used for fitting / extract.y
##    So the question is, do we subset it here, right away or do we pass it and delay the subsetting until the actual internal fitting is performed?
##
## 2) Need to handle DataStorage object as the input data.
##    For now we can directly obtain $dat.sVar and then call crossv_kfold(). However this is very inefficient.
##
## 3) Need to figure out how do this whole operation efficiently (by reference, so that DataStorage objects are not being copied everywhere.
##
## 4) For split-specific SL need to re-set the outcomes for each training fold to their respective fold-specific predictions from the previous SL.
##
## ----------------------------------------------------------------

#' @importFrom tibble obj_sum
#' @method obj_sum resample
# @export
obj_sum.resample <- function(x, ...) {
  paste0("resample [", big_mark(nrow(x)), " x ", big_mark(ncol(x)), "]")
}

# @export
print.resample <- function(x, ...) {
  n <- length(x$idx)
  if (n > 10) {
    id10 <- c(x$idx[1:10], "...")
  } else {
    id10 <- x$idx
  }

  cat("<", obj_sum.resample(x), "> ", paste(id10, collapse = ", "), "\n",
    sep = ""
  )
}

`[[.DataStorageClass` = function(x, i, exact = TRUE) {
  x$dat.sVar[[i]]
}

nrowS3 <- function(x, ...) { UseMethod("nrowS3") }

nrowS3.data.frame <- function(x, ...) {
  nrow(x)
}

nrowS3.DataStorageClass <- function(x, ...) {
  x$nobs
}

# @export
as.integer.resample <- function(x, ...) {
  x$idx
}

as.integer.ResampleDataClass <- function(x, ...) {
  x$as.integer
}

# @export
as.data.frame.resample <- function(x, ...) {
  x$data[x$idx, , drop = FALSE]
}

extract.y <- function(x, y, ...) { UseMethod("extract.y") }

extract.y.resample <- function(x, y, ...) {
  x$data[x$idx, y, drop = FALSE, with = FALSE][[1]]
}

extract.y.DataStorageClass <- function(x, y, ...) {
  x$get.outvar(var = y)
}

# @export
dim.resample <- function(x, ...) {
  c(length(x$idx), ncol(x$data))
}

id <- function(n) {
  width <- nchar(n)
  sprintf(paste0("%0", width, "d"), seq_len(n))
}

resample <- function(data, idx, ...) { UseMethod("resample") }

resample.data.frame <- function(data, idx, ...) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.integer(idx)) {
    stop("`idx` must be an integer vector.", call. = FALSE)
  }

  structure(
    list(
      data = data,
      idx = idx
    ),
    class = "resample"
  )
}

resample.DataStorageClass <- function(data, idx, subset_idx) {
  if (!is.DataStorageClass(data)) {
    stop("`data` must be an object of class `DataStorageClass`.", call. = FALSE)
  }
  if (!is.integer(idx)) {
    stop("`idx` must be an integer vector.", call. = FALSE)
  }
  if (!is.null(subset_idx) && !is.integer(subset_idx)) {
    stop("`subset_idx` must be NULL or an integer vector.", call. = FALSE)
  }
  ResampleDataClass$new(data, idx, subset_idx)
}

crossv_kfold <- function(data, id = ".id", fold_column = "fold", subset_idx) {
  # if (!is.numeric(k) || length(k) != 1) {
  #   stop("`k` must be a single integer.", call. = FALSE)
  # }

  n <- nrowS3(data)
  folds <- data[[fold_column]]
  k <- length(unique(folds))

  # folds <- sample(rep(1:k, length.out = n))

  idx <- seq_len(n)
  fold_idx <- split(idx, folds)

  fold <- function(test) {
    list(
      train = resample(data, setdiff(idx, test), subset_idx),
      test = resample(data, test, subset_idx)
    )
  }

  cols <- purrr::transpose(purrr::map(fold_idx, fold))
  cols[[id]] <- id(k)

  tibble::as_data_frame(cols)
}

#' @export
fit.splitCVStack <- function(models,
                        method = c("splitCV"),
                           ID,
                           t_name,
                           x,
                           y,
                           data,
                           fold_column,
                           seed = NULL,
                           subset_exprs = NULL,
                           subset_idx = NULL,
                           verbose = getOption("gridisl.verbose"),
                           ...) {

  method <- method[1L]
  gvars$method <- method
  gvars$verbose <- verbose

  loss_fun_MSE <- function(yhat, y0) (yhat - y0)^2
  resid_fun <- function(yhat, y0) as.numeric(yhat - y0)

  fn_model <- function(train, test, models, .id, ...){
    cat("training and scoring for fold: ", .id, "\n")
    fit_model(ID = ID,
              t_name = t_name,
              x = x,
              y = y,
              train_data = train,
              valid_data = test,
              # train_data = as.data.frame(train),
              # valid_data = as.data.frame(test),
              models = models)
  }

  data_cv <- crossv_kfold(data, ".id", fold_column, subset_idx)

  data_cv <-
    data_cv %>%
    dplyr::mutate(models = purrr::map(.id, ~ models))

  cv_fit <-
    data_cv %>%
    dplyr::mutate(fit = purrr::pmap(data_cv, fn_model))
    #  %>%
    # print()

  cv_fit <-
    cv_fit %>%
    dplyr::mutate(test_preds = purrr::map(fit, ~ gridisl:::predict_holdout(modelfit = .x, best_only = FALSE))) %>%
    dplyr::mutate(resid = purrr::map2(test, test_preds, ~ .y[, lapply(.SD, resid_fun, extract.y(.x, y))])) %>%
    dplyr::mutate(squared_resid = purrr::map(resid, ~ as.data.table(.x^2)))

  # predicted <- cv_fit %>% tidyr::unnest(test_preds)
  # resid <- cv_fit %>% tidyr::unnest(resid)
  squared_resid <- cv_fit %>% tidyr::unnest(squared_resid) %>% dplyr::select(-.id) %>% as.data.table

  ## Re-assign internally new MSE values to each fold-specific model object.
  ## These new MSE values are determined based on ALL validation folds.
  x <- purrr::map(cv_fit[["fit"]], ~ .x$reassignMSEs(squared_resid))
  x <- purrr::map(cv_fit[["fit"]], ~ print(.x$getMSEtab))

  cv_fit <-
    cv_fit %>%
    dplyr::select(train, test, .id, fit)

  class(cv_fit) <- c(class(cv_fit), "splitCVfits")

  return(cv_fit)
}

