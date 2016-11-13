#' S3 methods for printing model fit summary for PredictionModel R6 class object
#'
#' Prints the modeling summaries
#' @param modelstack The model fit object produced by functions \code{make_PredictionStack}.
#' @param model_stats Also print some model summaries?
#' @param all_fits Print all of the modeling fits contained in this object? Warning: this may produce a lot of output!
#' @param ... Additional options passed on to \code{print.PredictionModel}.
#' @export
print.PredictionStack <- function(modelstack, model_stats = FALSE, all_fits = FALSE, ...) {
  modelstack$show(model_stats = model_stats, all_fits = all_fits)
  return(invisible(NULL))
}

#' @export
make_PredictionStack <- function(...) {
  PredictionModels <- list(...)
  if (!all(unlist(lapply(PredictionModels, is.PredictionModel)))) {
    stop("All arguments must be of class 'PredictionModel'")
  }
  class(PredictionModels) <- c(class(PredictionModels), "PredictionStack")
  return(PredictionStack$new(PredictionModels))
}

#' @export
PredictionStack  <- R6Class(classname = "PredictionStack",
  cloneable = TRUE,
  portable = TRUE,
  class = TRUE,
  # inherit = PredictionModel,
  public = list(
    PredictionModels = NULL,
    initialize = function(PredictionModels) {
      if (!all(unlist(lapply(PredictionModels, is.PredictionModel)))) {
       stop("All arguments must be of class 'PredictionModel'")
      }
      assert_that("PredictionStack" %in% class(PredictionModels))
      self$PredictionModels <- PredictionModels
      return(self)
    },
    fit = function(overwrite = FALSE, data, predict = FALSE, validation_data = NULL, ...) {
      stop("...not implemented...")
      return(invisible(self))
    },
    predict = function(newdata, subset_vars, subset_exprs, MSE = TRUE, ...) {
      stop("...not implemented...")
      return(invisible(self))
    },
    score_CV = function(data_valid, MSE = TRUE, ...) {
      stop("...not implemented...")
    },
    evalMSE = function(test_values) {
      stop("...not implemented...")
      return(invisible(self))
    },
    # ------------------------------------------------------------------------------
    # return a model object by name / ID
    # ------------------------------------------------------------------------------
    getmodel_byname = function(model_names, model_IDs) {
      stop("...not implemented...")
      # return(self$ModelFitObject$getmodel_byname(model_names, model_IDs))
    },
    # ------------------------------------------------------------------------------
    # return top K models based on smallest validation / test MSE for each PredictionModel in a stack
    # ------------------------------------------------------------------------------
    get_best_MSEs = function(K = 1) {
      return(sort(unlist(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$get_best_MSEs(K = K)))))
    },
    # ------------------------------------------------------------------------------
    # return top K model objects ranked by prediction MSE on a holdout (CV) fold for each PredictionModel in a stack
    # ------------------------------------------------------------------------------
    get_best_models = function(K = 1) {
      best_models <- unlist(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$get_best_models(K = K)))
      best_models <- best_models[names(self$get_best_MSEs(K))]
      return(best_models)
    },
    # ------------------------------------------------------------------------------
    # return a data.frame with best mean MSEs, including SDs & corresponding model names
    # ------------------------------------------------------------------------------
    get_best_MSE_table = function(K = 1) {
      res_tab_list <- lapply(self$PredictionModels, function(PredictionModel) PredictionModel$get_best_MSE_table(K = K))
      res_tab <- do.call("rbind", res_tab_list)
      res_tab <- res_tab[order(res_tab$MSE.CV, decreasing = FALSE), ]
      return(res_tab)
    },
    define.subset.idx = function(data) {
      stop("not applicable to this class")
    },
    # Output info on the general type of regression being fitted:
    show = function(print_format = TRUE, model_stats = FALSE, all_fits = FALSE) {
      return(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$show(print_format = TRUE, model_stats = FALSE, all_fits = FALSE)))
    },
    summary = function(all_fits = FALSE) {
      return(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$summary(all_fits = FALSE)))
    }
  ),

  active = list(
    wipe.alldat = function() {
      lapply(self$PredictionModels, function(PredictionModel) PredictionModel$wipe.alldat())
      return(self)
    }
    # emptymodelfit = function() { self$ModelFitObject$emptymodelfit },
    # getprobA1 = function() { private$probA1 },
    # getsubset = function() { self$subset_idx },
    # getoutvarnm = function() { self$outvar },
    # getoutvarval = function() { self$ModelFitObject$getY },
    # getMSE = function() { private$MSE[["MSE_mean"]] },
    # getMSEvar = function() { private$MSE[["MSE_var"]] },
    # getMSEsd = function() { private$MSE[["MSE_sd"]] },
    # getvar = function() { private$MSE[["var"]] },
    # getfit = function() { self$ModelFitObject$model.fit },
    # getmodel_ids = function() { self$ModelFitObject$getmodel_ids },
    # getmodel_algorithms = function() { self$ModelFitObject$getmodel_algorithms }
  )
  # ,
  # private = list(
  #   # model.fit = list(),   # the model fit (either coefficients or the model fit object)
  #   # MSE = list(),
  #   # probA1 = NULL,    # Predicted probA^s=1 conditional on Xmat
  #   # probAeqa = NULL   # Likelihood of observing a particular value A^s=a^s conditional on Xmat
  # )
)
