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

#' Combine models into ensemble
#'
#' Combine several fitted models into a single ensemble model of class 'PredictionStack'.
#' @param ... Different objects of class "PredictionModel" separated by a comma.
#' @export
make_PredictionStack <- function(...) {
  PredictionModels <- list(...)
  if (!all(unlist(lapply(PredictionModels, is.PredictionModel)))) {
    stop("All arguments must be of class 'PredictionModel'")
  }
  class(PredictionModels) <- c(class(PredictionModels), "PredictionStack")
  return(PredictionStack$new(PredictionModels))
}

## *******************************************************************************************
## Needs to be renamed to ReportStack -- because is the only actual purpose of this class
## *******************************************************************************************
#' @export
PredictionStack  <- R6Class(classname = "PredictionStack",
  cloneable = TRUE,
  portable = TRUE,
  class = TRUE,
  # inherit = PredictionModel,
  public = list(
    PredictionModels = NULL,
    runCV = NULL,
    useH2Oframe = NULL,
    nodes = NULL,
    best_model_idx = NULL,
    initialize = function(PredictionModels) {
      if (!all(unlist(lapply(PredictionModels, is.PredictionModel)))) {
       stop("All arguments must be of class 'PredictionModel'")
      }
      assert_that("PredictionStack" %in% class(PredictionModels))
      self$PredictionModels <- PredictionModels
      self$nodes <- PredictionModels[[1]]$nodes
      return(self)
    },
    fit = function(overwrite = FALSE, data, predict = FALSE, validation_data = NULL, ...) {
      stop("...not implemented...")
      return(invisible(self))
    },

    refit_best_model = function(...) {
      # browser()
      # MSE_tab <- data.table::rbindlist(lapply(self$PredictionModels, '[[', "getMSEtab"))
      # setkeyv(MSE_tab, cols = "MSE")

      ## 1. Out of all ensembles in self$PredictionModels, first find the idx that actually contains the best model
      min_by_predmodel <- lapply(lapply(self$getMSE, unlist), min)
      best_model_idx <- which.min(unlist(min_by_predmodel))
      self$best_model_idx <- best_model_idx
      ## 2. Refit the best model for that PredictionModel object only
      model.fit <- self$PredictionModels[[best_model_idx]]$refit_best_model(...) # data, subset_exprs,
      ## 3. Clean up all PredictionModel obj in this ensemble:
      self$wipe.alldat

      return(invisible(model.fit))
    },


    # Predict the response E[Y|newdata];
    # , best_refit_only
    predict = function(best_only, ...) {
      ## obtain prediction from the best refitted model only
      if (best_only) {

        if (length(self$PredictionModels) == 1L) self$best_model_idx <- 1

        if (is.null(self$best_model_idx)) stop("The best refitted model doesn't appear to exist.")
        best_pred_model <- self$PredictionModels[[self$best_model_idx]]
        # newdata, subset_exprs, predict_model_names = NULL, , convertResToDT,
        preds <- best_pred_model$predict(..., best_refit_only = TRUE)
        return(preds)

      ## try to obtain predictions from all models, non-refitted (i.e., trained on non-holdout observations only)
      } else {

        preds <- lapply(self$PredictionModels, function(model_obj) {
          # newdata, subset_exprs, predict_model_names = NULL, best_refit_only = FALSE, convertResToDT,
          model_obj$predict(..., best_refit_only = FALSE)
        })

        return(preds)
      }
    },

    # Predict the response E[Y|newdata] for out of sample observations  (validation set / holdouts);
    predict_out_of_sample = function(best_only, ...) {
      # browser()
      MSE_tab <- self$getMSEtab

      ## obtain out-of-sample prediction from the best non-refitted model
      if (best_only) {

        best_model_idx <- MSE_tab[1, ][["model_idx"]]
        if (is.null(best_model_idx)) stop("Best model cannot be selected since the models were not scored yet")
        ## NEED TO KNOW WHAT WAS THE NAME OF THE BEST MODEL WITHIN THE SAME GRID / ENSEMBLE:
        best_pred_model <- self$PredictionModels[[best_model_idx]]
        predict_model_names <- best_pred_model$get_best_model_names(K = 1)
        preds <- best_pred_model$predict_out_of_sample(..., predict_model_names = predict_model_names)
        return(preds)

      ## try to obtain out-of-sample predictions from all models, non-refitted (i.e., trained on non-holdout observations only)
      } else {

        preds <- lapply(self$PredictionModels, function(model_obj) {
          model_obj$predict_out_of_sample(...)
        })
        return(preds)

      }
    },

    ## Predict the response E[Y|newdata] for within sample observations from models trained on NON-HOLDOUT OBS ONLY;
    ## This should be usefull for split-specific SL.
    ## For holdout SL this is fairly straightfoward, just call predict with best_refit_only set to FALSE.
    ## When running internal CV SL this requires manually accessing each CV model and calling predict on each.
    predict_within_sample = function(best_only, ...) {
      MSE_tab <- self$getMSEtab
      if (self$runCV) stop("...not implemented...")

      ## obtain prediction from the best non-refitted model only
      if (best_only) {

        if (length(self$PredictionModels) == 1L) self$best_model_idx <- 1

        if (is.null(self$best_model_idx)) stop("The best refitted model doesn't appear to exist.")
        best_pred_model <- self$PredictionModels[[self$best_model_idx]]
        # newdata, subset_exprs, predict_model_names = NULL, , convertResToDT,
        preds <- best_pred_model$predict(..., best_refit_only = FALSE)
        return(preds)

      ## try to obtain predictions from all models, non-refitted (i.e., trained on non-holdout observations only)
      } else {

        preds <- lapply(self$PredictionModels, function(model_obj) {
          # newdata, subset_exprs, predict_model_names = NULL, best_refit_only = FALSE, convertResToDT,
          model_obj$predict(..., best_refit_only = FALSE)
        })

        return(preds)
      }

    },

    # Score models (so far only MSE) based on either out of sample CV model preds or validation data preds;
    score_models = function(...) {
    # score_models = function(validation_data, subset_exprs, ...) {
      scored_m <- lapply(self$PredictionModels, function(PredictionModel) {
                          PredictionModel$score_models(...)
                        })
      return(invisible(self))
    },

    evalMSE = function(test_values) {
      stop("...not implemented...")
      return(invisible(self))
    },

    evalMSE_byID = function(test_values) {
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
    # return the parameters of the top K models as a list (ranked by prediction MSE on a holdout (CV) fold)
    # ------------------------------------------------------------------------------
    get_best_model_params = function(K = 1) {
      best_models <- unlist(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$get_best_model_params(K = K)))
      # best_models <- best_models[names(self$get_best_MSEs(K))]
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
      lapply(self$PredictionModels, function(PredictionModel) PredictionModel$wipe.alldat)
      return(self)
    },

    getMSE = function() { return(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$getMSE)) },

    getMSEtab = function() {
      MSE_tab <- data.table::rbindlist(lapply(self$PredictionModels, '[[', "getMSEtab"))
      setkeyv(MSE_tab, cols = "MSE")
      return(MSE_tab)
    },

    getRMSE = function() { return(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$getRMSE)) },

    OData_train = function() { return(self$PredictionModels[[1]]$OData_train) },
    OData_valid = function() { return(self$PredictionModels[[1]]$OData_valid) },

    get_out_of_sample_preds = function() {
      MSE_tab <- self$getMSEtab
      best_model_idx <- MSE_tab[1, ][["model_idx"]]
      return(self$PredictionModels[[best_model_idx]]$get_out_of_sample_preds)
    }
  )
)
