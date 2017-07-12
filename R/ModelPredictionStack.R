#' S3 methods for printing model fit summary for PredictionModel R6 class object
#'
#' Prints the modeling summaries
#' @param x The model fit object produced by functions \code{make_PredictionStack}.
#' @param ... Additional options passed on to \code{print.PredictionModel}.
#' @export
print.PredictionStack <- function(x, ...) {
  x$show(...)
  return(invisible(NULL))
}

#' Combine models into ensemble
#'
#' Combine several fitted models into a single ensemble model of class 'PredictionStack'.
#' @param ... Different objects of class "PredictionModel" separated by a comma.
#' @export
make_PredictionStack <- function(...) {
  PredictionModels <- list(...)
  if (!all(unlist(lapply(PredictionModels, is.PredictionModel))) && !all(unlist(lapply(PredictionModels, is.PredictionStack)))) {
    stop("All arguments must be of class 'PredictionModel' or 'PredictionStack'")
  }
  class(PredictionModels) <- c(class(PredictionModels), "PredictionStack")
  return(PredictionStack$new(PredictionModels))
}

## *******************************************************************************************
## Needs to be renamed to ReportStack -- because is the only actual purpose of this class
## *******************************************************************************************
## @export
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
    OData_train = NULL, # object of class DataStorageClass used for training
    OData_valid = NULL, # object of class DataStorageClass used for scoring models (contains validation data)
    SL_method = NULL,
    SL_coefs = NULL,

    initialize = function(PredictionModels) {
      if (!all(unlist(lapply(PredictionModels, is.PredictionModel))) && !all(unlist(lapply(PredictionModels, is.PredictionStack)))) {
       stop("All arguments must be of class 'PredictionModel' or 'PredictionStack'")
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

      ## 1. Out of all model objects in self$PredictionModels, first find the object idx that contains the best model
      # min_by_predmodel <- lapply(lapply(self$getMSE, unlist), min)
      # best_Model_idx <- which.min(unlist(min_by_predmodel))
      best_Model_idx <- self$best_Model_idx

      ## 2. Refit the best model for that PredictionModel object only
      model.fit <- self$PredictionModels[[best_Model_idx]]$refit_best_model(...) # data, subset_exprs,

      ## 3. Clean up all data in PredictionModel, OData pointers
      self$wipe.alldat$wipe.allOData

      ## Remove all modeling obj stored in daughter classes (don't need these if only going to do the best re-trained model predictions)
      ## self$wipe.allmodels

      return(invisible(model.fit))
    },


    # Predict the response E[Y|newdata];
    # , best_refit_only
    predict = function(best_only, ...) {
      ## obtain prediction from the best refitted model only
      if (best_only) {
        best_Model_idx <- self$best_Model_idx
        best_pred_model <- self$PredictionModels[[best_Model_idx]]

        # newdata, subset_exprs, predict_model_names = NULL, , convertResToDT,
        if (gvars$verbose) { print("obtaining predictions for the best model..."); print(best_pred_model) }

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
      ## obtain out-of-sample prediction from the best non-refitted model
      if (best_only) {

        best_Model_idx <- self$best_Model_idx
        ## NEED TO KNOW WHAT WAS THE NAME OF THE BEST MODEL WITHIN THE SAME GRID / ENSEMBLE:
        best_pred_model <- self$PredictionModels[[best_Model_idx]]
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

        best_Model_idx <- self$best_Model_idx
        best_pred_model <- self$PredictionModels[[best_Model_idx]]
        predict_model_names <- best_pred_model$get_best_model_names(K = 1)
        preds <- best_pred_model$predict(..., predict_model_names = predict_model_names, best_refit_only = FALSE)

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
      scored_m <- lapply(self$PredictionModels, function(PredictionModel)
                          PredictionModel$score_models(...,
                                                       OData_train = self$OData_train,
                                                       OData_valid = self$OData_valid))
      return(invisible(self))
    },

    # ------------------------------------------------------------------------------
    # return top K models based on smallest validation / test MSE for each PredictionModel in a stack
    # ------------------------------------------------------------------------------
    get_best_MSEs = function(K = 1) {
      return(sort(unlist(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$get_best_MSEs(K = K)))))
    },

    # ------------------------------------------------------------------------------
    # return top overall model across *ALL* models in self$PredictionModels
    # ------------------------------------------------------------------------------
    get_overall_best_model = function() { return(self$PredictionModels[[self$best_Model_idx]]$get_best_models(K = 1)) },

    # ------------------------------------------------------------------------------
    # return top K model fits from **FOR EVERY MODEL** in list self$PredictionModels
    # ------------------------------------------------------------------------------
    get_best_models = function(K = 1) {
      best_models <- NULL
      for (idx in seq_along(self$PredictionModels))
        best_models <- c(best_models, self$PredictionModels[[idx]]$get_best_models(K = K))
      # best_models <- unlist(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$get_best_models(K = K)))
      # best_models <- best_models[names(self$get_best_MSEs(K))]
      return(best_models)
    },

    reassignMSEs = function(sqresid_preds) {
      lapply(self$PredictionModels, function(PredictionModel) PredictionModel$reassignMSEs(sqresid_preds))
      return(invisible(NULL))
    },

    # ------------------------------------------------------------------------------
    # return the parameters of the top K models **FOR EVERY MODEL** in a list self$PredictionModels
    # ------------------------------------------------------------------------------
    get_best_model_params = function(K = 1) {
      best_model_params <- NULL
      for (idx in seq_along(self$PredictionModels))
        best_model_params <- c(best_model_params, self$PredictionModels[[idx]]$get_best_model_params(K = K))
      # best_models <- unlist(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$get_best_model_params(K = K)))
      # best_models <- best_models[names(self$get_best_MSEs(K))]
      return(best_model_params)
    },

    # ------------------------------------------------------------------------------
    # return a data.frame with best mean MSEs, including SDs & corresponding model names
    # ------------------------------------------------------------------------------
    get_best_MSE_table = function(K = 1) {
      res_tab_list <- lapply(self$PredictionModels, function(PredictionModel) PredictionModel$get_best_MSE_table(K = K))
      # res_tab <- do.call("rbind", res_tab_list)
      # res_tab <- res_tab[order(res_tab[["MSE"]], decreasing = FALSE), ]
      res_tab <- data.table::rbindlist(res_tab_list)
      data.table::setkeyv(res_tab, cols = "MSE")
      return(res_tab)
    },

    get_modelfits_grid = function() {
      res_DT_list <- lapply(self$PredictionModels, function(PredictionModel) {
        if (is.PredictionStack(PredictionModel)) PredictionModel <- PredictionModel$PredictionModels[[1]]
        PredictionModel$get_modelfits_grid()
      })
      return(res_DT_list)
    },

    # Output info on the general type of regression being fitted:
    show = function(print_format = TRUE, model_stats = FALSE, all_fits = FALSE) {

      out_res <- lapply(self$PredictionModels, function(PredictionModel) PredictionModel$show(print_format = TRUE, model_stats = FALSE, all_fits = FALSE))

      cat("\n", fill = getOption("width"))

      if (!is.null(self$SL_coefs)) {
        print(cbind(Risk = self$SL_coefs$cvRisk, Coef = self$SL_coefs$coef))
      }
      return(invisible(NULL))
    },

    summary = function(all_fits = FALSE) {
      return(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$summary(all_fits = FALSE)))
    },

    evalMSE = function(test_values) { stop("...not implemented...") },
    evalMSE_byID = function(test_values) { stop("...not implemented...") },
    getmodel_byname = function(model_names, model_IDs) { stop("...not implemented...") },
    define.subset.idx = function(data) { stop("not applicable to this class") }

  ),

  active = list(
    ## wipe out all data stored by daughter model classes
    wipe.alldat = function() {
      lapply(self$PredictionModels, function(PredictionModel) PredictionModel$wipe.alldat)
      return(self)
    },

    wipe.allOData = function() {
      lapply(self$PredictionModels, function(PredictionModel) PredictionModel$wipe.allOData)
      return(self)
    },

    ## wipe out all the model objects stored by daughter model classes
    wipe.allmodels = function() {
      lapply(self$PredictionModels, function(PredictionModel) PredictionModel$wipe.allmodels)
      return(self)
    },

    getMSE = function() { return(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$getMSE)) },
    getMSE_bysubj = function() {
      best_Model_idx <- self$best_Model_idx
      return(self$PredictionModels[[best_Model_idx]]$getMSE_bysubj)
    },
    getRMSE = function() { return(lapply(self$PredictionModels, function(PredictionModel) PredictionModel$getRMSE)) },

    best_Model_idx = function() {
      if (length(self$PredictionModels) == 1L) return(1L)

      MSE_tab <- self$getMSEtab
      metric_name <- "MSE"

      top_model_info <- MSE_tab[which.min(MSE_tab[[metric_name]]), ]
      best_Model_idx <- top_model_info[["Model_idx"]]

      return(best_Model_idx)
    },

    getMSEtab = function() {
      MSE_tab <- data.table::rbindlist(lapply(self$PredictionModels, '[[', "getMSEtab"))
      data.table::setkeyv(MSE_tab, cols = "MSE")
      return(MSE_tab)
    },

    # OData_train = function() { return(self$PredictionModels[[1]]$OData_train) },
    # OData_valid = function() { return(self$PredictionModels[[1]]$OData_valid) },

    get_out_of_sample_preds = function() {
      best_Model_idx <- self$best_Model_idx
      return(self$PredictionModels[[best_Model_idx]]$get_out_of_sample_preds)
    }
  )
)
