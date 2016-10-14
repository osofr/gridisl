#----------------------------------------------------------------------------------
# S3 classes for printing model summaries
#----------------------------------------------------------------------------------
#' @importFrom pander pander
NULL

#' Pander method for H2OBinomialMetrics class
#'
#' Prints a H2OBinomialMetrics object in Pandoc's markdown.
#' @param H2OBinomialMetricsObject H2OBinomialMetrics object
#' @return By default this function outputs (see: \code{?cat}) the result.
#' If you would want to catch the result instead, then call \code{pander_return} instead.
#' @export
pander.H2OBinomialMetrics <- function(H2OBinomialMetricsObject, type) {
  h2o_metrics <- H2OBinomialMetricsObject@metrics
  # modelID <- h2o_metrics$model$name
  categor <- h2o_metrics$model_category
  metricsDF <- t(data.frame(h2o_metrics[names(h2o_metrics) %in% c("nobs", "MSE", "RMSE", "r2", "mean_residual_deviance", "mae", "rmsle", "logloss", "AUC", "Gini")]))
  metricsDF <- data.frame(metric = rownames(metricsDF), value = as.numeric(metricsDF[,1]), stringsAsFactors = FALSE)

  if (H2OBinomialMetricsObject@algorithm == "glm") {
    glmDF <- data.frame(
      metric =
        c("Null Deviance:      ",
          "Residual Deviance:  ",
          "AIC:                "),
      value = c(h2o_metrics$null_deviance,
            h2o_metrics$residual_deviance,
            h2o_metrics$AIC),
      stringsAsFactors = FALSE)
    metricsDF <- rbind(metricsDF, glmDF)
  }
  colnames(metricsDF) <- NULL
  pander::pander(metricsDF, justify = c('left', 'center'), caption = type %+% " data metrics" %+% "; Category: " %+% categor)

  cm <- try(h2o::h2o.confusionMatrix(H2OBinomialMetricsObject), silent = TRUE)
  if( !is.null(cm) ) {
    pander::pander(cm, caption = "Confusion Matrix for F1-optimal threshold" %+% " (Model ID: " %+% modelID %+%")" )
  }
  max_matrics <- h2o_metrics$max_criteria_and_metric_scores
  caption <- attributes(max_matrics)$header %+% ": " %+% attributes(max_matrics)$description
  pander::pander(max_matrics, caption = caption %+% " (Model ID: " %+% modelID %+%")")
  # attributes(max_matrics)$formats
  # cat("\nGains/Lift Table: Extract with `h2o.gainsLift(<model>, <data>)` or `h2o.gainsLift(<model>, valid=<T/F>, xval=<T/F>)`")
  if (!is.null(h2o_metrics$gains_lift_table)) {
   # print(h2o_metrics$gains_lift_table)
   gain_tab <- h2o_metrics$gains_lift_table
   pander::pander(gain_tab, caption = attributes(gain_tab)$header %+% " (Model ID: " %+% modelID %+%")")
  }
  return(invisible(H2OBinomialMetricsObject))
}

#' Pander method for H2ORegressionMetrics class
#'
#' Prints a H2ORegressionMetrics object in Pandoc's markdown.
#' @param H2ORegressionMetricsObject H2ORegressionMetrics object
#' @return By default this function outputs (see: \code{?cat}) the result.
#' If you would want to catch the result instead, then call \code{pander_return} instead.
#' @export
pander.H2ORegressionMetrics <- function(H2ORegressionMetricsObject, type) {
  pander.H2OBinomialMetrics(H2ORegressionMetricsObject, type)
}

#' S3 methods for printing model fit summary for glmfit class object
#'
#' Prints the modeling summary for the glm fit (\code{stats::glm.fit} or \code{speedglm::speedglm.wfit})
#' @param model.fit The model fit object produced by functions stremr:::fit.glm or stremr:::fit.speedglm
#' @param ... Additional options passed on to \code{summary.GLMmodel}.
#' @return The output is printed with \code{cat}. To capture the markdown-formated model summary use \code{summary.GLMmodel}.
#' @export
print.GLMmodel <- function(model.fit, ...) {
  model.summary <- summary(model.fit, ...)
  cat(paste(model.summary, collapse = '\n'))
}

#' S3 methods for getting model fit summary for glmfit class object
#'
#' Prints the modeling summary for the GLM fit (\code{stats::glm.fit} or \code{speedglm::speedglm.wfit})
#' @param model.fit The model fit object produced by functions stremr:::glmfit.glm or stremr:::glmfit.speedglm
#' @param format_table Format the coefficients into a data.frame table?
#' @param ... Additional options (not used)
#' @return The markdown-formated model summary returned by \code{pander::pander_return}.
#' @export
summary.GLMmodel <- function(model.fit, format_table = TRUE, ...) {
  makeModelCaption <- function(model.fit) {
    return(
      "Model: " %+% model.fit$params$outvar %+% " ~ " %+% paste0(model.fit$params$predvars, collapse = " + ") %+% "; \\
       Stratify: " %+% model.fit$params$stratify %+% "; \\
       N: " %+% prettyNum(model.fit$nobs, big.mark = ",", scientific = FALSE) %+% "; \\
       Fit function: " %+% model.fit$fitfunname
    )
  }
  nobs <- model.fit$nobs
  coef_out <- model.fit$coef
  if (format_table) {
    if (is.null(coef_out)) {
      coef_out <- "---"; names(coef_out) <- coef_out
    }
    coef_out <- data.frame(Terms = names(coef_out), Coefficients = as.vector(coef_out))
    # coef_out <- data.frame(Terms = model.fit$params$predvars, Coefficients = as.vector(coef_out))
    rownames(coef_out) <- NULL
  }
  pander::set.caption(makeModelCaption(model.fit))
  out <- pander::pander_return(coef_out, justify = c('right', 'left'))
  out
}

#' S3 methods for getting model fit summary for H2ORegressionModel class object
#'
#' Prints the modeling summary for the h2o model fit (see \code{h2o} R package).
#' @param h2o.model The model fit object produced by h2o (and extracted with \code{getmodel_byname}).
#' @param only.coefs Skip all of the h2o-specific model stats, only print the coefficients table (when running \code{fit.algorithm = "glm"}).
# format_table Format the coefficients into a data.frame table (when running \code{fit.algorithm = "glm"})?
#' @param ... Additional options (not used)
#' @return The markdown-formated model summary returned by \code{pander::pander_return}.
#' @export
summary.H2ORegressionModel <- function(h2o.model, only.coefs = FALSE, ...) {
  # h2o.model <- model.fit$H2O.model.object
  modelID <- h2o.model@model$training_metrics@metrics$model$name
  out <- NULL

  # -----------------------------------------------------------------
  # some basic model info:
  # -----------------------------------------------------------------
  # coef_summary_out <- summary.GLMmodel(model.fit, format_table)
  if (!is.null(h2o.model@model$coefficients_table)) {
    coef_summary_out <- pander_return(h2o.model@model$coefficients_table, caption = attributes(h2o.model@model$coefficients_table)$description)
    out <- c(out, coef_summary_out)
  }

  if (!only.coefs) {
    # -----------------------------------------------------------------
    # model summary:
    # -----------------------------------------------------------------
    model_summary <- h2o.model@model$model_summary
    caption_summary <- attributes(model_summary)$header %+% " (Model ID: " %+% modelID %+%")"
    model_summary_out <- pander::pander_return(model_summary, caption = caption_summary)
    out <- c(out, model_summary_out)

    # -----------------------------------------------------------------
    # training data metrics:
    # -----------------------------------------------------------------
    train_model_metrics_out <- pander::pander_return(h2o.model@model$training_metrics, type = "Training")
    out <- c(out, train_model_metrics_out)

    # -----------------------------------------------------------------
    # validation data metrics:
    # -----------------------------------------------------------------
    H2OBinomialMetrics_val <- h2o.model@model$cross_validation_metrics
      if (!is.null(H2OBinomialMetrics_val@metrics)) {
      valid_model_metrics_out <- pander::pander_return(H2OBinomialMetrics_val, type = "Validation")
      out <- c(out, valid_model_metrics_out)
    }

    # -----------------------------------------------------------------
    # cross validation data metrics:
    # -----------------------------------------------------------------
    H2OBinomialMetrics_xval <- h2o.model@model$cross_validation_metrics
    if (!is.null(H2OBinomialMetrics_xval@metrics)) {
      xval_model_metrics_out <- pander::pander_return(H2OBinomialMetrics_xval, type = "Cross-validation")
      out <- c(out, xval_model_metrics_out)
    }

    # -----------------------------------------------------------------
    # variable importance:
    # -----------------------------------------------------------------
    # h2o.varimp(h2o.model)
    var_imp <- h2o.model@model$variable_importances
    var_imp_cap <- attributes(var_imp)$header %+% "Model ID: " %+% modelID %+%")"
    var_imp_out <- pander::pander_return(var_imp, caption = var_imp_cap)
    out <- c(out, var_imp_out)
  }
  return(out)
}

#' S3 methods for printing model fit summaries as pander tables
#'
#' Used internally to prints the modeling summary stats for reporting (see \code{\link{make_report_rmd}}).
#' @param model The model fit object produced by \code{get_fit} function.
#' @param only.coefs Set to \code{TRUE} to only print the table of coefficients (when using \code{fit.algorithm = "glm"}) and
#' omit all other model-related output statistics.
#' @param ... Additional options passed on to \code{summary(...)}.
#' @return The output is printed with \code{cat}. To capture the markdown-formated model summary use \code{summary(...)}.
#' @export
print_tables <- function(model, only.coefs = FALSE, ...) { UseMethod("print_tables") }


print_tables.face.sparse <- function(model, only.coefs = FALSE, ...) {
  cat("face OBJECTS DO NOT PROVIDE ANY MODEL SUMMARY OUTPUT")
}

#' S3 methods for printing model fit summaries for brokenstick class
print_tables.brokenstick <- function(model, only.coefs = FALSE, ...) {
  old.opt <- pander::panderOptions('knitr.auto.asis')
  pander::panderOptions('knitr.auto.asis', TRUE)
  print(model)
  pander::panderOptions('knitr.auto.asis', old.opt)
  return(invisible(NULL))
}

#' S3 methods for printing model fit summaries for H2OBinomialModel class
#'
#' Used internally to prints the modeling summary stats for reporting (see \code{\link{make_report_rmd}}).
#' @param model The model fit object produced by \code{get_fit} function.
#' @param only.coefs Set to \code{TRUE} to only print the table of coefficients (when using \code{fit.algorithm = "glm"}) and
#' omit all other model-related output statistics.
#' @param ... Additional options passed on to \code{summary(...)}.
#' @return The output is printed with \code{cat}. To capture the markdown-formated model summary use \code{summary(...)}.
#' @export
print_tables.H2OBinomialModel <- function(model, only.coefs = FALSE, ...) {
  cat(paste(summary(model, only.coefs, ...), collapse = '\n'))
}

#' S3 methods for printing model fit summaries for H2ORegressionModel class
#'
#' Used internally to prints the modeling summary stats for reporting (see \code{\link{make_report_rmd}}).
#' @param model The model fit object produced by \code{get_fit} function.
#' @param only.coefs Set to \code{TRUE} to only print the table of coefficients (when using \code{fit.algorithm = "glm"}) and
#' omit all other model-related output statistics.
#' @param ... Additional options passed on to \code{summary(...)}.
#' @return The output is printed with \code{cat}. To capture the markdown-formated model summary use \code{summary(...)}.
#' @export
print_tables.H2ORegressionModel <- function(model, only.coefs = FALSE, ...) {
  cat(paste(summary(model, only.coefs, ...), collapse = '\n'))
}



CVmetrics_H2Obasemodel <- function(basemodelfit) {
  # out <- NULL
  # model_params <- t(data.frame(basemodelfit@parameters))
  # pander::pander_return(model_params, caption = "Base model parameters")
  # str(basemodelfit@model$cross_validation_metrics)
  CV <- basemodelfit@model$cross_validation_metrics@metrics
  cap <- CV$description
  CV.metrics.tab <- data.frame(
              model = basemodelfit@model_id,
              CV$nobs,
              CV$MSE,
              CV$RMSE,
              CV$logloss,
              CV$r2,
              CV$AUC,
              CV$Gini,
              CV$mean_per_class_error,
              CV$model_category)
  # out <- c(out, pander::pander_return(CV.metrics.tab, caption = cap))
  # $residual_deviance
  # $null_deviance
  # $AIC
  # $null_degrees_of_freedom
  # $residual_degrees_of_freedom
  # CVsummarytab <- basemodelfit@model$cross_validation_metrics_summary
  # CVsummarytab <- CVsummarytab[, c("mean", "sd")]
  # caption <- attributes(basemodelfit@model$cross_validation_metrics_summary)$header %+% ": " %+% basemodelfit@model_id
  # out <- c(out, pander::pander_return(CVsummarytab, caption = caption))
  return(CV.metrics.tab)
}

CVsummary_H2Obasemodel <- function(basemodelfit) {
  out <- NULL
  # model_params <- t(data.frame(basemodelfit@parameters))
  # pander::pander_return(model_params, caption = "Base model parameters")
  # str(basemodelfit@model$cross_validation_metrics)
  # CV <- basemodelfit@model$cross_validation_metrics@metrics
  # cap <- CV$description
  # CV.metrics.tab <- t(data.frame(
  #             model = basemodelfit@model_id,
  #             CV$nobs,
  #             CV$MSE,
  #             CV$RMSE,
  #             CV$logloss,
  #             CV$r2,
  #             CV$AUC,
  #             CV$Gini,
  #             CV$mean_per_class_error,
  #             CV$model_category))
  # out <- c(out, pander::pander_return(CV.metrics.tab, caption = cap))
  # $residual_deviance
  # $null_deviance
  # $AIC
  # $null_degrees_of_freedom
  # $residual_degrees_of_freedom
  CVsummarytab <- basemodelfit@model$cross_validation_metrics_summary
  CVsummarytab <- CVsummarytab[, c("mean", "sd")]
  caption <- attributes(basemodelfit@model$cross_validation_metrics_summary)$header %+% ": " %+% basemodelfit@model_id
  out <- c(out, pander::pander_return(CVsummarytab, caption = caption))
  return(out)
}

#' S3 methods for getting model fit summary for H2Oensemblemodel class object
#'
#' Prints the modeling summary for the h2o model fit (see \code{h2o} R package).
#' @param h2o.ensemble The model fit object produced by h2oEnsemble package
#' @param only.coefs Skip all of the h2o-specific model stats, only print the coefficients table (when running \code{fit.algorithm = "glm"}).
#' @param format_table Format the coefficients into a data.frame table (when running \code{fit.algorithm = "glm"})?
#' @param ... Additional options (not used)
#' @return The markdown-formated model summary returned by \code{pander::pander_return}.
#' @export
summary.H2Oensemblemodel <- function(h2o.ensemble, only.coefs = FALSE, format_table = TRUE, ...) {
  # h2o.ensemble <- model.fit$H2O.model.object
  out <- NULL
  x <- h2o.ensemble$x
  y <- h2o.ensemble$y
  family <- h2o.ensemble$family
  learner <- h2o.ensemble$learner
  metalearner <- h2o.ensemble$metalearner
  Vfolds <- h2o.ensemble$cvControl$V
  seed <- h2o.ensemble$seed

  # modelID <- h2o.ensemble@model$training_metrics@metrics$model$name
  # str(h2o.ensemble)
  # h2o.ensemble$basefits
  # length(h2o.ensemble$basefits)
  # h2o.ensemble$basefits[[1]]
  # str(h2o.ensemble$basefits[[1]])

  # metafit <- h2o.ensemble$metafit
  # str(h2o.ensemble$metafit)
  # h2o.ensemble$metafit@model$coefficients
  # h2o.ensemble$metafit@model$model_summary
  # metafit <- list()
  # class(metafit) <- c(metafit, "H2Omodel")
  # metafit$H2O.model.object <- h2o.ensemble$metafit
  # metafit <- h2o.ensemble$metafit
  # print("SuperLearner fit:"); print(metafit)
  # str(h2o.ensemble)

  # -----------------------------------------------------------------
  # some basic model info:
  # -----------------------------------------------------------------
  CV.descr <- data.frame(rbind(metalearner = metalearner, Vfolds = Vfolds, seed = seed, N.learners = length(learner)))
  names(CV.descr) <- c("")
  CV.descr.tab <- pander::pander_return(CV.descr, caption = "SuperLearner settings")
  out <- c(out, CV.descr.tab)

  coef_summary_out <- summary.GLMmodel(model.fit, format_table)
  if (!is.null(h2o.ensemble@model$coefficients_table)) {
    coef_summary_out <- pander_return(h2o.ensemble@model$coefficients_table, caption = attributes(h2o.ensemble@model$coefficients_table)$description)
    out <- c(out, coef_summary_out)
  }

  out <- c(out, coef_summary_out)

  if (!only.coefs) {
    # -----------------------------------------------------------------
    # model summary:
    # -----------------------------------------------------------------
    # str(h2o.ensemble$metafit@model$model_summary)
    model_summary <- h2o.ensemble$metafit@model$model_summary
    caption_summary <- attributes(model_summary)$header %+% " Wrapper: " %+% metalearner
    model_summary_out <- pander::pander_return(model_summary, caption = caption_summary)
    out <- c(out, model_summary_out)

    # -----------------------------------------------------------------
    # CV metrics for each base learner
    # -----------------------------------------------------------------
    CVmetrics_tab <- NULL
    for (basemodel in h2o.ensemble$basefits) {
      CVmetrics_tab <- rbind(CVmetrics_tab, CVmetrics_H2Obasemodel(basemodel))
      # out <- c(out, CVsummary_H2Obasemodel(basemodel))
    }
    cap <- basemodel@model$cross_validation_metrics@metrics$description
    out <- c(out, pander::pander_return(CVmetrics_tab, caption = cap))

    # -----------------------------------------------------------------
    # training data metrics:
    # -----------------------------------------------------------------
    # H2OBinomialMetrics_training <- h2o.ensemble@model$training_metrics
    # train_model_metrics_out <- pander::pander_return(H2OBinomialMetrics_training)
    # out <- c(out, train_model_metrics_out)

    # -----------------------------------------------------------------
    # variable importance:
    # -----------------------------------------------------------------
    # var_imp <- h2o.ensemble@model$variable_importances
    # var_imp_cap <- attributes(var_imp)$header %+% "Model ID: " %+% modelID %+%")"
    # var_imp_out <- pander::pander_return(var_imp, caption = var_imp_cap)
    # out <- c(out, var_imp_out)
  }
  return(out)
}

#' S3 methods for printing model fit summary for H2Omodel class object
#'
#' Prints the modeling summary for the h2o model fit (see \code{h2o} R package).
#' @param model.fit The model fit object produced by any stremr S3 function starting with \code{stremr:::H2Omodel.}
#' @param only.coefs Skip all of the h2o-specific model stats, only print the coefficients table (when running \code{fit.algorithm = "GLM"}).
#' @param ... Additional options passed on to \code{summary.H2Omodel}.
#' @return The output is printed with \code{cat}. To capture the markdown-formated model summary use \code{summary.H2Omodel}.
#' @export
print.H2Oensemblemodel <- function(model.fit, only.coefs = FALSE, ...) {
  model.summary <- summary(model.fit, only.coefs, ...)
  cat(paste(model.summary, collapse = '\n'))
}
