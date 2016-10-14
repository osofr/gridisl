#' ---
#' title: "`r title`"
#' author: "`r author`"
#' date: "`r Sys.Date()`"
#' ---

#+ setup, include=FALSE
require("knitr")
require("pander")
opts_chunk$set(fig.path = figure.dir, comment = NA)

panderOptions("table.split.table", Inf)

#'
#' Number of unique independent units in the input data:
{{prettyNum(OData$nuniqueIDs, big.mark = ",", scientific = FALSE)}}
#'
#' Number of person-time observations in the input data:
{{prettyNum(OData$nobs, big.mark = ",", scientific = FALSE)}}

#'
#' ## Model Performance Based on Holdout / Validation MSE
#'

#+ echo=FALSE, warning=FALSE, message=FALSE
plotMSEs(modelfit, K = K, interactive = TRUE)

#' &nbsp;
#'
#' &nbsp;
#'

#+ echo=FALSE, warning=FALSE, message=FALSE, results='asis'
pander::set.caption("Top MSEs (Holdout / Validation MSE).")
pander::pander(modelfit$get_best_MSEs(K = K))

#' &nbsp;
#'
#' &nbsp;
#'

#+ echo=FALSE, warning=FALSE, message=FALSE, results='asis'
tab <- modelfit$get_best_MSE_table(K = K)
tabMSE <- tab[, names(tab)[!names(tab) %in% "model.id"]]
pander::set.caption("Best Performing Models (Based on Holdout / Validation MSE).")
pander::pander(tabMSE)

#' &nbsp;
#'
#' &nbsp;
#'

#'
#' ## Top Performing Models
#'

#+ echo=FALSE, warning=FALSE, message=FALSE, results='asis'
tab <- modelfit$get_best_MSE_table(K = K)
tabIDs <- tab[, names(tab)[names(tab) %in% c("model.id", "model")]]
pander::set.caption("Model ID and name for top performing models.")
pander::pander(tabIDs)

#' &nbsp;
#'
#' &nbsp;
#'

#+ echo=FALSE, warning=FALSE, message=FALSE, results='asis'
panderOptions('knitr.auto.asis', FALSE)
if (!skip.modelfits) {
  models.object <- modelfit$get_best_models(K = K)
  for (model_idx in seq_along(models.object)) {
    cat("\n\n");
    cat("###")
    cat('<a name=',paste0("jump",model_idx),'>', "Summaries for Model " %+% names(models.object)[model_idx], '</a> ')
    cat("\n\n");
    if (!is.null(models.object[[model_idx]])) {
      print_tables(models.object[[model_idx]])
    } else {
      cat("*No modeling objects found.*")
    }
    cat("\n\n"); cat("&nbsp;")
  }
}
panderOptions('knitr.auto.asis', TRUE)

#' &nbsp;
#'
#' &nbsp;
#'

#'
#' ## Detailed Modeling Parameters
#'

#+ echo=FALSE
if (!skip.modelfits) {
  print(modelfit, model_stats = TRUE, all_fits = print_all_fits)
  # print(modelfit, model_stats = TRUE)
  # models <- modelfit$getfit
  # single_models <- models$fitted_models_all[[1]]
  # for (single_model in models$fitted_models_all) {
  #   # print(models, only.coefs = only.coefs)
  #   print(single_model)
  #   # res <- capture.output(single_models)
  #   # pander(print(paste(res, collapse = '\n')))
  # }
}
# panderOptions('knitr.auto.asis', TRUE)