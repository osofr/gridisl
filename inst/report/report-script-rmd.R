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
# ggiraph::ggiraph(code = {print(plotMSEs(modelfit, K = K))})

#+ echo=FALSE, results='asis'
# panderOptions('knitr.auto.asis', FALSE)
# if (!missing(MSM.RDtables)) {
# for (RDtable in MSM.RDtables) {
pander::set.caption("Best Performing Models (Based on Holdout / Validation MSE).")
pander::pander(modelfit$get_best_MSE_table(K = K))
# }
# }
# panderOptions('knitr.auto.asis', TRUE)


#'
#' ## Detailed Modeling Parameters
#'

#+ echo=FALSE
# if (!skip.modelfits) {
#   print(modelfit, model_stats = TRUE, all_fits = print_all_fits)
#   # print(modelfit, model_stats = TRUE)
#   # models <- modelfit$getfit
#   # single_models <- models$fitted_models_all[[1]]
#   # for (single_model in models$fitted_models_all) {
#   #   # print(models, only.coefs = only.coefs)
#   #   print(single_model)
#   #   # res <- capture.output(single_models)
#   #   # pander(print(paste(res, collapse = '\n')))
#   # }
# }
# panderOptions('knitr.auto.asis', TRUE)


