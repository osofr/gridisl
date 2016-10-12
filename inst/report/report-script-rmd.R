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

#+ echo=FALSE, include=FALSE
f_plot_survest <- function(surv_list, t, t_int_sel, y_lab, x_lab, miny, x_legend, y_legend) {
  ptsize <- 0.7
  counter <- 0
  if (missing(y_lab)) y_lab <- ""
  if (missing(x_lab)) x_lab <- "Follow-up period since study entry"
  if (missing(t)) t <- seq_along(surv_list[[1]])
  if (missing(t_int_sel)) t_int_sel <- seq_along(t)
  if (missing(miny)) miny <- min(unlist(lapply(surv_list, function(x) min(x[t_int_sel], na.rm = TRUE))))
  if (missing(x_legend)) x_legend <- (max(t_int_sel, na.rm = TRUE) - min(t_int_sel, na.rm = TRUE)) * 2/3 + min(t_int_sel, na.rm = TRUE)
  if (missing(y_legend)) y_legend <- (1 - miny) * 4/5 + miny
  for(d.j in names(surv_list)){
    counter <- counter + 1
    plot(as.integer(t[t_int_sel]), surv_list[[d.j]][t_int_sel], col = counter, type = 'b', cex = ptsize, ylim = c(miny, 1), ylab = y_lab, xlab = x_lab)
    par(new=TRUE)
  }
  legend(x_legend, y_legend, legend = names(surv_list), col = c(1:length(names(surv_list))), cex = ptsize, pch = 1)
}

#'
#' Number of unique independent units in the input data:
{{prettyNum(OData$nuniqueIDs, big.mark = ",", scientific = FALSE)}}
#'
#' Number of person-time observations in the input data:
{{prettyNum(OData$nobs, big.mark = ",", scientific = FALSE)}}

#'
#' ## Detailed Model Specifications
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


