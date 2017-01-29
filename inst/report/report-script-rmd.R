#' ---
#' title: "`r title`"
#' author: "`r author`"
#' date: "`r Sys.Date()`"
#' ---

#+ setup, include=FALSE
  require("knitr")
  require("pander")
  opts_chunk$set(fig.path = figure.dir, comment = NA)
  # options(width = 200)
  panderOptions("table.split.table", Inf)

#'
#' Total number of unique independent units in the input data:
{{prettyNum(nuniqueIDs, big.mark = ",", scientific = FALSE)}}
#'
#' Total number of person-time observations in the input data:
{{prettyNum(nobs, big.mark = ",", scientific = FALSE)}}
#'
#' Total number of unique time-points in the input data:
{{prettyNum(nuniquets, big.mark = ",", scientific = FALSE)}}

#'
#' ## Model Performance Based on MSE for Holdout / Validation Data
#'

#+ warning=FALSE, message=FALSE
  plotMSEs(modelfit, K = K, interactive = TRUE)

#' &nbsp;
#'
#' &nbsp;
#'

#+ warning=FALSE, message=FALSE, results='asis'
  tabMSEonly <- modelfit$get_best_MSEs(K = K)
  pander::set.caption("Top MSEs (for Holdout / Validation Data).")
  pander::pander(data.frame(model = names(tabMSEonly), MSEs = tabMSEonly, row.names = NULL))


#' &nbsp;
#'
#' &nbsp;
#'

#+ warning=FALSE, message=FALSE, results='asis'
  tab <- modelfit$get_best_MSE_table(K = K)
  pander::set.caption("Best Performing Models (Based on MSE for Holdout / Validation Data).")
  pander::pander(tab)

#' &nbsp;
#'
#' &nbsp;
#'

#'
#' ## Summary of Model Grids
#'

#+ warning=FALSE, message=FALSE
  grids <- modelfit$get_modelfits_grid()
  for (grid in grids) {
    print(grid)
    cat("\n\n\n");
  }

#' &nbsp;
#'
#' &nbsp;
#'

#+ results='asis'
  panderOptions('knitr.auto.asis', FALSE)
  grids <- modelfit$get_modelfits_grid()
  for (grid in grids) {
    if (is.data.frame(grid) || is.data.table(grid))
      grid <- grid[ , names(grid)[!(names(grid) %in% c("glob_params", "xgb_fit", "fit", "params"))], with = FALSE]
    pander::pander(grid) # , caption = "Grid Details"
  }
  panderOptions('knitr.auto.asis', TRUE)

#' &nbsp;
#'
#' &nbsp;
#'

#'
#' ## Top Performing Models
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
        cat("*no modeling objects found*")
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
#' ## Model Stack Summaries
#'

#+ echo=FALSE
  if (!skip.modelfits) {
    print(modelfit, model_stats = TRUE, all_fits = print_all_fits)
    # print(modelfit, model_stats = TRUE)
    # models <- modelfit$getfit
    # single_models <- models$modelfits_all[[1]]
    # for (single_model in models$modelfits_all) {
    #   # print(models, only.coefs = only.coefs)
    #   print(single_model)
    #   # res <- utils::capture.output(single_models)
    #   # pander(print(paste(res, collapse = '\n')))
    # }
  }
# panderOptions('knitr.auto.asis', TRUE)