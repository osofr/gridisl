#' @useDynLib gridisl
#' @import R6
# @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom Rcpp sourceCpp
#' @importFrom graphics axis barplot hist par text  legend plot
#' @importFrom methods is
#' @importFrom stats approx binomial gaussian coef glm.control glm.fit plogis predict qlogis qnorm quantile rnorm terms var predict glm.control
#' @importFrom utils data head str
#' @importFrom stats as.formula glm na.exclude rbinom terms.formula
NULL

## -----------------------------------------------------------------------------
## Class Membership Tests
## -----------------------------------------------------------------------------
is.DataStorageClass <- function(DataStorageClass) "DataStorageClass"%in%class(DataStorageClass)
is.PredictionModel <- function(PredictionModel) "PredictionModel"%in%class(PredictionModel)
is.PredictionStack <- function(PredictionStack) "PredictionStack"%in%class(PredictionStack)
is.ModelStack <- function(obj) "ModelStack" %in% class(obj)
is.H2OFrame <- function(fr)  base::`&&`(!missing(fr), class(fr)[1]=="H2OFrame")

#-----------------------------------------------------------------------------
# Capture the arguments passed on as ... in a list
#-----------------------------------------------------------------------------
capture.exprs <- function(...) {
  # sVar.exprs <- eval(substitute(alist(...)))
  sVar.exprs <- list(...)
  if (is.null(names(sVar.exprs))) names(sVar.exprs) <- rep_len("", length(sVar.exprs))
  if (length(sVar.exprs)!=0 && any(names(sVar.exprs)%in%"")) {
    stop("all parameters passed to ... must be named")
  }
  return(sVar.exprs)
}

#' Save the best performing h2o model
#'
#' @param modelfit A model object of class \code{PredictionModel} returned by functions \code{fit_model} or \code{fit}.
#' @param file.path Specify the directory where the model object file should be saved.
#' @export
save_best_model <- function(modelfit, file.path = getOption('gridisl.file.path')) {
  stop("...not implemented...")
  assert_that(is.PredictionModel(modelfit))
  best_model_name <- modelfit$get_best_model_names(K = 1)
  message("saving the best model fit: " %+% best_model_name)
  ## Will obtain the best model object trained on TRAINING data only
  ## If CV SL was used this model is equivalent to the best model trained on all data
  ## However, for holdout SL this model will be trained only on non-holdout observations
  best_model_traindat <- modelfit$get_best_models(K = 1)[[1]]
  h2o::h2o.saveModel(best_model_traindat, file.path, force = TRUE)
  ## This model is always trained on all data (if exists)
  best_model_alldat <- modelfit$BestModelFitObject$model.fit$modelfits_all
  if (!is.null(best_model_alldat))
    h2o::h2o.saveModel(best_model_alldat[[1]], file.path, force = TRUE)

  return(invisible(NULL))
}
# ---------------------------------------------------------------------------------------
#' Convert a column of validation folds into origami format
#'
#' From a column of validation folds (length n) into an origami-based format for fold storage
#' for V-fold cross-validation
#' @param fold_column A vector with validation fold indicators
#' @return An object (list) or validation / training folds of length V, for V-fold cross-validation
#' @export
make_kfold_from_column <- function(fold_column) {
  if (missing(fold_column)) stop("must provide a fold column -- a vector with validation fold indicators")
# make_kfold_from_column <- function(data, id = ".id", fold_column = "fold") {
  n <- length(fold_column)
  # n <- nrow(data)
  folds <- fold_column
  # folds <- data[[fold_column]]
  k <- length(unique(folds))

  idx <- seq_len(n)
  fold_idx <- split(idx, folds)

  fold <- function(v, test) {
      origami::make_fold(v, setdiff(idx, test), test)
  }
  purrr::map2((1:k), fold_idx, fold)
}


# ---------------------------------------------------------------------------------------
#' Define a column of fold indicators for V-fold cross-validation
#'
#' The input data is assumed to have repeated observations per subjects, the folds are defined as clustered by subject IDs.
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param nfolds Number of unique folds (same fold is always assigned to all observations that share the same ID).
#' @param fold_column A name of the column that will contain the fold indicators
#' @param seed Random number seed for selecting a random fold.
#' @return An input data with added fold indicator column (as ordered factor with levels 1:nfolds).
#' @export
add_CVfolds_ind = function(data, ID, nfolds = 5, fold_column = "fold", seed = NULL) {
  nuniqueIDs = function() { length(unique(data[[ID]])) }
  data <- data.table::data.table(data)
  data.table::setkeyv(data, cols = ID)
  if (fold_column %in% names(data)) data[, (fold_column) := NULL]
  nuniqueIDs <- nuniqueIDs()
  if (!is.null(seed)) set.seed(as.numeric(seed))  #If seed is specified, set seed prior to next step

  # format fold IDs as characters with leading 0. That way the ordering of the factor levels remains consistent betweeen R and h2oFRAME
  fold_IDs <- sprintf("%02d", seq(nfolds))
  fold_id <- as.factor(sample(rep(fold_IDs, ceiling(nuniqueIDs/nfolds)))[1:nuniqueIDs])  # Cross-validation folds


  foldsDT <- data.table::data.table("ID" = unique(data[[ID]]), fold_column = fold_id)
  data.table::setnames(foldsDT, old = names(foldsDT), new = c(ID, fold_column))
  data.table::setkeyv(foldsDT, cols = ID)
  data <- merge(data, foldsDT, by = ID, all.x = TRUE)
  if (!is.null(seed)) set.seed(NULL)  #If seed was specified, reset it to NULL
  return(data)
}

# ---------------------------------------------------------------------------------------
#' Define and fit growth models evaluated on holdout observations.
#'
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param hold_column A name of the column that will contain the the holdout indicators
#' @param random Logical, specifying if the holdout observations should be selected at random.
#' If FALSE then the last observation for each subject is selected as a holdout.
#' @param seed Random number seed for selecting a random holdout.
#' @return An input data with added holdout indicator column (TRUE for holdout observation indicator, FALSE for training observation indicator).
#' @export
add_holdout_ind = function(data, ID, hold_column = "hold", random = TRUE, seed = NULL) {
  data <- data.table::data.table(data)
  data.table::setkeyv(data, cols = ID)
  if (!is.null(seed)) set.seed(as.numeric(seed))
  if (hold_column %in% names(data)) data[, (hold_column) := NULL]

  samplemax2 <- function(x) {
    if (x == 1L) {
      return(FALSE)
    } else {
      res <- rep.int(FALSE, x)
      res[ length(res) ] <- TRUE
      return(res)
    }
  }

  samplerandom2 <- function(x) {
    if (x == 1L) {
      return(FALSE)
    } else {
      res <- rep.int(FALSE, x)
      res[ sample((1:x), 1) ] <- TRUE
      return(res)
    }
  }

  if (random) {
    data[, (hold_column) := samplerandom2(.N), by = eval(ID)]
  } else {
    data[, (hold_column) := samplemax2(.N), by = eval(ID)]
  }
  # self$hold_column <- hold_column

  return(data)
}

# ---------------------------------------------------------------------------------------
#' Import data, define nodes (columns) and define input data R6 object
#'
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param covars Names of predictors (covariates) in the data.
#' @param OUTCOME Character name of the column containing outcomes.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(gridisl.verbose=TRUE)}.
#' @return An R6 object that contains the input data. This can be passed as an argument to \code{get_fit} function.
# @example tests/examples/1_gridisl_example.R
#' @export
importData <- function(data, ID = "Subject_ID", t_name = "time_period", covars, OUTCOME = "Y", verbose = getOption("gridisl.verbose")) {
  gvars$verbose <- verbose

  ## define time-varing covars (L) as everything else in data besides these vars
  if (missing(covars)) {
    covars <- setdiff(colnames(data), c(ID, OUTCOME))
  }

  nodes <- list(Lnodes = covars, Ynode = OUTCOME, IDnode = ID, tnode = t_name)
  OData <- DataStorageClass$new(Odata = data, nodes = nodes)

  for (Lnode in nodes$Lnodes) CheckVarNameExists(OData$dat.sVar, Lnode)
  return(OData)
}

## -----------------------------------------------------------------------------
## General utilities / Global Vars
## -----------------------------------------------------------------------------
`%+%` <- function(a, b) paste0(a, b)

# Return the left hand side variable of formula f as a character
LhsVars <- function(f) {
  f <- as.formula(f)
  return(as.character(f[[2]]))
}
# Return the right hand side variables of formula f as a character vector
RhsVars <- function(f) {
  f <- as.formula(f)
  return(all.vars(f[[3]]))
}

checkpkgs <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(pkg %+% " package needed for this function to work. Please install it.", call. = FALSE)
    }
  }
}

#if warning is in ignoreWarningList, ignore it; otherwise post it as usual
SuppressGivenWarnings <- function(expr, warningsToIgnore) {
  h <- function (w) {
    if (w$message %in% warningsToIgnore) invokeRestart( "muffleWarning" )
  }
  withCallingHandlers(expr, warning = h )
}

GetWarningsToSuppress <- function(update.step=FALSE) {
  warnings.to.suppress <- c("glm.fit: fitted probabilities numerically 0 or 1 occurred",
                            "prediction from a rank-deficient fit may be misleading",
                            "non-integer #successes in a binomial glm!",
                            "the matrix is either rank-deficient or indefinite")
  if (update.step) {
    warnings.to.suppress <- c(warnings.to.suppress, "glm.fit: algorithm did not converge")
  }
  return(warnings.to.suppress)
}

# returns NULL if no factors exist, otherwise return the name of the factor variable(s)
CheckExistFactors <- function(data) {
  testvec <- unlist(lapply(data, is.factor))
  if (any(testvec)) {
    return(names(data)[which(testvec)])
  } else {
    return(NULL)
  }
}

# # returns NULL if no character columns found, otherwise return the names of character column(s)
# CheckExistChar <- function(data) {
#   testvec <- unlist(lapply(data, is.character))
#   if (any(testvec)) {
#     return(names(data)[which(testvec)])
#   } else {
#     return(NULL)
#   }
# }

# throw exception if 1) varname doesn't exist; 2) more than one varname is matched
CheckVarNameExists <- function(data, varname) {
  idvar <- names(data) %in% varname
  if (sum(idvar) < 1) stop("variable name " %+% varname %+% " not found in data input")
  if (sum(idvar) > 1) stop("more than one column in the input data has been matched to name "
                            %+% varname %+% ". Consider renaming some of the columns: " %+%
                            paste0(names(data)[idvar], collapse=","))
  return(invisible(NULL))
}


if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("model",
                           "MSE",
                           "CIlow",
                           "CIhi",
                           "algorithm",
                           "tooltip",
                           "model.id",
                           "onclick"))
}

# ---------------------------------------------------------------------------------------
#' Plot the top (smallest) validation MSEs for an ensemble of prediction models
#'
#' @param PredictionModel Must be an R6 object of class \code{PredictionModel} (returned by \code{get_fit} function)
#' or an object of class \code{PredictionStack} (returned by \code{make_PredictionStack} function).
#' Must also contain validation /test set predictions and corresponding MSEs.
#' @param K How many top (smallest) MSEs should be plotted? Default is 5.
#' @param interactive Setting this to \code{TRUE} will produce an interactive plot in html format using the package \code{ggiraph}.
#' @export
plotMSEs <- function(PredictionModel, K = 1, interactive = FALSE) {
  # require("ggplot2")
  # require("ggiraph")
  assert_that(is.PredictionModel(PredictionModel) || is.PredictionStack(PredictionModel))
  assert_that(is.integerish(K))

  datMSE <- PredictionModel$get_best_MSE_table(K = K)
  # datMSE$model <- factor(datMSE$model, levels = datMSE$model[order(datMSE$MSE)]) # order when not flipping coords
  datMSE[["model"]] <- factor(datMSE[["model"]], levels = datMSE[["model"]][order(datMSE[["MSE"]], decreasing = TRUE)]) # order when flipping coords

  # datMSE$tooltip <- "MSE.CV = " %+% round(datMSE$MSE.CV, 2) %+% "; 95% CI: [" %+% round(datMSE$CIlow,2) %+% "-" %+% round(datMSE$CIhi,2)  %+%"]"
  # datMSE$tooltip <- "MSE.CV = " %+% format(datMSE$MSE.CV, digits = 3, nsmall=2) %+% "; 95% CI: [" %+% format(datMSE$CIlow, digits = 3, nsmall=2) %+% "-" %+% format(datMSE$CIhi, digits = 3, nsmall=2)  %+% "]"
  datMSE$tooltip <- "MSE = " %+% format(datMSE[["MSE"]], digits = 3, nsmall=2) %+% " [" %+% format(datMSE$CIlow, digits = 3, nsmall=2) %+% "-" %+% format(datMSE$CIhi, digits = 3, nsmall=2)  %+% "]"

  datMSE$onclick <- "window.location.hash = \"#jump" %+% 1:nrow(datMSE) %+% "\""
  # open a new browser window:
  # datMSE$onclick <- sprintf("window.open(\"%s%s\")", "http://en.wikipedia.org/wiki/", "Florida")  # pop-up box:
  # datMSE$onclick = paste0("alert(\"",datMSE$model.id, "\")")

  p <- ggplot2::ggplot(datMSE, aes(x = model, y = MSE, ymin=CIlow, ymax=CIhi)) # will use model name (algorithm)
  if (interactive) {
    p <- p + ggiraph::geom_point_interactive(aes(color = algorithm, tooltip = tooltip, data_id = model.id, onclick = onclick), size = 2, position = ggplot2::position_dodge(0.01)) # alpha = 0.8
    # p <- p + geom_point_interactive(aes(color = algorithm, tooltip = model.id, data_id = model.id, onclick = onclick), size = 2, position = position_dodge(0.01)) # alpha = 0.8
  } else {
    p <- p + ggplot2::geom_point(aes(color = algorithm), size = 2, position = ggplot2::position_dodge(0.01)) # alpha = 0.8
  }
  p <- p + ggplot2::geom_errorbar(aes(color = algorithm), width = 0.2, position = ggplot2::position_dodge(0.01))
  p <- p + ggplot2::theme_bw() + ggplot2::coord_flip()

  if (interactive){
    ggiraph::ggiraph(code = print(p), width = .6,
                     tooltip_extra_css = "padding:2px;background:rgba(70,70,70,0.1);color:black;border-radius:2px 2px 2px 2px;",
                     hover_css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;"
            )
    # to active zoom on a plot:
    # zoom_max = 2
  } else {
    print(p)
  }
  # return(invisible(NULL))
  # ggiraph(code = {print(p)})
  # , tooltip_offx = 20, tooltip_offy = -10
  # p <- p + facet_grid(N ~ ., labeller = label_both) + xlab('Scenario')
  # # p <- p + facet_grid(. ~ N, labeller = label_both) + xlab('Scenario')
  # p <- p + ylab('Mean estimate \\& 95\\% CI length')
  # p <- p + theme(axis.title.y = element_blank(),
  #                axis.title.x = element_text(size = 8),
  #                plot.margin = unit(c(1, 0, 1, 1), "lines"),
  #                legend.position="top")
}