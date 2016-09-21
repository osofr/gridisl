#' @importFrom assertthat assert_that
NULL

# ---------------------------------------------------------------------------------------
#' Import data, define nodes (columns), define dummies for factor columns and define input data R6 object
#'
#' @param data Input dataset, can be a data.frame or a data.table
#' @param ID ...
#' @param t_name ...
#' @param covars ...
#' @param MONITOR ...
#' @param OUTCOME ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
importData <- function(data, ID = "Subject_ID", t_name = "time_period", covars, OUTCOME = "Y", verbose = getOption("growthcurveSL.verbose")) {
  # MONITOR = "N",
  gvars$verbose <- verbose
  if (verbose) {
    current.options <- capture.output(str(gvars$opts))
    print("Using the following growthcurveSL options/settings: ")
    cat('\n')
    cat(paste0(current.options, collapse = '\n'), '\n')
  }
  if (missing(covars)) { # define time-varing covars (L) as everything else in data besides these vars
    # covars <- setdiff(colnames(data), c(ID, t_name, MONITOR, OUTCOME))
    covars <- setdiff(colnames(data), c(ID, t_name, OUTCOME))
  }
  # The ordering of variables in this list is the assumed temporal order!
  # nodes <- list(Lnodes = covars, Nnodes = MONITOR, Ynode = OUTCOME, IDnode = ID, tnode = t_name)
  nodes <- list(Lnodes = covars, Ynode = OUTCOME, IDnode = ID, tnode = t_name)
  OData <- DataStorageClass$new(Odata = data, nodes = nodes)

  # --------------------------------------------------------------------------------------------------------
  # Convert all character covars into factors?
  # --------------------------------------------------------------------------------------------------------
  # ....


  # --------------------------------------------------------------------------------------------------------
  # Create dummies for each factor
  # --------------------------------------------------------------------------------------------------------
  factor.Ls <- unlist(lapply(OData$dat.sVar, is.factor))
  factor.Ls <- names(factor.Ls)[factor.Ls]
  new.factor.names <- vector(mode="list", length=length(factor.Ls))
  names(new.factor.names) <- factor.Ls
  if (length(factor.Ls)>0 && verbose)
    message("...converting the following factor(s) to binary dummies (and droping the first factor levels): " %+% paste0(factor.Ls, collapse=","))
  for (factor.varnm in factor.Ls) {
    factor.levs <- levels(OData$dat.sVar[,factor.varnm, with=FALSE][[1]])
    factor.levs <- factor.levs[-1] # remove the first level (reference class)
    # use levels to define cat indicators:
    OData$dat.sVar[,(factor.varnm %+% "_" %+% factor.levs) := lapply(factor.levs, function(x) levels(get(factor.varnm))[get(factor.varnm)] %in% x)]
    # to remove the origional factor var: # OData$dat.sVar[,(factor.varnm):=NULL]
    new.factor.names[[factor.varnm]] <- factor.varnm %+% "_" %+% factor.levs
  }
  OData$new.factor.names <- new.factor.names

  # --------------------------------------------------------------------------------------------------------
  # Convert all logical vars to binary integers
  # --------------------------------------------------------------------------------------------------------
  logical.Ls <- unlist(lapply(OData$dat.sVar, is.logical))
  logical.Ls <- names(logical.Ls)[logical.Ls]
  if (length(logical.Ls)>0 && verbose) message("...converting logical columns to binary integers (0 = FALSE)...")
  for (logical.varnm in logical.Ls) {
    OData$dat.sVar[,(logical.varnm) := as.integer(get(logical.varnm))]
  }
  # for (Nnode in nodes$Nnodes) CheckVarNameExists(OData$dat.sVar, Nnode)
  for (Ynode in nodes$Ynode)  CheckVarNameExists(OData$dat.sVar, Ynode)
  for (Lnode in nodes$Lnodes) CheckVarNameExists(OData$dat.sVar, Lnode)
  return(OData)
}

# ---------------------------------------------------------------------------------------
#' Define various summaries of observed outcome
#'
#' @param OData Input data object created by \code{importData} function.
#' @param holdout ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
define_LR_summaries <- function(OData, holdout = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  nodes <- OData$nodes
  # browser()

  # Add new predictors (summaries), for each observed outcome, but ignore the holdouts, if holdout==TRUE
  # 1. Left Y[lt], lt, if exists (otherwise lt = t)
  # 2. Right Y[rt], rt, if exists (otherwise rt = t)

  # Define which observations are in the hold-out set (and thus should be ignored for creating summaries)
  if (!is.null(OData$hold_column) && holdout) {
    non_hold_idx <- !OData$dat.sVar[[OData$hold_column]]
  } else {
    non_hold_idx <- TRUE
  }

  OData$dat.sVar[, c("left.t", "right.t") := list(NULL, NULL)]
  OData$dat.sVar[, c("Yleft.t", "Yright.t") := list(NULL, NULL)]
  OData$dat.sVar[, c("left.most", "middle", "right.most") := list(NULL, NULL, NULL)]
  # OData$dat.sVar[, c("sum.Y","sum.Y.sq") := list(NULL, NULL)]

  OData$dat.sVar[non_hold_idx, c("left.t", "right.t") := list(shift(eval(as.name(nodes$tnode)), type = "lag"), shift(eval(as.name(nodes$tnode)), type = "lead")), by = eval(nodes$IDnode)]
  OData$dat.sVar[non_hold_idx, c("Yleft.t", "Yright.t") := list(shift(eval(as.name(nodes$Ynode)), type = "lag", fill = NA), shift(eval(as.name(nodes$Ynode)), type = "lead", fill = NA)), by = eval(nodes$IDnode)]

  OData$dat.sVar[non_hold_idx & is.na(Yleft.t), Yleft.t := Yright.t]
  OData$dat.sVar[non_hold_idx & is.na(Yright.t), Yright.t := Yleft.t]

  # Add dummy indicator column(s) of being left-most / middle / right-most observation:
  OData$dat.sVar[non_hold_idx, c("left.most", "middle", "right.most"):= list(0L, 0L, 0L)]
  OData$dat.sVar[non_hold_idx & is.na(left.t), left.most := 1L]
  OData$dat.sVar[non_hold_idx & !is.na(left.t) & !is.na(right.t), middle := 1L]
  OData$dat.sVar[non_hold_idx & is.na(right.t), right.most := 1L]

  # Set missing left.t & right.t to current t value:
  OData$dat.sVar[non_hold_idx & is.na(left.t), left.t := eval(as.name(nodes$tnode))]
  OData$dat.sVar[non_hold_idx & is.na(right.t), right.t := eval(as.name(nodes$tnode))]
  # OData$dat.sVar[1:100, ]

  # browser()
  # Only evaluate these summaries once on the training set (omit the holdout set)
  # OData$dat.sVar[, c("meanY") := list(NULL)]
  if (!is.null(OData$hold_column) && holdout) {
    OData$dat.sVar <- OData$dat.sVar[OData$dat.sVar[non_hold_idx, {meanY = mean(eval(as.name(nodes$Ynode))); list(meanY = meanY)}, by = eval(nodes$IDnode)]]
    # OData$dat.sVar <- OData$dat.sVar[OData$dat.sVar[non_hold_idx, {sum.Y.sq=sum(eval(as.name(nodes$Ynode))^2); list(sum.Y.sq = sum.Y.sq)}, by = eval(nodes$IDnode)]]
  }

  # OData$dat.sVar[, c("sum.Y","sum.Y.sq") := list(NULL, NULL)]
  # if (!is.null(OData$hold_column) && holdout) {
  #   OData$dat.sVar <- OData$dat.sVar[OData$dat.sVar[non_hold_idx, {sum.Y=sum(eval(as.name(nodes$Ynode))); list(sum.Y = sum.Y)}, by = eval(nodes$IDnode)]]
  #   OData$dat.sVar <- OData$dat.sVar[OData$dat.sVar[non_hold_idx, {sum.Y.sq=sum(eval(as.name(nodes$Ynode))^2); list(sum.Y.sq = sum.Y.sq)}, by = eval(nodes$IDnode)]]
  # }

  # Add total sum of observed Y's and other summaries?
  # ...

  return(OData)
}


# ---------------------------------------------------------------------------------------
#' Define and fit growth models.
#'
#' @param OData Input data object created by \code{importData} function.
#' @param predvars ...
#' @param params ...
#' @param holdout ...
#' @param random ...
#' @param seed ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
get_fit <- function(OData, predvars, params, holdout = FALSE, random = FALSE, seed = NULL,
                    verbose = getOption("growthcurveSL.verbose")) {
# stratify = NULL, reg,
  gvars$verbose <- verbose
  OData$nodes$predvars <- predvars
  nodes <- OData$nodes
  new.factor.names <- OData$new.factor.names

  if (holdout) OData$add_holdout_ind(hold_column = "hold", random = random, seed = seed)

  OData <- define_LR_summaries(OData, holdout)

  # ------------------------------------------------------------------------------------------
  # DEFINE a single regression class
  # ------------------------------------------------------------------------------------------
  # To select the non-holdout set for fitting the models:
  regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = predvars, outvar.class = list("binary"), subset_exprs = list("!hold"), model_contrl = params)
  # To select only the holdout set (for MSE evaluation):
  # regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = predvars, outvar.class = list("binary"), subset_exprs = list("hold"), model_contrl = params)

  modelfit <- OutcomeModel$new(reg = regobj)
  # Perform fitting:
  modelfit$fit(data = OData)
  # # Get predictions for holdout data only:
  # preds <- modelfit$predict(newdata = OData, subset_exprs = "hold == TRUE")
  # OData$dat.sVar[, holdoutPred := preds$getprobA1]
  # ------------------------------------------------------------------------------------------
  OData$modelfit <- modelfit
  return(OData)
}

# ---------------------------------------------------------------------------------------
#' Predict for a holdout set
#'
#' @param OData Input data object created by \code{importData} function.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @param modelIDs ...
#' @return ...
#' @export
predictHoldout <- function(OData, modelIDs, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  nodes <- OData$nodes
  modelfit <- OData$modelfit
  sel_vars <- c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData$new.factor.names), nodes$Ynode)

  OData <- define_LR_summaries(OData, holdout = FALSE)
  OData$dat.sVar
  # browser()

  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  # Get predictions for holdout data only when the actual outcome was also not missing:
  preds <- modelfit$predict(newdata = OData, subset_exprs = "hold == TRUE")
  holdoutDT <- OData$dat.sVar[, c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData$new.factor.names), nodes$Ynode, OData$hold_column), with = FALSE]

  holdoutDT[, (colnames(preds$getprobA1)) := as.data.table(preds$getprobA1)]

  # browser()
  MSE <- modelfit$evalMSE(OData)
  print("MSE: "); print(MSE)
  return(holdoutDT)
}

# ---------------------------------------------------------------------------------------
# TO DO: Add prediction only based on the subset of models (rather than predicting for all models)
# using modelIDs argument
# ---------------------------------------------------------------------------------------
#' Predict for a holdout set
#'
#' @param OData Input data object created by \code{importData} function.
#' @param tmin ...
#' @param tmax ...
#' @param modelIDs ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
predictCurve <- function(OData, tmin, tmax, modelIDs, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  nodes <- OData$nodes
  modelfit <- OData$modelfit
  sel_vars <- c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData$new.factor.names), nodes$Ynode)

  # browser()

  if (missing(tmin)) tmin <- OData$min.t
  if (missing(tmax)) tmax <- min(OData$max.t, 700)
  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")

  # Predict a full curve based on a grid for each subject. The grid is tmin to tmax. Covariates are carried forward:
  gridDT <- CJ(unique(OData$dat.sVar[[nodes$IDnode]]), tmin:tmax)
  colnames(gridDT) <- c(nodes$IDnode, nodes$tnode)
  setkeyv(gridDT, cols = c(nodes$IDnode, nodes$tnode))


  gridDT <- OData$dat.sVar[, sel_vars, with = FALSE][gridDT, roll = TRUE]
  gridDT <- OData$dat.sVar[][gridDT, roll = TRUE]

  # gridDT[1:100, ]

  newOData <- OData$clone()
  newOData$dat.sVar <- gridDT

  preds <- modelfit$predict(newdata = newOData, subset_vars = NULL, subset_exprs = NULL)

  if (is.matrix(preds$getprobA1)) {
    gridDT[, (colnames(preds$getprobA1)) := as.data.table(preds$getprobA1)]
  } else {
    gridDT[, ("PredModel1") := preds$getprobA1]
  }
  return(gridDT)
}







