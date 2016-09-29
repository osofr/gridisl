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
#' Define predictors for training or testing (validation) data
#'
#' @param dataDT Input data.table
#' @param nodes ...
#' @param train_set ...
#' @param holdout ...
#' @param hold_column ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
define_predictors <- function(dataDT, nodes, train_set = TRUE, holdout = TRUE, hold_column = "hold", verbose = getOption("growthcurveSL.verbose")) {
  # Making sure nothing gets modified by reference:
  dataDT <- copy(dataDT)

  # Define which observations are in the hold-out set (and thus should be ignored when creating predictors for training set)
  if (train_set && holdout) {
    non_hold_idx <- !OData$dat.sVar[[hold_column]]
  } else {
    # to define (Y.lt, Y.rt, lt, rt, l.obs, mid.obs, r.obs) for validation data points we use the entire observed data
    non_hold_idx <- rep.int(TRUE, nrow(dataDT))
  }

  # ---------------------------------------------------------------------------------------
  # (A: train_set && holdout) Define predictors for training data after dropping the holdout observations
  # (B: else) Use all observations to define predictors:
  #   The predictors below turn out to be equivalent when defining validation data point as well as when training on ALL data
  #   Will allow us to use every single observation as a test point for CV.MSE
  # ---------------------------------------------------------------------------------------
  # dataDT[, c("lt", "rt", "Y.lt", "Y.rt", "l.obs", "mid.obs", "r.obs") := list(NULL, NULL, NULL, NULL, NULL, NULL, NULL)]
  # dataDT[, c("meanY", "sumYsq") := list(NULL, NULL)]
  dataDT[non_hold_idx, c("Y.lt", "Y.rt") := list(shift(eval(as.name(nodes$Ynode)), type = "lag", fill = NA), shift(eval(as.name(nodes$Ynode)), type = "lead", fill = NA)), by = eval(nodes$IDnode)]
  # Add dummy indicator column(s) of being left-most (l.obs) / middle (mid.obs) / right-most (r.obs) observation:
  dataDT[non_hold_idx, c("l.obs", "mid.obs", "r.obs"):= list(0L, 0L, 0L)]
  # 1. Left Y[lt], lt, if exists (otherwise lt = rt)
  # 2. Right Y[rt], rt, if exists (otherwise rt = lt)
  dataDT[non_hold_idx, c("lt", "rt") := list(shift(eval(as.name(nodes$tnode)), type = "lag"), shift(eval(as.name(nodes$tnode)), type = "lead")), by = eval(nodes$IDnode)]
  dataDT[non_hold_idx, c("Y.lt", "Y.rt") := list(shift(eval(as.name(nodes$Ynode)), type = "lag", fill = NA), shift(eval(as.name(nodes$Ynode)), type = "lead", fill = NA)), by = eval(nodes$IDnode)]
  # Add dummy indicator column(s) of being left-most (l.obs) / middle (mid.obs) / right-most (r.obs) observation:
  dataDT[non_hold_idx, c("l.obs", "mid.obs", "r.obs"):= list(0L, 0L, 0L)]
  dataDT[non_hold_idx & is.na(lt), l.obs := 1L]
  dataDT[non_hold_idx & !is.na(lt) & !is.na(rt), mid.obs := 1L]
  dataDT[non_hold_idx & is.na(rt), r.obs := 1L]
  # Set missing lt & rt in the same manner as missing Yleft / Yright:
  dataDT[non_hold_idx & is.na(Y.lt), Y.lt := Y.rt]
  dataDT[non_hold_idx & is.na(lt),  lt  := rt]
  dataDT[non_hold_idx & is.na(Y.rt), Y.rt := Y.lt]
  dataDT[non_hold_idx & is.na(rt), rt := lt]

  if (train_set) {
    # Evaluate the summary (predictors) on a training set (for holdout=TRUE, this will evaluate the summaries by excluding the holdouts):
    dataDT <- dataDT[dataDT[non_hold_idx, {meanY = mean(eval(as.name(nodes$Ynode))); list(meanY = meanY)}, by = eval(nodes$IDnode)]]
    dataDT <- dataDT[dataDT[non_hold_idx, {sumYsq=sum(eval(as.name(nodes$Ynode))^2); list(sumYsq = sumYsq)}, by = eval(nodes$IDnode)]]
  } else {
    # Evaluate the summary (predictors) for validation set. Treats each row of data as if it is a validation data point.
    # Loop through every data-point row i, remove it, then evaluate the summary for that subject with row i removed.
    # This will allow us to do validation set predictions when doing V-fold CV.
    # NOTE: such summaries will allow us to use ANY observed data point as a validation point and do prediction ALL at once (rather than having to loop over each point)
    dataDT[, c("meanY", "meanY_tmp") := list(0.0, eval(as.name(nodes$Ynode)))]
    dataDT[, ("meanY") := { for (i in seq_len(.N)) { meanY[i] = mean(meanY_tmp[-i]) };  list(meanY = meanY)}, by = eval(nodes$IDnode)]
    dataDT[, ("meanY_tmp") :=  NULL]
    # dataDT <- dataDT[dataDT[, {meanY = eval(as.name(nodes$Ynode)); for (i in seq_len(.N)) {meanY[i] = mean(meanY[-i])};  list(meanY = meanY)}, by = eval(nodes$IDnode)]]
  }

#   # Add total sum of observed Y's and other summaries?
#   # ...

  return(dataDT)
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
# stratify = NULL, reg,
get_fit <- function(OData, predvars, params, holdout = TRUE, random = FALSE, seed = NULL, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  OData$nodes$predvars <- predvars
  nodes <- OData$nodes
  new.factor.names <- OData$new.factor.names

  if (holdout) OData$add_holdout_ind(hold_column = "hold", random = random, seed = seed)

  # ------------------------------------------------------------------------------------------
  # Define training data (excludes holdouts, summaries are created without the holdout observations):
  # ------------------------------------------------------------------------------------------
  dataDT <- OData$dat.sVar[,c(nodes$IDnode, nodes$tnode, nodes$Ynode, nodes$Lnodes, OData$hold_column, unlist(new.factor.names)), with = FALSE]
  dataDTtrain <- define_predictors(dataDT, nodes, train_set = TRUE, holdout = holdout, hold_column = "hold")
  OData_train <- OData$clone()
  OData_train$dat.sVar <- dataDTtrain[!dataDTtrain[[OData_train$hold_column]], ]

  # ------------------------------------------------------------------------------------------
  # Define validation data (includes the holdout only, summaries are created without the holdout observations):
  # ------------------------------------------------------------------------------------------
  dataDTvalid <- define_predictors(dataDT, nodes, train_set = FALSE, hold_column = "hold")
  OData_valid <- OData$clone()
  OData_valid$dat.sVar <- dataDTvalid[dataDTvalid[[OData_valid$hold_column]], ]

  # ------------------------------------------------------------------------------------------
  ## DEFINE a single regression class
  # ------------------------------------------------------------------------------------------
  ## To select the non-holdout set for fitting the models:
  regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = predvars, outvar.class = list("binary"), subset_exprs = list("!hold"), model_contrl = params)
  ## To select only the holdout set (for MSE evaluation):
  # regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = predvars, outvar.class = list("binary"), subset_exprs = list("hold"), model_contrl = params)
  # browser()
  modelfit <- OutcomeModel$new(reg = regobj)

  ## Perform fitting:
  modelfit$fit(data = OData_train, validation_data = OData_valid)
  ## Get predictions for holdout data only:
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
## @param all_obs Predict values for all observations (including holdout) or just the holdout observations?
## all_obs = FALSE,
predictHoldout <- function(OData, modelIDs, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  nodes <- OData$nodes
  modelfit <- OData$modelfit
  new.factor.names <- OData$new.factor.names

  sel_vars <- c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(new.factor.names), nodes$Ynode, OData$hold_column)

  dataDT <- OData$dat.sVar[, sel_vars, with = FALSE]

  dataDTvalid <- define_predictors(dataDT, nodes, train_set = FALSE, hold_column = "hold")
  OData_valid <- OData$clone()
  OData_valid$dat.sVar <- dataDTvalid[dataDTvalid[[OData_valid$hold_column]], ]
  # # OData$dat.sVar

  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  # if (all_obs) {
  #   subset_exprs <- NULL
  # } else {
    subset_exprs <- "hold == TRUE"
  # }

  # Get predictions for holdout data only when the actual outcome was also not missing:
  preds <- modelfit$predict(newdata = OData_valid, subset_exprs = subset_exprs)
  # preds <- modelfit$predict(newdata = OData, subset_exprs = subset_exprs)
  holdoutDT <- OData_valid$dat.sVar[, c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData$new.factor.names), nodes$Ynode, OData$hold_column), with = FALSE]

  holdoutDT[, (colnames(preds$getprobA1)) := as.data.table(preds$getprobA1)]
  # browser()
  MSE <- modelfit$evalMSE(OData_valid)
  # print("MSE: "); print(unlist(MSE))
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







