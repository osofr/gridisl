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
  nodes <- OData$nodes
  new.factor.names <- OData$new.factor.names

  if (holdout) {
    OData$add_holdout_ind(hold_column = "hold", random = random, seed = seed)
  }

  OData$dat.sVar

  # ------------------------------------------------------------------------------------------
  # DEFINE a single regression class
  # ------------------------------------------------------------------------------------------
  browser()

  # To select the non-holdout set for fitting the models:
  regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = predvars, outvar.class = list("binary"), subset_exprs = list("!hold"), model_contrl = params)

  # To select only the holdout set (for MSE evaluation):
  # regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = predvars, outvar.class = list("binary"), subset_exprs = list("hold"), model_contrl = params)
  modelfits <- OutcomeModel$new(reg = regobj)

  # Perform fit and prediction
  # modelfits.g0 <- GenericModel$new(reg = g_CAN_regs_list, DataStorageClass.g0 = OData)
  modelfits$fit(data = OData)
  # modelfits$fit(data = OData, predict = TRUE)

  # get predictions for new data (holdout + grid by time dataset):

  # h_gN <- modelfits.g0$predictAeqa(n = OData$nobs)

  modelfits$fit(data = OData)

  # ------------------------------------------------------------------------------------------
  OData$modelfit <- modelfits
  return(OData)
}
