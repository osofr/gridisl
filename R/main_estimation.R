#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggplot geom_point geom_errorbar theme_bw coord_flip aes position_dodge alpha
#' @import ggiraph
# @importFrom ggiraph geom_point_interactive ggiraph
NULL

# ---------------------------------------------------------------------------------------
# **** ALLOW data to be an R6 object of class DataStorageClass. This will enable calling h2o SL from other pkgs *****
# **** CONSIDER brining the nfold / fold_column / fold_assignment as args of this function
# 1. Making this work with random/last holdout schemes?
# 2. Making this work with h2o-internal C.V., but using either manually defined or automatic folds?
# 3. Working C.V. when predictors on validation set have to be defined differently from predictors in training set?
# 4. Is it at all possible to make 4. work with h2o-internal CV?
# 5. [LONG-TERM] Need wrappers for various h2o modeling functions
# ---------------------------------------------------------------------------------------
#' Generic modeling function for any longitudinal data.
#'
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param x A vector containing the names of predictor variables to use for modeling. If x is missing, then all columns except \code{ID}, \code{y} are used.
#' @param y A character string name of the column that represent the response variable in the model.
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param data_validation Optional \code{data.frame} or \code{data.table} with validation data. When provided, this dataset will be used for scoring the model fit(s).
#' @param params Parameters specifying the type of modeling procedure to be used.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
# stratify = NULL, reg,
fit_model <- function(ID, t_name, x, y, data, data_validation, params, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  if (missing(x)) x <- setdiff(colnames(data), c(ID, y))

  ## Import input data into R6 object, define nodes (and export them)
  OData_train <- importData(data = data, ID = ID, t_name = t_name, covars = x, OUTCOME = y)
  nodes <- OData_train$nodes

  ## **** If we wanted to define folds manually, this would be one way to do it:****
  # OData_train$define_CVfolds(nfolds = 3, fold_column = "fold_id", seed = 12345)
  ## If fold_column specified in the model parameters, add it to the data object:
  if (!is.null(params$fold_column)) OData_train$fold_column <- params$fold_column

  ## Optionally define holdout / CV indicator column and subset the input data into training / validation samples?
  # ...



  ## Define R6 object with validation data:
  if (!missing(data_validation)) {
    OData_valid <- importData(data = data_validation, ID = ID, t_name = t_name, covars = x, OUTCOME = y)
  } else {
    OData_valid <- NULL
  }



  ## Define R6 regression class (specify subset_exprs to select only specific obs during fitting, e.g., only non-holdouts)
  regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = x, outvar.class = list("binary"), model_contrl = params)
  # regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = x, outvar.class = list("binary"), subset_exprs = list("!hold"), model_contrl = params)



  ## Define a modeling object, perform fitting (real data is being passed for the first time here):
  modelfit <- PredictionModel$new(reg = regobj)$fit(data = OData_train, validation_data = OData_valid)


  ## Get predictions for holdout data only:
  # preds <- modelfit$predict(newdata = OData, subset_exprs = "hold == TRUE")
  # OData$dat.sVar[, holdoutPred := preds$getprobA1]

  # ------------------------------------------------------------------------------------------
  return(modelfit)
}

# ---------------------------------------------------------------------------------------
#' Import data, define nodes (columns), define dummies for factor columns and define input data R6 object
#'
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param covars Names of predictors (covariates) in the data.
#' @param OUTCOME Character name of the column containing outcomes.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return An R6 object that contains the input data. This can be passed as an argument to \code{get_fit} function.
# @example tests/examples/1_growthcurveSL_example.R
#' @export
importData <- function(data, ID = "Subject_ID", t_name = "time_period", covars, OUTCOME = "Y", verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  if (verbose) {
    current.options <- capture.output(str(gvars$opts))
    print("Using the following growthcurveSL options/settings: ")
    cat('\n')
    cat(paste0(current.options, collapse = '\n'), '\n')
  }
  if (missing(covars)) { # define time-varing covars (L) as everything else in data besides these vars
    covars <- setdiff(colnames(data), c(ID, OUTCOME))
  }
  # The ordering of variables in this list is the assumed temporal order!
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
#' @export
#' @return ...
define_predictors <- function(dataDT, nodes, train_set = TRUE, holdout = TRUE, hold_column = "hold", verbose = getOption("growthcurveSL.verbose")) {
  # Making sure nothing gets modified by reference:
  dataDT <- copy(dataDT)

  # Define which observations are in the hold-out set (and thus should be ignored when creating predictors for training set)
  if (train_set && holdout) {
    # non_hold_idx <- !OData$dat.sVar[[hold_column]]
    non_hold_idx <- !dataDT[[hold_column]]
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

    # Evaluate the summary (predictors) on a training set (for holdout=TRUE, this will evaluate the summaries among training data points (excluding the holdouts)):
    dataDT <- dataDT[dataDT[non_hold_idx, {
        nY = length(eval(as.name(nodes$Ynode)));
        meanY = mean(eval(as.name(nodes$Ynode)));
        sdY = sd(eval(as.name(nodes$Ynode)));
        medianY = median(eval(as.name(nodes$Ynode)));
        minY = min(eval(as.name(nodes$Ynode)));
        maxY = max(eval(as.name(nodes$Ynode)));
        list(nY = nY, meanY = meanY, sdY = sdY, medianY = medianY, minY = minY, maxY = maxY)
      }, by = eval(nodes$IDnode)]]

  } else {
    # Evaluate the summary (predictors) for validation set. Treats each row of data as if it is a validation data point.
    # Loop through every data-point row i, remove it, then evaluate the summary for that subject with row i removed.
    # This will allow us to do validation set predictions when doing V-fold CV.
    # NOTE: such summaries will allow us to use ANY observed data point as a validation point and do prediction ALL at once (rather than having to loop over each point)
    dataDT[, c("Y_tmp") := list(eval(as.name(nodes$Ynode)))]
    dataDT[, c("nY", "meanY", "sdY", "medianY", "minY", "maxY") := list(0.0,0.0,0.0,0.0,0.0,0.0)]
    dataDT[, c("nY", "meanY", "sdY", "medianY", "minY", "maxY") := {
            for (i in seq_len(.N)) {
              nY[i] = length(Y_tmp)-1
              meanY[i] = mean(Y_tmp[-i])
              sdY[i] = sd(Y_tmp[-i])
              medianY[i] = median(Y_tmp[-i])
              minY[i] = min(Y_tmp[-i])
              maxY[i] = max(Y_tmp[-i])
            };
          list(nY = nY, meanY = meanY, sdY = sdY, medianY = medianY, minY = minY, maxY = maxY)
      }, by = eval(nodes$IDnode)]
    dataDT[, ("Y_tmp") :=  NULL]
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
#' @param OData_train Optional R6 object containing the training data.
#' @param OData_valid Optional R6 object containing the validation data.
#' @param predvars The character vector of predictors to be used during modeling.
#' @param params Parameters specifying the type of modeling procedure to be used.
#' @param holdout Set to TRUE to train the model fit on non-holdout observations only. Set to FALSE to train the model on the entire input dataset.
#' @param hold_column The name of the column that contains the holdout observation indicators (TRUE/FALSE) in the input data.
#' This holdout column must be defined and added to the input data prior to calling this function.
#' @param random Logical, specifying if the holdout observations should be selected at random.
#' If FALSE then the last observation for each subject is selected as a holdout.
#' @param seed Random number seed for selecting a random holdout.
#' @param expr_to_train Additional logical expression which will further subset observations (rows) for training data.
#' Use this to restrict the model fitting to a specific subsample of the training datset.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
# @seealso \code{\link{growthcurveSL-package}} for the general overview of the package,
# @example tests/examples/1_growthcurveSL_example.R
#' @export
# stratify = NULL, reg,
fit_model_holdout <- function(OData, OData_train, OData_valid, predvars, params, holdout = TRUE, hold_column = NULL, random = FALSE, seed = NULL, expr_to_train = NULL, verbose = getOption("growthcurveSL.verbose")) {
  gvars$verbose <- verbose
  # OData$nodes$predvars <- predvars
  # nodes <- OData$nodes

  if (holdout && is.null(hold_column)) {
    OData$add_holdout_ind(hold_column = "hold", random = random, seed = seed)
  } else if (holdout && !is.null(hold_column)) {
    # assert_that(hold_column %in% names(OData$dat.sVar))
    # OData$hold_column <- hold_column
  }

  ## ------------------------------------------------------------------------------------------
  ## Define training data (excludes holdouts):
  ## ------------------------------------------------------------------------------------------
  if (missing(OData_train)) {
    OData_train <- OData$clone()
    OData$nodes$predvars <- predvars
    nodes <- OData$nodes
    dataDT <- OData$dat.sVar[,c(nodes$IDnode, nodes$tnode, nodes$Ynode, nodes$Lnodes, OData$hold_column, unlist(OData$new.factor.names)), with = FALSE]
    # OData_train$dat.sVar <- dataDT[!dataDT[[OData_train$hold_column]], ]
    dataDTtrain <- dataDT[!dataDT[[OData_train$hold_column]], ]
    # ------------------------------------------------------------------------------------------
    # Define training data summaries (excludes holdouts, summaries are created without the holdout observations):
    # ------------------------------------------------------------------------------------------
    dataDTtrain <- define_predictors(dataDTtrain, nodes, train_set = TRUE, holdout = holdout, hold_column = OData$hold_column)
    OData_train$dat.sVar <- dataDTtrain[!dataDTtrain[[OData_train$hold_column]], ]
  }
  if (!is.null(expr_to_train)) {
    OData_train$dat.sVar <- OData_train$dat.sVar[eval(parse(text=expr_to_train)), ]
  }

  ## ------------------------------------------------------------------------------------------
  ## Define validation data (includes the holdout only, summaries are created without the holdout observations):
  ## ------------------------------------------------------------------------------------------
  if (missing(OData_valid)) {
    OData_valid <- OData$clone()
    nodes <- OData$nodes
    dataDT <- OData$dat.sVar[,c(nodes$IDnode, nodes$tnode, nodes$Ynode, nodes$Lnodes, OData$hold_column, unlist(OData$new.factor.names)), with = FALSE]
    dataDTvalid <- define_predictors(dataDT, nodes, train_set = FALSE, hold_column = OData$hold_column)
    OData_valid$dat.sVar <- dataDTvalid[dataDTvalid[[OData_valid$hold_column]], ]
  }

  nodes <- OData_train$nodes

  ## ------------------------------------------------------------------------------------------
  ## DEFINE a single regression class
  ## ------------------------------------------------------------------------------------------
  ## To select the non-holdout set for fitting the models:
  regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = predvars, outvar.class = list("binary"), subset_exprs = list("!hold"), model_contrl = params)

  ## To select only the holdout set (for MSE evaluation):
  # regobj <- RegressionClass$new(outvar = nodes$Ynode, predvars = predvars, outvar.class = list("binary"), subset_exprs = list(OData$hold_column), model_contrl = params)
  modelfit <- PredictionModel$new(reg = regobj)

  ## Perform fitting:
  modelfit$fit(data = OData_train, validation_data = OData_valid)

  ## Get predictions for holdout data only:
  # preds <- modelfit$predict(newdata = OData, subset_exprs = "hold == TRUE")
  # OData$dat.sVar[, holdoutPred := preds$getprobA1]

  # ------------------------------------------------------------------------------------------
  return(modelfit)
}

# ---------------------------------------------------------------------------------------
#' Predict for a holdout set
#'
#' @param OData Input data object created by \code{importData} function.
#' @param OData_valid ...
#' @param modelfit Model fit object returned by \code{\link{get_fit}} function.
#' @param modelIDs ... Not implemented ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @return ...
#' @export
## @param all_obs Predict values for all observations (including holdout) or just the holdout observations?
## all_obs = FALSE,
predictHoldout <- function(OData, OData_valid, modelfit, modelIDs, verbose = getOption("growthcurveSL.verbose")) {
  assert_that(is.PredictionModel(modelfit))
  gvars$verbose <- verbose

  if (missing(OData_valid)) {
    assert_that(is.DataStorageClass(OData))
    OData_valid <- OData$clone()
    nodes <- OData$nodes
    sel_vars <- c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData$new.factor.names), nodes$Ynode, OData$hold_column)
    dataDT <- OData$dat.sVar[, sel_vars, with = FALSE]
    dataDTvalid <- define_predictors(dataDT, nodes, train_set = FALSE, hold_column = OData$hold_column)
    OData_valid$dat.sVar <- dataDTvalid[dataDTvalid[[OData_valid$hold_column]], ]
  }

  assert_that(is.DataStorageClass(OData_valid))
  nodes <- OData_valid$nodes

  if (is.null(modelfit)) stop("must call get_fit() prior to obtaining predictions")
  # if (all_obs) {
  #   subset_exprs <- NULL
  # } else {
    subset_exprs <- "hold == TRUE"
  # }

  # Get predictions for holdout data only when the actual outcome was also not missing:
  preds <- modelfit$predict(newdata = OData_valid, subset_exprs = subset_exprs)
  # preds <- modelfit$predict(newdata = OData, subset_exprs = subset_exprs)
  holdoutDT <- OData_valid$dat.sVar[, c(nodes$IDnode, nodes$tnode, nodes$Lnodes, unlist(OData_valid$new.factor.names), nodes$Ynode, OData_valid$hold_column), with = FALSE]
  holdoutDT[, (colnames(preds$getprobA1)) := as.data.table(preds$getprobA1)]
  # browser()
  # MSE <- modelfit$evalMSE(OData_valid)
  print("MSE: "); print(unlist(preds$getMSE))
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







