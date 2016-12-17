#' Predict and save the entire growth curve (grid of time points).
#'
#' A thin wrapper for \code{define_features_drop}, \code{define_tgrid} and \code{predict_SL} functions.
#' Evaluates predictions from the existing SuperLearner fit using an entire grid of time points.
#' Optionally, when \code{file.name} is not \code{NULL}, the resulting dataset of predictions is saved as a csv file.
#' @param SLfit SuperLearner fit returned by \code{\link{fit_holdoutSL}} or  \code{\link{fit_cvSL}}.
#' @param data Input data used for model training.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param y A character string name of the column that represent the response variable in the model.
#' @param tmin Min t value of the grid
#' @param tmax Max t value of the grid
#' @param incr Increment value for the grid of \code{t}'s
#' @param hold_column Column name in \code{dataDT} indicating the holdout observations (if used). Leave as \code{NULL} if no \code{hold_column} is present.
#' @param file.name A file name (without the .csv extension) in which the prediction dataset will be saved. Leave as NULL is no saving is necessary.
#' @param file.path A directory path in which the predictions file should be saved.
#' @return A \code{data.table} with subject IDs, grid of equally spaced time-points and the corresponding growth curve predictions.
#' The relevant subject summaries and predictors used for training will be also included in the output data.
#' In addition, the output dataset also contains an indicator column 'train_point', set to \code{TRUE} for all (ID,time-points) that also appear
#' in the input data \code{dataDT}. That is 'train_point' indicates if the row might have been previously used for model training.
#' Finally, if the argument 'hold_column' is not NULL, the output dataset will contain the indicator column of holdout observations
#' (named according to hold_column argument).
#' @export
predict_save_tgrid <- function(SLfit, data, ID, t_name, y, tmin = 1, tmax = 500, incr = 2, hold_column = NULL,
                               file.name = NULL,
                               file.path = getOption('longDiSL.file.path')) {

  inputDT_all_train <- define_features_drop(data, ID = ID, t_name = t_name, y = y, train_set = TRUE)
  inputDT_tgrid <- define_tgrid(inputDT_all_train, ID = ID, t_name = t_name, y = y, tmin = tmin, tmax = tmax, incr = incr, hold_column = hold_column)
  preds_tgrid <- predict_SL(SLfit, newdata = inputDT_tgrid, grid = TRUE, add_subject_data = TRUE)

  preds_tgrid[, ("train_point") := inputDT_tgrid[["train_point"]]]
  if (!is.null(hold_column))
    preds_tgrid[, (hold_column) := inputDT_tgrid[[hold_column]]]

  data.table::setcolorder(preds_tgrid, c(names(preds_tgrid)[-(ncol(preds_tgrid)-2)], names(preds_tgrid)[(ncol(preds_tgrid)-2)]))

  if (!is.null(file.name)) {
    full_path <- file.path(file.path, paste0(file.name, ".csv"))
    message(paste0("...writing csv file with predictions to: ", full_path))
    data.table::fwrite(preds_tgrid[, c(ID, t_name, "train_point", hold_column, "SL.preds"), with = FALSE],
                       file = full_path)
  }

  return(preds_tgrid)
}

#' Define data with grids of time-points for growth curve prediction
#'
#' @param dataDT Input data with all the relevant summaries that were used for modeling being already defined.
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param y A character string name of the column that represent the response variable in the model.
#' @param tmin Min t value of the grid
#' @param tmax Max t value of the grid
#' @param incr Increment value for the grid of \code{t}'s
#' @param hold_column Column name in \code{dataDT} indicating the holdout observations (if used).
#' @return A \code{data.table} of subject IDs and time-points, along with all the relevant subject specific curve summaries and predictors.
#' In addition, the output dataset also contains an indicator column 'train_point', set to \code{TRUE} for all (ID,time-points) that also appear
#' in the input data \code{dataDT}. That is 'train_point' indicates if the row might have been previously used for model training.
#' Finally, if the argument 'hold_column' is not NULL, the output dataset will contain the indicator column of holdout observations
#' (named according to hold_column argument).
#' @export
define_tgrid <- function(dataDT, ID, t_name, y, tmin = 1, tmax = 500, incr = 2, hold_column = NULL) {
  if (!is.data.table(dataDT)) stop("critical error: dataDT must be a data.table, use 'data.table(dataDT)'")
  if ("train_point" %in% names(dataDT)) stop("critical error: columns named 'train_point' are not allowed in dataDT")
  if (!is.null(hold_column)) {
    if (!(hold_column %in% names(dataDT))) stop("critical error: hold_column is not null, but cannot find the corresponding column name in dataDT")
  }

  ## 1. Define the grid of time-points for which to predict
  t_grid <- seq(tmin, tmax, by = incr)
  gridDT <- CJ(unique(dataDT[[ID]]), as.integer(t_grid))
  colnames(gridDT) <- c(ID, t_name)
  setkeyv(gridDT, cols = c(ID, t_name))

  ## 2. Remove person-time combos that already appear in input data (will be later added w/ rbind)
  gridDT <- fsetdiff(gridDT, dataDT[, c(ID, t_name), with = FALSE])
  gridDT[, ("train_point") :=  FALSE] # indicator that this row hasn't been used for training
  if (!is.null(hold_column)) gridDT[, (hold_column) :=  FALSE]

  # Prep data for defining (Y.lt, lt) & (Y.rt, rt)
  sel_vars <- c(ID, t_name, y) # sel_vars <- c(ID, t_name, y, c("l.obs", "mid.obs", "r.obs"))
  dataDT_sel <- dataDT[, sel_vars, with = FALSE]
  dataDT_sel[, ("t.cp") := eval(as.name(t_name))]
  setkeyv(dataDT_sel, cols = c(ID, t_name))

  ## 3a. Define (Y.lt, lt) with rolling join (+)
  gridDT <- dataDT_sel[, , with = FALSE][gridDT, roll = Inf]
  setnames(gridDT, old = c(y, "t.cp"), new = c("Y.lt", "lt"))

  ## 3b. Define (Y.rt, rt) with rolling join (-)
  gridDT <- dataDT_sel[, , with = FALSE][gridDT, roll = -Inf]
  setnames(gridDT, old = c(y, "t.cp"), new = c("Y.rt", "rt"))

  ## 4. Select first row of each subject and merge these covars into the grid data
  sel_covars <- names(dataDT)[!(names(dataDT) %in% c(t_name, y, "lt", "rt", "Y.lt", "Y.rt", hold_column))]
  setkeyv(dataDT, c(ID)) # set key to allow binary search using `J()`
  dataDT_ID_bsl <- dataDT[J(unique(eval(as.name(ID)))), sel_covars, mult ='first', with = FALSE]   # if you wanted the first row for each x
  # dataDT_ID_bsl <- dataDT[J(unique(eval(as.name(ID)))), sel_covars, mult ='last', with = FALSE]    # subset out the last row for each x
    # bsl_covars <- covars # baseline covars
    # dataDT_ID_bsl <- unique(dataDT[, c(ID, bsl_covars), with = FALSE])
  setkeyv(dataDT_ID_bsl, cols = c(ID))
  setkeyv(gridDT, cols = c(ID))
  gridDT <- merge(gridDT, dataDT_ID_bsl)

  ## TV covars? Apply last observation carried forward approach. TBD.
    # TDvars <- c()        # time-dep covars
    # setkeyv(dataDT, cols = c(ID, t_name))
    # gridDT <- dataDT[, , with = FALSE][gridDT, roll = Inf]
    # setnames(gridDT, old = c(y, "t.cp"), new = c("Y.lt", "lt"))

  ## 5. rbind the actual observations into the grid data
  gridDT <- rbindlist(list(gridDT, dataDT[, ("train_point") := TRUE][, names(gridDT), with = FALSE]))
  dataDT[, ("train_point") := NULL]
  setkeyv(gridDT, cols = c(ID, t_name))
  setkeyv(dataDT, cols = c(ID, t_name))

  return(gridDT)
}

# ---------------------------------------------------------------------------------------
#' Define curve-based features and summaries for training or validation data
#'
#' Defines additional features and summaries of the growth curve person-time observations. Used for modeling and defining the training and validation sets
#' (e.g., random holdout and cross-validation).
#' By setting \code{train_set} to \code{TRUE} this function will define features using all data points as a
#' full training set (no holdouts, summaries use all person-time rows).
#' In contrast, when \code{train_set = TRUE} and \code{hold_column} is not missing, these features are defined only for non-holdout observations, excluding the holdout rows
#' (i.e., curve summaries will be defined based on training points only while dropping all holdout observations).
#' Finally, by setting \code{train_set} to \code{FALSE} one can create a validation dataset (e.g., for scoring with CV).
#' In this case the summaries and features will be defined for each row data point (X_i,Y_i)
#' by first dropping (X_i,Y_i) and then evaluating the summaries for (X_i,Y_i) based on the remaining observations.
#' This process is repeated in a loop for all person-time rows in the data.
#'
#' @param dataDT Input data.table
#' @param ID A character string name of the column that contains the unique subject identifiers.
#' @param t_name A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param y A character string name of the column that represent the response variable in the model.
#' @param train_set Set to \code{TRUE} to define growth curve features and summaries for training data.
#' Set to \code{FALSE} to define the summaries for validation data. In the latter case the summaries are defined for observation (X_i,Y_i) by first dropping that observation
#' and then evaluating the summaries for the remaining observations. This is repeated in a loop for all person-time rows in the data.
#' @param hold_column A column with a logical flag for holdout rows / observations (\code{TRUE} indicates that the row is a holdout).
#' When \code{train_set} is \code{TRUE} the resulting output data will contain all non-HOLDOUT observations (training data points).
#' When \code{train_set} is \code{FALSE} the resulting output data will contain the HOLDOUT observations only (validation data points).
#' To evaluate either training or validation data summaries FOR ALL observations this argument must be missing
#' (in which case all observation from the input data are returned with their corresponding summaries).
#' @param noNAs ...
#' @param includeRLMIDind ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(longDiSL.verbose=TRUE)}.
#' @export
#' @example tests/RUnit/RUnit_tests_01_features_cpp.R
#' @return ...
define_features_drop <- function(dataDT, ID, t_name, y, train_set = TRUE, hold_column, noNAs = FALSE, includeRLMIDind = FALSE, verbose = getOption("longDiSL.verbose")) {
  # dataDT <- data.table(dataDT)
  dataDT <- copy(dataDT) # Making sure nothing gets modified by reference:
  if (train_set && !missing(hold_column)) {
    # Define which observations are in the hold-out set (and thus should be ignored when creating predictors for training set)
    non_hold_idx <- !dataDT[[hold_column]]
    dataDT <- dataDT[non_hold_idx, ]
  } else {
    # to define (Y.lt, Y.rt, lt, rt) for validation data points we will use the entire observed data
    non_hold_idx <- rep.int(TRUE, nrow(dataDT))
  }

  # ---------------------------------------------------------------------------------------
  # When train_set is TRUE the features / summaries will be defined for the training data only (without holdouts if hold_column is specified),
  # where the holdout observations are ignored (but still kept in the data)
  # When train_set is FALSE then all observations will be used to define features / predictors.
  #   - The predictors below turn out to be equivalent when defining validation data points as well as when training on ALL data
  #   - This allows us to use every single observation as a test point for CV.MSE
  # ---------------------------------------------------------------------------------------
  # 1. Set left (Y[lt], lt) if exists (otherwise NA); 2. Set right (Y[rt], rt) if exists (otherwise NA)
  dataDT[, c("lt", "rt") := list(shift(eval(as.name(t_name)), type = "lag"), shift(eval(as.name(t_name)), type = "lead")), by = eval(ID)]
  dataDT[, c("Y.lt", "Y.rt") := list(shift(eval(as.name(y)), type = "lag", fill = NA), shift(eval(as.name(y)), type = "lead", fill = NA)), by = eval(ID)]

  if (includeRLMIDind) {
    # Add dummy indicator column(s) of being left-most (l.obs) / middle (mid.obs) / right-most (r.obs) observation:
    dataDT[, c("l.obs", "mid.obs", "r.obs"):= list(0L, 0L, 0L)]
    dataDT[is.na(lt), l.obs := 1L]
    dataDT[!is.na(lt) & !is.na(rt), mid.obs := 1L]
    dataDT[is.na(rt), r.obs := 1L]
  }

  if (noNAs) {
    ##  If left (Y[lt], lt) does't exist then (Y[lt], lt) = (Y[rt], rt);
    dataDT[is.na(Y.lt), Y.lt := Y.rt]
    dataDT[is.na(lt),  lt  := rt]
    ##  If left (Y[rt], rt) does't exist then (Y[rt], rt) = (Y[lt], lt);
    dataDT[is.na(Y.rt), Y.rt := Y.lt]
    dataDT[is.na(rt), rt := lt]
  }

  if (train_set) {
    # Evaluate the summary (predictors) on a training set (for holdout=TRUE, this will evaluate the summaries among training data points (excluding the holdouts)):
    # dataDT <- dataDT[dataDT[non_hold_idx, {
    dataDT <- dataDT[dataDT[, {
        nY = length(eval(as.name(y)));
        meanY = mean(eval(as.name(y)));
        sdY = sd(eval(as.name(y)));
        medianY = median(eval(as.name(y)));
        minY = min(eval(as.name(y)));
        maxY = max(eval(as.name(y)));
        list(nY = nY, meanY = meanY, sdY = sdY, medianY = medianY, minY = minY, maxY = maxY)
      }, by = eval(ID)]]

  } else {
    # Evaluate the summary (predictors) for validation set. Treats each row of data as if it is a validation data point.
    # Loop through every data-point row i, remove it, then evaluate the summary for that subject with row i removed.
    # This will allow us to do validation set predictions when doing V-fold CV.
    # NOTE: such summaries will allow us to use ANY observed data point as a validation point and do prediction ALL at once (rather than having to loop over each point)
    dataDT[, c("Y_tmp") := list(eval(as.name(y)))]
    dataDT[, c("nY", "meanY", "sdY", "medianY", "minY", "maxY") := list(0.0,0.0,0.0,0.0,0.0,0.0)]
    dataDT[, c("nY", "meanY", "sdY", "medianY", "minY", "maxY") := {
            for (i in seq_len(.N)) {
              nY[i] = length(Y_tmp[-i])
              meanY[i] = mean(Y_tmp[-i])
              sdY[i] = sd(Y_tmp[-i])
              medianY[i] = median(Y_tmp[-i])
              minY[i] = ifelse(length(Y_tmp[-i]) > 0, min(Y_tmp[-i]), NA)
              maxY[i] = ifelse(length(Y_tmp[-i]) > 0, max(Y_tmp[-i]), NA)
              # maxY[i] = max(Y_tmp[-i])
            };
          list(nY = nY, meanY = meanY, sdY = sdY, medianY = medianY, minY = minY, maxY = maxY)
      }, by = eval(ID)]
    dataDT[, ("Y_tmp") :=  NULL]
    # dataDT <- dataDT[dataDT[, {meanY = eval(as.name(y)); for (i in seq_len(.N)) {meanY[i] = mean(meanY[-i])};  list(meanY = meanY)}, by = eval(ID)]]
    if (!missing(hold_column)) dataDT <- dataDT[dataDT[[hold_column]], ]
  }

#   # Add total sum of observed Y's and other summaries?
#   # ...

  return(dataDT)
}

# ---------------------------------------------------------------------------------------
#' Define features (predictors) for training or testing (validation) data
#'
#' @param dataDT Input data.table
#' @param nodes ...
#' @param train_set ...
#' @param holdout ...
#' @param hold_column ...
#' @param noNAs ...
#' @param includeRLMIDind ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(longDiSL.verbose=TRUE)}.
#' @export
#' @return ...
define_features <- function(dataDT, nodes, train_set = TRUE, holdout = TRUE, hold_column = "hold", noNAs = FALSE, includeRLMIDind = FALSE, verbose = getOption("longDiSL.verbose")) {
  warning("this function is deprecated")

  # dataDT <- data.table(dataDT)
  dataDT <- copy(dataDT) # Making sure nothing gets modified by reference:

  # Define which observations are in the hold-out set (and thus should be ignored when creating predictors for training set)
  if (train_set && holdout) {
    # non_hold_idx <- !OData$dat.sVar[[hold_column]]
    non_hold_idx <- !dataDT[[hold_column]]
  } else {
    # to define (Y.lt, Y.rt, lt, rt, l.obs, mid.obs, r.obs) for validation data points we use the entire observed data
    non_hold_idx <- rep.int(TRUE, nrow(dataDT))
  }

  # ---------------------------------------------------------------------------------------
  # When both train_set and holdout are TRUE the features / summaries will be defined for the training data (!holdout) only, after dropping the holdout observations
  # When either train_set or holdout are FALSE then all observations will be used to define features / predictors.
  #   - The predictors below turn out to be equivalent when defining validation data points as well as when training on ALL data
  #   - This allows us to use every single observation as a test point for CV.MSE
  # ---------------------------------------------------------------------------------------
  # dataDT[, c("lt", "rt", "Y.lt", "Y.rt", "l.obs", "mid.obs", "r.obs") := list(NULL, NULL, NULL, NULL, NULL, NULL, NULL)]
  # dataDT[, c("meanY", "sumYsq") := list(NULL, NULL)]
  # 1. Left Y[lt], lt, if exists (otherwise lt = rt)
  # 2. Right Y[rt], rt, if exists (otherwise rt = lt)
  dataDT[non_hold_idx, c("lt", "rt") := list(shift(eval(as.name(nodes$tnode)), type = "lag"), shift(eval(as.name(nodes$tnode)), type = "lead")), by = eval(nodes$IDnode)]
  dataDT[non_hold_idx, c("Y.lt", "Y.rt") := list(shift(eval(as.name(nodes$Ynode)), type = "lag", fill = NA), shift(eval(as.name(nodes$Ynode)), type = "lead", fill = NA)), by = eval(nodes$IDnode)]

  if (includeRLMIDind) {
    # Add dummy indicator column(s) of being left-most (l.obs) / middle (mid.obs) / right-most (r.obs) observation:
    dataDT[non_hold_idx, c("l.obs", "mid.obs", "r.obs"):= list(0L, 0L, 0L)]
    dataDT[non_hold_idx & is.na(lt), l.obs := 1L]
    dataDT[non_hold_idx & !is.na(lt) & !is.na(rt), mid.obs := 1L]
    dataDT[non_hold_idx & is.na(rt), r.obs := 1L]
  }

  if (noNAs) {
    ##  If left (Y[lt], lt) does't exist then (Y[lt], lt) = (Y[rt], rt);
    dataDT[non_hold_idx & is.na(Y.lt), Y.lt := Y.rt]
    dataDT[non_hold_idx & is.na(lt),  lt  := rt]
    ##  If left (Y[rt], rt) does't exist then (Y[rt], rt) = (Y[lt], lt);
    dataDT[non_hold_idx & is.na(Y.rt), Y.rt := Y.lt]
    dataDT[non_hold_idx & is.na(rt), rt := lt]
  }

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