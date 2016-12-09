# ---------------------------------------------------------------------------------------
#' Define features (predictors) for training or testing (validation) data
#'
#' @param dataDT Input data.table
#' @param nodes ...
#' @param train_set ...
#' @param holdout ...
#' @param hold_column ...
#' @param noNAs ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @export
#' @return ...
define_features <- function(dataDT, nodes, train_set = TRUE, holdout = TRUE, hold_column = "hold", noNAs = FALSE, verbose = getOption("growthcurveSL.verbose")) {
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

  # Add dummy indicator column(s) of being left-most (l.obs) / middle (mid.obs) / right-most (r.obs) observation:
  dataDT[non_hold_idx, c("l.obs", "mid.obs", "r.obs"):= list(0L, 0L, 0L)]
  dataDT[non_hold_idx & is.na(lt), l.obs := 1L]
  dataDT[non_hold_idx & !is.na(lt) & !is.na(rt), mid.obs := 1L]
  dataDT[non_hold_idx & is.na(rt), r.obs := 1L]

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
#' @param nodes ...
#' @param train_set Set to \code{TRUE} to define growth curve features and summaries for training data.
#' Set to \code{FALSE} to define the summaries for validation data. In the latter case the summaries are defined for observation (X_i,Y_i) by first dropping that observation
#' and then evaluating the summaries for the remaining observations. This is repeated in a loop for all person-time rows in the data.
#' @param hold_column A column with a logical flag for holdout rows / observations (\code{TRUE} indicates that the row is a holdout).
#' When \code{train_set} is \code{TRUE} the resulting output data will contain all non-HOLDOUT observations (training data points).
#' When \code{train_set} is \code{FALSE} the resulting output data will contain the HOLDOUT observations only (validation data points).
#' To evaluate either training or validation data summaries FOR ALL observations this argument must be missing
#' (in which case all observation from the input data are returned with their corresponding summaries).
#' @param noNAs ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @export
#' @example tests/RUnit/RUnit_tests_01_features_cpp.R
#' @return ...
define_features_drop <- function(dataDT, ID, t_name, y, train_set = TRUE, hold_column, noNAs = FALSE, verbose = getOption("growthcurveSL.verbose")) {
# define_features2 <- function(dataDT, ID, t_name, y, train_set = TRUE, holdout = TRUE, hold_column = "hold", noNAs = FALSE, verbose = getOption("growthcurveSL.verbose")) {
  # dataDT <- data.table(dataDT)
  dataDT <- copy(dataDT) # Making sure nothing gets modified by reference:
  # Define which observations are in the hold-out set (and thus should be ignored when creating predictors for training set)
  if (train_set && !missing(hold_column)) {
    non_hold_idx <- !dataDT[[hold_column]]
    dataDT <- dataDT[non_hold_idx, ]
  } else {
    # to define (Y.lt, Y.rt, lt, rt, l.obs, mid.obs, r.obs) for validation data points we will use the entire observed data
    non_hold_idx <- rep.int(TRUE, nrow(dataDT))
  }

  # ---------------------------------------------------------------------------------------
  # When train_set is TRUE the features / summaries will be defined for the training data only (without holdouts if hold_column is specified),
  # where the holdout observations are ignored (but still kept in the data)
  # When train_set is FALSE then all observations will be used to define features / predictors.
  #   - The predictors below turn out to be equivalent when defining validation data points as well as when training on ALL data
  #   - This allows us to use every single observation as a test point for CV.MSE
  # ---------------------------------------------------------------------------------------
  # dataDT[, c("lt", "rt", "Y.lt", "Y.rt", "l.obs", "mid.obs", "r.obs") := list(NULL, NULL, NULL, NULL, NULL, NULL, NULL)]
  # dataDT[, c("meanY", "sumYsq") := list(NULL, NULL)]
  # 1. Set left (Y[lt], lt) if exists (otherwise NA); 2. Set right (Y[rt], rt) if exists (otherwise NA)
  dataDT[, c("lt", "rt") := list(shift(eval(as.name(t_name)), type = "lag"), shift(eval(as.name(t_name)), type = "lead")), by = eval(ID)]
  dataDT[, c("Y.lt", "Y.rt") := list(shift(eval(as.name(y)), type = "lag", fill = NA), shift(eval(as.name(y)), type = "lead", fill = NA)), by = eval(ID)]

  # Add dummy indicator column(s) of being left-most (l.obs) / middle (mid.obs) / right-most (r.obs) observation:
  dataDT[, c("l.obs", "mid.obs", "r.obs"):= list(0L, 0L, 0L)]
  dataDT[is.na(lt), l.obs := 1L]
  dataDT[!is.na(lt) & !is.na(rt), mid.obs := 1L]
  dataDT[is.na(rt), r.obs := 1L]

  if (noNAs) {
    ##  If left (Y[lt], lt) does't exist then (Y[lt], lt) = (Y[rt], rt);
    dataDT[is.na(Y.lt), Y.lt := Y.rt]
    dataDT[is.na(lt),  lt  := rt]
    ##  If left (Y[rt], rt) does't exist then (Y[rt], rt) = (Y[lt], lt);
    dataDT[is.na(Y.rt), Y.rt := Y.lt]
    dataDT[is.na(rt), rt := lt]
  }

  # browser()

  # t_grid <- c(0, seq(1, 2558,by=100))
  # gridDT <- CJ(unique(dataDT[[ID]]), t_grid)
  # colnames(gridDT) <- c(ID, t_name)
  # sel_vars <- c(ID, t_name, y)
  # dataDT_sel <- dataDT[, sel_vars, with = FALSE]
  # dataDT_sel[, ("t.cp") := eval(as.name(t_name))]
  # setkeyv(gridDT, cols = c(ID, t_name))
  # setkeyv(dataDT_sel, cols = c(ID, t_name))

  # # add left Y.lt & lt with rolling join
  # gridDT <- dataDT_sel[, , with = FALSE][gridDT, roll = Inf]
  # setnames(gridDT, old = c(y, "t.cp"), new = c("Y.lt", "lt"))
  # # add right Y.rt & rt with rolling join
  # gridDT <- dataDT_sel[, , with = FALSE][gridDT, roll = -Inf]
  # setnames(gridDT, old = c(y, "t.cp"), new = c("Y.rt", "rt"))
  # gridDT[1:100, ]
  # dataDT_sel[1:20, ]

  # dataDT_sel <- dataDT[, c(sel_vars, "hold", "lt", "rt", "Y.lt", "Y.rt", "l.obs", "mid.obs", "r.obs"), with = FALSE]
  # datDT_gridX <- merge(datDT_tgrid, datDT_subjID, by = c("agedays", "subjid"))
  # merge(dataDT_sel, gridDT, by = c("subjid", "agedays"), all = TRUE)
  # merge(dataDT_sel, gridDT, by = c("subjid", "agedays"), all.x = TRUE)
  # # by.x = NULL, by.y = NULL,
  # setkeyv(datDT_tgrid, cols = "agedays")
  # dataDT_sel <- dataDT[, c("subjid", "agedays", "haz")]
  # setkeyv(dataDT_sel, cols = c("subjid", "agedays"))
  # datDT_tgrid[dataDT_sel, roll = Inf]
  # dataDT[datDT_tgrid, roll = TRUE, agedays, by = eval(ID)]
  # datDT_tgrid[dataDT, roll = TRUE, agedays, by = eval(ID)]

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
