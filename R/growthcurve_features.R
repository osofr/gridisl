# ---------------------------------------------------------------------------------------
#' Define features (predictors) for training or testing (validation) data
#'
#' @param dataDT Input data.table
#' @param nodes ...
#' @param train_set ...
#' @param holdout ...
#' @param hold_column ...
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. Turn this on by default using \code{options(growthcurveSL.verbose=TRUE)}.
#' @export
#' @return ...
define_features <- function(dataDT, nodes, train_set = TRUE, holdout = TRUE, hold_column = "hold", verbose = getOption("growthcurveSL.verbose")) {
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

