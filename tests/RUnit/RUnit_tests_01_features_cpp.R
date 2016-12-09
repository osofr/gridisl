## ------------------------------------------------------------------------------------
## Features of the curve for training / validation sets (including random holdout and CV validation)
## ------------------------------------------------------------------------------------
test.holdout.features <- function() {
  options(growthcurveSL.verbose = TRUE)
  data(cpp)
  cpp <- cpp[!is.na(cpp[, "haz"]), ]
  covars <- c("apgar1", "apgar5", "parity", "gagebrth", "mage", "meducyrs", "sexn")
  head(cpp)
  nodes <- list(Lnodes = covars, Ynode = "haz", IDnode = "subjid", tnode = "agedays")

  ## add holdout indicator column
  cpp_holdout <- add_holdout_ind(data = cpp, ID = "subjid", hold_column = "hold", random = TRUE, seed = 12345)
  ## define CV folds (respecting that multiple observations per subject must fall within the same fold)
  cpp_folds <- add_CVfolds_ind(data = cpp, ID = "subjid", nfolds = 5, seed = 23)

  ## ------------------------------------------------------------------------------------
  ## Define features using all data points as a full training set (no holdouts, summaries use all obs)
  ## ------------------------------------------------------------------------------------
  cpp_all_train <- define_features(cpp_holdout, nodes = nodes, train_set = TRUE, holdout = FALSE)
  cpp_all_train2 <- define_features_drop(cpp_holdout, ID = "subjid", t_name = "agedays", y = "haz", train_set = TRUE)
  checkTrue(all.equal(cpp_all_train, cpp_all_train2))

  ## ------------------------------------------------------------------------------------
  ## Define features for training rows excluding holdouts (summaries defined based on training points only while dropping the holdout observations)
  ## ------------------------------------------------------------------------------------
  ## TO DO: Might exclude holdout rows from final dataset, since their Y.lt / Y.rt are undefined anyways
  cpp_all_train_hold <- define_features(cpp_holdout, nodes = nodes, train_set = TRUE, holdout = TRUE)
  cpp_all_train_hold <- cpp_all_train_hold[hold != TRUE, ]
  ## By spec holdcolumn name we automatically exclude holdout rows
  cpp_all_train_hold2 <- define_features_drop(cpp_holdout, ID = "subjid", t_name = "agedays", y = "haz", train_set = TRUE, hold_column = "hold")
  checkTrue(all.equal(cpp_all_train, cpp_all_train2))

  ## ------------------------------------------------------------------------------------
  ## Define the validation data using ALL rows (e.g., for scoring with CV)
  ## These summaries / features are defined for each row data point (X_i,Y_i)
  ## by first dropping (X_i,Y_i) and then evaluating the summaries for (X_i,Y_i) based on the remaining observations.
  ## This process is repeated in a loop for all person-time rows in the data.
  ## ------------------------------------------------------------------------------------
  valid_data_all <- define_features(cpp_holdout, nodes, train_set = FALSE, holdout = FALSE)
  valid_data_all2 <- define_features_drop(cpp_holdout, ID = "subjid", t_name = "agedays", y = "haz", train_set = FALSE)
  checkTrue(all.equal(valid_data_all, valid_data_all2))

  valid_data_holdonly_test <- valid_data_all[hold == TRUE, ]
  valid_data_holdonly <- define_features_drop(cpp_holdout, ID = "subjid", t_name = "agedays", y = "haz", train_set = FALSE, hold_column = "hold")
  checkTrue(all.equal(valid_data_holdonly_test, valid_data_holdonly))
}


test.holdout.features.grid <- function() {
  valid_data_holdonly <- define_features_drop(cpp_holdout, ID = "subjid", t_name = "agedays", y = "haz", train_set = FALSE, hold_column = "hold")

  cpp_all_train2 <- define_features_drop(cpp_holdout, ID = "subjid", t_name = "agedays", y = "haz", train_set = TRUE)

define_features_drop
}



