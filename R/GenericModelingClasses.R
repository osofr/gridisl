#----------------------------------------------------------------------------------
# Classes that control modelling of the multivariate joint probability model P(sA|sW).
# NOT USED
#----------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# S3 constructors for the summary model classes:
# ---------------------------------------------------------------------------------
# newsummarymodel <- function(regClass, reg, DataStorageClass.g0, ...) { UseMethod("newsummarymodel") }
# # Summary model constructor for generic regression with multivariate outcome, but one set of predictors
# newsummarymodel.generic <- function(regClass, reg, DataStorageClass.g0, ...) GenericModel$new(reg = reg, DataStorageClass.g0 = DataStorageClass.g0, ...)
# # Summary model constructor for continuous outcome sA[j]:
# newsummarymodel.contin <- function(regClass, reg, DataStorageClass.g0, ...) ContinModel$new(reg = reg, DataStorageClass.g0 = DataStorageClass.g0, ...)
# # Summary model constructor for categorical outcome sA[j]:
# newsummarymodel.categor <- function(regClass, reg, DataStorageClass.g0, ...) CategorModel$new(reg = reg, DataStorageClass.g0 = DataStorageClass.g0, ...)
# # Summary model constructor for stratification (by reg$subset_exprs):
# newsummarymodel.stratify <- function(regClass, reg, DataStorageClass.g0, ...) StratifiedModel$new(reg = reg, DataStorageClass.g0 = DataStorageClass.g0, ...)
# # Summary model constructor for binary outcome sA[j]:
# newsummarymodel.binary <- function(regClass, reg, ...) BinaryOutcomeModel$new(reg = reg, ...)
# # Summary model constructor for Q-learning (sequential regression):
# newsummarymodel.Qlearn <- function(regClass, reg, ...) QlearnModel$new(reg = reg, ...)
# # For evaluating propensity scores under g.star (counterfactual probabilities)
# newsummarymodel.deterministic <- function(regClass, reg, ...) DeterministicBinaryOutcomeModel$new(reg = reg, ...)
