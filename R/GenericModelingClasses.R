#----------------------------------------------------------------------------------
# Classes that control modelling of the multivariate joint probability model P(sA|sW).
# NOT USED
#----------------------------------------------------------------------------------

## ---------------------------------------------------------------------------------
## S3 constructors for the modeling classes:
## ---------------------------------------------------------------------------------
newFitModel <- function(fit.algorithm, fit.package, reg, ...) { UseMethod("newFitModel") }
newFitModel.face <- function(fit.algorithm, fit.package, reg, ...) faceModelClass$new(fit.algorithm, fit.package, reg, ...)
newFitModel.brokenstick <- function(fit.algorithm, fit.package, reg, ...) brokenstickModelClass$new(fit.algorithm, fit.package, reg, ...)
newFitModel.xgboost <- function(fit.algorithm, fit.package, reg, ...) xgboostModelClass$new(fit.algorithm, fit.package, reg, ...)
newFitModel.glm <- function(fit.algorithm, fit.package, reg, ...) glmModelClass$new(fit.algorithm, fit.package, reg, ...)
newFitModel.h2o <- function(fit.algorithm, fit.package, reg, ...) h2oModelClass$new(fit.algorithm, fit.package, reg, ...)

# # Summary model constructor for continuous outcome sA[j]:
# newsummarymodel.contin <- function(regClass, reg, DataStorageClass.g0, ...) ContinModel$new(reg = reg, DataStorageClass.g0 = DataStorageClass.g0, ...)
# # Summary model constructor for categorical outcome sA[j]:
# newsummarymodel.categor <- function(regClass, reg, DataStorageClass.g0, ...) CategorModel$new(reg = reg, DataStorageClass.g0 = DataStorageClass.g0, ...)
# # Summary model constructor for stratification (by reg$subset_exprs):
# newsummarymodel.stratify <- function(regClass, reg, DataStorageClass.g0, ...) StratifiedModel$new(reg = reg, DataStorageClass.g0 = DataStorageClass.g0, ...)
# # Summary model constructor for binary outcome sA[j]:
# newsummarymodel.binary <- function(regClass, reg, ...) PredictionModel$new(reg = reg, ...)
# # Summary model constructor for Q-learning (sequential regression):
# newsummarymodel.Qlearn <- function(regClass, reg, ...) QlearnModel$new(reg = reg, ...)
# # For evaluating propensity scores under g.star (counterfactual probabilities)
# newsummarymodel.deterministic <- function(regClass, reg, ...) DeterministicPredictionModel$new(reg = reg, ...)
