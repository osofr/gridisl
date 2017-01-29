## ---------------------------------------------------------------------------------
## S3 constructors for new modeling classes:
## ---------------------------------------------------------------------------------
## @export
newFitModel <- function(fit.package, fit.algorithm, reg, ...) { UseMethod("newFitModel") }

## @export
newFitModel.h2o <- function(fit.package, fit.algorithm, reg, useH2Oframe, ...) {
  h2oModelClass$new(fit.algorithm, fit.package, reg, ...)
  if (fit.algorithm %in% "grid") {
    ModelFitObject <- h2oModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, useH2Oframe = useH2Oframe, ...)
  } else if (fit.algorithm %in% "resid_grid") {
    ModelFitObject <- h2oResidualModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, useH2Oframe = useH2Oframe, ...)
  } else {
    ModelFitObject <- h2oModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, useH2Oframe = useH2Oframe, ...)
  }
  return(ModelFitObject)
}

## @export
newFitModel.xgboost <- function(fit.package, fit.algorithm, reg, useDMatrix, ...) XGBoostClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, useDMatrix = useDMatrix, ...)

## @export
newFitModel.glm <- function(fit.package, fit.algorithm, reg, ...) ModelFitObject <- glmModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)

## @export
newFitModel.speedglm <- function(fit.package, fit.algorithm, reg, ...) ModelFitObject <- glmModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)

## @export
newFitModel.face <- function(fit.package, fit.algorithm, reg, ...) faceModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)

## @export
newFitModel.brokenstick <- function(fit.package, fit.algorithm, reg, ...) brokenstickModelClass$new(fit.algorithm = fit.algorithm, fit.package = fit.package, reg = reg, ...)
