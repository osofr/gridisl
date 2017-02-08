## ---------------------------------------------------------------------
## R6 class that defines a learner / model for fitting E(Y|W)
## This R6 class defines fields and methods that controls all the parameters of a regression model / machine learning algorithm.

RegressionClass <- R6Class("RegressionClass",
  class = TRUE,
  portable = TRUE,
  public = list(
    Model_idx = 1,
    ReplMisVal0 = TRUE,            # if TRUE all gvars$misval among predicators are replaced with with gvars$misXreplace (0)
    # fit.package = character(),
    # fit.algorithm = character(),
    outvar = character(),          # vector of regression outcome variable names
    predvars = character(),        # vector of predictor names
    runCV = FALSE,
    fold_column = NULL,
    subset_vars = list(),          # Named LIST for subset vars, one list item per outcome in outvar, each list item can be a character vector.
                                   # Later these are tested for missing values, which forms the basis of the logical subset vector)
    subset_exprs = list(),         # Named LIST of subset expressions (as strings), one list item per outcome in outvar.
                                   # Each item is a vector of different subsetting expressions (form stratified models)
                                   # These expressions are evaluated in the envir of the data, must evaluate to a logical vector
    estimator = character(),
    model_contrl = list(),
    initialize = function(Model_idx = 1,
                          ReplMisVal0 = TRUE,
                          outvar,
                          predvars,
                          runCV = FALSE,
                          fold_column = NULL,
                          subset_vars = NULL,
                          subset_exprs = NULL,
                          estimator = NULL,
                          model_contrl = list()) {

      ## Confirm the args are of the same type as the initials
      self$ReplMisVal0 <- ReplMisVal0
      self$Model_idx <- Model_idx
      # self$fit.package <- fit.package
      # self$fit.algorithm <- fit.algorithm

      assert_that(is.character(outvar))
      assert_that(is.character(predvars) || is.null(predvars))

      self$outvar <- outvar
      self$predvars <- predvars

      self$runCV <- runCV
      self$fold_column <- fold_column

      if (!is.null(subset_vars)) {
        self$subset_vars <- self$checkInputList(subset_vars)
      } else {
        self$subset_vars <- lapply(self$outvar, function(var) {var})
        names(self$subset_vars) <- self$outvar
      }

      if (!is.null(subset_exprs)) {
        self$subset_exprs <- self$checkInputList(subset_exprs)
      } else {
        self$subset_exprs <- lapply(self$outvar, function(var) {NULL})
        names(self$subset_exprs) <- self$outvar
      }

      assert_that(is.list(model_contrl))
      if (length(model_contrl)>0) {
        if (!is.ModelStack(model_contrl) && (any(is.null(names(model_contrl))) || any(names(model_contrl) %in% ""))) stop("all items in list 'model_contrl' must be named")
      }
      self$estimator <- estimator
      self$model_contrl <- model_contrl

      return(self)
    },

    show = function() {
      str(self$get.reg)
      return(invisible(self$get.reg))
    },

    checkInputList = function(inputlist) {
      assert_that(is.list(inputlist))
      assert_that(length(inputlist) == length(self$outvar))
      assert_that(all(names(inputlist) %in% self$outvar))
      return(inputlist)
    }
  ),
  active = list(
    get.reg = function() {
      list(outvar = self$outvar,
           predvars = self$predvars,
           subset_vars = self$subset_vars,
           subset_exprs = self$subset_exprs,
           runCV = self$runCV,
           fold_column = self$fold_column,
           model_contrl = self$model_contrl
           )
    }
  )
)

