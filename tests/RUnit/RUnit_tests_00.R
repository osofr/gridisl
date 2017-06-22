### --- Test setup ---

if(FALSE) {
  # library("RUnit")
  # library("roxygen2")
  library("devtools")
  setwd(".."); setwd(".."); getwd()
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:

  setwd("..");
  install("gridisl", build_vignettes = FALSE, dependencies = FALSE) # INSTALL W/ devtools:
  library("gridisl")
  # system("echo $PATH") # see the current path env var
  # system("R CMD Rd2pdf gridisl")  # just create the pdf manual from help files
  # CHECK AND BUILD PACKAGE:
  getwd()
  # setwd("./gridisl"); setwd(".."); getwd()
  devtools::check() # runs full check
  devtools::check(args = c("--no-vignettes"), build_args = c("--no-build-vignettes")) # runs faster
  devtools::build_win(args = "--compact-vignettes") # build package on CRAN servers (windows os?)
  devtools::build(args = "--compact-vignettes") # build package tarball compacting vignettes

  # check reverse dependen cies:
  devtools::revdep(dependencies = c("Depends", "Imports", "Suggests", "LinkingTo"), recursive = FALSE, ignore = NULL)
  res <- devtools::revdep_check()
  devtools::revdep_check_summary(res)
  # revdep_check_save_logs(res)

  # devtools::install_github('osofr/gridisl', build_vignettes = FALSE, dependencies = FALSE)

  setwd("..")
}

psi_RDs_DAG2a <- NULL
psi_RDs_DAG2b <- NULL


test.learners <- function() {
  checkException(
    params <- gridisl::defModel(estimator = "xgboost__gbm", family = "quasibinomial", nthread = 1,
                               nrounds = 500, early_stopping_rounds = 10,
                               learning_rate = 0.1, # learning_rate = 0.01,
                               param_grid = c(
                                colsample_bytree = c(0.3, 0.5, 0.7, 0.9, 1)
                                )
                            )
    )

  checkException(
    params <- gridisl::defModel(estimator = "xgboost__gbm", family = "quasibinomial", nthread = 1,
                               nrounds = 500, early_stopping_rounds = 10,
                               learning_rate = 0.1, # learning_rate = 0.01,
                               param_grid = list(
                                colsample_bytree = c(0.3, 0.5, 0.7, 0.9, 1),
                                c(0:10)
                                )
                            )
  )

  checkException(
    params <- gridisl::defModel(estimator = "xgboost__gbm", family = "quasibinomial", nthread = 1,
                               nrounds = 500, early_stopping_rounds = 10,
                               learning_rate = 0.1, # learning_rate = 0.01,
                               param_grid = list(
                                c(0.3, 0.5, 0.7, 0.9, 1),
                                c(0:10)
                                )
                            )
  )

}


sample_checks <- function() {   # doesnt run, this is just to show what test functions can be used
  print("Starting tests...")
  checkTrue(1 < 2, "check1")     ## passes fine
  ## checkTrue(1 > 2, "check2")  ## appears as failure in the test protocol
  v <- 1:3
  w <- 1:3
  checkEquals(v, w)               ## passes fine
  names(v) <- c("A", "B", "C")
  ## checkEquals(v, w)            ## fails because v and w have different names
  checkEqualsNumeric(v, w)        ## passes fine because names are ignored
  x <- rep(1:12, 2)
  y <- rep(0:1, 12)
  res <- list(a=1:3, b=letters, LM=lm(y ~ x))
  res2 <- list(a=seq(1,3,by=1), b=letters, LM=lm(y ~ x))
  checkEquals( res, res2)        ## passes fine
  checkIdentical( res, res)
  checkIdentical( res2, res2)
  ## checkIdentical( res, res2)  ## fails because element 'a' differs in type
  fun <- function(x) {
   if(x)
   {
    stop("stop conditions signaled")
   }
   return()
  }
  checkException(fun(TRUE))      ## passes fine
  ## checkException(fun(FALSE))  ## failure, because fun raises no error
  checkException(fun(TRUE), silent=TRUE)
  ##  special constants
  ##  same behaviour as for underlying base functions
  checkEquals(NA, NA)
  checkEquals(NaN, NaN)
  checkEquals(Inf, Inf)
  checkIdentical(NA, NA)
  checkIdentical(NaN, NaN)
  checkIdentical(-Inf, -Inf)
}
`%+%` <- function(a, b) paste0(a, b)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
allNA = function(x) all(is.na(x))

