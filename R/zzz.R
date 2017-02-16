options("datatable.print.nrows" = 20)
options("datatable.print.class" = TRUE)

#-----------------------------------------------------------------------------
# Global State Vars (can be controlled globally with options(gridisl.optname = ))
#-----------------------------------------------------------------------------
gvars <- new.env(parent = emptyenv())
gvars$verbose <- TRUE       # verbose mode (print all messages)
gvars$method <- "none"      # Model selection method used ("cv", "holdout", "none")
gvars$opts <- list()        # named list of package options that is controllable by the user (set_all_gridisl_options())
gvars$misval <- NA_integer_ # the default missing value for observations (# gvars$misval <- -.Machine$integer.max)
gvars$misXreplace <- 0L     # the default replacement value for misval that appear in the design matrix
gvars$tolerr <- 10^-12      # tolerance error: assume for abs(a-b) < gvars$tolerr => a = b
gvars$sVartypes <- list(bin = "binary", cat = "categor", cont = "contin")
gvars$noCENScat <- 0L       # the reference category that designates continuation of follow-up

allowed.fit.package <- c("speedglm", "glm", "h2o", "xgboost", "brokenstick", "face")
allowed.fit.algorithm = c("glm", "gbm", "randomForest", "drf", "deeplearning", "grid", "resid_grid")
allowed.bin.method = c("equal.mass", "equal.len", "dhist")

#' Querying/setting a single \code{gridisl} option
#'
#' To list all \code{gridisl} options, just run this function without any parameters provided. To query only one value, pass the first parameter.
#' To set that, use the \code{value} parameter too.
#'
#'
#' @param o Option name (string).
#' @param value Value to assign (optional)
#' @export
#' @examples \dontrun{
#' gridislOptions()
#' gridislOptions('fit.package')
#' gridislOptions('fit.package', 'h2o')
#' }
gridislOptions <- function (o, value)  {
  res <- getOption("gridisl")
  if (missing(value)) {
    if (missing(o))
        return(res)
    if (o %in% names(res))
        return(res[[o]])
    print("Possible `gridisl` options:")
    print(names(res))
    stop(o %+% ": this options does not exist")
  } else {
    if (!o %in% names(res))
      stop(paste("Invalid option name:", o))
    if (is.null(value)) {
      res[o] <- list(NULL)
    }
    else {
      res[[o]] <- value
    }
    # options(gridisl = res)
    do.call("set_all_gridisl_options", res)
  }
}

# getopt <- function(optname) {
#   return(gridislOptions(o = optname))
#   # opt <- gvars$opts
#   # if (!(optname %in% (names(opt)))) stop(optname %+% ": this options does not exist")
#   # return(opt[[optname]])
# }

#' Print Current Option Settings for \code{gridisl}
#' @return Invisibly returns a list of \code{gridisl} options.
#' @seealso \code{\link{set_all_gridisl_options}}
#' @export
print_gridisl_opts <- function() {
  print(gvars$opts)
  invisible(gvars$opts)
}

#' Setting \code{gridisl} Options
#'
#' Options that control \code{gridisl} package.
#' \strong{Will reset all unspecified options (omitted arguments) to their default values}.
#' The preferred way to set options for \code{gridisl} is to use \code{\link{gridislOptions}}, which allows specifying individual options without having to reset all other options.
#' To reset all options to their defaults simply run \code{set_all_gridisl_options()} without any parameters/arguments.
#' @param fit.package Specify the default package for performing model fitting: c("speedglm", "glm", "h2o")
#' @param fit.algorithm Specify the default fitting algorithm: c("glm", "gbm", "randomForest", "SuperLearner")
#' @return Invisibly returns a list with old option settings.
#' @seealso \code{\link{gridislOptions}}, \code{\link{print_gridisl_opts}}
#' @export
set_all_gridisl_options <- function(fit.package = c("speedglm", "glm", "h2o", "xgboost", "brokenstick", "face"),
                                    fit.algorithm = c("glm", "gbm", "randomForest", "drf", "deeplearning", "grid", "resid_grid")) {

  old.opts <- gvars$opts
  fit.package <- fit.package[1L]
  fit.algorithm <- fit.algorithm[1L]
  if (!(fit.package %in% allowed.fit.package)) stop("fit.package must be one of: " %+% paste0(allowed.fit.package, collapse=", "))
  if (!(fit.algorithm %in% allowed.fit.algorithm)) stop("fit.algorithm must be one of: " %+% paste0(allowed.fit.algorithm, collapse=", "))

  opts <- list(
    fit.package = fit.package,
    fit.algorithm = fit.algorithm
  )

  gvars$opts <- opts
  options(gridisl = opts)
  invisible(old.opts)
}


# returns a function (alternatively a call) that tests for missing values in (sA, sW)
testmisfun <- function() {
  if (is.na(gvars$misval)) {
    return(is.na)
  } else if (is.null(gvars$misval)){
    return(is.null)
  } else if (is.integer(gvars$misval)) {
    return(function(x) {x==gvars$misval})
  } else {
    return(function(x) {x%in%gvars$misval})
  }
}

get.misval <- function() {
  gvars$misfun <- testmisfun()
  gvars$misval
}

set.misval <- function(gvars, newmisval) {
  oldmisval <- gvars$misval
  gvars$misval <- newmisval
  gvars$misfun <- testmisfun()    # EVERYTIME gvars$misval HAS CHANGED THIS NEEDS TO BE RESET/RERUN.
  invisible(oldmisval)
}
gvars$misfun <- testmisfun()

# Allows gridisl functions to use e.g., getOption("gridisl.verbose") to get verbose printing status
.onLoad <- function(libname, pkgname) {
  # reset all options to their defaults on load:
  set_all_gridisl_options()
  op <- options()
  op.gridisl <- list(
    gridisl.verbose = gvars$verbose,
    gridisl.file.path = tempdir(),
    gridisl.temp.dir = tempdir(),
    # gridisl.file.name = 'gridisl-report-%T-%N-%n'
    gridisl.file.name = 'gridisl-report-'%+%Sys.Date()
  )
  toset <- !(names(op.gridisl) %in% names(op))
  if (any(toset)) options(op.gridisl[toset])
  invisible()
}

# Runs when attached to search() path such as by library() or require()
.onAttach <- function(...) {
  if (interactive()) {
  	packageStartupMessage('gridisl')
  	# packageStartupMessage('Version: ', utils::packageDescription('gridisl')$Version)
  	packageStartupMessage('Version: ', utils::packageDescription('gridisl')$Version, '\n')
  	packageStartupMessage('Please note this package is still in its early stages of development. Check for updates and report bugs at http://github.com/osofr/gridisl.', '\n')
  	# packageStartupMessage('To see the vignette use vignette("gridisl_vignette", package="gridisl"). To see all available package documentation use help(package = "gridisl") and ?gridisl.', '\n')
  	# packageStartupMessage('To see the latest updates for this version, use news(package = "gridisl").', '\n')
  }
}