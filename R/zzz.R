
#-----------------------------------------------------------------------------
# Global State Vars (can be controlled globally with options(longDiSL.optname = ))
#-----------------------------------------------------------------------------
gvars <- new.env(parent = emptyenv())
gvars$verbose <- FALSE      # verbose mode (print all messages)
gvars$opts <- list()        # named list of package options that is controllable by the user (set_all_longDiSL_options())
gvars$misval <- NA_integer_ # the default missing value for observations (# gvars$misval <- -.Machine$integer.max)
gvars$misXreplace <- 0L     # the default replacement value for misval that appear in the design matrix
gvars$tolerr <- 10^-12      # tolerance error: assume for abs(a-b) < gvars$tolerr => a = b
gvars$sVartypes <- list(bin = "binary", cat = "categor", cont = "contin")
gvars$noCENScat <- 0L       # the reference category that designates continuation of follow-up

allowed.fit.package <- c("face", "brokenstick", "speedglm", "glm", "h2o")
allowed.fit.algorithm = c("face", "brokenstick", "glm", "gbm", "randomForest", "deeplearning", "GridLearner", "ResidGridLearner")
# , "SuperLearner"
allowed.bin.method = c("equal.mass", "equal.len", "dhist")

#' Querying/setting a single \code{longDiSL} option
#'
#' To list all \code{longDiSL} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The arguments of \code{\link{set_all_longDiSL_options}} list all available \code{longDiSL} options.
#'
#' @param o Option name (string). See \code{\link{set_all_longDiSL_options}}.
#' @param value Value to assign (optional)
#' @export
#' @seealso \code{\link{set_all_longDiSL_options}}
#' @examples \dontrun{
#' longDiSLOptions()
#' longDiSLOptions('fit.package')
#' longDiSLOptions('fit.package', 'h2o')
#' }
longDiSLOptions <- function (o, value)  {
  res <- getOption("longDiSL")
  if (missing(value)) {
    if (missing(o))
        return(res)
    if (o %in% names(res))
        return(res[[o]])
    print("Possible `longDiSL` options:")
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
    # options(longDiSL = res)
    do.call("set_all_longDiSL_options", res)
  }
}

getopt <- function(optname) {
  return(longDiSLOptions(o = optname))
  # opt <- gvars$opts
  # if (!(optname %in% (names(opt)))) stop(optname %+% ": this options does not exist")
  # return(opt[[optname]])
}

#' Print Current Option Settings for \code{longDiSL}
#' @return Invisibly returns a list of \code{longDiSL} options.
#' @seealso \code{\link{set_all_longDiSL_options}}
#' @export
print_longDiSL_opts <- function() {
  print(gvars$opts)
  invisible(gvars$opts)
}

#' Setting \code{longDiSL} Options
#'
#' Options that control \code{longDiSL} package.
#' \strong{Will reset all unspecified options (omitted arguments) to their default values}.
#' The preferred way to set options for \code{longDiSL} is to use \code{\link{longDiSLOptions}}, which allows specifying individual options without having to reset all other options.
#' To reset all options to their defaults simply run \code{set_all_longDiSL_options()} without any parameters/arguments.
#' @param fit.package Specify the default package for performing model fitting: c("speedglm", "glm", "h2o")
#' @param fit.algorithm Specify the default fitting algorithm: c("glm", "gbm", "randomForest", "SuperLearner")
#' @param maxncats Max number of unique categories a categorical variable can have. More than these number and it is deemed continuous.
#' @return Invisibly returns a list with old option settings.
#' @seealso \code{\link{longDiSLOptions}}, \code{\link{print_longDiSL_opts}}
#' @export
set_all_longDiSL_options <- function( fit.package = c("h2o", "speedglm", "glm", "brokenstick", "face"),
                                           fit.algorithm = c("glm", "gbm", "randomForest", "deeplearning", "GridLearner"),
                                           maxncats = 20) {
  # , "SuperLearner"

  old.opts <- gvars$opts
  fit.package <- fit.package[1L]
  fit.algorithm <- fit.algorithm[1L]
  if (!(fit.package %in% allowed.fit.package)) stop("fit.package must be one of: " %+% paste0(allowed.fit.package, collapse=", "))
  if (!(fit.algorithm %in% allowed.fit.algorithm)) stop("fit.algorithm must be one of: " %+% paste0(allowed.fit.algorithm, collapse=", "))

  opts <- list(
    fit.package = fit.package,
    fit.algorithm = fit.algorithm,
    maxncats = maxncats
  )
  gvars$opts <- opts
  options(longDiSL = opts)
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

# Allows longDiSL functions to use e.g., getOption("longDiSL.verbose") to get verbose printing status
.onLoad <- function(libname, pkgname) {
  # reset all options to their defaults on load:
  set_all_longDiSL_options()
  op <- options()
  op.longDiSL <- list(
    longDiSL.verbose = gvars$verbose,
    longDiSL.file.path = tempdir(),
    # longDiSL.file.name = 'longDiSL-report-%T-%N-%n'
    longDiSL.file.name = 'longDiSL-report-'%+%Sys.Date()
  )
  toset <- !(names(op.longDiSL) %in% names(op))
  if (any(toset)) options(op.longDiSL[toset])
  invisible()
}

# Runs when attached to search() path such as by library() or require()
.onAttach <- function(...) {
  if (interactive()) {
  	packageStartupMessage('longDiSL')
  	# packageStartupMessage('Version: ', utils::packageDescription('longDiSL')$Version)
  	packageStartupMessage('Version: ', utils::packageDescription('longDiSL')$Version, '\n')
  	packageStartupMessage('Please note this package is still in its early stages of development. Check for updates and report bugs at http://github.com/osofr/longDiSL.', '\n')
  	# packageStartupMessage('To see the vignette use vignette("longDiSL_vignette", package="longDiSL"). To see all available package documentation use help(package = "longDiSL") and ?longDiSL.', '\n')
  	# packageStartupMessage('To see the latest updates for this version, use news(package = "longDiSL").', '\n')
  }
}