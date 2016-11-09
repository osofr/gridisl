
#-----------------------------------------------------------------------------
# Global State Vars (can be controlled globally with options(growthcurveSL.optname = ))
#-----------------------------------------------------------------------------
gvars <- new.env(parent = emptyenv())
gvars$verbose <- FALSE      # verbose mode (print all messages)
gvars$opts <- list()        # named list of package options that is controllable by the user (set_all_growthcurveSL_options())
gvars$misval <- NA_integer_ # the default missing value for observations (# gvars$misval <- -.Machine$integer.max)
gvars$misXreplace <- 0L     # the default replacement value for misval that appear in the design matrix
gvars$tolerr <- 10^-12      # tolerance error: assume for abs(a-b) < gvars$tolerr => a = b
gvars$sVartypes <- list(bin = "binary", cat = "categor", cont = "contin")
gvars$noCENScat <- 0L       # the reference category that designates continuation of follow-up

allowed.fit.package <- c("face", "brokenstick", "speedglm", "glm", "h2o")
allowed.fit.algorithm = c("glm", "gbm", "randomForest", "deeplearning", "SuperLearner", "GridLearner")
allowed.bin.method = c("equal.mass", "equal.len", "dhist")

#' Querying/setting a single \code{growthcurveSL} option
#'
#' To list all \code{growthcurveSL} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The arguments of \code{\link{set_all_growthcurveSL_options}} list all available \code{growthcurveSL} options.
#'
#' @param o Option name (string). See \code{\link{set_all_growthcurveSL_options}}.
#' @param value Value to assign (optional)
#' @export
#' @seealso \code{\link{set_all_growthcurveSL_options}}
#' @examples \dontrun{
#' growthcurveSLOptions()
#' growthcurveSLOptions('fit.package')
#' growthcurveSLOptions('fit.package', 'h2o')
#' }
growthcurveSLOptions <- function (o, value)  {
  res <- getOption("growthcurveSL")
  if (missing(value)) {
    if (missing(o))
        return(res)
    if (o %in% names(res))
        return(res[[o]])
    print("Possible `growthcurveSL` options:")
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
    # options(growthcurveSL = res)
    do.call("set_all_growthcurveSL_options", res)
  }
}

getopt <- function(optname) {
  return(growthcurveSLOptions(o = optname))
  # opt <- gvars$opts
  # if (!(optname %in% (names(opt)))) stop(optname %+% ": this options does not exist")
  # return(opt[[optname]])
}

#' Print Current Option Settings for \code{growthcurveSL}
#' @return Invisibly returns a list of \code{growthcurveSL} options.
#' @seealso \code{\link{set_all_growthcurveSL_options}}
#' @export
print_growthcurveSL_opts <- function() {
  print(gvars$opts)
  invisible(gvars$opts)
}

#' Setting \code{growthcurveSL} Options
#'
#' Options that control \code{growthcurveSL} package.
#' \strong{Will reset all unspecified options (omitted arguments) to their default values}.
#' The preferred way to set options for \code{growthcurveSL} is to use \code{\link{growthcurveSLOptions}}, which allows specifying individual options without having to reset all other options.
#' To reset all options to their defaults simply run \code{set_all_growthcurveSL_options()} without any parameters/arguments.
#' @param fit.package Specify the default package for performing model fitting: c("speedglm", "glm", "h2o")
#' @param fit.algorithm Specify the default fitting algorithm: c("glm", "gbm", "randomForest", "SuperLearner")
#' @param maxncats Max number of unique categories a categorical variable can have. More than these number and it is deemed continuous.
#' @return Invisibly returns a list with old option settings.
#' @seealso \code{\link{growthcurveSLOptions}}, \code{\link{print_growthcurveSL_opts}}
#' @export
set_all_growthcurveSL_options <- function( fit.package = c("h2o", "speedglm", "glm", "brokenstick", "face"),
                                           fit.algorithm = c("glm", "gbm", "randomForest", "deeplearning", "GridLearner", "SuperLearner"),
                                           maxncats = 20) {

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
  options(growthcurveSL = opts)
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

# Allows growthcurveSL functions to use e.g., getOption("growthcurveSL.verbose") to get verbose printing status
.onLoad <- function(libname, pkgname) {
  # reset all options to their defaults on load:
  set_all_growthcurveSL_options()
  op <- options()
  op.growthcurveSL <- list(
    growthcurveSL.verbose = gvars$verbose,
    growthcurveSL.file.path = tempdir(),
    # growthcurveSL.file.name = 'growthcurveSL-report-%T-%N-%n'
    growthcurveSL.file.name = 'growthcurveSL-report-'%+%Sys.Date()
  )
  toset <- !(names(op.growthcurveSL) %in% names(op))
  if (any(toset)) options(op.growthcurveSL[toset])
  invisible()
}

# Runs when attached to search() path such as by library() or require()
.onAttach <- function(...) {
  if (interactive()) {
  	packageStartupMessage('growthcurveSL')
  	# packageStartupMessage('Version: ', utils::packageDescription('growthcurveSL')$Version)
  	packageStartupMessage('Version: ', utils::packageDescription('growthcurveSL')$Version, '\n')
  	packageStartupMessage('Please note this package is still in its early stages of development. Check for updates and report bugs at http://github.com/osofr/growthcurveSL.', '\n')
  	# packageStartupMessage('To see the vignette use vignette("growthcurveSL_vignette", package="growthcurveSL"). To see all available package documentation use help(package = "growthcurveSL") and ?growthcurveSL.', '\n')
  	# packageStartupMessage('To see the latest updates for this version, use news(package = "growthcurveSL").', '\n')
  }
}