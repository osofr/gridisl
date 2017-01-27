# @import pander
# @importFrom pander evalsOptions
NULL

# nocov start
#' Open file
#'
#' Tries to open a file with operating system's default program.
#' @param f file (with full path)
#' @references This function is a fork of David Hajage's \code{convert} function: \url{https://github.com/eusebe/ascii/blob/master/R/export.r}
#' @export
openFileInOS <- function(f) {
  if (missing(f)) {
    stop('No file to open!')
  }
  f <- path.expand(f)
  if (!file.exists(f)) {
    stop('File not found!')
  }
  if (grepl('w|W', .Platform$OS.type)) {
    # we are on Windows
    shell.exec(f) #nolint
  } else {
    if (grepl('darwin', version$os)) {
      # Mac
      system(paste(shQuote('open'), shQuote(f)), wait = FALSE, ignore.stderr = TRUE)
    } else {
      # Linux-like
      system(paste(shQuote('/usr/bin/xdg-open'), shQuote(f)), #nolint
             wait = FALSE,
             ignore.stdout = TRUE)
    }
  }
}
# nocov end

# ---------------------------------------------------------------------------------------------
#' Generate report(s) with modeling stats using pandoc.
#'
#' @param modelfit Model fit object returned by model fitting functions.
#' @param data Input dataset used for model fitting (optional).
#' @param K The number of top performing models for which to provide performance measures. Defaults to 5 or the total number of models
#' fit, whichever is smaller.
#' @param format Choose the Pandoc output format for the report file (html, pdf or word).
#' Note that the html report file is always produced in addition to any other selected format.
#' @param file.name File name for the report file without extension. Default file name is assigned based on the current date.
#' @param file.path Directory path where the report file(s) should be written. Default is to use the system temporary directory.
#' @param openFile Open the report file with OS default viewer?
#' @param keep_md Keep the source .md files?
#' @param keep_tex Keep the source .tex files for pdf output?
#' @param ... Additional arguments may specify the report title (\code{author}), author (\code{title}).
#' Specifying the logical flag \code{only.coefs=TRUE} disables printing of all h2o-specific model summaries.
# Additional set of arguments control the survival plotting, these are passed on to the function \code{f_plot_survest}:
# \code{t_int_sel}, \code{y_lab}, \code{x_lab}, \code{miny}, \code{x_legend}, \code{y_legend}.
#' @return String specifying the path to the main report file.
#' @export
make_model_report <- function(modelfit, data, K = 5, format = c("html", "pdf", "word"),
                            file.name = getOption('gridisl.file.name'), file.path = getOption('gridisl.file.path'),
                            openFile = TRUE, keep_md = FALSE, keep_tex = FALSE, ...) {
  optArgReport <- list(...)

  if (is.list(modelfit) && ("modelfit" %in% names(modelfit))) modelfit <- modelfit$modelfit

  if (!rmarkdown::pandoc_available(version = "1.12.3"))
    stop(
"Report functionality requires pandoc (version 1.12.3 or higher).
Please install it.
For more information, go to: http://pandoc.org/installing.html",
call. = FALSE)

  if ("author" %in% names(optArgReport)) {
    author <- optArgReport[['author']]
    assert_that(is.character(author))
  } else {
    author <- "Insert Author"
  }
  if ("title" %in% names(optArgReport)) {
    title <- optArgReport[['title']]
    assert_that(is.character(author))
  } else {
    title <- "Growth Curve Modeling Report"
  }

  if ("only.coefs" %in% names(optArgReport)) {
    only.coefs <- optArgReport[['only.coefs']]
    assert_that(is.logical(only.coefs))
  } else {
    only.coefs <- FALSE
  }
  if ("skip.modelfits" %in% names(optArgReport)) {
    skip.modelfits <- optArgReport[['skip.modelfits']]
    assert_that(is.logical(skip.modelfits))
  } else {
    skip.modelfits <- FALSE
  }

  if ("print_all_fits" %in% names(optArgReport)) {
    print_all_fits <- optArgReport[['print_all_fits']]
    assert_that(is.logical(print_all_fits))
  } else {
    print_all_fits <- FALSE
  }

  # -------------------------------------------------------------------------------------
  # Input data
  # -------------------------------------------------------------------------------------
  nodes <- modelfit$nodes
  if (!missing(data)) {
    nobs <- nrow(data)
    nuniqueIDs <- length(unique(data[[nodes$IDnode]]))
    nuniquets <- length(unique(data[[nodes$tnode]]))
  } else {
    nobs <- NA
    nuniqueIDs <- NA
    nuniquets <- NA
  }

  # -------------------------------------------------------------------------------------
  # MODEL FITS:
  # -------------------------------------------------------------------------------------
  # fitted.model <- modelfit$getfit
  # class(modelfit)
  # length(fitted.model)
  # str(fitted.model)
  # class(fitted.model)

  # fitted.model <- OData$modelfit$get.fits()
  # fitted.coefs.gA <- OData$modelfit.gA$get.fits()
  # fitted.coefs.gN <- OData$modelfit.gN$get.fits()

  # -------------------------------------------------------------------------------------
  ## path issue on Windows
  file.path     <- gsub('\\', '/', file.path, fixed = TRUE)
  # find the full path to the report template:
  report.file <- system.file('report', "report-script-rmd.R", package = 'gridisl')

  ## set working directory where to write the report:
  opts.bak <- options() # backup options
  wd.bak   <- getwd()
  setwd(file.path)

  format <- format[1L]
  format_pandoc <- format %+% "_document"
  outfile <- file.name %+% "." %+% ifelse(format %in% "word", "docx", format)

  message("writing report to directory: " %+% getwd())
  figure.dir <- file.path(getwd(), "figure/gridisl-")
  report.html <- tryCatch(rmarkdown::render(report.file,
                          output_dir = getwd(), intermediates_dir = getwd(), output_file = file.name%+%".html", clean = TRUE,
                          output_options = list(keep_md = keep_md, toc = TRUE, toc_float = TRUE,
                                                number_sections = TRUE, fig_caption = TRUE,
                                                # mathjax = "local", self_contained = FALSE)
                                                md_extensions = "+escaped_line_breaks")
                          ),
                  error = function(e) e)

  if (inherits(report.html, 'error')) {
    options(opts.bak)
    setwd(wd.bak)
    stop(report.html$message)
  }

  if (!format %in% "html"){
      if (format %in% "pdf") {
        output_options <- list(keep_tex = keep_tex, toc = TRUE, number_sections = TRUE, fig_caption = TRUE,
                               md_extensions = "+escaped_line_breaks")
        # output_options <- list(keep_tex = TRUE, pandoc = list(arg="+escaped_line_breaks"))
        # output_options <- list(keep_tex = TRUE, pandoc = list(arg="markdown+escaped_line_breaks"))
      } else {
        output_options <- NULL
      }
    report.other <- tryCatch(rmarkdown::render(report.file,
                            output_dir = getwd(), intermediates_dir = getwd(), output_file = outfile,
                            output_format = format_pandoc, clean = TRUE,
                            output_options = output_options),
                    error = function(e) e)
    if (inherits(report.other, 'error')) {
      options(opts.bak)
      setwd(wd.bak)
      stop(report.other$message)
    }
  }

  if (openFile) openFileInOS(outfile)

  # resetting directory and other options
  options(opts.bak)
  setwd(wd.bak)
  return(file.path(file.path, outfile))
}
