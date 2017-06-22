is.integerish <- function (x) {
  suppressWarnings(
    is.integer(x[!is.na(x)]) ||
    (is.numeric(x) && all(x[!is.na(x)] == as.integer(x[!is.na(x)]))) ||
    (is.character(x) && x[!is.na(x)] == as.integer(x[!is.na(x)]))
  )
}

is.numericish <- function (x) {
  suppressWarnings(
    is.numeric(x[!is.na(x)]) ||
    (is.character(x) && x[!is.na(x)] == as.numeric(x[!is.na(x)])) ||
    (is.character(x) && all(!is.na(as.numeric(x[!is.na(x)]))))
  )
}

recover_true_type <- function(data, check_type_f, want_type_f, make_type_f, verbose = FALSE) {
  ## these columns are really integers/numerics, but are coded as character
  update_cols_idx <- unlist(lapply(data, check_type_f))
  update_cols <- update_cols_idx[(update_cols_idx & !(unlist(lapply(data, want_type_f)))) %in% TRUE]
  update_cols <- names(update_cols)
  if (length(update_cols) > 0) {
    if (verbose) cat("\nchanging the type of the following columns from 'character' to 'integer' or 'numeric':", paste(update_cols, collapse=","),"\n")
    data <- make_type_f(data, update_cols)
  }
  return(data)
}

#' Convert specific columns in \code{vars} to numeric
#'
#' @param data Input dataset, as \code{data.table}.
#' @param vars Column name(s) that should be converted to numeric type.
#' @export
as.int <- function(data, vars) {
  for (var in vars)
    data[, (var) := as.numeric(get(var))]
  return(data)
}

#' Convert specific columns in \code{vars} to integers
#'
#' @param data Input dataset, as \code{data.table}.
#' @param vars Column name(s) that should be converted to numeric type.
#' @export
as.num <- function(data, vars) {
  for (var in vars)
    data[, (var) := as.integer(get(var))]
  return(data)
}

#' Wrapper for several data processing functions.
#'
#' Clean up the input data by dropping observations with missing outcomes \code{OUTCOME},
#' convert desired columns into numerics (\code{vars_to_numeric}),
#' convert all logical columns into binary integers
#' convert all character columns into factors
#' convert all factors into integers,
#' by defining additional dummy variables for every factor with > 2 levels.
#'
#' @param data Input dataset, can be a \code{data.frame} or a \code{data.table}.
#' @param OUTCOME Character name of the column of outcomes.
#' @param vars_to_numeric Column name(s) that should be converted to numeric type.
#' @param vars_to_int Column name(s) that should be converted to integer type.
#' @param skip_vars These columns will not be converted into other types
#' @param drop_vars Drop the variables in skip_vars and drop the factor variables that were re-coded into binary dummies from the resulting analytic dataset?
#' @param verbose Print processing info
#' @export
prepare_data <- function(data, OUTCOME, vars_to_numeric, vars_to_int, skip_vars, drop_vars = FALSE, verbose = FALSE) {
  data <- data.table::data.table(data)
  data <- drop_NA_y(data, OUTCOME)

  ## convert all non-integers, non-characters & non-numerics to numeric type by default
  unknown_types <- unlist(lapply(data, function(x) !is.numeric(x) && !is.integer(x) && !is.character(x)))
  vars_to_num <- names(unknown_types)[unknown_types]
  if (!missing(vars_to_numeric)) vars_to_num <- unique(c(vars_to_num, vars_to_numeric))

  if (length(vars_to_num) > 0) data <- as.num(data, vars_to_num)

  if (!missing(vars_to_int)) data <- as.int(data, vars_to_int)

  data <- recover_true_type(data, is.integerish, is.integer, as.int, verbose)
  data <- recover_true_type(data, is.numericish, is.numeric, as.num, verbose)

  data <- logical_to_int(data, skip_vars)
  data <- char_to_factor(data, skip_vars)
  data <- factor_to_dummy(data, skip_vars, drop_vars, verbose)

  if (verbose) {
    cat("\ndefined the following new dummy columns:\n")
    print(unlist(attributes(data)$new.factor.names))
  }

  if (drop_vars && !missing(skip_vars)) {
    really_skip_vars <- names(data)[names(data) %in% skip_vars]
    if (verbose) cat("\ndropping the following columns: ", paste(really_skip_vars, collapse=","),"\n")
    for (var in really_skip_vars) set(data, j = var, value = NULL)
  }

  return(data)
}


#' Drop all observation rows with missing outcomes
#'
#' @param data Input dataset, as \code{data.table}.
#' @param OUTCOME Character name of the column of outcomes.
#' @export
drop_NA_y <- function(data, OUTCOME) data[!is.na(data[[OUTCOME]]),]

## generic function for converting from one column type to another
fromtype_totype <- function(data, fromtypefun, totypefun, skip_vars) {
  assert_that(data.table::is.data.table(data))
  # Convert all logical vars to binary integers
  vars <- unlist(lapply(data, fromtypefun))
  vars <- names(vars)[vars]
  if (!missing(skip_vars) && length(vars) > 0) {
    assert_that(is.character(skip_vars))
    vars <- vars[!(vars %in% skip_vars)]
  }
  for (varnm in vars) {
    data[,(varnm) := totypefun(get(varnm))]
  }
  return(data)
}

#' Convert logical covariates to integers
#' @param data Input dataset, as \code{data.table}.
#' @param skip_vars These columns will not be converted to integer
#' @export
logical_to_int <- function(data, skip_vars) fromtype_totype(data, is.logical, as.integer, skip_vars)

#' Convert all character columns to factors
#'
#' @param data Input dataset, as \code{data.table}.
#' @param skip_vars These columns will not be converted to factor
#' @export
char_to_factor <- function(data, skip_vars) fromtype_totype(data, is.character, as.factor, skip_vars)

#' Convert factors to binary indicators, for factors with > 2 levels drop the first factor level and define several dummy variables for the rest of the levels
#' @param data Input dataset, as \code{data.table}.
#' @param skip_vars These columns will not be converted
#' @param drop_factor Drop the factor variables that were re-coded into binary dummies from the resulting analytic dataset?
#' @param verbose  Print processing info
#' @export
factor_to_dummy <- function(data, skip_vars, drop_factor = FALSE, verbose = FALSE) {
  # verbose <- gvars$verbose

  # Create dummies for each factor in the data
  factor.Ls <- as.character(CheckExistFactors(data))
  if (!missing(skip_vars) && length(factor.Ls) > 0) {
    assert_that(is.character(skip_vars))
    factor.Ls <- factor.Ls[!(factor.Ls %in% skip_vars)]
  }

  new.factor.names <- vector(mode="list", length=length(factor.Ls))
  names(new.factor.names) <- factor.Ls
  if (length(factor.Ls)>0 && verbose)
    message("...converting the following factor(s) to binary dummies (and droping the first factor levels): " %+% paste0(factor.Ls, collapse=","))
  for (factor.varnm in factor.Ls) {
    factor.levs <- levels(data[[factor.varnm]])

    ## only define new dummies for factors with > 2 levels
    if (length(factor.levs) > 2) {
      factor.levs <- factor.levs[-1] # remove the first level (reference class)
      factor.levs.code <- seq_along(factor.levs)
      # use levels to define cat indicators:
      data[,(factor.varnm %+% "_" %+% factor.levs.code) := lapply(factor.levs, function(x) as.integer(levels(get(factor.varnm))[get(factor.varnm)] %in% x))]
      # Remove the original factor var:
      if (drop_factor) {
        if (verbose) cat("dropping the following factor: ", factor.varnm,"\n")
        data[,(factor.varnm) := NULL]
      }
      new.factor.names[[factor.varnm]] <- factor.varnm %+% "_" %+% factor.levs.code

    ## Convert existing factor variable to integer
    } else {
      data[, (factor.varnm) := as.integer(levels(get(factor.varnm))[get(factor.varnm)] %in% factor.levs[2])]
      # new.factor.names[[factor.varnm]] <- factor.varnm
      new.factor.names[[factor.varnm]] <- NULL
    }
  }
  data.table::setattr(data,"new.factor.names",new.factor.names)
  return(data)
}
