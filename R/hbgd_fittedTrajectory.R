#' Create a fittedTrajectory object with fits for a single subject
#'
#' Creates objects of class \code{fittedTrajectory} with growth curve predictions, one object for subject ID in the data.
#' This can be used for further growth curve analysis with hbgd R package.
#' Requires the the dataset containing the predicted growth curves for all subjects \code{grid_fits_dat}.
#' @param ID The value of the subject ID for which the trajectory object will be created.
#' @param subj_dat ...
#' @param grid_fits_dat ...
#' @param holdout_fits_dat ...
#' @param ID_var A character string name of the column in \code{grid_fits_dat} that contains the unique subject identifiers.
#' @param t_var A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param y_var A character string name of the column that represent the response variable in the model.
#' @param sex_var ...
#' @param method ...
#' @param xy_pair_name ...
#' @param grid_fits_var ...
#' @param holdout_fits_var ...
#' @param fun_y_to_raw ...
#' @param fun_y_to_z ...
#' @return ...
#' @export
create_all_fittedTrajectory <- function(subj_dat, grid_fits_dat, holdout_fits_dat = NULL, ID_var, t_var, y_var, sex_var, method,
                                        xy_pair_name = c("agedays","htcm"),
                                        grid_fits_var = "SL.preds",
                                        holdout_fits_var = "holdout_fit",
                                        checkpoints = c(50, 100),
                                        fun_y_to_raw = hbgd::who_zscore2htcm,
                                        fun_y_to_z = function(x, y, ...) return(y)) {

    subj_dat <- as.data.table(subj_dat)
    grid_fits_dat <- as.data.table(grid_fits_dat)
    if (!is.null(holdout_fits_dat)) holdout_fits_dat <- as.data.table(holdout_fits_dat)

    # Divide a dataset into subsets by subject
    by_subject <- function(dat, ID_var, newID = "subjid") {
        if (ID_var %in% names(dat)) setnames(dat, ID_var, newID)
        return(res <- datadr::divide(dat, by = newID))
    }

    subj_dat_split <- by_subject(subj_dat, ID_var)
    grid_fits_dat_split <- by_subject(grid_fits_dat, ID_var)
    if (!is.null(holdout_fits_dat)) {
        holdout_fits_dat_split <- by_subject(holdout_fits_dat, ID_var)
    } else {
        holdout_fits_dat_split <- NULL
    }

    # Join three datasets
    if (is.null(holdout_fits_dat_split)) {
        new_dr <- datadr::drJoin(subj_dat = subj_dat_split, grid_fits_dat = grid_fits_dat_split)
    } else {
        new_dr <- datadr::drJoin(subj_dat = subj_dat_split, grid_fits_dat = grid_fits_dat_split, holdout_fits_dat = holdout_fits_dat_split)
    }

    fits_dat <- new_dr %>% datadr::addTransform(function(k, subj_comb_dat) {
        create_fittedTrajectory_1subj(subj_comb_dat$subj_dat, subj_comb_dat$grid_fits_dat, subj_comb_dat$holdout_fits_dat,
                                     t_var = t_var, y_var = y_var, sex_var = sex_var, method = method,
                                     xy_pair_name = xy_pair_name,
                                     grid_fits_var = grid_fits_var, holdout_fits_var = holdout_fits_var,
                                     checkpoints = checkpoints, fun_y_to_raw = fun_y_to_raw, fun_y_to_z = fun_y_to_z)
      })
    fits_dat
}


#' Create a fittedTrajectory object with fits for a single subject
#'
#' Creates an object of class \code{fittedTrajectory} with growth curve predictions.
#' This can be used for further growth curve analysis with hbgd R package.
#' Requires the the dataset containing the predicted growth curves for a single subject \code{grid_fits_dat_1subj}.
#' @param subj_dat_1subj Data on a single subject
#' @param grid_fits_dat_1subj Grid of equally spaced model predictions for a single subject
#' @param holdout_fits_dat_1subj Data with model predictions for holdout datapoints for a single subject (optional)
#' @param t_var A character string name of the column with integer-valued measurement time-points (in days, weeks, months, etc).
#' @param y_var A character string name of the column that represent the response variable in the model.
#' @param sex_var A character string name identifying the subject's gender in \code{subj_dat_1subj}.
#' @param method A fitting method.
#' @param xy_pair_name ...
#' @param grid_fits_var ...
#' @param holdout_fits_var ...
#' @param checkpoints ...
#' @param fun_y_to_raw ...
#' @param fun_y_to_z ...
#' @return An object of class \code{fittedTrajectory} for further downstream analysis.
#' @export
create_fittedTrajectory_1subj <- function(subj_dat_1subj, grid_fits_dat_1subj, holdout_fits_dat_1subj = NULL,
                                          t_var, y_var, sex_var, method,
                                          xy_pair_name = c("agedays","htcm"),
                                          grid_fits_var = "SL.preds",
                                          holdout_fits_var = "holdout_fit",
                                          checkpoints = c(50, 100),
                                          fun_y_to_raw = hbgd::who_zscore2htcm,
                                          fun_y_to_z = function(x, y, ...) return(y)) {

    subj_dat_1subj <- as.data.table(subj_dat_1subj)
    grid_fits_dat_1subj <- as.data.table(grid_fits_dat_1subj)
    if (!is.null(holdout_fits_dat_1subj)) holdout_fits_dat_1subj <- as.data.table(holdout_fits_dat_1subj)

    sex_val <- subj_dat_1subj[[sex_var]][1]

    res <- list()
    ## observed data coded as x,y cols on raw scale (and optionally z column for z-scale):
    res$xy <- subj_dat_1subj[, c(t_var), with = FALSE]; setnames(res$xy, c("x"))
    ## transform to raw-scale if fitted on Z scale, otherwise keep as is
    res$xy[,
        y    := fun_y_to_raw(x, subj_dat_1subj[[y_var]], sex = sex_val)][,
        z    := fun_y_to_z(x, subj_dat_1subj[[y_var]], sex = sex_val)][,
        yfit := fun_y_to_raw(x, grid_fits_dat_1subj[train_point == TRUE,][[grid_fits_var]], sex = sex_val)][,
        zfit := fun_y_to_z(x, grid_fits_dat_1subj[train_point == TRUE,][[grid_fits_var]], sex = sex_val)]
    data.table::setDF(res$xy)

    ## a vector of fitted values (predictions) for each data-points in xy
    res$fit <- fun_y_to_raw(res$xy$x, grid_fits_dat_1subj[train_point == TRUE,][[grid_fits_var]], sex = sex_val)
    ## the residuals of the fit
    res$resid <- res$xy[["y"]] - res$fit

    # a data frame with the model fits applied across an equally-spaced grid of “x” points with columns (x,y,z), z is optional z-scores of y
    grid_pts_only <- grid_fits_dat_1subj[train_point == FALSE, ]
    res$fitgrid <- grid_pts_only[, c(t_var), with = FALSE]; setnames(res$fitgrid, c("x"))
    # transform to raw-scale if fitted on Z scale, otherwise keep as is
    res$fitgrid[,
        y := fun_y_to_raw(x, grid_pts_only[[grid_fits_var]], sex = sex_val)][,
        z := fun_y_to_z(x, grid_pts_only[[grid_fits_var]], sex = sex_val)]
    data.table::setDF(res$fitgrid)

    ## add derivative on original and z-score scale
    if (!is.null(res$fitgrid$y)) res$fitgrid$dy <- hbgd::grid_deriv(res$fitgrid$x, res$fitgrid$y)
    if (!is.null(res$fitgrid$z)) res$fitgrid$dz <- hbgd::grid_deriv(res$fitgrid$x, res$fitgrid$z)

    if (!is.null(holdout_fits_dat_1subj)) {
        res$holdout <- holdout_fits_dat_1subj[, c(t_var), with = FALSE]; setnames(res$holdout, c("x"))
        res$holdout[,
            y := fun_y_to_raw(x, holdout_fits_dat_1subj[[holdout_fits_var]], sex = sex_val)][,
            z := fun_y_to_z(x, holdout_fits_dat_1subj[[holdout_fits_var]], sex = sex_val)]
        data.table::setDF(res$holdout)
    }

    res$checkpoint <- data.frame(x = checkpoints, y = NA, z = NA, zcat = NA)

    # a z-score categorization (e.g. <-2 or >-2) for where the subject’s growth falls in the z-score scale at each checkpoint
    res$zcat <- NA
    # a named list of fitting parameters supplied to the particular fitting method
    res$pars <- NA
    # all columns of the data argument for this subject (preserved for later analyses)
    res$data <- subj_dat_1subj
    # the subject’s sex
    res$sex <- sex_val

    # the names of the variables used for “x” and “y” in the trajectory fitting
    res$x_var <- xy_pair_name[1]
    res$y_var <- xy_pair_name[2]

    # the name of the fitting method used
    res$method <- method

    class(res) <- "fittedTrajectory"
    res
}



