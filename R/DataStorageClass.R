## ---------------------------------------------------------------------
# Detecting vector types: sVartypes <- list(bin = "binary", cat = "categor", cont = "contin")
## ---------------------------------------------------------------------
detect.col.types <- function(sVar_mat){
  detect_vec_type <- function(vec) {
    vec_nomiss <- vec[!gvars$misfun(vec)]
    nvals <- length(unique(vec_nomiss))
    if (nvals <= 2L) {
      sVartypes$bin
    } else if ((nvals <= maxncats) && (is.integerish(vec_nomiss))) {
      sVartypes$cat
    } else {
      sVartypes$cont
    }
  }
  assert_that(is.integerish(getopt("maxncats")) && getopt("maxncats") > 1)
  maxncats <- getopt("maxncats")
  sVartypes <- gvars$sVartypes

  if (is.matrix(sVar_mat)) { # for matrix:
    return(as.list(apply(sVar_mat, 2, detect_vec_type)))
  } else if (is.data.table(sVar_mat)) { # for data.table:
    return(as.list(sVar_mat[, lapply(.SD, detect_vec_type)]))
  } else {
    stop("unrecognized sVar_mat class: " %+% class(sVar_mat))
  }
}

#-----------------------------------------------------------------------------
# DataStorageClass CLASS:
#-----------------------------------------------------------------------------

is.H2OFrame <- function(fr)  base::`&&`(!missing(fr), class(fr)[1]=="H2OFrame")

#' @importFrom assertthat assert_that is.count is.flag
#' @export
DataStorageClass <- R6Class(classname = "DataStorageClass",
  portable = TRUE,
  class = TRUE,
  public = list(
    modelfit = NULL,
    new.factor.names = NULL,
    YnodeVals = NULL,       # Values of the binary outcome (Ynode) in observed data where det.Y = TRUE obs are set to NA
    det.Y = NULL,           # Logical vector, where YnodeVals[det.Y==TRUE] are deterministic (0 or 1)
    curr_data_A_g0 = TRUE,  # is the current data in OdataDT generated under observed (g0)? If FALSE, current data is under g.star (intervention)
    fold_column = NULL,
    nfolds = NULL,
    hold_column = NULL,

    initialize = function(Odata, nodes, YnodeVals, det.Y, ...) {
      assert_that(is.data.frame(Odata) | is.data.table(Odata))
      self$dat.sVar <- data.table(Odata) # makes a copy of the input data (shallow)

      # set the keys for quick search:
      setkeyv(self$dat.sVar, cols = c(nodes$IDnode, nodes$tnode))

      if (!missing(nodes)) self$nodes <- nodes

      if (!missing(YnodeVals)) self$addYnode(YnodeVals = YnodeVals, det.Y = det.Y)

      self$def.types.sVar() # Define the type of each sVar[i]: bin, cat or cont

      invisible(self)
    },

    # taken from hbgd R package:
    add_holdout_ind = function(hold_column = "hold", random = TRUE, seed = NULL) {
      if (!is.null(seed)) set.seed(as.numeric(seed))
      if (hold_column %in% names(self$dat.sVar)) {
        self$dat.sVar[, (hold_column) := NULL]
      }
      samplemax2 <- function(x) {
        if (x == 1L) {
          return(FALSE)
        } else {
          res <- rep.int(FALSE, x)
          res[ length(res) ] <- TRUE
          return(res)
        }
      }

      samplerandom2 <- function(x) {
        if (x == 1L) {
          return(FALSE)
        } else {
          res <- rep.int(FALSE, x)
          res[ sample((1:x), 1) ] <- TRUE
          return(res)
        }
      }

      if (random) {
        self$dat.sVar[, (hold_column) := samplerandom2(.N), by = eval(self$nodes$IDnode)]
      } else {
        self$dat.sVar[, (hold_column) := samplemax2(.N), by = eval(self$nodes$IDnode)]
      }
      self$hold_column <- hold_column

      return(invisible(self))
    },

    define_CVfolds = function(nfolds = 5, fold_column = "fold_id", seed = 1) {
      if (fold_column %in% names(self$dat.sVar)) {
        self$dat.sVar[, (fold_column) := NULL]
      }
      nuniqueIDs <- self$nuniqueIDs
      if (is.numeric(seed)) set.seed(seed)  #If seed is specified, set seed prior to next step
      fold_id <- as.factor(sample(rep(seq(nfolds), ceiling(nuniqueIDs/nfolds)))[1:nuniqueIDs])  # Cross-validation folds (stratified folds not yet supported)
      foldsDT <- data.table("ID" = unique(self$dat.sVar[[self$nodes$IDnode]]), fold_column = fold_id)
      setnames(foldsDT, old = names(foldsDT), new = c(self$nodes$IDnode, fold_column))
      setkeyv(foldsDT, cols = self$nodes$IDnode)
      self$dat.sVar <- merge(self$dat.sVar, foldsDT, by = self$nodes$IDnode, all.x = TRUE)
      self$fold_column <- fold_column
      self$nfolds <- nfolds
      return(invisible(self))
    },

    # -----------------------------------------------------------------------------
    # Create an H2OFrame and save a pointer to it as a private field (using faster data.table::fwrite)
    # -----------------------------------------------------------------------------
    fast.load.to.H2O = function(dat.sVar, saveH2O = TRUE, destination_frame = "H2O.dat.sVar") {
      if (missing(dat.sVar)) {
        dat.sVar <- self$dat.sVar
      }

      tmpf <- tempfile(fileext = ".csv")
      assertthat::assert_that(is.data.table(dat.sVar))
      data.table::fwrite(dat.sVar, tmpf, turbo = TRUE, verbose = TRUE, na = "NA_h2o")

      types <- sapply(dat.sVar, class)
      types <- gsub("integer64", "numeric", types)
      types <- gsub("integer", "numeric", types)
      types <- gsub("double", "numeric", types)
      types <- gsub("complex", "numeric", types)
      types <- gsub("logical", "enum", types)
      types <- gsub("factor", "enum", types)
      types <- gsub("character", "string", types)
      types <- gsub("Date", "Time", types)

      # replace all irregular characters to conform with destination_frame regular exprs format:
      tmpf.dest1 <- gsub('/', 'X', tmpf, fixed = TRUE)
      tmpf.dest2 <- gsub('.', 'X', tmpf.dest1, fixed = TRUE)
      tmpf.dest3 <- gsub('_', 'X', tmpf.dest2, fixed = TRUE)

      H2O.dat.sVar <- h2o::h2o.importFile(path = tmpf,
                                          header = TRUE,
                                          col.types = types,
                                          na.strings = rep(c("NA_h2o"), ncol(dat.sVar)),
                                          destination_frame = destination_frame)

      if (saveH2O) self$H2O.dat.sVar <- H2O.dat.sVar
      file.remove(tmpf)
      # return(invisible(self))
      return(invisible(H2O.dat.sVar))
    },

    # add protected Y nodes to private field and set to NA all determinisitc Y values for public field YnodeVals
    addYnode = function(YnodeVals, det.Y) {
        if (missing(det.Y)) det.Y <- rep.int(FALSE, length(YnodeVals))
        self$noNA.Ynodevals <- YnodeVals  # Adding actual observed Y as protected (without NAs)
        self$YnodeVals <- YnodeVals
        self$YnodeVals[det.Y] <- NA       # Adding public YnodeVals & setting det.Y values to NA
        self$det.Y <- det.Y
    },

    # ---------------------------------------------------------------------
    # Eval the subsetting expression (in the environment of the data.table "data" + global constants "gvars"):
    # ---------------------------------------------------------------------
    # Could also do evaluation in a special env with a custom subsetting fun '[' that will dynamically find the correct dataset that contains
    # sVar.name (dat.sVar or dat.bin.sVar) and will return sVar vector
    evalsubst = function(subset_vars, subset_exprs = NULL) {
      res <- rep.int(TRUE, self$nobs)
      if (!missing(subset_vars)) {
        assert_that(is.character(subset_vars))
        for (subsetvar in subset_vars) {
          # *) find the var of interest (in self$dat.sVar or self$dat.bin.sVar), give error if not found
          sVar.vec <- self$get.outvar(var = subsetvar)
          assert_that(!is.null(sVar.vec))
          # *) reconstruct correct expression that tests for missing values
          res <- res & (!gvars$misfun(sVar.vec))
        }
      }
      if (!is.null(subset_exprs)) {
        if (is.logical(subset_exprs)) {
          res <- res & subset_exprs
        } else if (is.character(subset_exprs)){
          # ******************************************************
          # data.table evaluation of the logical subset expression
          # Note: This can be made a lot more faster by also keying data.table on variables in eval(parse(text = subset_exprs))
          # ******************************************************
          res.tmp <- self$dat.sVar[, eval(parse(text = subset_exprs)), by = get(self$nodes$ID)][["V1"]]
          assert_that(is.logical(res.tmp))
          res <- res & res.tmp
        }
      }
      return(res)
    },

    # ---------------------------------------------------------------------
    # Functions for subsetting/returning covariate design mat for PredictionModel Class or outcome variable
    # ---------------------------------------------------------------------
    get.dat.sVar = function(rowsubset = TRUE, covars) {
      if (!missing(covars)) {
        # browser()
        if (length(unique(colnames(self$dat.sVar))) < length(colnames(self$dat.sVar))) {
          warning("repeating column names in the final data set; please check for duplicate summary measure / node names")
        }
        # columns to select from main design matrix (in the same order as listed in covars):
        sel.sWsA <- intersect(covars, colnames(self$dat.sVar))
        if (is.matrix(self$dat.sVar)) {
          dfsel <- self$dat.sVar[rowsubset, sel.sWsA, drop = FALSE] # data stored as matrix
        } else if (is.data.table(self$dat.sVar)) {
          dfsel <- self$dat.sVar[rowsubset, sel.sWsA, drop = FALSE, with = FALSE] # data stored as data.table
        } else {
          stop("self$dat.sVar is of unrecognized class: " %+% class(self$dat.sVar))
        }
        # columns to select from binned continuous/cat var matrix (if it was previously constructed):
        if (!is.null(self$dat.bin.sVar)) {
          sel.binsA <- intersect(covars, colnames(self$dat.bin.sVar))
        } else {
          sel.binsA <- NULL
        }
        if (length(sel.binsA)>0) {
          dfsel <- cbind(dfsel, self$dat.bin.sVar[rowsubset, sel.binsA, drop = FALSE])
        }
        found_vars <- covars %in% colnames(dfsel)
        if (!all(found_vars)) stop("some covariates can't be found (perhaps not declared as summary measures (def_sW(...) or def_sW(...))): "%+%
                                    paste(covars[!found_vars], collapse=","))
        return(dfsel)
      } else {
        return(self$dat.sVar[rowsubset, , drop = FALSE])
      }
    },

    get.outvar = function(rowsubset = TRUE, var) {
      if (length(self$nodes) < 1) stop("DataStorageClass$nodes list is empty!")
      if (var %in% self$names.sVar) {
        out <- self$dat.sVar[rowsubset, var, with = FALSE]
      } else if (var %in% colnames(self$dat.bin.sVar)) {
        out <- self$dat.bin.sVar[rowsubset, var]
      } else if ((var %in% self$nodes$Ynode) && !is.null(self$YnodeVals)) {
        out <- self$YnodeVals[rowsubset]
      } else {
        stop("requested variable " %+% var %+% " does not exist in DataStorageClass!")
      }
      if ((is.list(out) || is.data.table(out)) && (length(out)>1)) {
        stop("selecting regression outcome covariate resulted in more than one column: " %+% var)
      } else if (is.list(out) || is.data.table(out)) {
        return(out[[1]])
      } else {
        return(out)
      }
    },

    # --------------------------------------------------
    # Replace all missing (NA) values with a default integer (0)
    # --------------------------------------------------
    fixmiss_sVar = function() {
      if (is.matrix(self$dat.sVar)) {
        private$fixmiss_sVar_mat()
      } else if (is.data.table(self$dat.sVar)) {
        private$fixmiss_sVar_DT()
      } else {
        stop("self$dat.sVar is of unrecognized class")
      }
    },

    # --------------------------------------------------
    # Methods for sVar types. Define the type (class) of each variable (sVar) in input data: gvars$sVartypes$bin,  gvars$sVartypes$cat or gvars$sVartypes$cont
    # --------------------------------------------------
    # type.sVar acts as a flag: only detect types when !is.null(type.sVar), otherwise can pass type.sVar = list(sVar = NA, ...) or a value type.sVar = NA/gvars$sVartypes$bin/etc
    def.types.sVar = function(type.sVar = NULL) {
      if (is.null(type.sVar)) {
        private$.type.sVar <- detect.col.types(self$dat.sVar)
      } else {
        n.sVar <- length(self$names.sVar)
        len <- length(type.sVar)
        assert_that((len == n.sVar) || (len == 1L))
        if (len == n.sVar) { # set types for each variable
          assert_that(is.list(type.sVar))
          assert_that(all(names(type.sVar) %in% self$names.sVar))
        } else { # set one type for all vars
          assert_that(is.string(type.sVar))
          type.sVar <- as.list(rep(type.sVar, n.sVar))
          names(type.sVar) <- self$names.sVar
        }
        private$.type.sVar <- type.sVar
      }
      invisible(self)
    },
    set.sVar.type = function(name.sVar, new.type) { private$.type.sVar[[name.sVar]] <- new.type },
    get.sVar.type = function(name.sVar) { if (missing(name.sVar)) { private$.type.sVar } else { private$.type.sVar[[name.sVar]] } },
    is.sVar.bin = function(name.sVar) { self$get.sVar.type(name.sVar) %in% gvars$sVartypes$bin },
    is.sVar.cat = function(name.sVar) { self$get.sVar.type(name.sVar) %in% gvars$sVartypes$cat },
    is.sVar.cont = function(name.sVar) { self$get.sVar.type(name.sVar) %in% gvars$sVartypes$cont },

    # ---------------------------------------------------------------------
    # Directly replace variable(s) in the storage data.table (by reference)
    # ---------------------------------------------------------------------
    get.sVar = function(name.sVar) {
      x <- self$dat.sVar[, name.sVar, with=FALSE]
      if (is.list(x) || is.data.table(x) || is.data.frame(x)) x <- x[[1]]
      return(x)
    },
    set.sVar = function(name.sVar, new.sVarVal) {
      assert_that(length(new.sVarVal)==self$nobs | length(new.sVarVal)==1)
      assert_that(name.sVar %in% colnames(self$dat.sVar))
      self$dat.sVar[, (name.sVar) := new.sVarVal]
      invisible(self)
    },

    # ---------------------------------------------------------------------------
    # Cast long format data into wide format:
    # bslcovars - names of covariates that shouldn't be cast (remain invariant with t)
    # TO DO - add excludevars arg to exclude covariates
    # ---------------------------------------------------------------------------
    convert.to.wide = function(cast.vars) {
      # nodes <- self$nodes
      # cast.vars <- c(nodes$Lnodes,nodes$Cnodes, nodes$Anodes, nodes$Nnodes, nodes$Ynode)
      # if (!missing(bslcovars)) cast.vars <- setdiff(cast.vars, bslcovars)
      odata_wide <- dcast(self$dat.sVar, formula = nodes$ID %+% " ~ " %+% nodes$tnode, value.var = cast.vars)
      return(odata_wide)
    }
  ),

  active = list(
    min.t = function() { min(self$dat.sVar[[self$nodes[['tnode']]]], na.rm = TRUE) },
    max.t = function() { max(self$dat.sVar[[self$nodes[['tnode']]]], na.rm = TRUE) },
    nobs = function() { nrow(self$dat.sVar) },
    nuniqueIDs = function() { length(unique(self$dat.sVar[[self$nodes$IDnode]])) },
    nuniquets = function() { length(unique(self$dat.sVar[[self$nodes$tnode]])) },
    names.sVar = function() { colnames(self$dat.sVar) },
    ncols.sVar = function() { length(self$names.sVar) },
    dat.sVar = function(dat.sVar) {
      if (missing(dat.sVar)) {
        return(private$.mat.sVar)
      } else {
        assert_that(is.matrix(dat.sVar) | is.data.table(dat.sVar))
        private$.mat.sVar <- dat.sVar
      }
    },
   H2O.dat.sVar = function(dat.sVar) {
      if (missing(dat.sVar)) {
        return(private$.H2O.mat.sVar)
      } else {
        assert_that(is.H2OFrame(dat.sVar))
        private$.H2O.mat.sVar <- dat.sVar
      }
    },
    emptydat.sVar = function() { private$.mat.sVar <- NULL },         # wipe out mat.sVar
    # wipe out binirized .mat.sVar:
    noNA.Ynodevals = function(noNA.Yvals) {
      if (missing(noNA.Yvals)) return(private$.protected.YnodeVals)
      else private$.protected.YnodeVals <- noNA.Yvals
    },
    nodes = function(nodes) {
      if (missing(nodes)) {
        return(private$.nodes)
      } else {
        assert_that(is.list(nodes))
        private$.nodes <- nodes
      }
    },
    type.sVar = function() { private$.type.sVar }
  ),

  private = list(
    .nodes = list(),              # names of the important nodes in the data (ID, t, outcome)
    .protected.YnodeVals = NULL,  # Actual observed values of the binary outcome (Ynode), along with deterministic vals
    .mat.sVar = NULL,             # pointer to data.table object storing the entire dataset
    .H2O.mat.sVar = NULL,         # pointer to H2OFrame object that stores equivalent data to private$.mat.sVar
    .type.sVar = NULL,            # Named list with sVar types: list(names.sVar[i] = "binary"/"categor"/"contin"), can be overridden
    # Replace all missing (NA) values with a default integer (0) for matrix
    fixmiss_sVar_mat = function() {
      self$dat.sVar[gvars$misfun(self$dat.sVar)] <- gvars$misXreplace
      invisible(self)
    },
    # Replace all missing (NA) values with a default integer (0) for data.table
    fixmiss_sVar_DT = function() {
      # see http://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
      dat.sVar <- self$dat.sVar
      for (j in names(dat.sVar))
        set(dat.sVar, which(gvars$misfun(dat.sVar[[j]])), j , gvars$misXreplace)
      invisible(self)
    }
  )
)

