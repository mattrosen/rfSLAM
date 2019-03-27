impute.rfsrc <- function(formula,
                         data,
                         ntree = 500,
                         mtry = NULL,
                         xvar.wt = NULL,
                         nodesize = 1,
                         splitrule = NULL,
                         nsplit = 10,
                         
                         na.action = c("na.impute"),
                         
                          
                         nimpute = 2,
                         mf.q, blocks,
                         always.use = NULL, 
                         max.iter = 10,
                         eps = 0.01,
                         verbose = TRUE, 
                         do.trace = FALSE,
                         ...)
{
  ## get any hidden options
  user.option <- list(...)
  ytry <- is.hidden.ytry(user.option)
  ## terminate if there is no data
  if (missing(data)) {
    stop("data is missing")
  }
  ## identify the missing data
  ## if none/all: return the data
  which.na <- is.na(data)
  if (!any(which.na) || all(which.na)) {
    return(invisible(data))
  }
  ## acquire various dimensions/information
  p <- ncol(data)
  n <- nrow(data)
  ## we must dispose of all rows, columns with all missigness
  ## check after processing there still exists missing data
  all.r.na <- rowSums(which.na) == p
  all.c.na <- colSums(which.na) == n
  data <- data[!all.r.na, !all.c.na, drop = FALSE]
  which.na <- which.na[!all.r.na, !all.c.na, drop = FALSE]
  if (!any(which.na)) {
    return(data)
  }
  p <- ncol(data)
  n <- nrow(data)
  ## extract the variable names to retain consistency of their order
  all.var.names <- colnames(data)
  ## mforest details
  if (missing(mf.q)) {
    mforest <- FALSE
  }
    else {
      mforest <- TRUE
    }
  ## set the number of blocks used to subdivide the data
  ## hence subdivide the problem into more manageable pieces
  if (!missing(blocks)) {
    blocks <- cv.folds(nrow(data), max(1, blocks))
  }
    else {
      blocks <- list(1:nrow(data))
    }
  ## generic impute call
  if (!mforest) {
    ## if the formula is missing we are implementing unsupervised forests
    if (missing(formula)) {
      if (is.null(ytry)) {
        ytry <- min(p - 1, max(25, ceiling(sqrt(p))))
      }
      formula <- as.formula(paste("Unsupervised(", ytry, ") ~ ."))
    }
    ## loop over data blocks
    nullBlocks <- lapply(blocks, function(blk) {
      ## assign the blocked data
      dta <- data[blk,, drop = FALSE]
      ## make the generic impute call
      retO <- tryCatch({generic.impute.rfsrc(formula = formula,
                                             data = dta,
                                             ntree = ntree,
                                             nimpute = nimpute,
                                             mtry = mtry,
                                             nodesize = nodesize,
                                             splitrule = splitrule,
                                             nsplit = nsplit,
                                             na.action = na.action,
                                             xvar.wt = xvar.wt,
                                             do.trace = do.trace)}, error = function(e) {NULL})
      ## if the impute object is non-null proceed ahead
      if (!is.null(retO)) {
        ## overlay the imputed data
        ## we retain the same column order
        if (!is.null(retO$missing$row)) {
          blk <- blk[-retO$missing$row]
        }
        if (!is.null(retO$missing$col)) {
          ynames <- all.var.names[-retO$missing$col]
        }
          else {
            ynames <- all.var.names
          }
        ## data reassignment
        data[blk, ynames] <<- retO$data[, ynames, drop = FALSE]
      }
      ## memory management
      NULL
    })
    rm(nullBlocks)
  }
  ## mforest call
  if (mforest) {
    ## identify which variables have missing data
    ## store the information as a convenient list
    x.na <- lapply(1:p, function(k) {
      if (sum(which.na[, k]) > 0) {
        as.numeric(which(which.na[, k]))
      }
        else {
          NULL
        }
    })
    which.x.na <- which(sapply(x.na, length) > 0)
    names(x.na) <- all.var.names <- colnames(data)
    var.names <- all.var.names[which.x.na] 
    ## always.use? if so - convert to index
    if (!is.null(always.use)) {
      always.use <- is.element(all.var.names, always.use)
      if (sum(always.use) > 0) {
        always.use <- which(always.use)
      }
    }
    ## set the multivariate response dimension
    ## convert mf.q to a fraction
    p0 <- length(which.x.na)
    if (mf.q == 0) {
      stop("mf.q must be greater than zero")
    }
    if (mf.q >= 1) {
      mf.q <- min(p0 - 1, mf.q) / p0
    }
    ## convert mf.q to K-fold selection
    K <- max(1 / mf.q, 2)
    ## quick and rough impute
    ## uncomment the following line for a better initial estimate
    data <- generic.impute.rfsrc(data = data,
                                 nimpute = 3,
                                 ntree = 250,
                                 mtry = mtry,
                                 nodesize = nodesize,
                                 nsplit = nsplit)$data
    ###############################################################
    ## main loop: data blocks/groups of variables
    ## we use lapply to avoid for-looping
    ###############################################################
    ## set flags
    diff.err <- Inf
    check <- TRUE
    nullWhile <- lapply(1:max.iter, function(m) {
      ## break
      if (!check) {
        return(NULL)
      }
      ## verbose
      if (verbose && max.iter > 1) {
        cat("\t iteration", m, "\n")
      }
      ## save the current state of data for assessing convergence
      data.old <- data
      ##--------------------------------------------------------
      ## loop over data blocks
      ##--------------------------------------------------------
      nullBlocks <- lapply(blocks, function(blk) {
        ## determine the grouping of the multivariate response
        var.grp <- cv.folds(p0, K)
        ##--------------------------------------------------------
        ## loop over the multivariate response groupings
        ##--------------------------------------------------------
        nullObj <- lapply(var.grp, function(grp) {
          ## multivariate formula
          ynames <- unique(c(var.names[grp], all.var.names[always.use]))
          f <- as.formula(paste("Multivar(", paste(ynames, collapse = ","), paste(") ~ ."), sep = ""))
          ## reset the chosen y missing data back to NA
          dta <- data[blk,, drop = FALSE]
          dta[, ynames] <- lapply(ynames, function(nn) {
            xk <- data[, nn]
            xk[unlist(x.na[nn])] <- NA
            xk[blk]
          })
          ## multivariate missForest call
          ## first column of returned object contains the imputed "y-values" which is what we want
          ## we do not use the other imputed data: this is very important
          retO <- tryCatch({generic.impute.rfsrc(f,
                                                 dta,
                                                 ntree = ntree,
                                                 nimpute = 1,
                                                 na.action =  na.action,
                                                 mtry = mtry,
                                                 nodesize = nodesize,
                                                 splitrule = splitrule,
                                                 nsplit = nsplit)}, error = function(e) {NULL})
          ## confirm that the impute object is non-null in order to proceed
          if (!is.null(retO)) {
            ## overlay the imputed data
            ## we retain the same column order
            if (!is.null(retO$missing$row)) {
              blk <- blk[-retO$missing$row]
            }
            if (!is.null(retO$missing$col)) {
              ynames <- ynames[-retO$missing$col]
            }
            data[blk, ynames] <<- retO$data[, ynames, drop = FALSE]
            rm(dta)
          }
          ## return NULL (memory management)
          NULL
        })
        ## return NULL (memory management)
        NULL
      })
      ## check whether algorithm has converged
      diff.new.err <- mean(sapply(var.names, function(nn) {
        xo <- data.old[unlist(x.na[nn]), nn]
        xn <- data[unlist(x.na[nn]), nn]
        if (!is.numeric(xo)) {
          sum(xn != xo, na.rm = TRUE) / (.001 + length(xn))
        }
          else {
            var.xo <- var(xo, na.rm = TRUE)
            if (is.na(var.xo)) {
              var.xo <- 0
            }
            sqrt(mean((xn - xo)^2, na.rm = TRUE) / (.001 + var.xo))
          }
      }), na.rm = TRUE)
      if (verbose) {
        cat("         >> ", diff.new.err, diff.err - diff.new.err, "\n")
      }
      check <<- ((diff.err - diff.new.err) >= eps)
      diff.err <<- diff.new.err
      rm(data.old)
      ## return NULL (memory management)
      NULL
    })
  }
  ## #############################################################
  ## return the imputed data
  ## #############################################################
  invisible(data)
}


#' Impute Only Mode
#' 
#' Fast imputation mode.  A random forest is grown and used to impute missing
#' data.  No ensemble estimates or error rates are calculated.
#' 
#' \enumerate{
#' 
#' \item Grow a forest and use this to impute data.  All external calculations
#' such as ensemble calculations, error rates, etc. are turned off.  Use this
#' function if your only interest is imputing the data.
#' 
#' \item Split statistics are calculated using non-misssing data only.  If a
#' node splits on a variable with missing data, the variable's missing data is
#' imputed by randomly drawing values from non-missing in-bag data.  The
#' purpose of this is to make it possible to assign cases to daughter nodes
#' based on the split.
#' 
#' \item If no formula is specified, unsupervised splitting is implemented
#' using a \code{ytry} value of sqrt(\code{p}) where \code{p} equals the number
#' of variables.  More precisely, \code{mtry} variables are selected at random,
#' and for each of these a random subset of \code{ytry} variables are selected
#' and defined as the multivariate pseudo-responses.  A multivariate composite
#' splitting rule of dimension \code{ytry} is then applied to each of the
#' \code{mtry} multivariate regression problems and the node split on the
#' variable leading to the best split (Tang and Ishwaran, 2017).
#' 
#' \item If \code{mf.q} is specified, a multivariate version of missForest
#' imputation (Stekhoven and Buhlmann, 2012) is applied.  A fraction
#' \code{mf.q} of variables are used as multivariate responses and split by the
#' remaining variables using multivariate composite splitting (Tang and
#' Ishwaran, 2017).  Missing data for responses are imputed by prediction.  The
#' process is repeated using a new set of variables for responses (mutually
#' exclusive to the previous fit), until all variables have been imputed.  This
#' is one iteration.  The entire process is repeated, and the algorithm
#' iterated until a convergence criteria is met (specified using options
#' \code{max.iter} and \code{eps}).  Integer values for \code{mf.q} are allowed
#' and interpreted as a request that \code{mf.q} variables be selected for the
#' multivariate response.  This is generally the most accurate of all the
#' imputation procedures, but also the most computationally demanding.
#' 
#' \item Prior to imputation, the data is processed and records with all values
#' missing are removed, as are variables having all missing values.
#' 
#' \item If there is no missing data, either before or after processing of the
#' data, the algorithm returns the processed data and no imputation is
#' performed.
#' 
#' \item The default choice \command{nimpute=2} is chosen for coherence with
#' the default missing data algorithm implemented in grow mode.  Thus, if the
#' user imputes data with \command{nimpute=2} and runs a grow forest using this
#' imputed data, then performance values such as VIMP and error rates will
#' coincide with those obtained by running a grow forest on the original
#' non-imputed data using \code{na.action = "na.impute"}.  Ignored for
#' multivariate missForest.
#' 
#' \item All options are the same as \command{rfsrc} and the user should
#' consult the \command{rfsrc} help file for details.  }
#' 
#' @aliases impute.rfsrc impute
#' @param formula A symbolic description of the model to be fit.  Can be left
#' unspecified if there are no outcomes or we don't care to distinguish between
#' y-outcomes and x-variables in the imputation.
#' @param data Data frame containing the data to be imputed.
#' @param ntree Number of trees to grow.
#' @param mtry Number of variables randomly sampled at each split.
#' @param nodesize Forest average terminal node size.
#' @param splitrule Splitting rule used to grow trees.  Do not use for
#' unsupervised or multivariate splitting unless \code{splitrule}="random"
#' (used as a strategy for speeding up calculations).
#' @param nsplit Non-negative integer value used to specify random splitting.
#' @param na.action Missing value action. See details below.
#' @param nimpute Number of iterations of the missing data algorithm.  Ignored
#' for multivariate missForest; in which case the algorithm iterates until a
#' convergence criteria is achieved (users can however enforce a maximum number
#' of iterations with the option \code{max.iter}).
#' @param mf.q Fraction of variables (between 0 and 1) used as responses in
#' multivariate missForest imputation.  By default, multivariate missForest
#' imputation is not performed if left unspecifed.  Can be an integer, in which
#' case this equals the number of multivariate responses.
#' @param blocks Integer value specifying the number of blocks the data should
#' be broken up into (by rows).  This can improve computational efficiency when
#' the sample size is large but imputation efficiency decreases.  By default,
#' no action is taken if left unspecified.
#' @param always.use Character vector of variable names to always be included
#' as a response in multivariate missForest imputation.  Does not apply for
#' other imputation methods.
#' @param xvar.wt Weights for selecting variables for splitting on.
#' @param max.iter Maximum number of iterations used when implementing
#' multivariate missForest imputation.
#' @param eps Tolerance value used to determine convergence of multivariate
#' missForest imputation.
#' @param verbose Send verbose output to terminal (only applies to multivariate
#' missForest imputation).
#' @param do.trace Number of seconds between updates to the user on approximate
#' time to completion.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly, the data frame containing the orginal data with imputed
#' data overlayed.
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{rfsrc}}
#' @references Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S.
#' (2008).  Random survival forests, \emph{Ann. App.  Statist.}, 2:841-860.
#' 
#' Stekhoven D.J. and Buhlmann P. (2012). MissForest--non-parametric missing
#' value imputation for mixed-type data.  \emph{Bioinformatics}, 28(1):112-118.
#' 
#' Tang F. and Ishwaran H. (2017).  Random forest missing data algorithms.
#' \emph{Statistical Analysis and Data Mining}, 10, 363-377.
#' @keywords missing data
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## example of survival imputation
#' ## ------------------------------------------------------------
#' 
#' ## unsupervised splitting
#' data(pbc, package = "randomForestSRC")
#' pbc1.d <- impute(data = pbc, nimpute = 5)
#' 
#' #imputation using outcome splitting
#' f <- as.formula(Surv(days, status) ~ .)
#' pbc2.d <- impute(f, data = pbc, nsplit = 3)
#' 
#' #random splitting can be reasonably good
#' pbc4.d <- impute(f, data = pbc, splitrule = "random", nimpute = 5)
#' 
#' ## ------------------------------------------------------------
#' ## example of regression imputation
#' ## ------------------------------------------------------------
#' 
#' air1.d <- impute(data = airquality, nimpute = 5)
#' air2.d <- impute(Ozone ~ ., data = airquality, nimpute = 5)
#' air3.d <- impute(Ozone ~ ., data = airquality, nimpute = 5,
#'            splitrule = "random", nodesize = 1)
#' 
#' ## ------------------------------------------------------------
#' ## multivariate missForest imputation
#' ## ------------------------------------------------------------
#' 
#' data(pbc, package = "randomForestSRC")
#' 
#' ## use 10 percent of variables as responses
#' ## i.e. multivariate missForest
#' pbc.d <- impute(data = pbc, mf.q = .01)
#' 
#' ## use 1 variable as the response
#' ## i.e. original missForest algorithm
#' pbc.d <- impute(data = pbc, mf.q = 1)
#' 
#' ## faster call by using random splitting
#' pbc.d <- impute(data = pbc, mf.q = 1, splitrule = "random")
#' 
#' ## faster call by increasing nodesize
#' pbc.d <- impute(data = pbc, mf.q = 1, nodesize = 20, splitrule = "random")
#' 
#' 
#' }
#' 
impute <- impute.rfsrc
