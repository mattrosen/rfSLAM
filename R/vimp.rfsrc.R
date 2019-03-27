vimp.rfsrc <- function(object,
                       xvar.names,
                       m.target = NULL,
                       importance = c("permute", "random", "anti", "permute.ensemble", "random.ensemble", "anti.ensemble"),
                       joint = FALSE,
                       subset,
                       seed = NULL,
                       do.trace = FALSE,
                       ...)
{
  ## Incoming parameter checks.  All are fatal.
  if (missing(object)) {
    stop("object is missing")
  }
  if (object$family == "unsupv") {
    stop("vimp does not apply to unsupervised forests: consider using max.subtree and var.select")
  }
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2    &
      sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, forest)'")
  }
  ## Process the importance specification
  if (!is.logical(joint)) {
    stop("joint must be a logical value")
  }
  importance <- importance[1]
  if (joint & importance != "none") {
    i.str <- unlist(strsplit(importance, "\\."))
    if (length(i.str) == 1) {
      importance <- paste(i.str[1], ".joint", sep = "")
    }
      else if (length(i.str) == 2) {
        importance <- paste(i.str[1], ".joint.", i.str[2], sep = "")
      }
  }
  importance <- match.arg(importance, c(FALSE, TRUE,
                                        "none", "permute", "random", "anti",
                                        "permute.ensemble", "random.ensemble", "anti.ensemble",
                                        "permute.joint", "random.joint", "anti.joint",
                                        "permute.joint.ensemble", "random.joint.ensemble", "anti.joint.ensemble"))
  ## grow objects under non-standard bootstrapping are devoid of
  ## performance values
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) == 2) {
    if (is.null(object$forest)) {
      stop("The forest is empty.  Re-run rfsrc (grow) call with forest=TRUE")
    }
      else {
        bootstrap <- object$forest$bootstrap
      }
  }
    else {
      bootstrap <- object$bootstrap
    }
  if (bootstrap != "by.root") {
    stop("grow objects under non-standard bootstrapping are devoid of performance values")
  }
  ## Process the subsetted index 
  ## Assumes the entire data set is to be used if not specified
  if (missing(subset)) {
    subset <- NULL
  }
    else {
      ## convert the user specified subset into a usable form
      if (is.logical(subset)) {
        subset <- which(subset)
      }
      subset <- unique(subset[subset >= 1 & subset <= nrow(object$xvar)])
      if (length(subset) == 0) {
        stop("'subset' not set properly")
      }
    }
  ## make the call to generic predict
  result <- generic.predict.rfsrc(object,
                                  m.target = m.target,
                                  importance = importance,
                                  importance.xvar = xvar.names,
                                  seed = seed,
                                  do.trace = do.trace,
                                  membership = FALSE,
                                  subset = subset,
                                  ...)
  return(result)
}


#' VIMP for Single or Grouped Variables
#' 
#' Calculate variable importance (VIMP) for a single variable or group of
#' variables for training or test data.
#' 
#' Using a previously grown forest, calculate the VIMP for variables
#' \code{xvar.names}.  By default, VIMP is calculated for the original data,
#' but the user can specify a new test data for the VIMP calculation using
#' \code{newdata}.  Depending upon the option \code{importance}, VIMP is
#' calculated either by random daughter assignment or by random permutation of
#' the variable(s).  The default is Breiman-Cutler permutation VIMP.  See
#' \command{rfsrc} for more details.
#' 
#' Joint VIMP is requested using \option{joint}.  The joint VIMP is the
#' importance for a group of variables when the group is perturbed
#' simultaneously.
#' 
#' @aliases vimp vimp.rfsrc
#' @param object An object of class \code{(rfsrc, grow)} or \code{(rfsrc,
#' forest)}. Requires \option{forest=TRUE} in the original \command{rfsrc}
#' call.
#' @param xvar.names Names of the x-variables to be used.  If not specified all
#' variables are used.
#' @param m.target Character value for multivariate families specifying the
#' target outcome to be used.  If left unspecified, the algorithm will choose a
#' default target.
#' @param importance Type of VIMP.
#' @param joint Individual or joint VIMP?
#' @param subset Vector indicating which rows of the grow data to restrict VIMP
#' calculations to; i.e. this option yields VIMP which is restricted to a
#' specific subset of the data.  Note that the vector should correspond to the
#' rows of \code{object$xvar} and not the original data passed in the grow
#' call.  All rows used if not specified.
#' @param seed Negative integer specifying seed for the random number
#' generator.
#' @param do.trace Number of seconds between updates to the user on approximate
#' time to completion.
#' @param ... Further arguments passed to or from other methods.
#' @return An object of class \code{(rfsrc, predict)}, which is a list with the
#' following key components: \item{err.rate}{OOB error rate for the ensemble
#' restricted to the subsetted data.} \item{importance}{Variable importance
#' (VIMP).}
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{rfsrc}}
#' @references Ishwaran H. (2007).  Variable importance in binary regression
#' trees and forests, \emph{Electronic J. Statist.}, 1:519-537.
#' @keywords variable selection predict
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## classification example
#' ## showcase different vimp
#' ## ------------------------------------------------------------
#' 
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' 
#' # Breiman-Cutler permutation vimp
#' print(vimp(iris.obj)$importance)
#' 
#' # Breiman-Cutler random daughter vimp
#' print(vimp(iris.obj, importance = "random")$importance)
#' 
#' # Breiman-Cutler joint permutation vimp 
#' print(vimp(iris.obj, joint = TRUE)$importance)
#' 
#' # Breiman-Cuter paired vimp
#' print(vimp(iris.obj, c("Petal.Length", "Petal.Width"), joint = TRUE)$importance)
#' print(vimp(iris.obj, c("Sepal.Length", "Petal.Width"), joint = TRUE)$importance)
#' 
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## compare Breiman-Cutler vimp to ensemble based vimp
#' ## ------------------------------------------------------------
#' 
#' airq.obj <- rfsrc(Ozone ~ ., airquality)
#' vimp.all <- cbind(
#'      ensemble = vimp(airq.obj, importance = "permute.ensemble")$importance,
#'      breimanCutler = vimp(airq.obj, importance = "permute")$importance)
#' print(vimp.all)
#' 
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## calculate VIMP on test data
#' ## ------------------------------------------------------------
#' 
#' set.seed(100080)
#' train <- sample(1:nrow(airquality), size = 80)
#' airq.obj <- rfsrc(Ozone~., airquality[train, ])
#' 
#' #training data vimp
#' print(airq.obj$importance)
#' print(vimp(airq.obj)$importance)
#' 
#' #test data vimp
#' print(vimp(airq.obj, newdata = airquality[-train, ])$importance)
#' 
#' ## ------------------------------------------------------------
#' ## survival example
#' ## study how vimp depends on tree imputation
#' ## makes use of the subset option
#' ## ------------------------------------------------------------
#' 
#' data(pbc, package = "randomForestSRC")
#' 
#' # determine which records have missing values
#' which.na <- apply(pbc, 1, function(x){any(is.na(x))})
#' 
#' # impute the data using na.action = "na.impute"
#' pbc.obj <- rfsrc(Surv(days,status) ~ ., pbc, nsplit = 3,
#'         na.action = "na.impute", nimpute = 1)
#' 
#' # compare vimp based on records with no missing values
#' # to those that have missing values
#' # note the option na.action="na.impute" in the vimp() call
#' vimp.not.na <- vimp(pbc.obj, subset = !which.na, na.action = "na.impute")$importance
#' vimp.na <- vimp(pbc.obj, subset = which.na, na.action = "na.impute")$importance
#' print(data.frame(vimp.not.na, vimp.na))
#' }
#' 
vimp <- vimp.rfsrc
