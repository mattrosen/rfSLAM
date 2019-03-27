quantileReg.rfsrc <- function(obj, oob = TRUE, prob = (1:10) / 10, newdata = NULL) {
  ## forest object must contain regression outcomes
  ## this includes multivariate regression and mixed multivariate regression
  if (!(obj$family == "regr" | !is.null(obj$regrOutput))) {
    stop("this function only applies to regression settings\n")
  }
  ## pull the target outcome names
  if (obj$family == "regr") {
    ynames <- obj$yvar.names
  }
  else {
    ynames <- names(obj$regrOutput)
  }
  ## training:
  ## forest weights
  ## test whether forest weights are available
  ## cost saving measure for advanced users
  ## otherwise pull the forest weights by calling predict
  ##
  ## testing:
  ## pull forest weights for test data set
    if (!is.null(obj$forest.wt) && is.null(newdata)) {
    fwt <- obj$forest.wt
  }
  else {
    if (is.null(newdata)) {
      if (oob) {
        fwt <- predict(obj, forest.wt = "oob")$forest.wt
      }
      else {
        fwt <- predict(obj, forest.wt = TRUE)$forest.wt
      }
    }
    else {##test data is available
      fwt <- predict(obj, newdata, forest.wt = TRUE)$forest.wt
    }
  } 
  ## calculate the quantiles
  rO <- lapply(ynames, function(yn) {
    ## extract y
    if (ncol(cbind(obj$yvar)) > 1) {
      y <- obj$yvar[, yn]
    }
    else {
      y <- obj$yvar
    }
    ## cdf calculations
    yunq <- sort(unique(y))
    ind.matx <- do.call(rbind, mclapply(yunq, function(yy) {y <= yy}))
    cdf <- t(apply(fwt, 1, function(wt) {ind.matx %*% wt}))
    ## step function interpolation
    sIndex <- function(x, y) {sapply(1:length(y), function(j) {sum(x <= y[j])})}
    ## quantiles
    quant <- t(apply(cdf, 1, function(pr) {
      c(min(yunq, na.rm = TRUE), yunq)[1 + sIndex(pr, prob)]
    }))
    ## return the object
    list(quantiles = quant,
         cdf = cdf,
         prob = prob,
         density = t(apply(cbind(0,cdf), 1, diff)),
         yunq = yunq)
  })
  ## return the goodies
  if (obj$family == "regr") {
    rO[[1]]
  }
  else {
    names(rO) <- ynames
    rO
  }
}


#' Quantile Regression Forests
#' 
#' Determines the conditional quantiles and conditional density for a
#' regression forest.  Applies to both univariate and multivariate forests.
#' Can be used for both training and testing purposes.
#' 
#' Given a regression forest, or a multivariate forest with at least one
#' regression outcome, returns the conditional quantiles for the target
#' outcomes.  Also returns the conditional density, which can be used to
#' calculate conditional moments, such as the mean and standard deviation.
#' 
#' @aliases quantileReg quantileReg.rfsrc
#' @param obj A previously grown forest.
#' @param oob Return OOB (out-of-bag) quantiles?  If false, in-bag values are
#' returned.
#' @param prob Target quantile probabilities.
#' @param newdata Test data (optional) over which conditional quantiles are
#' evaluated over.
#' @return Quantiles for each of the requested probabilities, conditional
#' density (and conditional cdf) for each unique y-value, are returned for each
#' data point in the training set, or if a test set is provided, the values are
#' returned for each data point in the test data.  If more than one target
#' outcome is available, the returned object will be a list of length equal to
#' the number of target outcomes.
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{rfsrc}}
#' @references Meinshausen N. (2006) Quantile Regression Forests, \emph{Journal
#' of Machine Learning Research}, 7:983--999.
#' @keywords quantile regression forests
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' 
#' ## grow call, followed by quantileReg call
#' o <- rfsrc(mpg ~ ., mtcars)
#' qo <- quantileReg(o, prob = c(0.25, 0.5, 0.75))
#' 
#' ## calculate the conditional mean, compare to OOB predicted value
#' c.mean <- qo$density %*% qo$yunq
#' print(data.frame(c.mean = c.mean, pred.oob = o$predicted.oob))
#' 
#' ## calculate conditional standard deviation
#' c.std <- sqrt(qo$density %*% qo$yunq^2 - c.mean ^ 2)
#' quant <- qo$quantile
#' colnames(quant) <- paste("q", 100 * qo$prob, sep = "")
#' print(data.frame(quant, c.std))
#' 
#' 
#' ## ------------------------------------------------------------
#' ## train/test regression example
#' ## ------------------------------------------------------------
#' 
#' ## grow call, followed by quantileReg call
#' o <- rfsrc(mpg ~ ., mtcars[1:20,])
#' qo <- quantileReg(o, newdata = mtcars[-(1:20),], prob = c(0.25, 0.5, 0.75))
#' 
#' ## calculate test set conditional mean and standard deviation
#' c.mean <- qo$density %*% qo$yunq
#' c.std <- sqrt(qo$density %*% qo$yunq^2 - c.mean ^ 2)
#' quant <- qo$quant
#' colnames(quant) <- paste("q", 100 * qo$prob, sep = "")
#' print(data.frame(quant, c.mean, c.std))
#' 
#' 
#' ## ------------------------------------------------------------
#' ## multivariate mixed outcomes example
#' ## ------------------------------------------------------------
#' 
#' dta <- mtcars
#' dta$cyl <- factor(dta$cyl)
#' dta$carb <- factor(dta$carb, ordered = TRUE)
#' o <- rfsrc(cbind(carb, mpg, cyl, disp) ~., data = dta)
#' qo <- quantileReg(o)
#' 
#' print(head(qo$mpg$quant))
#' print(head(qo$disp$quant))
#' 
#' 
#' ## ------------------------------------------------------------
#' ## quantile regression plot for Boston Housing data
#' ## ------------------------------------------------------------
#' 
#' if (library("mlbench", logical.return = TRUE)) {
#' 
#'   ## apply quantile regression to Boston Housing data
#'   data(BostonHousing)
#'   o <- rfsrc(medv ~ ., BostonHousing)
#'   qo <- quantileReg(o, prob = c(0.25, 0.5, 0.75))
#' 
#'   ## quantile data for plotting 
#'   quant.dat <- qo$quant
#'   y <- o$yvar
#' 
#'   ## quantile regression plot
#'   plot(range(y), range(quant.dat), xlab = "y",
#'        ylab = ".25-.75 Quantiles", type = "n")
#'   jitter.y <- jitter(y, 10)
#'   points(jitter.y, quant.dat[, 2], pch = 15, col = 4, cex = 0.75)
#'   segments(jitter.y, quant.dat[, 2], jitter.y, quant.dat[, 1], col = "grey")
#'   segments(jitter.y, quant.dat[, 2], jitter.y, quant.dat[, 3], col = "grey")
#'   points(jitter.y, quant.dat[, 1], pch = "-", cex = 1)
#'   points(jitter.y, quant.dat[, 3], pch = "-", cex = 1)
#'   abline(0, 1, lty = 2, col = 2)
#' 
#' }
#' 
#' ## ------------------------------------------------------------
#' ## example of quantile regression for ordinal data
#' ## ------------------------------------------------------------
#' 
#' ## is there an internet connection? (needs RCurl)
#' if (library("RCurl", logical.return = TRUE)) {
#'  if (!is.null(tryCatch({getURL("www.google.com")}, error = function(e) {NULL}))) {
#' 
#'  ## use the wine data for illustration
#'  wn.url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality"
#'  wn.url <- paste(wn.url, "/winequality-white.csv", sep ="")
#'  dta <- read.table(wn.url, header = TRUE, sep = ";")
#' 
#'  ## regression call - request forest weights
#'  o <- rfsrc(quality ~ ., dta, nsplit = 10, ntree = 100, forest.wt = "oob")
#' 
#'  ## run quantile regression/extract "probabilities" = density values
#'  oq <- quantileReg(o, prob = (1:100)/100)
#'  yunq <- oq$yunq
#'  yvar <- factor(cut(o$yvar, c(-1, yunq), labels = yunq)) 
#'  oq.dens <- oq$density
#'  colnames(oq.dens) <- yunq 
#'  oq.class <- randomForestSRC:::bayes.rule(oq.dens)
#'  oq.confusion <- table(yvar, oq.class)
#'  oq.err <- 1-diag(oq.confusion)/rowSums(oq.confusion)
#'  oq.confusion <- cbind(oq.confusion, oq.err)
#'  print(oq.confusion)
#'  cat("Normalized Brier:", 100 * randomForestSRC:::brier(yvar, oq.dens), "\n")
#' 
#' }}
#' 
#' 
#' 
#' }
quantileReg <- quantileReg.rfsrc
