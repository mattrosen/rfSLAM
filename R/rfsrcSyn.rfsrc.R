rfsrcSyn.rfsrc <-
  function(formula,          
           data,
           object,
           newdata,
           ntree = 1000,
           mtry = NULL,
           mtrySeq = NULL,
           nodesize = 5,
           nodesizeSeq = c(1:10,20,30,50,100),
           nsplit = 0,
           min.node = 3,
           use.org.features = TRUE,
           
           na.action = c("na.omit", "na.impute"),
           
            
           oob = TRUE,
           verbose = TRUE,
           ...
           )
{
  ## --------------------------------------------------------------
  ##   
  ##   preliminary processing
  ##
  ## --------------------------------------------------------------
  ## Verify key options
  
  na.action <- match.arg(na.action, c("na.omit", "na.impute"))
  
   
  if (!missing(object)) {
    ## incoming parameter check
    if (sum(inherits(object, c("rfsrc", "synthetic"), TRUE) == c(1, 2)) != 2) {
      stop("this function only works for objects of class `(rfsrc, synthetic)'")
    }
    ## extract necessary objects
    M <- length(object$rfMachines)
    fmly <- object$rfMachines[[1]]$family
    xvar.names <- object$rfMachines[[1]]$xvar.names
    yvar.names <- object$rfMachines[[1]]$yvar.names
    rfMachines <- object$rfMachines
    rfSyn <- object$rfSyn
    synthetic <- object$synthetic
    opt.machine <- object$opt.machine
    list.names <- lapply(synthetic, function(ss) {colnames(ss)})
  }
  else {
    if (missing(formula) || missing(data)) {
      stop("need to specify 'formula' and 'data' or provide a grow forest object")
    }
    f <- as.formula(formula)
    ## impute the data
    
    if (na.action == "na.impute" && any(is.na(data))) {
    
     
        if (verbose) {
        cat("\t imputing the data\n")
      }
      data <- impute.rfsrc(data = data, ntree = ntree, nodesize = nodesize, nsplit = nsplit)
    }
    ##use fast forests for parsing the data
    preObj <- rfsrc(f, data, ntree = 1, importance = "none",
                    nodesize = nrow(data), splitrule = "random")
    fmly <- preObj$family
    ##check coherence of families
    if (!(fmly == "regr" | fmly == "regr+" | fmly == "class" | fmly == "class+" | fmly == "mix+")) {
      stop("this function only applies to regression/classification based families")
    }
    ##pull xvar/yvar names
    xvar.names <- preObj$xvar.names
    yvar.names <- preObj$yvar.names
    preObj$yvar <- data.frame(preObj$yvar)
    colnames(preObj$yvar) <- yvar.names
    ##mtry sequence
    p <- length(xvar.names)
    ##conditions under which mtrySeq is assinged to default values or to mtry
    if (is.null(mtrySeq)) {
      mtrySeq <- ceiling(p/3)
    }
    else {
      mtrySeq <- unique(ceiling(mtrySeq))
      mtrySeq <- mtrySeq[mtrySeq>=1 & mtrySeq <= p]
      if (length(mtrySeq) == 0) {
        stop("invalid choice for mtrySeq:", mtrySeq)
      }
    }
    ##sort the nodesize sequence
    nodesizeSeq <- sort(nodesizeSeq)
  }
  ## verify key options
  
  na.action <- match.arg(na.action, c("na.omit", "na.impute"))
  
   
  ## --------------------------------------------------------------
  ##   
  ##   synthetic forests
  ##
  ## --------------------------------------------------------------
  if (missing(object)) {
    ## generate a fixed inbag sample if oob is in effect
    if (oob) {
      ## fast forest to determine sample size (due to NA's this may not = nrow(data))
      samp.size <- nrow(rfsrc(f, data, ntree = 1, nodesize = nrow(data), splitrule = "random")$xvar)
      samp <- make.sample(ntree, samp.size)
    }
    ## construct RF machines for each nodesize
    rfMachines <- lapply(nodesizeSeq, function(nn) {
      lapply(mtrySeq, function(mm) {
        if (verbose) {
          cat("\t RF nodesize:", nn, "mtry:", mm, "\r")
        }
        if (oob) {
          rfsrc(f, data, ntree = ntree, mtry = mm, nodesize = nn,
                bootstrap = "by.user", samp = samp, 
                nsplit = nsplit, importance = "none")
        }
        else {
          rfsrc(f, data, ntree = ntree, mtry = mm, nodesize = nn, 
                nsplit = nsplit, importance = "none")
        }
      })
    })
    ## convert list of lists to a single list
    rfMachines <- unlist(rfMachines, recursive = FALSE)
    list.names <- paste(rep(nodesizeSeq, each = length(mtrySeq)), mtrySeq, sep = ".")
    M <- length(rfMachines)                         
    ## discard stumpy forests
    if (is.numeric(min.node) && min.node > 0) {
      good.machines <- which(sapply(1:M, function(m) {
        mean(rfMachines[[m]]$leaf.count, na.rm = TRUE) > min.node}))
      if (length(good.machines) == 0) {
        good.machines <- 1
      }
      list.names <- list.names[good.machines]
      rfMachines <- lapply(good.machines, function(m) {rfMachines[[m]]})
      M <- length(rfMachines)
    }
    ## assign names to the machines
    names(rfMachines) <- paste("x.s.", list.names, sep = "")
    ## determine the optimal machine
    opt.machine <- rf.opt(rfMachines)
    ## construct the training synthetic features
    if (verbose) {
      cat("\t making the synthetic features\n")
    }
    ## for each synthetic machine, parse predicted values for each y-variable
    ## (in univariate models, there is only one y-variable)
    ## pull the last column in the case of classification
    synthetic <- lapply(1:M, function(m) {
      do.call(cbind, lapply(rfMachines[[m]]$yvar.names, function(nn) {
        ## this coercion does nothing for univariate families
        ## !! no need to call get.univariate.target first !!
        o.coerced <- coerce.multivariate(rfMachines[[m]], nn)
        yhat <- cbind(o.coerced$predicted.oob)
        J <- ncol(yhat)
        if (J > 1) {
          yhat <- yhat[ ,1:(J-1), drop = FALSE]
        }
        if (J > 2) {
          if (o.coerced$univariate) {
            colnames(yhat) <- paste(1:(J-1), list.names[m], sep = ".")
          }
            else {
              colnames(yhat) <- paste(nn, 1:(J-1), list.names[m], sep = ".")
            }
        }
          else {
            if (o.coerced$univariate) {
              colnames(yhat) <- paste(list.names[m], sep = ".")
            }
              else {
                colnames(yhat) <- paste(nn, list.names[m], sep = ".")
              }
          }
        yhat
      }))
    })
    ## bind the synthetic features
    x.s <- do.call("cbind", synthetic)
    list.names <- lapply(synthetic, function(ss) {colnames(ss)})
    ## assign names to the synthetic features
    names(synthetic) <- names(rfMachines)
    ## synthetic forest call
    if (verbose) {
      cat("\t making the synthetic forest\n")
    }
    if (use.org.features) {
      data <- data.frame(preObj$yvar, preObj$xvar, x.s = x.s)
    }
      else {
        data <- data.frame(preObj$yvar, x.s = x.s)
      }
    ## the synthetic forest call
    ## for generality, the formula is specified as multivariate but this reverts to univariate families
    ## when there is only one y-variable
    rfSyn.f <- as.formula(paste("Multivar(", paste(yvar.names, collapse = ","), paste(") ~ ."), sep = ""))
    if (oob) {
      rfSyn <- rfsrc(rfSyn.f, data, ntree = ntree, mtry = mtry, nodesize = nodesize,
                     bootstrap = "by.user", samp = samp,                      
                     nsplit = nsplit, ... )
    }
    else {
      rfSyn <- rfsrc(rfSyn.f, data, ntree = ntree, mtry = mtry, nodesize = nodesize,
                     nsplit = nsplit, ... )
    }
  }
  ## --------------------------------------------------------------
  ##   
  ##   prediction
  ##
  ## --------------------------------------------------------------
  if (!missing(newdata)) {
    ## impute the test data
    
    if (na.action == "na.impute" && any(is.na(newdata))) {
    
     
      if (verbose) {
        cat("\t imputing the test data\n")
      }
      newdata <- impute.rfsrc(data = newdata, ntree = ntree, nodesize = nodesize, nsplit = nsplit)
    }
    if (verbose) {
      cat("\t making the test set synthetic features\n")
    }
    ## make test set synthetic features
    xtest <- newdata[, xvar.names, drop = FALSE]
    ## for each synthetic machine, parse predicted values for each y-variable
    ## pull the last column in the case of classification
    synthetic <- lapply(1:M, function(m) {
      predO <- predict(rfMachines[[m]], xtest, importance = "none")
      syn.o <- do.call(cbind, lapply(rfMachines[[m]]$yvar.names, function(nn) {
        ## this coercion does nothing for univariate families
        ## !! no need to call get.univariate.target first !!
        o.coerced <- coerce.multivariate(predO, nn)
        yhat <- cbind(o.coerced$predicted)
        J <- ncol(yhat)
        if (J > 1) {
          yhat <- yhat[ ,1:(J-1), drop = FALSE]
        }
        yhat
      }))
      colnames(syn.o) <- list.names[[m]]
      syn.o
    })
    ## bind the synthetic test features
    xtest.s <- do.call("cbind", synthetic)
    ## make the test data: dependent on presence of y-outcomes
    if (length(intersect(colnames(newdata), yvar.names) > 0) &&
        setequal(intersect(colnames(newdata), yvar.names), yvar.names)) {
      data.test <- data.frame(newdata[, yvar.names, drop = FALSE], x.s = xtest.s)
    }
    else {
      data.test <- data.frame(x.s = xtest.s)
    }
    if (use.org.features) {
      data.test <- data.frame(data.test, xtest)
    }
    ## drop the test data down the synthetic forest for final prediction
    rfSynPred <- predict(rfSyn, data.test, ...)
  }
  else {
    rfSynPred <- NULL
  }
  ## --------------------------------------------------------------
  ##   
  ##   return
  ##
  ## --------------------------------------------------------------
  retObj <- list(rfMachines = rfMachines,
                 rfSyn = rfSyn,
                 rfSynPred = rfSynPred,
                 synthetic = synthetic,
                 opt.machine = opt.machine)
  if (oob) {
    class(retObj) <- c("rfsrc", "synthetic", "oob")
  }
  else {
    class(retObj) <- c("rfsrc", "synthetic", "inb")
  }
  retObj
}
## --------------------------------------------------------------
##  
## internal functions
##
## --------------------------------------------------------------
## determine the optimal RF machine using OOB error rate
## for multivariate families we take an average over standardized MSE
## and normalized Brier score - caution with ordered factors which need
## to be converted to numeric
rf.opt <- function(obj)
{
  which.min(sapply(1:length(obj), function(m) {
    mean(sapply(obj[[m]]$yvar.names, function(nn) {
      o.coerced <- coerce.multivariate(obj[[m]], nn)
      yhat <- o.coerced$predicted.oob
      if (o.coerced$family == "class") {
        brier(o.coerced$yvar, yhat)
      }
        else {
          yvar <- as.numeric(o.coerced$yvar) 
          mean((yvar - yhat)^2, na.rm = TRUE) / var(yvar, na.rm = TRUE) 
        }
    }), na.rm = TRUE)
  }))[1]
}


#' Synthetic Random Forests
#' 
#' Grows a synthetic random forest (RF) using RF machines as synthetic
#' features.  Applies only to regression and classification settings.
#' 
#' A collection of random forests are fit using different nodesize values.  The
#' predicted values from these machines are then used as synthetic features
#' (called RF machines) to fit a synthetic random forest (the original features
#' are also used in constructing the synthetic forest).  Currently only
#' implemented for regression and classification settings (univariate and
#' multivariate).
#' 
#' Synthetic features are calculated using out-of-bag (OOB) data to avoid
#' over-using training data.  However, to guarantee that performance values
#' such as error rates and VIMP are honest, bootstrap draws are fixed across
#' all trees used in the construction of the synthetic forest and its synthetic
#' features.  The option \option{oob=TRUE} ensures that this happens.  Change
#' this option at your own peril.
#' 
#' If values for \code{mtrySeq} are given, RF machines are constructed for each
#' combination of nodesize and mtry values specified by \code{nodesizeSeq}
#' \code{mtrySeq}.
#' 
#' @aliases rfsrcSyn rfsrcSyn.rfsrc
#' @param formula A symbolic description of the model to be fit.  Must be
#' specified unless \code{object} is given.
#' @param data Data frame containing the y-outcome and x-variables in the
#' model. Must be specified unless \code{object} is given.
#' @param object An object of class \code{(rfsrc, synthetic)}.  Not required
#' when \code{formula} and \code{data} are supplied.
#' @param newdata Test data used for prediction (optional).
#' @param ntree Number of trees.
#' @param mtry mtry value for synthetic forest.
#' @param mtrySeq Sequence of mtry values used for fitting the collection of RF
#' machines.  If \code{NULL}, set to the default value \code{p}/3.
#' @param nodesize Nodesize value for the synthetic forest.
#' @param nodesizeSeq Sequence of nodesize values used for the fitting the
#' collection of RF machines.
#' @param nsplit If non-zero, nsplit-randomized splitting is used which can
#' significantly increase speed.
#' @param min.node Minimum forest averaged number of nodes a RF machine must
#' exceed in order to be used as a synthetic feature.
#' @param use.org.features In addition to synthetic features, should the
#' original features be used when fitting synthetic forests?
#' @param na.action Missing value action. The default \code{na.omit} removes
#' the entire record if even one of its entries is \code{NA}.  The action
#' \code{na.impute} pre-imputes the data using fast imputation via
#' \command{impute.rfsrc}.
#' @param oob Preserve "out-of-bagness" so that error rates and VIMP are
#' honest?  Default is yes (\option{oob=TRUE}).
#' @param verbose Set to \code{TRUE} for verbose output.
#' @param ... Further arguments to be passed to the \code{rfsrc} function used
#' for fitting the synthetic forest.
#' @return A list with the following components: \item{rfMachines}{RF machines
#' used to construct the synthetic features.} \item{rfSyn}{The (grow) synthetic
#' RF built over training data.} \item{rfSynPred}{The predict synthetic RF
#' built over test data (if available).} \item{synthetic}{List containing the
#' synthetic features.} \item{opt.machine}{Optimal machine: RF machine with
#' smallest OOB error rate.}
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{rfsrc}}, \command{\link{impute.rfsrc}}
#' @references Ishwaran H. and Malley J.D. (2014).  Synthetic learning
#' machines.  \emph{BioData Mining}, 7:28.
#' @keywords forest predict
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## compare synthetic forests to regular forest (classification)
#' ## ------------------------------------------------------------
#' 
#' ## rfsrc and rfsrcSyn calls
#' if (library("mlbench", logical.return = TRUE)) {
#' 
#'   ## simulate the data 
#'   ring <- data.frame(mlbench.ringnorm(250, 20))
#' 
#'   ## classification forests
#'   ringRF <- rfsrc(classes ~., data = ring)
#' 
#'   ## synthetic forests:
#'   ## 1 = nodesize varied
#'   ## 2 = nodesize/mtry varied
#'   ringSyn1 <- rfsrcSyn(classes ~., data = ring)
#'   ringSyn2 <- rfsrcSyn(classes ~., data = ring, mtrySeq = c(1, 10, 20))
#' 
#'   ## test-set performance
#'   ring.test <- data.frame(mlbench.ringnorm(500, 20))
#'   pred.ringRF <- predict(ringRF, newdata = ring.test)
#'   pred.ringSyn1 <- rfsrcSyn(object = ringSyn1, newdata = ring.test)$rfSynPred
#'   pred.ringSyn2 <- rfsrcSyn(object = ringSyn2, newdata = ring.test)$rfSynPred
#' 
#' 
#'   print(pred.ringRF)
#'   print(pred.ringSyn1)
#'   print(pred.ringSyn2)
#' 
#' }
#' 
#' ## ------------------------------------------------------------
#' ## compare synthetic forest to regular forest (regression)
#' ## ------------------------------------------------------------
#' 
#' ## simulate the data
#' n <- 250
#' ntest <- 1000
#' N <- n + ntest
#' d <- 50
#' std <- 0.1
#' x <- matrix(runif(N * d, -1, 1), ncol = d)
#' y <- 1 * (x[,1] + x[,4]^3 + x[,9] + sin(x[,12]*x[,18]) + rnorm(n, sd = std)>.38)
#' dat <- data.frame(x = x, y = y)
#' test <- (n+1):N
#' 
#' ## regression forests
#' regF <- rfsrc(y ~ ., data = dat[-test, ], )
#' pred.regF <- predict(regF, dat[test, ], importance = "none")
#' 
#' ## synthetic forests
#' ## we pass both the training and testing data
#' ## but this can be split into separate commands as in the
#' ## previous classification example
#' synF1 <- rfsrcSyn(y ~ ., data = dat[-test, ],
#'   newdata = dat[test, ])
#' synF2 <- rfsrcSyn(y ~ ., data = dat[-test, ],
#'   newdata = dat[test, ], mtrySeq = c(1, 10, 20, 30, 40, 50))
#' 
#' ## standardized MSE performance
#' mse <- c(tail(pred.regF$err.rate, 1),
#'          tail(synF1$rfSynPred$err.rate, 1),
#'          tail(synF2$rfSynPred$err.rate, 1)) / var(y[-test])
#' names(mse) <- c("forest", "synthetic1", "synthetic2")
#' print(mse)
#' 
#' ## ------------------------------------------------------------
#' ## multivariate synthetic forests
#' ## ------------------------------------------------------------
#' 
#' mtcars.new <- mtcars
#' mtcars.new$cyl <- factor(mtcars.new$cyl)
#' mtcars.new$carb <- factor(mtcars.new$carb, ordered = TRUE)
#' trn <- sample(1:nrow(mtcars.new), nrow(mtcars.new)/2)
#' mvSyn <- rfsrcSyn(cbind(carb, mpg, cyl) ~., data = mtcars.new[trn,])
#' mvSyn.pred <- rfsrcSyn(object = mvSyn, newdata = mtcars.new[-trn,])
#' }
#' 
rfsrcSyn <- rfsrcSyn.rfsrc
