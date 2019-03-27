stat.split.rfsrc <- function(object, ...)
{
  ## is this a synthetic forest?
  if (sum(inherits(object, c("rfsrc", "synthetic"), TRUE) == c(1, 2)) == 2) {
    object <- object$rfSyn
  }
  ## Incoming parameter checks.  All are fatal.
  if (is.null(object)) stop("Object is empty!")
  if ((sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) &&
      (sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2))
    stop("This function only works for objects of class `(rfsrc, grow)' or `(rfsrc, pred)',")
  ## Acquire the inbag counts.  This is part of the grow object and
  ## not part of the forest object.
  inbag <- object$inbag
  ## Acquire the native array.  Make it global so we can access it during recursion.
  if (is.null(object$node.stats)) {    
    stop("RF-SRC statistics content is NULL.  Please re-run grow call with 'statistics=TRUE'")
  }
    else {
      extendedNativeArray <- object$node.stats
    }
  ## Acquire prune information
  if (is.null(object$pstn.membership)) {
    prune <- FALSE
  }
    else {
      prune <- TRUE
    }
  if (is.null(object$forest)) {
    stop("Forest is empty!  Re-run grow call with forest set to 'TRUE'.")
  }
  ## Discard the incoming grow object in favour of the forest.
  object <- object$forest  
  extendedNativeArray <- cbind(object$nativeArray, extendedNativeArray)
  xvar.names <- object$xvar.names
  if (is.null(xvar.names)) {
    stop("RFSRC xvar.names content is NULL.  Please ensure the object is valid.")
  }
  if (is.null(object$xvar)) {
    stop("RFSRC xvar content is NULL.  Please ensure the object is valid.")
  }
  ## Count the number of trees in the forest.
  numTree <- length(as.vector(unique(extendedNativeArray$treeID)))
  ## Count the number of parameters in the data set.
  numParm <- length(xvar.names)
  ## Acquire the incoming xvar matrix of dim [n] x [p].  Make it
  ## global so we can access it during recursion.
  xvar <- object$xvar
  ## #######################################################################
  ## Loop through all trees.  This is a LOCAL operation.
  ##
  ## Depends on external variables:  numParm, inbag, prune
  ## #######################################################################
  splitTreeObj <- mclapply(1:numTree, function(b) {
    ## Acquire the replicates for the current bootstrap.
    inbag.local <- inbag[, b]
    replicates <-  NULL
    ## Grab a replicate from the xvar matrix and decrement its inbag
    ## occurrence.  Do this until all replicates have been accounted
    ## for.
    while (sum(inbag.local) > 0) {
      replicates <- c(replicates, which(inbag.local > 0))
      inbag.local <-  sapply(1:length(inbag.local), function(k) { max(inbag.local[k] - 1, 0)})
    }
    ## Create the recursive output object. 
    splitInfo <- vector("list", numParm)
    names(splitInfo) <- xvar.names
    recursiveObject <- list(offset     = min(which(extendedNativeArray$treeID == b)),
                            splitInfo  = splitInfo,
                            terminal   = FALSE)
    ## Recursively parse the tree in the primary protocol.
    return(spParseTree(recursiveObject, extendedNativeArray, xvar,
                       b,
                       depth=0,
                       membership = replicates,
                       ptnFlag = FALSE,
                       prune = prune))
  })
  ## Assign the results to a list of length numTree.
  result <- vector("list", numTree)
  for (b in 1:numTree) {
    ## Clean the object up by removing internal variables used for recursion.
    result[[b]] <- splitTreeObj[[b]]$splitInfo
  }
  invisible(result)
}
## #######################################################################
##
##  PRIMARY RECURSION
##
##  Note:  Currently this function is ONLY valid for continuous splits.
##         Fator splits will return a split end cut preference statistic of NA.
##
##  INPUTS:
##
##  b          =  treeID
##  depth      =  zero (0) based depth of split
##  membership =  index of in-bag individual
##
##  OUTPUTS:
##
##  recursiveObject:
##
##  splitinfo - list of length [numParm] with
##  [[p]] = c(depth, parmID, nodeID, contPT, spltTY)  where
##
##  nodeID   = internal node identifier
##  parmID   = x-var parameter identifier
##  contPT   = continuous split value
##  mvcpSZ   = mwcp size (factor flag)
##
##  p        = x-var parameter identifier
##  depth    = depth of split
##
##  spltTY   = split type for parent node
##             bit 1   bit 0   meaning
##             -----   -----   ------- 
##               0       0     0 = both daughters have valid splits 
##               1       0     2 = only the left daughter is terminal 
##               0       1     1 = only the right daughter is terminal 
##               1       1     3 = both daughters are terminal
##
##  spltEC   = end cut preference statistic
##              currently as follows:
##              Let x1 < x2 < ... < xn
##              Let s = xj be the actual split point where
##              X <= s is a left node and
##              X >  s is a right node.
##              Thus, 1 <= j <= n-1.  Define
##              ECP = min { |n-1-j|, |j-1| } / (n - 1)
##
##
##  spltST   = if prune = FLASE:  split statistic on which the node was split
##           = if prune =  TRUE:  pseudo-terminal node indicator
##
##
##  Note:  this function depends on the global variable xvar.
##
## #######################################################################
### Recursive function to determine first the split point information for a tree.
spParseTree <- function(recursiveObject, extendedNativeArray, xvar,
                        b,
                        depth,
                        membership,
                        ptnFlag,
                        prune = prune) {
  ## Weak consistency check to ensure that the iteration matches the treeID in the nativeArray record.
  if(b != extendedNativeArray$treeID[recursiveObject$offset]) {
    stop("Invalid nativeArray input record (treeID) at ", recursiveObject$offset, ".  Please contact Technical Support.")
  }
  ## Read the current nativeArray split information.
  treeID <- extendedNativeArray$treeID[recursiveObject$offset]
  nodeID <- extendedNativeArray$nodeID[recursiveObject$offset]  
  parmID <- extendedNativeArray$parmID[recursiveObject$offset]
  mwcpSZ <- extendedNativeArray$mwcpSZ[recursiveObject$offset]
  contPT <- extendedNativeArray$contPT[recursiveObject$offset]
  spltST <- extendedNativeArray$spltST[recursiveObject$offset]
  ## We must continue to parse the native array until we reach a terminal node.
  ## Determine whether this is a terminal node.
  if (parmID == 0) {
    recursiveObject$terminal <- TRUE    
  }
    else {
      recursiveObject$terminal <- FALSE
    }
  ## If we are pruning, we do not update the outputs if we have descended past a pseudo-terminal
  ## node.  Thus, if the PTN flag has been turned on, it stays on for all nodes below
  ## that point, AFTER the update.
  ## Assume we update as the default.
  if (prune) {
    if (ptnFlag == FALSE) {
      if (spltST == 1) {
        ptnFlag = TRUE
      }
    }
  }
  updateFlag <- TRUE
  if ((parmID == 0) || (ptnFlag)) {
    updateFlag <- FALSE    
  }
  if (updateFlag) {
    if(mwcpSZ != 0) {
      ## Factor split:  the EC statistic will not be coherent.
    }
    ## Calculate the unique split points in this node on xvar that is being split.
    split.vec <- sort(unique(xvar[unique(membership), parmID]))
    ## Determine the location of the split point in this vector.
    split.idx <- which(split.vec == contPT)
    spltEC <-  min(length(split.vec) - 1 - split.idx, split.idx - 1) / (length(split.vec) - 1) 
    ## Determine the left and right membership replicate vectors.
    left.membership <- membership[which(xvar[membership, parmID] <= contPT)]
    rght.membership <- membership[which(xvar[membership, parmID] >  contPT)]    
    ## Update the recursive object containing the split information.
    ## This will be the depth, node identifier, and (continuous) split
    ## value.  We currently do not have the ability to return factor
    ## split values, but we do identify their occurrence.
    localInfo <- c(treeID, nodeID, parmID, contPT, mwcpSZ, depth, 0, spltEC, spltST)
    if (is.null(recursiveObject$splitInfo[[parmID]])) {
      recursiveObject$splitInfo[[parmID]] <- rbind(recursiveObject$splitInfo[[parmID]], localInfo, deparse.level = 0)
      ## Attach names only once.
      colnames(recursiveObject$splitInfo[[parmID]]) <- c("treeID", "nodeID", "parmID", "contPT", "mwcpSZ", "dpthID",  "spltTY", "spltEC", "spltST")  
    }
      else {
        recursiveObject$splitInfo[[parmID]] <- rbind(recursiveObject$splitInfo[[parmID]], localInfo, deparse.level = 0)
      }
    ## Get the row and column identifier for the split type statistic
    ## and use these below to update the statistic.  This statistic depends
    ## on the daughter calls to the recursive function, and hence we cannot
    ## populate them at or before this point.
    col.idx <-  which(colnames(recursiveObject$splitInfo[[parmID]]) == "spltTY")    
    row.idx <-  dim(recursiveObject$splitInfo[[parmID]])[1]
  }
  ## Increment the offset regardless of terminal node status.  We
  ## always parse the native forest object at each entry into the
  ## recursive routine after initializing the nodal information.
  recursiveObject$offset <- recursiveObject$offset + 1
  ## Parse left and then right, if this is not a terminal node.
  if (!recursiveObject$terminal) {
    ## Increment the (parsed) tree depth. 
    depth <- depth + 1
    ## Parse left:
    recursiveObject <- spParseTree(recursiveObject, extendedNativeArray, xvar,
                                   b, depth, left.membership, ptnFlag, prune)
    ## The following updates the split type as a result of a left
    ## split. This is only done if updates have been requested.
    if(updateFlag) {    
      split.type <- recursiveObject$splitInfo[[parmID]][row.idx, ]
      split.type[col.idx] <- split.type[col.idx] + 2 
      recursiveObject$splitInfo[[parmID]][row.idx, ] = split.type
      recursiveObject$terminal <-  FALSE          
    }
    ## Parse right:
    recursiveObject <- spParseTree(recursiveObject, extendedNativeArray, xvar,
                                   b, depth, rght.membership, ptnFlag, prune)
    ## The following updates the split type as a result of a left
    ## split. This is only done if updates have been requested.
    if(updateFlag) {    
      split.type <- recursiveObject$splitInfo[[parmID]][row.idx, ]
      split.type[col.idx] <- split.type[col.idx] + 1 
      recursiveObject$splitInfo[[parmID]][row.idx, ] = split.type
      recursiveObject$terminal <-  FALSE          
    }
  }
  return(recursiveObject)
}


#' Acquire Split Statistic Information
#' 
#' Extract split statistic information from the forest.  The function returns a
#' list of length \code{ntree}, in which each element corresponds to a tree.
#' The element [[b]] is itself a vector of length xvar.names identified by its
#' x-variable name.  Each element [[b]]$xvar contains the complete list of
#' splits on xvar with associated identifying information.  The information is
#' as follows:
#' 
#' \enumerate{
#' 
#' \item \emph{treeID} Tree identifier.  \item \emph{nodeID} Node identifier.
#' \item \emph{parmID} Variable indentifier.  \item \emph{contPT} Value node
#' was split in the case of a continuous variable.  \item \emph{mwcpSZ} Size of
#' the multi-word complementary pair in the case of a factor split.  \item
#' \emph{dpthID} Zero (0) based depth of split.  \item \emph{spltTY} Split type
#' for parent node:
#' 
#' \tabular{lll}{ bit 1 \tab bit 0 \tab meaning\cr ----- \tab ----- \tab
#' ------- \cr 0 \tab 0 \tab 0 = both daughters have valid splits\cr 0 \tab 1
#' \tab 1 = only the right daughter is terminal\cr 1 \tab 0 \tab 2 = only the
#' left daughter is terminal\cr 1 \tab 1 \tab 3 = both daughters are
#' terminal\cr }
#' 
#' \item \emph{spltEC} End cut statistic for real valued variables between
#' [0,0.5] that is small when the split is towards the edge and large when the
#' split is towards the middle.  Subtracting this value from 0.5 yields the end
#' cut statistic studied in Ishwaran (2014) and is a way to identify ECP
#' behavior (end cut preference behavior).
#' 
#' \item \emph{spltST} Split statistic:
#' 
#' \enumerate{ \item For objects of class (rfsrc, grow), this is the split
#' statistic that resulted in the variable being choosen for the split.  \item
#' For an object of class (rfsrc, pred) this is the variance of the response
#' within the node for the test data.  This value is relevant only for real
#' valued responses.  In classification and survival, it is not relevant.  } }
#' 
#' 
#' @aliases stat.split stat.split.rfsrc
#' @param object An object of class \code{(rfsrc, grow)}, \code{(rfsrc,
#' synthetic)} or \code{(rfsrc, predict)}
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly, a list with the following components: \item{...}{...}
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @references Ishwaran H. (2015).  The effect of splitting on random forests.
#' \emph{Machine Learning}, 99:75-118.
#' @keywords splitting behavior
#' @examples
#' 
#' \donttest{
#' ## run a forest, then make a call to stat.split
#' grow.obj <- rfsrc(mpg ~., data = mtcars, membership=TRUE, statistics=TRUE)
#' stat.obj <- stat.split(grow.obj)
#' 
#' ## nice wrapper to extract split-statistic for desired variable
#' ## for continuous variables plots ECP data
#' get.split <- function(splitObj, xvar, inches = 0.1, ...) {
#'   which.var <- which(names(splitObj[[1]]) == xvar)
#'   ntree <- length(splitObj)
#'   stat <- data.frame(do.call(rbind, sapply(1:ntree, function(b) {
#'     splitObj[[b]][which.var]})))
#'   dpth <- stat$dpthID
#'   ecp <- 1/2 - stat$spltEC
#'   sp <- stat$contPT
#'   if (!all(is.na(sp))) {
#'     fgC <- function(x) {
#'       as.numeric(as.character(cut(x, breaks = c(-1, 0.2, 0.35, 0.5),
#'       labels = c(1, 4, 2))))
#'     }
#'     symbols(jitter(sp), jitter(dpth), ecp, inches = inches, bg = fgC(ecp),
#'       xlab = xvar, ylab = "node depth", ...)
#'     legend("topleft", legend = c("low ecp", "med ecp", "high ecp"),
#'       fill = c(1, 4, 2))
#'    }
#'   invisible(stat)
#' }
#' 
#' ## use get.split to investigate ECP behavior of variables
#' get.split(stat.obj, "disp")
#' }
#' 
stat.split <- stat.split.rfsrc
