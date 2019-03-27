find.interaction.rfsrc <- function(
  object, 
  xvar.names,
  cause,
  m.target = NULL,
  importance = c("permute", "random", "anti", "permute.ensemble", "random.ensemble", "anti.ensemble"),
  method = c("maxsubtree", "vimp"),
  sorted = TRUE,
  nvar = NULL, 
  nrep  = 1,
  subset,
  na.action = c("na.omit", "na.impute"),
  seed = NULL,
  do.trace = FALSE,
  verbose = TRUE,
  ...)
{
  ## Check that 'object' is of the appropriate type.
  if (is.null(object)) stop("Object is empty!")
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2    &
      sum(inherits(object, c("rfsrc", "forest"), TRUE) == c(1, 2)) != 2)
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, forest)'.")
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) == 2) {
    if (is.null(object$forest)) 
      stop("Forest is empty!  Re-run grow call with forest set to 'TRUE'.")
  }
  ## specify the default event type for CR
  if (missing(cause)) {
    cause <- 1
  }
  ## unsupervised family: minimal depth is the only permissible method
  if (object$family == "unsupv") {
    method <- "maxsubtree"
  }
  ## Verify key options
  method <- match.arg(method,  c("maxsubtree", "vimp"))
  importance <- match.arg(importance, c("permute", "random", "anti",
                                        "permute.ensemble", "random.ensemble", "anti.ensemble"))
  ## get the event data
  event.info <- get.event.info(object)
  n.event <- max(1, length(event.info$event.type))
  ## acquire the target outcome (if there is one)
  m.target <- get.univariate.target(object, m.target)
  ## extract the importance
  if (n.event > 1) {
    object.imp <- NULL
    interact.imp.list.names <- paste("event.", 1:length(event.info$event.type), sep = "")
  }
    else {
      object.imp <- cbind(coerce.multivariate(object, m.target)$importance)[, cause, drop = FALSE]
    }
  ## pull the original xvariable names
  xvar.org.names <- object$xvar.names
  ## has the user provided a subset of variables to focus on?
  if (!missing(xvar.names)) {
    if (sum(is.element(xvar.org.names, xvar.names)) == 0) {
      stop("Variables do not match original analysis:", xvar.names)
    }
    xvar.names <- unique(xvar.names[is.element(xvar.names, xvar.org.names)])
    if (length(xvar.names) == 1) 
      stop("Pairwise comparisons require more than one candidate variable.")
  }
    else {
      xvar.names <- xvar.org.names
    }
  ## determine the number of remaining variables
  n.interact <- length(xvar.names)
  ## sort the variables by VIMP?
  if (sorted) {
    if (!is.null(object.imp)) {
      o.r <- order(object.imp[xvar.names, ], decreasing = TRUE)
      xvar.names <- xvar.names[o.r]
    }
  }
  ## restrict attention to top nvar variables?
  if (!missing(nvar)) {
    n.interact <- min(n.interact, max(round(nvar), 1))
    xvar.names <- xvar.names[1:n.interact]
  }
  if (n.interact == 1) {
    stop("Pairwise comparisons require more than one candidate variable.")
  }
  ## VIMP approach
  if (method == "vimp") {
    yvar.dim <- ncol(object$yvar)
    ## Save the original family.
    family.org <- object$family
    if (n.event > 1) {
      interact.imp.list <- vector("list", n.event)
      names(interact.imp.list) <- interact.imp.list.names
    }
    for (j in 1:n.event) {
      rownames.interact.imp <- interact.imp <- NULL
      target.dim <- ifelse(n.event > 1, j, 1)
      if (verbose && n.event > 1) {
        cat("--> event", j, "\n")
      }
      for (k in 1:(n.interact-1)) {
        n.joint.var <- n.interact - k
        imp <- rep(0 , 1 + n.joint.var)
        imp.joint <- rep(0, n.joint.var)
        for (l in (k+1):n.interact) {
          if (verbose) {
            cat("Pairing",xvar.names[k],"with",xvar.names[l],"\n")
          }
          for (m in 1:nrep) {
            imp.indv.m <- c(cbind(coerce.multivariate(vimp(object, xvar.names[c(k,l)],
                                                           m.target = m.target,
                                                           importance = importance, joint = FALSE, 
                                                           na.action = na.action, subset = subset, seed = seed, 
                                                           do.trace = do.trace), m.target)$importance)[, target.dim])
            imp.joint.m <- coerce.multivariate(vimp(object, xvar.names[c(k,l)], m.target = m.target,
                                                    importance = importance, joint = TRUE,
                                                    na.action = na.action, subset = subset, seed = seed,
                                                    do.trace = do.trace), m.target)$importance[target.dim]
            imp[1] <- imp[1] + imp.indv.m[1]
            imp[l-k+1] <- imp[l-k+1] + imp.indv.m[2]
            imp.joint[l-k] <- imp.joint[l-k] + imp.joint.m
          }
        }
        imp[1] <- imp[1] / n.joint.var
        imp <- imp/nrep
        imp.joint <- imp.joint/nrep
        interact.imp <- rbind(interact.imp,
                              cbind(imp[1], imp[-1], imp.joint, (imp[1] + imp)[-1], imp.joint - (imp[1] + imp)[-1]))
        rownames.interact.imp <- c(rownames.interact.imp,
                                   paste(xvar.names[k],":",xvar.names[(k+1):n.interact],
                                         sep=""))
      }
      colnames(interact.imp) <- c("Var 1", "Var 2","Paired","Additive","Difference")
      rownames(interact.imp) <- rownames.interact.imp
      if (n.event > 1) {
        interact.imp.list[[j]] <- interact.imp
      }
    }
    ## CR details
    if (n.event > 1) {
      interact.imp <- interact.imp.list
    }
    ## output table
    if (verbose) {
      cat("\n")
      cat("                              Method: ", method,                       "\n", sep="")
      cat("                    No. of variables: ", n.interact,                   "\n", sep="")
      if (family.org == "regr+" | family.org == "class+" | family.org == "mix+") {
        cat("              Total no. of responses: ", yvar.dim,                 "\n", sep="")
        cat("         User has requested response: ", m.target,           "\n", sep="")
      }
      cat("           Variables sorted by VIMP?: ", sorted,                       "\n", sep="")
      cat("   No. of variables used for pairing: ", n.interact,                   "\n", sep="")
      cat("    Total no. of paired interactions: ", length(rownames.interact.imp),"\n", sep="")
      cat("            Monte Carlo replications: ", nrep,                         "\n", sep="")
      cat("    Type of noising up used for VIMP: ", importance,                   "\n", sep="")
      cat("\n")
      if (n.event == 1) print(round(interact.imp, 4)) else print(interact.imp)
    }
    ## return the goodies
    invisible(interact.imp)
  }
    else {
      ## maximal subtree approach
      max.obj <- max.subtree(object, sub.order = TRUE, max.order = 1)
      sub.order <- max.obj$sub.order
      if (sorted) {
        o.r <- order(diag(sub.order), decreasing = FALSE)
        sub.order <- sub.order[o.r, o.r]
      }
      xvar.pt <- is.element(colnames(sub.order), xvar.names[1:n.interact])
      sub.order <- sub.order[xvar.pt, xvar.pt]
      ## output table
      if (verbose) {
        cat("\n")
        cat("                              Method: ", method,              "\n", sep="")
        cat("                    No. of variables: ", n.interact,          "\n", sep="")
        cat("  Variables sorted by minimal depth?: ", sorted,              "\n", sep="")
        cat("\n")
        print(round(sub.order, 2))
      }
      ## return the goodies
      invisible(sub.order)
    }
}


#' Find Interactions Between Pairs of Variables
#' 
#' Find pairwise interactions between variables.
#' 
#' Using a previously grown forest, identify pairwise interactions for all
#' pairs of variables from a specified list.  There are two distinct approaches
#' specified by the option \option{method}.
#' 
#' \enumerate{ \item \option{method="maxsubtree"}
#' 
#' This invokes a maximal subtree analysis.  In this case, a matrix is returned
#' where entries [i][i] are the normalized minimal depth of variable [i]
#' relative to the root node (normalized wrt the size of the tree) and entries
#' [i][j] indicate the normalized minimal depth of a variable [j] wrt the
#' maximal subtree for variable [i] (normalized wrt the size of [i]'s maximal
#' subtree).  Smaller [i][i] entries indicate predictive variables.  Small
#' [i][j] entries having small [i][i] entries are a sign of an interaction
#' between variable i and j (note: the user should scan rows, not columns, for
#' small entries).  See Ishwaran et al. (2010, 2011) for more details.
#' 
#' \item \option{method="vimp"}
#' 
#' This invokes a joint-VIMP approach.  Two variables are paired and their
#' paired VIMP calculated (refered to as 'Paired' importance).  The VIMP for
#' each separate variable is also calculated.  The sum of these two values is
#' refered to as 'Additive' importance.  A large positive or negative
#' difference between 'Paired' and 'Additive' indicates an association worth
#' pursuing if the univariate VIMP for each of the paired-variables is
#' reasonably large.  See Ishwaran (2007) for more details.  }
#' 
#' Computations might be slow depending upon the size of the data and the
#' forest.  In such cases, consider setting \option{nvar} to a smaller number.
#' If \option{method="maxsubtree"}, consider using a smaller number of trees in
#' the original grow call.
#' 
#' If \option{nrep} is greater than 1, the analysis is repeated \code{nrep}
#' times and results averaged over the replications (applies only when
#' \option{method="vimp"}).
#' 
#' @aliases find.interaction find.interaction.rfsrc
#' @param object An object of class \code{(rfsrc, grow)} or \code{(rfsrc,
#' forest)}.
#' @param xvar.names Character vector of names of target x-variables.  Default
#' is to use all variables.
#' @param cause For competing risk families, integer value between 1 and
#' \code{J} indicating the event of interest, where \code{J} is the number of
#' event types. The default is to use the first event type.
#' @param m.target Character value for multivariate families specifying the
#' target outcome to be used.  If left unspecified, the algorithm will choose a
#' default target.
#' @param importance Type of variable importance (VIMP). See \command{rfsrc}
#' for details.
#' @param method Method of analysis: maximal subtree or VIMP.  See details
#' below.
#' @param sorted Should variables be sorted by VIMP?  Does not apply for
#' competing risks.
#' @param nvar Number of variables to be used.
#' @param nrep Number of Monte Carlo replicates when \option{method="vimp"}.
#' @param subset Vector indicating which rows of the x-variable matrix from the
#' \code{object} are to be used. Uses all rows if not specified.
#' @param na.action Action to be taken if the data contains \code{NA} values.
#' Applies only when \option{method="vimp"}.
#' @param seed Seed for random number generator.  Must be a negative integer.
#' @param do.trace Number of seconds between updates to the user on approximate
#' time to completion.
#' @param verbose Set to \code{TRUE} for verbose output.
#' @param ... Further arguments passed to or from other methods.
#' @return Invisibly, the interaction table (a list for competing risk data) or
#' the maximal subtree matrix.
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{max.subtree}}, \command{\link{var.select}},
#' \command{\link{vimp}}
#' @references Ishwaran H. (2007).  Variable importance in binary regression
#' trees and forests, \emph{Electronic J. Statist.}, 1:519-537.
#' 
#' Ishwaran H., Kogalur U.B., Gorodeski E.Z, Minn A.J. and Lauer M.S. (2010).
#' High-dimensional variable selection for survival data.  \emph{J. Amer.
#' Statist. Assoc.}, 105:205-217.
#' 
#' Ishwaran H., Kogalur U.B., Chen X. and Minn A.J. (2011).  Random survival
#' forests for high-dimensional data. \emph{Statist. Anal. Data Mining},
#' 4:115-132.
#' @keywords variable selection
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## find interactions, survival setting
#' ## ------------------------------------------------------------
#' 
#' data(pbc, package = "randomForestSRC") 
#' pbc.obj <- rfsrc(Surv(days,status) ~ ., pbc, nsplit = 10, importance = TRUE)
#' find.interaction(pbc.obj, method = "vimp", nvar = 8)
#' 
#' ## ------------------------------------------------------------
#' ## find interactions, competing risks
#' ## ------------------------------------------------------------
#' 
#' data(wihs, package = "randomForestSRC")
#' wihs.obj <- rfsrc(Surv(time, status) ~ ., wihs, nsplit = 3, ntree = 100,
#'                        importance = TRUE)
#' find.interaction(wihs.obj)
#' find.interaction(wihs.obj, method = "vimp")
#' 
#' ## ------------------------------------------------------------
#' ## find interactions, regression setting
#' ## ------------------------------------------------------------
#' 
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality, importance = TRUE)
#' find.interaction(airq.obj, method = "vimp", nrep = 3)
#' find.interaction(airq.obj)
#' 
#' ## ------------------------------------------------------------
#' ## find interactions, classification setting
#' ## ------------------------------------------------------------
#' 
#' iris.obj <- rfsrc(Species ~., data = iris, importance = TRUE)
#' find.interaction(iris.obj, method = "vimp", nrep = 3)
#' find.interaction(iris.obj)
#' 
#' ## ------------------------------------------------------------
#' ## interactions for multivariate mixed forests
#' ## ------------------------------------------------------------
#' 
#' mtcars2 <- mtcars
#' mtcars2$cyl <- factor(mtcars2$cyl)
#' mtcars2$carb <- factor(mtcars2$carb, ordered = TRUE)
#' mv.obj <- rfsrc(cbind(carb, mpg, cyl) ~., data = mtcars2, importance = TRUE)
#' find.interaction(mv.obj, method = "vimp", outcome.target = "carb")
#' find.interaction(mv.obj, method = "vimp", outcome.target = "mpg")
#' find.interaction(mv.obj, method = "vimp", outcome.target = "cyl")
#' }
#' 
find.interaction <- find.interaction.rfsrc
