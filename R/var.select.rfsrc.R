var.select.rfsrc <-
  function(formula,          
           data,
           object,
           cause,
           m.target = NULL,
           method = c("md", "vh", "vh.vimp"),
           conservative = c("medium", "low", "high"),
           ntree = (if (method == "md") 1000 else 500),
           mvars = (if (method != "md") ceiling(ncol(data)/5) else NULL),
           mtry = (if (method == "md") ceiling(ncol(data)/3) else NULL),
           nodesize = 2,
           splitrule = NULL,
           nsplit = 10,
           xvar.wt = NULL,
           refit = (method != "md"),
           fast = FALSE,
           na.action = c("na.omit", "na.impute"),
           always.use = NULL,  
           nrep = 50,        
           K = 5,             
           nstep = 1,         
           prefit =  list(action = (method != "md"), ntree = 100, mtry = 500, nodesize = 3, nsplit = 1),
           verbose = TRUE,
           ...
           )
{
  ## --------------------------------------------------------------
  ##  
  ##  workhorse: variable hunting algorithm
  ##
  ## --------------------------------------------------------------
  rfsrc.var.hunting <- function(train.id, var.pt, nstep) {
    ## ------------------filtering step-----------------------
    if (verbose) cat("\t", paste("selecting variables using", mName), "...\n")
    ## which variables to include
    drop.var.pt <- setdiff(var.columns, var.pt)
    ## family specific checks
    if (grepl("surv", family)) {
      if (sum(data[train.id, 2], na.rm = TRUE) < 2) {
        stop("training data has insufficient deaths: K is probably set too high\n")
      }
    }
    ## filtered forest
    ## over-ride user mtry setting: use an aggressive value
    rfsrc.filter.obj  <- rfsrc(rfsrc.all.f,
                               data=(if (LENGTH(var.pt, drop.var.pt)) data[train.id, -drop.var.pt]
                                       else data[train.id, ]),
                               ntree = ntree,
                               splitrule = splitrule,
                               nsplit = nsplit,
                               mtry = Mtry(var.columns, drop.var.pt),
                               nodesize = nodesize,
                               cause = cause,
                               na.action = na.action,
                               importance=TRUE,
                               err.block = err.block, perf.type = perf.type)
    ## set the target dimension for CR families
    if (rfsrc.filter.obj$family == "surv-CR") {
      target.dim <- max(1, min(cause, max(get.event.info(rfsrc.filter.obj)$event.type)), na.rm = TRUE)
    }
    ## extract vimp
    ## for multivariate families we must manually extract the importance and error rate
    imp <- get.varselect.imp(coerce.multivariate(rfsrc.filter.obj, m.target), target.dim)
    names(imp) <- rfsrc.filter.obj$xvar.names
    ## selection using vimp
    if (method == "vh.vimp") {
      VarStrength <- sort(imp, decreasing = TRUE)
      lower.VarStrength <- min(VarStrength) - 1 #need theoretical lower bound to vimp
      n.lower <- min(2, length(VarStrength))    #n.lower cannot be > no. available variables
      forest.depth <- m.depth <- NA
      sig.vars.old <- names(VarStrength)[1]
    }
    ## selection using minimal depth
      else {
        max.obj <- max.subtree(rfsrc.filter.obj, conservative = (conservative == "high"))
        if (is.null(max.obj$order)) {
          ## maximal information failed; revert to vimp
          VarStrength <- lower.VarStrength <- 0
          forest.depth <- m.depth <- NA
          sig.vars.old <- names(sort(imp, decreasing = TRUE))[1]
        }
          else {
            m.depth <- VarStrength <- max.obj$order[, 1]
            forest.depth <- floor(mean(apply(max.obj$nodes.at.depth, 2, function(x){sum(!is.na(x))}), na.rm=TRUE))
            exact.threshold <- ifelse(conservative == "low", max.obj$threshold.1se, max.obj$threshold)
            n.lower <- max(min(2, length(VarStrength)),#n.lower cannot be > no. available variables
                           sum(VarStrength <= exact.threshold))
            VarStrength <- max(VarStrength) - VarStrength
            names(m.depth) <- names(VarStrength) <- rfsrc.filter.obj$xvar.names
            VarStrength <- sort(VarStrength, decreasing = TRUE)
            lower.VarStrength <- -1 #need theoretical upper bound to first order statistic
            sig.vars.old <- names(VarStrength)[1]
          }
      }
    ## set nstep
    nstep <- max(round(length(rfsrc.filter.obj$xvar.names)/nstep), 1)
    imp.old <- 0
    ## regularized forward selection using joint vimp
    for (b in 1:nstep) {
      if (b == 1) {
        if (sum(VarStrength > lower.VarStrength) == 0) {
          sig.vars <- sig.vars.old
          break
        }
        n.upper <- max(which(VarStrength > lower.VarStrength), n.lower)
        threshold <- unique(round(seq(n.lower, n.upper, length = nstep)))
        if (length(threshold) < nstep) {
          threshold <- c(threshold, rep(max(threshold), nstep - length(threshold)))
        }
      }
      sig.vars <- names(VarStrength)[1:threshold[b]]
      if (!is.null(always.use)) {
        sig.vars <- unique(c(sig.vars, always.use))
      }
      if (length(sig.vars) <= 1) {#break if there is only one variable
        sig.vars <- sig.vars.old
        break
      }
      imp <- coerce.multivariate(vimp(rfsrc.filter.obj, sig.vars, m.target = m.target,
                                      joint = TRUE), m.target)$importance[target.dim]
      ## verbose output
      if (verbose) cat("\t iteration: ", b,
                       "  # vars:",     length(sig.vars),
                       "  joint-vimp:",  round(imp, 3),
                       "\r")
      ## break when joint vimp no longer increases (strict inequality is a safety feature)
      if (imp  <= imp.old) {
        sig.vars <- sig.vars.old
        break
      }
        else {
          var.pt <- var.columns[match(sig.vars, xvar.names)]
          sig.vars.old <- sig.vars
          imp.old <- imp
        }
    }
    ## refit forest and exit
    ## over-ride user specified nodesize: use default mtry as variables 
    ## should now be filtered and default settings should therefore apply
    var.pt <- var.columns[match(sig.vars, xvar.names)]
    drop.var.pt <- setdiff(var.columns, var.pt)
    rfsrc.obj  <- rfsrc(rfsrc.all.f,
                        data=(if (LENGTH(var.pt, drop.var.pt)) data[train.id, -drop.var.pt]
                                else data[train.id, ]),
                        ntree = ntree,
                        splitrule = splitrule,
                        nsplit = nsplit,
                        cause = cause,
                        na.action = na.action,
                        perf.type = perf.type)
    return(list(rfsrc.obj=rfsrc.obj, sig.vars=rfsrc.obj$xvar.names, forest.depth=forest.depth, m.depth=m.depth))
  }
  ## --------------------------------------------------------------
  ##   
  ##   preliminary processing
  ##
  ## --------------------------------------------------------------
  ## Incoming parameter checks.  All are fatal.
  if (!missing(object)) {
    if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2) {
      stop("This function only works for objects of class `(rfsrc, grow)'")
    }
    if (is.null(object$forest)) {
      stop("Forest is empty!  Re-run grow call with forest set to 'TRUE'")
    }
    rfsrc.all.f <- object$formula
  }
    else {
      if (missing(formula) || missing(data)) {
        ## allowance for users who overlook the correct way to assign the object
        if (sum(inherits(formula, c("rfsrc", "grow"), TRUE) == c(1, 2)) == 2) {
          object <- formula
        }
          else {
            stop("Need to specify 'formula' and 'data' or provide a grow forest object")
          }
      }
      rfsrc.all.f <- formula
    }
  ## If an object is provided, after the above checks, make the m.target coherent.
  if (!missing(object)) {
    ## initialize the target outcome, in case it is NULL
    ## coersion of an object depends on a target outcome
    m.target <- get.univariate.target(object, m.target)
  }
  ## rearrange the data
  ## need to handle unsupervised families carefully: minimal depth is the only permissible method
  ## pull performance parameters 
  if (missing(object)) {
    ## parse the formula
    formulaDetail <- finalizeFormula(parseFormula(rfsrc.all.f, data), data)
    family <- formulaDetail$family
    xvar.names <- formulaDetail$xvar.names
    yvar.names <- formulaDetail$yvar.names
    if (family != "unsupv") {
      data <- cbind(data[, yvar.names, drop = FALSE], data[, match(xvar.names, names(data))])
      yvar <- data[, yvar.names]
      yvar.dim <- ncol(cbind(yvar))
    }
      else {
        data <- data[, match(xvar.names, names(data))]
        yvar.dim <- 0
        method <- "md"
    }
    dots <- list(...)
    err.block <- dots$err.block
    perf.type <- is.hidden.perf.type(dots)
  }
    else {
      ## parse the object
      family <- object$family
      xvar.names <- object$xvar.names
      if (family != "unsupv") {
        yvar.names <- object$yvar.names
        data <- data.frame(object$yvar, object$xvar)
        colnames(data) <- c(yvar.names, xvar.names)
        yvar <- data[, yvar.names]
        yvar.dim <- ncol(cbind(yvar))
      }
        else {
          data <- object$xvar
          yvar.dim <- 0
          method <- "md"
      }
    err.block <- object$err.block
    perf.type <- object$forest$perf.type
    }
  ## specify the default event type for CR
  if (missing(cause)) {
    cause <- 1
  }
  ## verify key options
  method <- match.arg(method, c("md", "vh", "vh.vimp"))
  conservative = match.arg(conservative, c("medium", "low", "high"))
  ## pretty names for method
  mName <- switch(method,
                  "md"      = "Minimal Depth",
                  "vh"      = "Variable Hunting",
                  "vh.vimp" = "Variable Hunting (VIMP)")
  ## simplify the formula: needed when we drop variables
  rfsrc.all.f <- switch(family,
                        "surv"   = as.formula(paste("Surv(",yvar.names[1],",",yvar.names[2],") ~ .")),
                        "surv-CR"= as.formula(paste("Surv(",yvar.names[1],",",yvar.names[2],") ~ .")),
                        "regr"   = as.formula(paste(yvar.names, "~ .")),
                        "class"  = as.formula(paste(yvar.names, "~ .")),
                        "unsupv" = NULL,
                        "regr+"  = as.formula(paste("Multivar(", paste(yvar.names, collapse = ","), paste(") ~ ."), sep = "")),
                        "class+" = as.formula(paste("Multivar(", paste(yvar.names, collapse = ","), paste(") ~ ."), sep = "")),
                        "mix+"   = as.formula(paste("Multivar(", paste(yvar.names, collapse = ","), paste(") ~ ."), sep = ""))
                        )
  ## initialize dimensions
  n <- nrow(data)
  P <- length(xvar.names)
  ## Specify the target event.  Later will be over-written for CR
  target.dim <- 1
  ## make special allowance for always.use x-variables
  var.columns <- (1 + yvar.dim):ncol(data)
  if (!is.null(always.use)) {
    always.use.pt <- var.columns[match(always.use, xvar.names)]
  }
    else {
      always.use.pt <- NULL
    }
  ## if xvar weight is specified, then make sure it is defined correctly
  xvar.wt <- get.weight(xvar.wt, P)
  ## Final checks on option parameters  
  if (!is.null(mtry)) {
    mtry <- round(mtry)
    if (mtry < 1 | mtry > P) mtry <- max(1, min(mtry, P))
  }
  ## prefit forest parameter details
  prefit.masterlist <- list(action = (method != "md"), ntree = 100, mtry = 500, nodesize = 3, nsplit = 1)
  parm.match <- na.omit(match(names(prefit), names(prefit.masterlist)))
  if (length(parm.match) > 0) {
    for (l in 1:length(parm.match)) {
      prefit.masterlist[[parm.match[l]]] <- prefit[[l]]
    }
  }
  prefit <- prefit.masterlist
  prefit.flag  <- prefit$action
  ## --------------------------------------------------------------
  ##  
  ##  minimal depth analysis
  ##
  ## --------------------------------------------------------------
  if (method == "md") {
    ## ------------------------------------------------
    ## run preliminary forest to determine weights for variables
    ## we do this OUTSIDE of the loop
    if (prefit.flag && is.null(xvar.wt) && missing(object)) {
      if (verbose) cat("Using forests to preweight each variable's chance of splitting a node...\n")
      rfsrc.prefit.obj  <- rfsrc(rfsrc.all.f,
                                 data = data,
                                 ntree = prefit$ntree,
                                 nodesize = prefit$nodesize,
                                 mtry = prefit$mtry,
                                 splitrule = splitrule,
                                 nsplit = prefit$nsplit,
                                 cause = cause,
                                 na.action = na.action,
                                 importance = TRUE,
                                 err.block = err.block, perf.type = perf.type)
      ## set the target dimension for CR families
      if (rfsrc.prefit.obj$family == "surv-CR") {
        target.dim <- max(1, min(cause, max(get.event.info(rfsrc.prefit.obj)$event.type)), na.rm = TRUE)
      }
      ## for multivariate families we must manually extract the importance and error rate
      wts <- pmax(get.varselect.imp(coerce.multivariate(rfsrc.prefit.obj, m.target), target.dim), 0)
      if (any(wts > 0)) {
        xvar.wt <- get.weight(wts, P)
      }
      rm(rfsrc.prefit.obj)
    }
    ## ------------------------------------------------
    ## extract minimal depth
    if (!missing(object)) {
    }
    if (!missing(object) && !prefit.flag) {
      if (verbose) cat("minimal depth variable selection ...\n")
      md.obj <- max.subtree(object, conservative = (conservative == "high"))
      ## for multivariate families we must manually extract the importance and error rate
      object <- coerce.multivariate(object, m.target)
      m.target <- object$outcome.target
      pe <- get.varselect.err(object)
      ntree <- object$ntree
      nsplit <- object$nsplit
      mtry <- object$mtry
      nodesize <- object$nodesize
      ## set the target dimension for CR families
      if (family == "surv-CR") {
        target.dim <- max(1, min(cause, max(get.event.info(object)$event.type)), na.rm = TRUE)
      }
      imp <- get.varselect.imp(object, target.dim)
      imp.all <- get.varselect.imp.all(object)
      rm(object)
    }
    ## ------------------------------------------------
    ## otherwise run forests...then extract minimal depth    
      else {
        if (verbose) cat("running forests ...\n")
        rfsrc.obj <- rfsrc(rfsrc.all.f,
                           data,
                           ntree = ntree,
                           mtry = mtry,
                           nodesize = nodesize,
                           splitrule = splitrule,
                           nsplit = nsplit,
                           cause = cause,
                           na.action = na.action,
                           xvar.wt = xvar.wt,
                           importance = TRUE,
                           err.block = err.block, perf.type = perf.type)
        ## set the target dimension for CR families
        if (rfsrc.obj$family == "surv-CR") {
          target.dim <- max(1, min(cause, max(get.event.info(rfsrc.obj)$event.type)), na.rm = TRUE)
        }
        if (verbose) cat("minimal depth variable selection ...\n")
        md.obj <- max.subtree(rfsrc.obj, conservative = (conservative == "high"))
        ## for multivariate families we must manually extract the importance and error rate
        rfsrc.obj <- coerce.multivariate(rfsrc.obj, m.target)
        m.target <- rfsrc.obj$outcome.target
        pe <- get.varselect.err(rfsrc.obj)
        imp <- get.varselect.imp(rfsrc.obj, target.dim)
        imp.all <- get.varselect.imp.all(rfsrc.obj)
        mtry <- rfsrc.obj$mtry
        nodesize <- rfsrc.obj$nodesize
        n <- nrow(rfsrc.obj$xvar)
        family <- rfsrc.obj$family
        rm(rfsrc.obj)#don't need the grow object
      }
    ## parse minimal depth information
    depth <- md.obj$order[, 1]
    threshold <- ifelse(conservative == "low", md.obj$threshold.1se, md.obj$threshold)
    top.var.pt <- (depth <= threshold)
    modelsize <- sum(top.var.pt)
    o.r.m <- order(depth, decreasing = FALSE)
    top.var.pt <- top.var.pt[o.r.m]
    varselect <- as.data.frame(cbind(depth = depth, vimp = imp.all))[o.r.m, ]
    topvars <- unique(c(always.use, rownames(varselect)[top.var.pt]))
    ## fit a forest to final variable list
    ## use default settings for nodesize, mtry due to dimension reduction
    if (refit == TRUE) {
      if (verbose) cat("fitting forests to minimal depth selected variables ...\n")
      var.pt <- var.columns[match(topvars, xvar.names)]
      var.pt <- unique(c(var.pt, always.use.pt))
      drop.var.pt <- setdiff(var.columns, var.pt)
      rfsrc.refit.obj  <- rfsrc(rfsrc.all.f,
                                data=(if (LENGTH(var.pt, drop.var.pt)) data[, -drop.var.pt, drop = FALSE] else data),
                                ntree = ntree,
                                splitrule = splitrule,
                                nsplit = nsplit,
                                na.action = na.action,
                                perf.type = perf.type)
      ## for multivariate families we must manually extract the importance and error rate
      rfsrc.refit.obj <- coerce.multivariate(rfsrc.refit.obj, m.target)
    }
      else {
        rfsrc.refit.obj <- NULL
      }
    ## output: all nicely packaged
    if (verbose) {
      cat("\n\n")
      cat("-----------------------------------------------------------\n")
      cat("family             :", family, "\n")
      if (family == "regr+" | family == "class+" | family == "mix+") {
        cat("no. y-variables    : ", yvar.dim,       "\n", sep="")
        cat("response used      : ", m.target, "\n", sep="")
      }    
      cat("var. selection     :", mName, "\n")
      cat("conservativeness   :", conservative, "\n")
      cat("x-weighting used?  :", !is.null(xvar.wt), "\n")
      cat("dimension          :", P, "\n")
      cat("sample size        :", n, "\n")
      cat("ntree              :", ntree, "\n")
      cat("nsplit             :", nsplit, "\n")
      cat("mtry               :", mtry, "\n")
      cat("nodesize           :", nodesize, "\n")
      cat("refitted forest    :", refit, "\n")
      cat("model size         :", modelsize, "\n")
      cat("depth threshold    :", round(threshold, 4), "\n")
      if (!prefit.flag) {
        cat("PE (true OOB)      :", round(pe, 4), "\n")
      }
        else {
          cat("PE (biased)        :", round(pe, 4), "\n")
        }
      cat("\n\n")
      cat("Top variables:\n")
      print(round(varselect[top.var.pt, ], 3))
      cat("-----------------------------------------------------------\n")
    }
    ## Return the goodies
    return(invisible((list(err.rate=pe,
                           modelsize=modelsize,
                           topvars=topvars,
                           varselect=varselect,
                           rfsrc.refit.obj=rfsrc.refit.obj,
                           md.obj=md.obj
                           ))))
  }  
  ## --------------------------------------------------------------
  ##  
  ##  VH algorithm
  ##
  ## --------------------------------------------------------------
  ## vectors/matrices etc.
  pred.results <- dim.results <- forest.depth <- rep(0, nrep)
  var.signature <- NULL
  var.depth <- matrix(NA, nrep, P)
  ## ------------------------------------------------
  ## run preliminary forest to determine weights for variables
  ## we do this OUTSIDE of the loop
  outside.loop <- FALSE
  if (prefit.flag & is.null(xvar.wt)) {
    if (verbose) cat("Using forests to select a variables likelihood of splitting a node...\n")
    rfsrc.prefit.obj  <- rfsrc(rfsrc.all.f,
                               data = data,
                               ntree = prefit$ntree,
                               mtry = prefit$mtry,
                               nodesize = prefit$nodesize,
                               nsplit = prefit$nsplit,
                               cause = cause,
                               splitrule = splitrule,
                               na.action = na.action,
                               perf.type = perf.type)
    ## set the target dimension for CR families
    if (rfsrc.prefit.obj$family == "surv-CR") {
      target.dim <- max(1, min(cause, max(get.event.info(rfsrc.prefit.obj)$event.type)), na.rm = TRUE)
    }
    ## record that a pre-fit has occurred
    outside.loop <- TRUE
  }
  ## ------------------------------------------------
  ## loop
  for (m in 1:nrep) {
    if (verbose & nrep>1) cat("---------------------  Iteration:", m, "  ---------------------\n")
    ## train/test subsamples
    ## use balanced sampling for CR/multiclass
    all.folds <- switch(family,
                        "surv"     =  balanced.folds(yvar[, 2], K),
                        "surv-CR"  =  balanced.folds(yvar[, 2], K),
                        "class"    =  balanced.folds(yvar, K),
                        "regr"     =  cv.folds(n, K),
                        "class+"   =  balanced.folds(yvar, K),
                        "regr+"    =  cv.folds(n, K),
                        "mix+"     =  cv.folds(n, K)
                        )    
    if (fast == TRUE) {
      train.id <- all.folds[[1]]
      test.id <- all.folds[[2]]
    }
      else {
        test.id <- all.folds[[1]]
        train.id <- setdiff(1:n, test.id)
      }
    ## run preliminary forest to determine weights for variables
    ## we do this INSIDE of the loop
    if (is.null(xvar.wt)) {
      if (!prefit.flag) {
        if (verbose) cat("Using forests to determine variable selection weights...\n")
        rfsrc.prefit.obj  <- rfsrc(rfsrc.all.f,
                                   data = data[train.id,, drop = FALSE],
                                   ntree = prefit$ntree,
                                   mtry = prefit$mtry,
                                   nodesize = prefit$nodesize,                                    
                                   nsplit = prefit$nsplit,
                                   cause = cause,
                                   splitrule = splitrule,
                                   na.action = na.action,
                                   importance = TRUE,
                                   err.block = err.block, perf.type = perf.type)
        ## set the target dimension for CR families
        if (rfsrc.prefit.obj$family == "surv-CR") {
          target.dim <- max(1, min(cause, max(get.event.info(rfsrc.prefit.obj)$event.type)), na.rm = TRUE)
        }
      }
      ## for multivariate families we must manually extract the importance and error rate
      rfsrc.prefit.obj <- coerce.multivariate(rfsrc.prefit.obj, m.target)
      wts <- pmax(get.varselect.imp(rfsrc.prefit.obj, target.dim), 0)
      if (any(wts > 0)) {
        var.pt <- unique(resample(var.columns, mvars, replace = TRUE, prob = wts))
      }
        else {
          var.pt <- var.columns[1:P]
        }
    }
      else {
        var.pt <- var.columns[1:P]
      }
    ## pre-guided gene selection
    if (!is.null(xvar.wt)) {
      var.pt <- unique(resample(var.columns, mvars, replace = TRUE, prob = xvar.wt))
    }
    ## always.use variables 
    if (!is.null(always.use)) {
      var.pt <- unique(c(var.pt, always.use.pt))
    }
    ## RFSRC gene hunting call
    object <- rfsrc.var.hunting(train.id, var.pt, nstep)
    rfsrc.obj <- object$rfsrc.obj
    m.target <- get.univariate.target(rfsrc.obj, m.target)
    sig.vars <- object$sig.vars
    if (method == "vh") {
      forest.depth[m] <- object$forest.depth
      var.depth[m, match(names(object$m.depth), xvar.names)] <- object$m.depth
    }
    ## RFSRC prediction
    ## for multivariate families we must manually extract the importance and error rate
    pred.out <- coerce.multivariate(predict(rfsrc.obj, data[test.id, ]), m.target)
    pred.results[m] <- get.varselect.err(pred.out)[target.dim] 
    dim.results[m] <- length(sig.vars)
    var.signature <- c(var.signature, sig.vars)
    ## nice output
    if (verbose) {
      cat("\t                                                                \r")
      cat("\t PE:", round(pred.results[m], 4), "     dim:", dim.results[m], "\n")
    }
  }
  ## --------------------------------------------------------------
  ## finalize details
  ## remove NA's in PE 
  pred.results <- c(na.omit(pred.results))
  ## frequency
  var.freq.all.temp <- 100 * tapply(var.signature, var.signature, length) / nrep
  freq.pt <- match(names(var.freq.all.temp), xvar.names)
  var.freq.all <- rep(0, P)
  var.freq.all[freq.pt] <- var.freq.all.temp
  ##  package it up for output
  if (method == "vh") {
    var.depth.all <- apply(var.depth, 2, mean, na.rm = T)
    varselect <- cbind(depth = var.depth.all, rel.freq = var.freq.all)
  }
    else {
      varselect <- cbind(rel.freq = var.freq.all)
    }
  o.r.f <- order(var.freq.all, decreasing = TRUE)
  rownames(varselect) <- xvar.names
  varselect <- varselect[o.r.f,, drop = FALSE]
  modelsize <- ceiling(mean(dim.results))  
  topvars <- unique(c(always.use, rownames(varselect)[1:modelsize]))
  ## fit a forest to final variable list
  if (refit == TRUE) {
    if (verbose) cat("fitting forests to final selected variables ...\n")
    var.pt <- var.columns[match(rownames(varselect)[1:modelsize], xvar.names)]
    drop.var.pt <- setdiff(var.columns, var.pt)
    rfsrc.refit.obj  <- rfsrc(rfsrc.all.f,
                              data = (if (LENGTH(var.pt, drop.var.pt)) data[, -drop.var.pt]
                                        else data),
                              na.action = na.action,
                              ntree = ntree,
                              nodesize = nodesize,
                              nsplit = nsplit,
                              cause = cause,
                              splitrule = splitrule,
                              err.block = err.block, perf.type = perf.type)
  }
    else {
      rfsrc.refit.obj <- NULL
    }
  ## output: all nicely packaged
  if (verbose) {
    cat("\n\n")
    cat("-----------------------------------------------------------\n")
    cat("family             :", family, "\n")
    if (family == "regr+" | family == "class+" | family == "mix+") {
      cat("no. y-variables    : ", yvar.dim,              "\n", sep="")
      cat("response used      : ", m.target, "\n", sep="")
    }    
    cat("var. selection     :", mName, "\n")
    cat("conservativeness   :", conservative, "\n")
    cat("dimension          :", P, "\n")
    cat("sample size        :", n, "\n")
    cat("K-fold             :", K, "\n")
    cat("no. reps           :", nrep, "\n")
    cat("nstep              :", nstep, "\n")
    cat("ntree              :", ntree, "\n")
    cat("nsplit             :", nsplit, "\n")
    cat("mvars              :", mvars, "\n")
    cat("nodesize           :", nodesize, "\n")
    cat("refitted forest    :", refit, "\n")
    if (method == "vh") {
      cat("depth ratio        :", round(mean(mvars/(2^forest.depth)), 4), "\n")
    }
    cat("model size         :", round(mean(dim.results), 4), "+/-", round(SD(dim.results), 4), "\n")
    if (outside.loop) {
      cat("PE (K-fold, biased):", round(mean(pred.results), 4), "+/-", round(SD(pred.results), 4), "\n")
    }
      else {
        cat("PE (K-fold)        :", round(mean(pred.results), 4), "+/-", round(SD(pred.results), 4), "\n")
      }
    cat("\n\n")
    cat("Top variables:\n")
    print(round(varselect[1:modelsize,, drop = FALSE], 3))
    cat("-----------------------------------------------------------\n")
  }
  ## return the goodies 
  return(invisible(list(err.rate=pred.results,
                        modelsize=modelsize,
                        topvars=topvars,
                        varselect=varselect,
                        rfsrc.refit.obj=rfsrc.refit.obj,
                        md.obj=NULL
                        )))
}
## --------------------------------------------------------------
##  
## internal functions
##
## --------------------------------------------------------------
get.varselect.imp <- function(f.o, target.dim) {
  if (!is.null(f.o$importance)) {
    c(cbind(f.o$importance)[, target.dim])
  }
    else {
      rep(NA, length(f.o$xvar.names))
    }
}
get.varselect.imp.all <- function(f.o) {
  if (!is.null(f.o$importance)) {
    imp.all <- cbind(f.o$importance)
    if (ncol(imp.all) == 1) {
      colnames(imp.all) <- "vimp"
    }
      else {
        colnames(imp.all) <- paste("vimp.", colnames(imp.all), sep = "")
      }
    imp.all
  }
    else {
      rep(NA, length(f.o$xvar.names))
    }
}
get.varselect.err <- function(f.o) {
  if (!is.null(f.o$err.rate)) {
    if (grepl("surv", f.o$family)) {
      err <- 100 * cbind(f.o$err.rate)[f.o$ntree, ]
    }
      else {
        err <- cbind(f.o$err.rate)[f.o$ntree, ]
      }
  }
    else {
      err = NA
    }
  err
}
SD <- function(x) {
  if (all(is.na(x))) {
    NA
  }
    else {
      sd(x, na.rm = TRUE)
    }
}
LENGTH <- function(x, y) {
  (length(x) > 0 & length(y) > 0)
}
Mtry <- function(x, y) {
  mtry <- round((length(x) - length(y))/3)
  if (mtry == 0) {
    round(length(x)/3)
  }
    else {
      mtry
    }
}
permute.rows <-function(x) {
  n <- nrow(x)
  p <- ncol(x)
  mm <- runif(length(x)) + rep(seq(n) * 10, rep(p, n))
  matrix(t(x)[order(mm)], n, p, byrow = TRUE)
}
balanced.folds <- function(y, nfolds = min(min(table(y)), 10)) {
  y[is.na(y)] <- resample(y[!is.na(y)], size = sum(is.na(y)), replace = TRUE)
  totals <- table(y)
  if (length(totals) < 2) {
    return(cv.folds(length(y), nfolds))
  }
    else {
      fmax <- max(totals)
      nfolds <- min(nfolds, fmax)     
      nfolds <- max(nfolds, 2)
      folds <- as.list(seq(nfolds))
      yids <- split(seq(y), y) 
      bigmat <- matrix(NA, ceiling(fmax/nfolds) * nfolds, length(totals))
      for(i in seq(totals)) {
        if(length(yids[[i]])>1){bigmat[seq(totals[i]), i] <- sample(yids[[i]])}
        if(length(yids[[i]])==1){bigmat[seq(totals[i]), i] <- yids[[i]]}
      }
      smallmat <- matrix(bigmat, nrow = nfolds)
      smallmat <- permute.rows(t(smallmat)) 
      res <- vector("list", nfolds)
      for(j in 1:nfolds) {
        jj <- !is.na(smallmat[, j])
        res[[j]] <- smallmat[jj, j]
      }
      return(res)
    }
}


#' Variable Selection
#' 
#' Variable selection using minimal depth.
#' 
#' This function implements random forest variable selection using tree minimal
#' depth methodology (Ishwaran et al., 2010).  The option \option{method}
#' allows for two different approaches:
#' 
#' \enumerate{ \item \option{method="md"}
#' 
#' Invokes minimal depth variable selection.  Variables are selected using
#' minimal depth variable selection.  Uses all data and all variables
#' simultaneously.  This is basically a front-end to the \command{max.subtree}
#' wrapper.  Users should consult the \command{max.subtree} help file for
#' details.
#' 
#' Set \option{mtry} to larger values in high-dimensional problems.
#' 
#' \item \option{method="vh"} or \option{method="vh.vimp"}
#' 
#' Invokes variable hunting.  Variable hunting is used for problems where the
#' number of variables is substantially larger than the sample size (e.g., p/n
#' is greater than 10).  It is always prefered to use \option{method="md"}, but
#' to find more variables, or when computations are high, variable hunting may
#' be preferred.
#' 
#' When \option{method="vh"}: Using training data from a stratified K-fold
#' subsampling (stratification based on the y-outcomes), a forest is fit using
#' \code{mvars} randomly selected variables (variables are chosen with
#' probability proportional to weights determined using an initial forest fit;
#' see below for more details).  The \code{mvars} variables are ordered by
#' increasing minimal depth and added sequentially (starting from an initial
#' model determined using minimal depth selection) until joint VIMP no longer
#' increases (signifying the final model).  A forest is refit to the final
#' model and applied to test data to estimate prediction error.  The process is
#' repeated \code{nrep} times.  Final selected variables are the top P ranked
#' variables, where P is the average model size (rounded up to the nearest
#' integer) and variables are ranked by frequency of occurrence.
#' 
#' The same algorithm is used when \option{method="vh.vimp"}, but variables are
#' ordered using VIMP.  This is faster, but not as accurate.  }
#' \emph{Miscellanea} \enumerate{ \item When variable hunting is used, a
#' preliminary forest is run and its VIMP is used to define the probability of
#' selecting a variable for splitting a node.  Thus, instead of randomly
#' selecting \code{mvars} at random, variables are selected with probability
#' proportional to their VIMP (the probability is zero if VIMP is negative).  A
#' preliminary forest is run once prior to the analysis if
#' \code{prefit$action=TRUE}, otherwise it is run prior to each iteration (this
#' latter scenario can be slow).  When \option{method="md"}, a preliminary
#' forest is fit only if \code{prefit$action=TRUE}.  Then instead of randomly
#' selecting \code{mtry} variables at random, \code{mtry} variables are
#' selected with probability proportional to their VIMP.  In all cases, the
#' entire option is overridden if \code{xvar.wt} is non-null.
#' 
#' \item If \code{object} is supplied and \option{method="md"}, the grow forest
#' from \code{object} is parsed for minimal depth information.  While this
#' avoids fitting another forest, thus saving computational time, certain
#' options no longer apply.  In particular, the value of \code{cause} plays no
#' role in the final selected variables as minimal depth is extracted from the
#' grow forest, which has already been grown under a preselected \code{cause}
#' specification.  Users wishing to specify \code{cause} should instead use the
#' formula and data interface.  Also, if the user requests a prefitted forest
#' via \code{prefit$action=TRUE}, then \code{object} is not used and a refitted
#' forest is used in its place for variable selection.  Thus, the effort spent
#' to construct the original grow forest is not used in this case.
#' 
#' \item If \option{fast=TRUE}, and variable hunting is used, the training data
#' is chosen to be of size n/K, where n=sample size (i.e., the size of the
#' training data is swapped with the test data).  This speeds up the algorithm.
#' Increasing K also helps.
#' 
#' \item Can be used for competing risk data.  When \option{method="vh.vimp"},
#' variable selection based on VIMP is confined to an event specific cause
#' specified by \code{cause}.  However, this can be unreliable as not all
#' y-outcomes can be guaranteed when subsampling (this is true even when
#' stratifed subsampling is used as done here).  }
#' 
#' @aliases var.select var.select.rfsrc
#' @param formula A symbolic description of the model to be fit.  Must be
#' specified unless \code{object} is given.
#' @param data Data frame containing the y-outcome and x-variables in the
#' model. Must be specified unless \code{object} is given.
#' @param object An object of class \code{(rfsrc, grow)}.  Not required when
#' \code{formula} and \code{data} are supplied.
#' @param cause Integer value between 1 and \code{J} indicating the event of
#' interest for competing risks, where \code{J} is the number of event types
#' (this option applies only to competing risk families).  The default is to
#' use the first event type.
#' @param m.target Character value for multivariate families specifying the
#' target outcome to be used.  If left unspecified, the algorithm will choose a
#' default target.
#' @param method Variable selection method: \describe{
#' \item{list("md")}{minimal depth (default).}\item{:}{minimal depth
#' (default).} \item{list("vh")}{variable hunting.}\item{:}{variable hunting.}
#' \item{list("vh.vimp")}{variable hunting with VIMP (variable
#' importance).}\item{:}{variable hunting with VIMP (variable importance).} }
#' @param conservative Level of conservativeness of the thresholding rule used
#' in minimal depth selection: \describe{ \item{list("high")}{Use the most
#' conservative threshold.}\item{:}{Use the most conservative threshold.}
#' \item{list("medium")}{Use the default less conservative tree-averaged
#' threshold.}\item{:}{Use the default less conservative tree-averaged
#' threshold.} \item{list("low")}{Use the more liberal one standard error
#' rule.}\item{:}{Use the more liberal one standard error rule.} }
#' @param ntree Number of trees to grow.
#' @param mvars Number of randomly selected variables used in the variable
#' hunting algorithm (ignored when \option{method="md"}).
#' @param mtry The mtry value used.
#' @param nodesize Forest average terminal node size.
#' @param splitrule Splitting rule used.
#' @param nsplit If non-zero, the specified tree splitting rule is randomized
#' which significantly increases speed.
#' @param xvar.wt Vector of non-negative weights specifying the probability of
#' selecting a variable for splitting a node.  Must be of dimension equal to
#' the number of variables.  Default (\code{NULL}) invokes uniform weighting or
#' a data-adaptive method depending on \code{prefit$action}.
#' @param refit Should a forest be refit using the selected variables?
#' @param fast Speeds up the cross-validation used for variable hunting for a
#' faster analysis.  See miscellanea below.
#' @param na.action Action to be taken if the data contains \code{NA} values.
#' @param always.use Character vector of variable names to always be included
#' in the model selection procedure and in the final selected model.
#' @param nrep Number of Monte Carlo iterations of the variable hunting
#' algorithm.
#' @param K Integer value specifying the \code{K}-fold size used in the
#' variable hunting algorithm.
#' @param nstep Integer value controlling the step size used in the forward
#' selection process of the variable hunting algorithm.  Increasing this will
#' encourage more variables to be selected.
#' @param prefit List containing parameters used in preliminary forest analysis
#' for determining weight selection of variables.  Users can set all or some of
#' the following parameters: \describe{ \item{list("action")}{Determines how
#' (or if) the preliminary forest is fit.  See details
#' below.}\item{:}{Determines how (or if) the preliminary forest is fit.  See
#' details below.} \item{list("ntree")}{Number of trees used in the preliminary
#' analysis.}\item{:}{Number of trees used in the preliminary analysis.}
#' \item{list("mtry")}{mtry used in the preliminary analysis.}\item{:}{mtry
#' used in the preliminary analysis.} \item{list("nodesize")}{nodesize used in
#' the preliminary analysis.}\item{:}{nodesize used in the preliminary
#' analysis.} \item{list("nsplit")}{nsplit value used in the preliminary
#' analysis.}\item{:}{nsplit value used in the preliminary analysis.} }
#' @param verbose Set to \code{TRUE} for verbose output.
#' @param ... Further arguments passed to forest grow call.
#' @return Invisibly, a list with the following components:
#' \item{err.rate}{Prediction error for the forest (a vector of length
#' \code{nrep} if variable hunting is used).} \item{modelsize}{Number of
#' variables selected.} \item{topvars}{Character vector of names of the final
#' selected variables.} \item{varselect}{Useful output summarizing the final
#' selected variables.} \item{rfsrc.refit.obj}{Refitted forest using the final
#' set of selected variables (requires \option{refit=TRUE}).}
#' \item{md.obj}{Minimal depth object.  \code{NULL} unless
#' \option{method="md"}.}
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{find.interaction}}, \command{\link{max.subtree}},
#' \command{\link{vimp}}
#' @references Ishwaran H., Kogalur U.B., Gorodeski E.Z, Minn A.J. and Lauer
#' M.S. (2010).  High-dimensional variable selection for survival data.
#' \emph{J. Amer. Statist. Assoc.}, 105:205-217.
#' 
#' Ishwaran H., Kogalur U.B., Chen X. and Minn A.J. (2011).  Random survival
#' forests for high-dimensional data. \emph{Statist. Anal. Data Mining},
#' 4:115-132.
#' @keywords variable selection
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## Minimal depth variable selection
#' ## survival analysis
#' ## ------------------------------------------------------------
#' 
#' data(pbc, package = "randomForestSRC")
#' pbc.obj <- rfsrc(Surv(days, status) ~ ., pbc, nsplit = 10, importance = TRUE)
#' 
#' # default call corresponds to minimal depth selection
#' vs.pbc <- var.select(object = pbc.obj)
#' topvars <- vs.pbc$topvars
#' 
#' # the above is equivalent to
#' max.subtree(pbc.obj)$topvars
#' 
#' # different levels of conservativeness
#' var.select(object = pbc.obj, conservative = "low")
#' var.select(object = pbc.obj, conservative = "medium")
#' var.select(object = pbc.obj, conservative = "high")
#' 
#' ## ------------------------------------------------------------
#' ## Minimal depth variable selection
#' ## competing risk analysis
#' ## ------------------------------------------------------------
#' 
#' ## competing risk data set involving AIDS in women
#' data(wihs, package = "randomForestSRC")
#' vs.wihs <- var.select(Surv(time, status) ~ ., wihs, nsplit = 3, 
#'                       ntree = 100, importance = TRUE)
#' 
#' ## competing risk analysis of pbc data from survival package
#' ## implement cause-specific variable selection 
#' if (library("survival", logical.return = TRUE)) {
#'   data(pbc, package = "survival")
#'   pbc$id <- NULL
#'   var.select(Surv(time, status) ~ ., pbc, nsplit = 10, cause = 1)
#'   var.select(Surv(time, status) ~ ., pbc, nsplit = 10, cause = 2)
#' }
#' 
#' ## ------------------------------------------------------------
#' ## Minimal depth variable selection
#' ## classification analysis
#' ## ------------------------------------------------------------
#' 
#' vs.iris <- var.select(Species ~ ., iris)
#' 
#' ## ------------------------------------------------------------
#' ## Minimal depth variable selection
#' ## Regression analysis
#' ## ------------------------------------------------------------
#' 
#' #Variable hunting (overkill for low dimensions)
#' vh.air <- var.select(Ozone ~., airquality, method = "vh", nrep = 10, mvars = 5)
#' 
#' #better analysis
#' vs.air <- var.select(Ozone ~., airquality)
#' 
#' ## ------------------------------------------------------------
#' ## Minimal depth high-dimensional example
#' ## van de Vijver microarray breast cancer survival data
#' ## predefined weights for *selecting* a gene for node splitting
#' ## determined from a preliminary forest analysis
#' ## ------------------------------------------------------------
#' 
#' data(vdv, package = "randomForestSRC")
#' md.breast <- var.select(Surv(Time, Censoring) ~ ., vdv,
#'   prefit = list(action = TRUE))
#' 
#' ## same analysis, but with customization for the preliminary forest fit
#' ## note the large mtry and small nodesize values used
#' md.breast.custom <- var.select(Surv(Time, Censoring) ~ ., vdv, 
#'   prefit = list(action = TRUE, mtry = 500, nodesize = 1))
#' 
#' ## ------------------------------------------------------------
#' ## Minimal depth high-dimensional example
#' ## van de Vijver microarray breast cancer survival data
#' ## predefined weights for genes for *splitting* tree nodes
#' ## weights defined in terms of cox p-values
#' ## ------------------------------------------------------------
#' 
#' if (library("survival", logical.return = TRUE) 
#'     & library("parallel", logical.return = TRUE))
#' {
#'   cox.weights <- function(rfsrc.f, rfsrc.data) {
#'     event.names <- all.vars(rfsrc.f)[1:2]
#'     p <- ncol(rfsrc.data) - 2
#'     event.pt <- match(event.names, names(rfsrc.data))
#'     xvar.pt <- setdiff(1:ncol(rfsrc.data), event.pt)
#'     unlist(mclapply(1:p, function(j) {
#'       cox.out <- coxph(rfsrc.f, rfsrc.data[, c(event.pt, xvar.pt[j])])
#'       pvalue <- summary(cox.out)$coef[5]
#'       if (is.na(pvalue)) 1.0 else 1/(pvalue + 1e-100)
#'     }))
#'   }       
#'   data(vdv, package = "randomForestSRC")
#'   rfsrc.f <- as.formula(Surv(Time, Censoring) ~ .)
#'   cox.wts <- cox.weights(rfsrc.f, vdv)
#'   breast.obj <- rfsrc(rfsrc.f, vdv, nsplit = 10, xvar.wt = cox.wts,
#'                       importance = TRUE)
#'   md.breast.splitwt <- var.select(object = breast.obj) 
#' }
#' 
#' 
#' ## ------------------------------------------------------------
#' ## Variable hunting high-dimensional example
#' ## van de Vijver microarray breast cancer survival data
#' ## nrep is small for illustration; typical values are nrep = 100
#' ## ------------------------------------------------------------
#' 
#' data(vdv, package = "randomForestSRC")
#' vh.breast <- var.select(Surv(Time, Censoring) ~ ., vdv,
#'       method = "vh", nrep = 10, nstep = 5)
#' 
#' # plot top 10 variables
#' plot.variable(vh.breast$rfsrc.refit.obj,
#'   xvar.names = vh.breast$topvars[1:10])
#' plot.variable(vh.breast$rfsrc.refit.obj,
#'   xvar.names = vh.breast$topvars[1:10], partial = TRUE)
#' 
#' ## similar analysis, but using weights from univarate cox p-values
#' if (library("survival", logical.return = TRUE))
#' {
#'   cox.weights <- function(rfsrc.f, rfsrc.data) {
#'     event.names <- all.vars(rfsrc.f)[1:2]
#'     p <- ncol(rfsrc.data) - 2
#'     event.pt <- match(event.names, names(rfsrc.data))
#'     xvar.pt <- setdiff(1:ncol(rfsrc.data), event.pt)
#'     sapply(1:p, function(j) {
#'       cox.out <- coxph(rfsrc.f, rfsrc.data[, c(event.pt, xvar.pt[j])])
#'       pvalue <- summary(cox.out)$coef[5]
#'       if (is.na(pvalue)) 1.0 else 1/(pvalue + 1e-100)
#'     })
#'   }       
#'   data(vdv, package = "randomForestSRC")
#'   rfsrc.f <- as.formula(Surv(Time, Censoring) ~ .)
#'   cox.wts <- cox.weights(rfsrc.f, vdv)
#'   vh.breast.cox <- var.select(rfsrc.f, vdv, method = "vh", nstep = 5,
#'     nrep = 10, xvar.wt = cox.wts)
#' }
#' 
#' ## ------------------------------------------------------------
#' ## variable selection for multivariate mixed forests
#' ## ------------------------------------------------------------
#' 
#' mtcars.new <- mtcars
#' mtcars.new$cyl <- factor(mtcars.new$cyl)
#' mtcars.new$carb <- factor(mtcars.new$carb, ordered = TRUE)
#' mv.obj <- rfsrc(cbind(carb, mpg, cyl) ~., data = mtcars.new,
#'               importance = TRUE)
#' var.select(mv.obj, method = "vh.vimp", nrep = 10)
#' 
#' }
#' 
var.select <- var.select.rfsrc
