plot.competing.risk.rfsrc <- function (x, plots.one.page = FALSE, ...) {
  ## Incoming parameter checks.  All are fatal.
  if (is.null(x)) {
    stop("object x is empty!")
  }
  if (sum(inherits(x, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
      sum(inherits(x, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
  if (x$family != "surv-CR") {
    stop("this function only supports competing risk settings")
  }
  ## work-horse plotting function
  matPlot <- function(matx, ylab = "", legend = "", pos = 1) {
    m <- dim(cbind(matx))[2]
    if (m > 1) legend <- paste(legend, 1:m, "  ")
    matplot(x$time.interest, matx, xlab = "Time", ylab = ylab, type = "l",
            col = (1:m), lty = 1, lwd = 3)
    legend(c("topright", "bottomright")[pos], legend = legend, col = (1:m), lty = 1, lwd = 3)
  }
  ## save par settings
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  if (plots.one.page) par(mfrow = c(1,1)) else par(mfrow = c(1,2))
  matPlot(apply(x$chf, c(2, 3), mean, na.rm = TRUE), "CHF", "CSCHF", pos = 2)
  matPlot(100 * apply(x$cif, c(2, 3), mean, na.rm = TRUE), "Probability (%)", "CIF", 2)
}


#' Plots for Competing Risks
#' 
#' Plot the ensemble cumulative incidence function (CIF) and cause-specific
#' cumulative hazard function (CSCHF) from a competing risk analysis.
#' 
#' Ensemble ensemble CSCHF and CIF functions for each event type.  Does not
#' apply to right-censored data.
#' 
#' @aliases plot.competing.risk plot.competing.risk.rfsrc
#' @param x An object of class \code{(rfsrc, grow)} or \code{(rfsrc, predict)}.
#' @param plots.one.page Should plots be placed on one page?
#' @param ... Further arguments passed to or from other methods.
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @seealso \command{\link{follic}}, \command{\link{hd}},
#' \command{\link{rfsrc}}, \command{\link{wihs}}
#' @references Ishwaran H., Gerds T.A., Kogalur U.B., Moore R.D., Gange S.J.
#' and Lau B.M. (2014). Random survival forests for competing risks.
#' \emph{Biostatistics}, 15(4):757-773.
#' @keywords plot
#' @examples
#' 
#' \donttest{
#' ## ------------------------------------------------------------
#' ## follicular cell lymphoma
#' ## ------------------------------------------------------------
#' 
#'   data(follic, package = "randomForestSRC")
#'   follic.obj <- rfsrc(Surv(time, status) ~ ., follic, nsplit = 3, ntree = 100)
#'   plot.competing.risk(follic.obj)
#' 
#' ## ------------------------------------------------------------
#' ## competing risk analysis of pbc data from the survival package
#' ## events are transplant (1) and death (2)
#' ## ------------------------------------------------------------
#' 
#' if (library("survival", logical.return = TRUE)) {
#'    data(pbc, package = "survival")
#'    pbc$id <- NULL
#'    plot.competing.risk(rfsrc(Surv(time, status) ~ ., pbc, nsplit = 10))
#' }
#' }
#' 
plot.competing.risk <- plot.competing.risk.rfsrc
