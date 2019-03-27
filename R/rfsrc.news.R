#' Show the NEWS file
#' 
#' Show the NEWS file of the \pkg{randomForestSRC} package.
#' 
#' 
#' @param ... Further arguments passed to or from other methods.
#' @return None.
#' @author Hemant Ishwaran and Udaya B. Kogalur
#' @keywords documentation
rfsrc.news <- function(...) {
  newsfile <- file.path(system.file(package="randomForestSRC"), "NEWS")
  file.show(newsfile)
}
