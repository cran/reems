#' @useDynLib reems, .registration = TRUE
#' @import Rcpp 
#' @import raster sf sp
#' @import graphics grDevices
#' @importFrom stats lm na.omit
#' @importFrom utils read.table write.table capture.output
NULL

#' Make sure a suggested package is installed
#'
#' @param package Name of the package to check.
#' @param required.by Name of the option or function that needs it, used in the messages.
#' @returns Called for its side effect: stops if `package` is not installed and
#'   shows a message otherwise.
#' @noRd
load.required.package <- function(package, required.by) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(paste0(
      "'", required.by, "' requires the '", package, "' package. ",
      "Please install '", package, "' first."
    ))
  } else {
    message(paste0("Loading ", package, " (required by ", required.by, ")"))
  }
}

