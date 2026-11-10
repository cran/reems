#' Run the C++ unit tests
#'
#' Thin wrapper around the Catch test runner compiled from `src/test-*.cpp`.
#' Its main job is to reference `run_testthat_tests` from R code, so that
#' `Rcpp::compileAttributes()` adds the routine to the registration table in
#' `src/RcppExports.cpp`. `testthat::run_cpp_tests("reems")` (called from
#' `tests/testthat/test-cpp.R`) needs that registration to find the runner.
#'
#' @param use_xml Logical; if `TRUE`, report results as JUnit XML.
#' @return Logical; `TRUE` if all C++ tests passed.
#' @noRd
run.cpp.tests <- function(use_xml = FALSE) {
    .Call(run_testthat_tests, use_xml)
}
