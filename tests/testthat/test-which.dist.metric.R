metric_from_lines <- function(lines) {
    mcmcpath <- tempfile("eemsrun")
    dir.create(mcmcpath)
    on.exit(unlink(mcmcpath, recursive = TRUE, force = TRUE))
    writeLines(lines, file.path(mcmcpath, "eemsrun.txt"))
    suppressMessages(which.dist.metric(mcmcpath))
}

test_that("which.dist.metric() reads the padded format written by EEMS", {
    header <- c("Input parameter values:", "               seed = 1")
    expect_equal(metric_from_lines(c(header, "               distance = euclidean")), "euclidean")
    expect_equal(metric_from_lines(c(header, "               distance = greatcirc")), "greatcirc")
})

test_that("which.dist.metric() ignores whitespace and case", {
    expect_equal(metric_from_lines("distance=greatcirc"), "greatcirc")
    expect_equal(metric_from_lines("\tdistance\t=\tgreatcirc"), "greatcirc")
    expect_equal(metric_from_lines("distance = GreatCirc  "), "greatcirc")
})

test_that("which.dist.metric() falls back to euclidean without a distance line", {
    expect_equal(metric_from_lines("seed = 1"), "euclidean")
})

test_that("which.dist.metric() rejects other metrics", {
    expect_error(metric_from_lines("distance = greatcircle"), "greatcirc")
})

test_that("which.dist.metric() reads the bundled example", {
    eems_results <- file.path(system.file("extdata", package = "reems"), "EEMS-example")
    expect_equal(suppressMessages(which.dist.metric(eems_results)), "euclidean")
})
