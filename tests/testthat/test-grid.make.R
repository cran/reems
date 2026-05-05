test_that("grid.make returns a list of demes and edges", {
    data_path <- system.file("extdata", package = "reems")
    input <- file.path(data_path, "barrier-schemeX-nIndiv300-nSites3000")
    outer <- read.table(paste0(input, ".outer"))
    hole1 <- data.frame(V1 = c(2., 5., 5., 2., 2.), V2 = c(2., 2., 5., 5., 2.))
    hole2 <- data.frame(V1 = c(6.5, 10., 8., 6.5), V2 = c(2.5, 5., 5., 2.5))
    
    output <- grid.make(outer = outer, holes = list(hole1, hole2), nDemes = 300)
    expect_true(is.matrix(output$demes))
    expect_true(is.matrix(output$edges))
    expect_equal(ncol(output$demes), 2)
    expect_equal(ncol(output$edges), 2)
})

 
