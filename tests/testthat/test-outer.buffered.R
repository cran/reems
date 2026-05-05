test_that("outer.buffered returns a matrix of coordinates", {
    outer <- outer.buffered(ex.coords)
    expect_true(is.matrix(outer))
    expect_equal(ncol(outer), 2)
})

 
