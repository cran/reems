test_that("check.greatcirc.coords() accepts longitude/latitude in degrees", {
    expect_silent(check.greatcirc.coords(coordinates = ex.coords))
    expect_silent(check.greatcirc.coords(coordinates = cbind(c(0, 359.5), c(-89, 89))))
    expect_silent(check.greatcirc.coords(coordinates = ex.coords, demes = NULL))
})

test_that("check.greatcirc.coords() rejects projected coordinates", {
    expect_error(
        check.greatcirc.coords(coordinates = ex.coords * 111000),
        "some coordinates are outside"
    )
})

test_that("check.greatcirc.coords() rejects latitudes beyond the poles", {
    expect_error(
        check.greatcirc.coords(coordinates = ex.coords, outer = cbind(c(0, 10, 10), c(0, 0, 95))),
        "some outer are outside"
    )
})

test_that("check.greatcirc.coords() cannot detect swaps within 90 degrees", {
    # Swapped Michigan coordinates are still a valid point
    expect_silent(check.greatcirc.coords(coordinates = ex.coords[, 2:1]))
})
