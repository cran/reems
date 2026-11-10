test_that("sequential eems() writes output", {
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)
    mcmcpath <- file.path(mcmcdir,"example")
        
    dir <- eems(
        freqs = ex.freqs,
        coords = ex.coords,
        mcmcpath = mcmcpath,   
        numMCMCIter = 2000,
        numBurnIter = 1000,
        numThinIter = 99
    )
    
    
    files <- list.files(dir, full.names = TRUE)
    expect_length(files, 31)
    expect_true(all(file.info(files)$size > 0))
    # The default metric must be one that eems.plots() can read back
    expect_equal(suppressMessages(which.dist.metric(dir)), "greatcirc")
    unlink(mcmcdir, recursive = TRUE, force = TRUE)
})

test_that("eems() rejects projected coordinates for great circle distance", {
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)
    mcmcpath <- file.path(mcmcdir,"example")

    expect_error(
        eems(
            freqs = ex.freqs,
            coords = ex.coords * 111000,
            mcmcpath = mcmcpath,
            numMCMCIter = 2000,
            numBurnIter = 1000,
            numThinIter = 99
        ),
        "some coordinates are outside"
    )
    expect_length(list.files(mcmcdir), 0)
    unlink(mcmcdir, recursive = TRUE, force = TRUE)
})

test_that("eems() accepts projected coordinates for euclidean distance", {
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)
    mcmcpath <- file.path(mcmcdir,"example")

    dir <- eems(
        freqs = ex.freqs,
        coords = ex.coords * 111000,
        mcmcpath = mcmcpath,
        distance = "euclidean",
        numMCMCIter = 200,
        numBurnIter = 100,
        numThinIter = 99
    )
    expect_equal(suppressMessages(which.dist.metric(dir)), "euclidean")
    unlink(mcmcdir, recursive = TRUE, force = TRUE)
})

test_that("eems() rejects an unknown distance metric", {
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)
    mcmcpath <- file.path(mcmcdir,"example")

    expect_error(
        eems(
            freqs = ex.freqs,
            coords = ex.coords,
            mcmcpath = mcmcpath,
            distance = "greatcircle",
            numMCMCIter = 2000,
            numBurnIter = 1000,
            numThinIter = 99
        ),
        "either 'euclidean' or 'greatcirc'"
    )
    unlink(mcmcdir, recursive = TRUE, force = TRUE)
})

test_that("parallel eems() rejects an unknown distance metric", {
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)
    mcmcpath <- file.path(mcmcdir,"example")

    expect_error(
        eems(
            freqs = ex.freqs,
            coords = ex.coords,
            mcmcpath = mcmcpath,
            distance = "greatcircle",
            numMCMCIter = 2000,
            numBurnIter = 1000,
            numThinIter = 99,
            parallel = TRUE,
            nWorkers = 2,
            nChains = 2
        ),
        "either 'euclidean' or 'greatcirc'"
    )
    unlink(mcmcdir, recursive = TRUE, force = TRUE)
})

test_that("parallel eems() writes output and logs", {
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)
    mcmcpath <- file.path(mcmcdir,"example")
        
    dirs <- eems(
        freqs = ex.freqs,
        coords = ex.coords,
        mcmcpath = mcmcpath,   
        numMCMCIter = 2000,
        numBurnIter = 1000,
        numThinIter = 99,
        parallel = TRUE,
        nWorkers = 2,
        nChains = 2
    )
    

    files <- list.files(dirs[[1]], full.names = TRUE)

    expect_length(files, 32)
    expect_true(all(file.info(files)$size > 0))

    files <- list.files(dirs[[2]], full.names = TRUE)

    expect_length(files, 32)
    expect_true(all(file.info(files)$size > 0))

    unlink(mcmcdir, recursive = TRUE, force = TRUE)
})

    
