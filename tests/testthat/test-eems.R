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

    
