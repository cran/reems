test_that("eems.from.files() writes output", {
    data_path <- system.file("extdata", package = "reems")
    input <- file.path(data_path, "barrier-schemeX-nIndiv300-nSites3000")
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)
    
eems.from.files(
    datapath = input,
    mcmcpath = mcmcdir,
    nDemes = 200,
    nIndiv = 300,
    nSites = 3000,
    diploid = FALSE,
    numMCMCIter = 2000,
    numBurnIter = 1000,
    numThinIter = 99,
    )
    
    files <- list.files(mcmcdir, full.names = TRUE)
    expect_length(files, 31)
    expect_true(all(file.info(files)$size > 0))
    unlink(mcmcdir, recursive = TRUE, force = TRUE)
})


    
