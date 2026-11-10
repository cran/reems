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

test_that("eems.from.files() rejects an unknown distance metric", {
    data_path <- system.file("extdata", package = "reems")
    input <- file.path(data_path, "barrier-schemeX-nIndiv300-nSites3000")
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)

    expect_error(
        eems.from.files(
            datapath = input,
            mcmcpath = mcmcdir,
            nDemes = 200,
            nIndiv = 300,
            nSites = 3000,
            diploid = FALSE,
            distance = "greatcircle",
            numMCMCIter = 2000,
            numBurnIter = 1000,
            numThinIter = 99
        ),
        "either 'euclidean' or 'greatcirc'"
    )
    expect_length(list.files(mcmcdir), 0)
    unlink(mcmcdir, recursive = TRUE, force = TRUE)
})

test_that("eems.from.files() rejects projected coordinates for great circle distance", {
    data_path <- system.file("extdata", package = "reems")
    input <- file.path(data_path, "barrier-schemeX-nIndiv300-nSites3000")
    projected <- file.path(tempdir(), "projected")
    for (ext in c(".coord", ".outer")) {
        write.table(read.table(paste0(input, ext)) * 1e5, paste0(projected, ext),
                    row.names = FALSE, col.names = FALSE)
    }
    file.copy(paste0(input, ".diffs"), paste0(projected, ".diffs"), overwrite = TRUE)
    mcmcdir <- file.path(tempdir(), "eems_out")
    dir.create(mcmcdir, showWarnings = FALSE)

    expect_error(
        eems.from.files(
            datapath = projected,
            mcmcpath = mcmcdir,
            nDemes = 200,
            nIndiv = 300,
            nSites = 3000,
            diploid = FALSE,
            distance = "greatcirc",
            numMCMCIter = 2000,
            numBurnIter = 1000,
            numThinIter = 99
        ),
        "some coordinates are outside"
    )
    expect_length(list.files(mcmcdir), 0)
    unlink(c(mcmcdir, paste0(projected, c(".coord", ".outer", ".diffs"))), recursive = TRUE, force = TRUE)
})


    
