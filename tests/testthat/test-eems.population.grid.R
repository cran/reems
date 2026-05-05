test_that("eems.population.grid writes output", {
    extdata_path <- system.file("extdata", package = "reems")
    eems_results <- file.path(extdata_path, "EEMS-example")
    outdir <- file.path(tempdir(), "path_out")
    dir.create(outdir, showWarnings = FALSE)
    name_figures <- file.path(outdir, "EEMS-grid_connected")

    eems.population.grid(eems_results,
                         name_figures,
                         longlat = TRUE,
                         add.outline = TRUE, col.outline = "purple", lwd.outline = 3,
                         add.grid = TRUE, col.grid = "green", lwd.grid = 2
                         )
    
    
    files <- list.files(outdir, full.names = TRUE)
    expect_length(files, 1)
    expect_true(all(file.info(files)$size > 0))
    unlink(outdir, recursive = TRUE, force = TRUE)
})

    
