test_that("eems.voronoi.samples writes output", {
    extdata_path <- system.file("extdata", package = "reems")
    eems_results <- file.path(extdata_path, "EEMS-example")
    outdir <- file.path(tempdir(), "path_out")
    dir.create(outdir, showWarnings = FALSE)
    name_figures <- file.path(outdir, "eemsplot_out")

    eems.voronoi.samples(
        mcmcpath = eems_results,
        plotpath = paste0(name_figures, "-voronoi-diagrams"),
        longlat = TRUE, post.draws = 2
)
    
    files <- list.files(outdir, full.names = TRUE)
    expect_length(files, 4)
    expect_true(all(file.info(files)$size > 0))
    unlink(outdir, recursive = TRUE, force = TRUE)
})

    
