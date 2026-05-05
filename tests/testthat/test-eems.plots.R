test_that("eems.plots() writes output under default conditions", {
    extdata_path <- system.file("extdata", package = "reems")
    eems_results <- file.path(extdata_path, "EEMS-example")
    outdir <- file.path(tempdir(), "plot_out")
    dir.create(outdir, showWarnings = FALSE) 
    name_figures <- file.path(outdir, "EEMS-example")
    
    eems.plots(
        mcmcpath = eems_results,
        plotpath = paste0(name_figures, "-default"),
        longlat = TRUE
    )
    
    files <- list.files(outdir, full.names = TRUE)
    expect_length(files, 9)
    expect_true(all(file.info(files)$size > 0))
    unlink(outdir, recursive = TRUE, force = TRUE)
})

test_that("eems.plots() writes output when called with map overlay", {
    extdata_path <- system.file("extdata", package = "reems")
    eems_results <- file.path(extdata_path, "EEMS-example")
    outdir <- file.path(tempdir(), "plot_out")
    dir.create(outdir, showWarnings = FALSE) 
    name_figures <- file.path(outdir, "EEMS-example")
    
    projection_none <- "+proj=longlat +datum=WGS84"
    projection_mercator <- "+proj=merc +datum=WGS84"
    
    map_world <- rworldmap::getMap()
    map_africa <- map_world[which(map_world@data$continent == "Africa"), ]
    map_africa <- sp::spTransform(map_africa,  sp::CRS(projection_mercator))
    eems.plots(
        mcmcpath = eems_results,
        plotpath =   paste0(name_figures, "-with-map"),
        longlat = TRUE,
        projection.in = projection_none,
        projection.out = projection_mercator,
        m.plot.xy = {
            plot(map_africa, col = NA, add = TRUE)
        },
        q.plot.xy = {
            plot(map_africa, col = NA, add = TRUE)
        }
    )

    files <- list.files(outdir, full.names = TRUE)
    expect_length(files, 9)
    expect_true(all(file.info(files)$size > 0))
    unlink(outdir, recursive = TRUE, force = TRUE)    
})

test_that("eems.plots() writes reasonable objects to .RData file", {
    extdata_path <- system.file("extdata", package = "reems")
    eems_results <- file.path(extdata_path, "EEMS-example")
    outdir <- file.path(tempdir(), "plot_out")
    dir.create(outdir, showWarnings = FALSE) 
    name_figures <- file.path(outdir, "EEMS-example")
    
    xy.coords <- matrix(c(
        13.70, 3.20,
        37.10, -7.20,
        36.10, -4.10,
        34.58, -5.67
    ), ncol = 2, byrow = TRUE)
    
    eems.plots(
        mcmcpath = eems_results,
        plotpath = paste0(name_figures,"-with-coords"),
        longlat = TRUE,
        xy.coords = xy.coords
    )
    
    files <- list.files(outdir, full.names = TRUE)
    expect_length(files, 9)
    expect_true(all(file.info(files)$size > 0))

    file_path <- paste0(name_figures,"-with-coords", "-rdist.RData")
    temp_env <- new.env()
    load(file_path, envir = temp_env)
    expect_equal(nrow(temp_env$B.component),1485)
    expect_equal(ncol(temp_env$B.component),8)
    expect_equal(nrow(temp_env$W.component),55)
    expect_equal(ncol(temp_env$W.component),6)
    expect_equal(nrow(temp_env$G.component),1485)
    expect_equal(ncol(temp_env$G.component),8)
    expect_equal(nrow(temp_env$xym.values),4)
    expect_equal(ncol(temp_env$xym.values),3)
    expect_equal(nrow(temp_env$xyq.values),4)
    expect_equal(ncol(temp_env$xyq.values),3)
})


