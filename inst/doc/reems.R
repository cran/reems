## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(reems)

## -----------------------------------------------------------------------------
# eems(
#   freqs = freqs,
#   coords = coords,
#   mcmcpath = "/full/path/to/directory/prefix"
# )	

## -----------------------------------------------------------------------------
# eems(
#   seed = 1024,	
#   freqs = freqs,
#   coords = coords,
#   mcmcpath = "/full/path/to/directory/prefix",
#   diploid = FALSE,
#   numMCMCIter = 100000,
#   numBurnIter = 50000,
#   numThinIter = 999
# )	

## -----------------------------------------------------------------------------
# eems(
#   freqs = freqs,
#   coords = coords,
#   mcmcpath = "full/path/to/directory/prefix",
#   nChains = 5,
#   parallel = TRUE
# )	

## -----------------------------------------------------------------------------
# data_path <- system.file("extdata", package = "reems")
# input <- file.path(data_path, "barrier-schemeX-nIndiv300-nSites3000")
# 
# outer <- read.table(paste0(input, ".outer"))
# 
# hole1 <- data.frame(V1 = c(2., 5., 5., 2., 2.), V2 = c(2., 2., 5., 5., 2.))
# hole2 <- data.frame(V1 = c(6.5, 10., 8., 6.5), V2 = c(2.5, 5., 5., 2.5))
# 
# # Create the new grid  with holes
# grid <- grid.make(outer = outer, holes = list(hole1, hole2), nDemes = 300)
# 
# eems(
#   freqs = freqs,
#   coords = coords,
#   mcmcpath = "full/path/to/directory/prefix",
#   demes = grid$demes,
#   edges = grid$edges,
#   nChains = 5,
#   parallel = TRUE
# )	
# 

## -----------------------------------------------------------------------------
# ?eems

## -----------------------------------------------------------------------------
# eems_results <- eems(
#   freqs = freqs,
#   coords = coords,
#   mcmcpath = "/full/path/to/directory/prefix",
#   nChains = 5,
#   parallel = TRUE
# )	
# 
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/prefix",
#   longlat = TRUE
# )

## -----------------------------------------------------------------------------
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/with-sampling",
#   longlat = TRUE,
#   m.plot.xy = {
#     points(coord, col = "purple")
#   },
#   q.plot.xy = {
#     points(coord, col = "purple")
#   }
# )

## -----------------------------------------------------------------------------
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath =  "/full/path/to/file/axes-flipped",
#   longlat = FALSE
# )

## -----------------------------------------------------------------------------
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath =  "/full/path/to/file/output-PDFs",
#   longlat = TRUE,
#   plot.height = 12,
#   plot.width = 10
# )

## -----------------------------------------------------------------------------
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath =  "/full/path/to/file/output-PNGs",
#   longlat = TRUE,
#   plot.height = 9,
#   plot.width = 8,
#   res = 600,
#   out.png = TRUE
# )

## -----------------------------------------------------------------------------
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath =  "/full/path/to/file/demes-and-edges",
#   longlat = TRUE,
#   add.grid = TRUE,
#   col.grid = "gray90",
#   lwd.grid = 2,
#   add.outline = TRUE,
#   col.outline = "blue",
#   lwd.outline = 5,
#   add.demes = TRUE,
#   col.demes = "red",
#   pch.demes = 5,
#   min.cex.demes = 0.5,
#   max.cex.demes = 1.5
# )

## -----------------------------------------------------------------------------
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/new-eems-colors",
#   longlat = TRUE,
#   eems.colors = RColorBrewer::Rbrewer.pal(11, "RdBu")
# )

## -----------------------------------------------------------------------------
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/fix-colscales",
#   longlat = TRUE,
#   add.outline = TRUE,
#   col.outline = "gray",
#   m.colscale = c(-3, 3),
#   q.colscale = c(-0.3, +0.3)
# )

## -----------------------------------------------------------------------------
# projection_none <- "+proj=longlat +datum=WGS84"
# projection_mercator <- "+proj=merc +datum=WGS84"
# 
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/cartographic-projections",
#   longlat = TRUE,
#   projection.in = projection_none,
#   projection.out = projection_mercator
# )

## -----------------------------------------------------------------------------
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/geographic-map",
#   longlat = TRUE,
#   projection.in = projection_none,
#   projection.out = projection_mercator,
#   add.map = TRUE,
#   col.map = "black",
#   lwd.map = 5
# )

## -----------------------------------------------------------------------------
# map_world <- rworldmap::getMap()
# map_africa <- map_world[which(map_world@data$continent == "Africa"), ]
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/shapefile",
#   longlat = TRUE,
#   m.plot.xy = {
#     plot(map_africa, col = NA, add = TRUE)
#   },
#   q.plot.xy = {
#     plot(map_africa, col = NA, add = TRUE)
#   }
# )

## -----------------------------------------------------------------------------
# map_africa <- sp::spTransform(map_africa,  sp::CRS(projection_mercator))
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath =  "/full/path/to/file/shapefile-projected",
#   longlat = TRUE,
#   projection.in = projection_none,
#   projection.out = projection_mercator,
#   m.plot.xy = {
#     plot(map_africa, col = NA, add = TRUE)
#   },
#   q.plot.xy = {
#     plot(map_africa, col = NA, add = TRUE)
#   }
# )

## -----------------------------------------------------------------------------
# coords <- matrix(c(
#   -10, 10,
#   10, 10,
#   30, 0,
#   40, -10,
#   30, -20
# ), ncol = 2, byrow = TRUE)
# colors <- c("red", "green", "blue", "purple", "orange")
# labels <- LETTERS[1:5]
# coords_merc <- sp::spTransform(
#   sp::SpatialPoints(coords, sp::CRS(projection_none)),
#   sp::CRS(projection_mercator)
# )
# 
# coords_merc <- coords_merc@coords
# 
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/labels-projected",
#   longlat = TRUE,
#   projection.in = projection_none,
#   projection.out = projection_mercator,
#   m.plot.xy = {
#     text(coords_merc, col = colors, pch = labels, font = 2)
#   },
#   q.plot.xy = {
#     text(coords_merc, col = colors, pch = labels, font = 2)
#   }
# )

## -----------------------------------------------------------------------------
# xy.coords <- matrix(c(
#   13.70, 3.20,
#   37.10, -7.20,
#   36.10, -4.10,
#   34.58, -5.67
# ), ncol = 2, byrow = TRUE)
# eems.plots(
#   mcmcpath = eems_results,
#   plotpath = "/full/path/to/file/default",
#   longlat = TRUE,
#   xy.coords = xy.coords
# )
# load(paste0("/full/path/to/file/default", "-rdist.RData"))

## -----------------------------------------------------------------------------
# ?eems.plots

