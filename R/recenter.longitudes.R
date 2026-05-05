#' Shift spatial object longitudinally
#'
#' The function `sp::recenter` shifts coordinates for a Pacific view by shifting longitudes
#' from `[-180, 180]` to `[0, 360]`. I modified `sp::recenter` to shift longitudes by any degree.
#'
#' @param obj A SpatialPolygons or a SpatialPolygonsDataFrame object.
#' @param degree Number of degrees (positive or negative) to shift.
#' @returns A shifted SpatialPolygons or a SpatialPolygonsDataFrame object.
#' @examples
#' # The input is an EEMS run with the default regular triangular grid without holes
#' extdata <- system.file("extdata", package = "reems")
#' eems_run_with_default_habitat <- file.path(extdata, "EEMS-barrier")
#' # Load the population habitat
#' outer <- read.table(file.path(eems_run_with_default_habitat, "outer.txt"))
#' # Each hole is a ring (simple closed polygon) and the holes don't overlap
#' hole1 <- data.frame(V1 = c(2., 5., 5., 2., 2.), V2 = c(2., 2., 5., 5., 2.))
#' hole2 <- data.frame(V1 = c(6.5, 10., 8., 6.5), V2 = c(2.5, 5., 5., 2.5))
#'
#' # Create the new habitat with holes and explicitly setting its projection as long/lat. 
#' new_habitat_with_holes <- create_habitat_with_holes(outer, list(hole1, hole2))
#' sp::proj4string(new_habitat_with_holes) <- sp::CRS("+proj=longlat +datum=WGS84")
#' recenter_by(new_habitat_with_holes, 60)
#' @noRd
setGeneric("recenter_by", function(obj, degree) {
  standardGeneric("recenter_by")
})

recenter_by.SpatialPolygons <- function(obj, degree) {
  proj <- is.projected(obj)
  if (is.na(proj)) stop("unknown coordinate reference system")
  if (proj) stop("cannot shift projected coordinate reference system")
  projargs <- methods::slot(obj, "proj4string")
  pls <- methods::slot(obj, "polygons")
  Srl <- lapply(pls, recenter_by.Polygons, degree)
  res <- SpatialPolygons(Srl, proj4string = projargs)
  res
}

recenter_by.Polygons <- function(obj, degree) {
  ID <- methods::slot(obj, "ID")
  rings <- methods::slot(obj, "Polygons")
  srl <- lapply(rings, recenter_by.Polygon, degree)
  res <- Polygons(srl, ID = ID)
  res
}


recenter_by.Polygon <- function(obj, degree) {
  crds <- methods::slot(obj, "coords")
  hole <- methods::slot(obj, "hole")
  crds[, 1] <- crds[, 1] + degree
  res <- Polygon(crds, hole)
  res
}
