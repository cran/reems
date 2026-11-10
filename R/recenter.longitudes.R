#' Shift spatial object longitudinally
#'
#' The function `sp::recenter` gives a Pacific view by adding 360 to negative
#' longitudes. This version instead shifts every longitude by `degree`.
#'
#' Note that no S4 methods are registered for this generic (sp's
#' `setMethod()` calls were not copied), so calling `recenter_by()` errors;
#' call `recenter_by.SpatialPolygons()` directly. Unused in the package.
#'
#' @param obj The object to shift; see `recenter_by.SpatialPolygons()` for what
#'   is supported.
#' @param degree Number of degrees (positive or negative) to shift.
#' @returns A shifted `SpatialPolygons` object; the data of a
#'   `SpatialPolygonsDataFrame` is dropped.
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
#' recenter_by.SpatialPolygons(new_habitat_with_holes, 60)
#' @noRd
setGeneric("recenter_by", function(obj, degree) {
  standardGeneric("recenter_by")
})

#' Shift the longitudes of a `SpatialPolygons` object
#'
#' Helper for `recenter_by()`.
#'
#' @param obj A `SpatialPolygons` (or `SpatialPolygonsDataFrame`) object with
#'   an unprojected (long/lat) CRS; stops if the CRS is projected or unknown.
#' @param degree Number of degrees to add to every longitude.
#' @returns A `SpatialPolygons` object with shifted coordinates and the same CRS.
#'   The data of a `SpatialPolygonsDataFrame` is dropped.
#' @noRd
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

#' Shift the longitudes of a `Polygons` object (one feature with all its rings)
#'
#' Helper for `recenter_by()`.
#'
#' @param obj A `Polygons` object (one feature with all its rings).
#' @param degree Number of degrees to add to every longitude.
#' @returns An object of the same class with shifted coordinates.
#' @noRd
recenter_by.Polygons <- function(obj, degree) {
  ID <- methods::slot(obj, "ID")
  rings <- methods::slot(obj, "Polygons")
  srl <- lapply(rings, recenter_by.Polygon, degree)
  res <- Polygons(srl, ID = ID)
  res
}


#' Shift the longitudes of a single `Polygon` ring
#'
#' Helper for `recenter_by()`.
#'
#' @param obj A single `Polygon` ring.
#' @param degree Number of degrees to add to every longitude.
#' @returns An object of the same class with shifted coordinates.
#' @noRd
recenter_by.Polygon <- function(obj, degree) {
  crds <- methods::slot(obj, "coords")
  hole <- methods::slot(obj, "hole")
  crds[, 1] <- crds[, 1] + degree
  res <- Polygon(crds, hole)
  res
}
