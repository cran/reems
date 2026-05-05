#' Create a buffered habitat around a set of points.
#'
#' Creates a polygon a certain distance from each of a set of points.
#' It is used for creating a default habitat outline for [eems()].
#' @param coords A two-column matrix of point coordinates (longitude, latitude).
#' @param d Rough buffer distance. Not necessarily kept completely to keep the polygon simple.
#' Defaults to 0.2 of the length of the hull of the `coords`. 
#' @returns A two-column matrix of coordinates with the outer boundary points of the containing polygon. 
#' @examples
#' #  Create a buffer around the example habitat
#' outer <- outer.buffered(ex.coords)
#' @export
outer.buffered <- function(coords, d = NULL) {
    pts <- st_as_sf(
      data.frame(x = coords[,1], y = coords[,2]),
    coords = c("x", "y")
  )    
    hull <- pts |>
        st_union() |>
        st_convex_hull()
    if (is.null(d)) {
        d = 0.2 * st_length(hull)
    }
    buffered <- st_buffer(hull, dist = d)
    simple_buffered <- st_simplify(buffered, dTolerance = 0.5*d)
    boundary <- st_cast(simple_buffered, "POLYGON")
    xy <- st_coordinates(boundary)[, 1:2]
    xy
    
}

geno2diffs <- function(Geno) {
  nIndiv <- nrow(Geno)
  nSites <- ncol(Geno)
  Miss <- is.na(Geno)
  ## Impute NAs with the column means (= twice the allele frequencies)
  Mean <- matrix(colMeans(Geno, na.rm = TRUE), ## a row of means
		 nrow = nIndiv, ncol = nSites, byrow = TRUE) ## a matrix with nIndiv identical rows of means
  GenoImputed <- Geno
  Mean[Miss == 0] <- 0 ## Set the means that correspond to observed genotypes to 0
  GenoImputed[Miss == 1] <- 0 ## Set the missing genotypes to 0 (used to be NA) 
  GenoImputed <- GenoImputed + Mean
  ## Compute similarities
  Sim <- GenoImputed %*% t(GenoImputed) / nSites
  SelfSim <- diag(Sim) ## self-similarities
  vector1s <- rep(1, nIndiv) ## vector of 1s
  ## This chunk generates a `diffs` matrix
  Diffs <- SelfSim %*% t(vector1s) + vector1s %*% t(SelfSim) - 2 * Sim
  Diffs
}

write_for_eems <- function(matrix, filename) { 
    write.table(matrix, filename, 
                col.names = FALSE, row.names = FALSE, quote = FALSE)
}
