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

#' Compute the EEMS dissimilarity matrix from genotypes
#'
#' Missing genotypes are imputed with the column (site) mean. The result is the
#' matrix of average squared genotype differences between individuals, which is
#' what EEMS expects in its `.diffs` input file. A site that is missing in every
#' individual has no mean, and makes the whole result `NaN`; drop such sites first.
#'
#' @param Geno Numeric matrix with one row per individual and one column per SNP,
#'   coded as 0/1/2 allele counts (or allele frequencies). `NA` marks missing data.
#' @returns A symmetric `nIndiv` x `nIndiv` matrix of pairwise dissimilarities
#'   with a zero diagonal.
#' @noRd
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

#' Write a matrix in the plain-text format read by the EEMS C++ code
#'
#' Values are space-separated, with no header, row names or quoting.
#'
#' @param matrix A matrix or data frame to write.
#' @param filename Path of the file to create (for example `<datapath>.coord`).
#' @returns Called for its side effect; returns `NULL` invisibly.
#' @noRd
write_for_eems <- function(matrix, filename) { 
    write.table(matrix, filename, 
                col.names = FALSE, row.names = FALSE, quote = FALSE)
}
