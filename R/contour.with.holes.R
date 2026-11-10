#' Create a habitat with holes
#' 
#'  `grid.make()` constructs a regular triangular grid with circa nDemes vertices. This function creates three files:
#'  * The pair gridpath.demes and gridpath.edges which specify a custom population grid with holes
#'  * The plot gridpath.png which visualizes the custom grid with demes in red and edges in green
#' @param outer A two-column matrix of coordinates of boundary points (longitude, latitude).
#' @param holes List of holes. Each hole is a two-column matrix of coordinates of boundary points (longitude, latitude).
#' This is optional, and defaults to no holes.
#' @param nDemes Number of demes, roughly, in the grid without accounting for possible holes.
#' @param plotpath Output filename for the visualisation PNG file. Optional.
#' @returns A two-entry list of `demes` and `edges`, both two-column matrices.
#' @examples
#' # We use example input from Petkova et al. (2016), found in the '/extdata' directory
#' data_path <- system.file("extdata", package = "reems")
#' input <- file.path(data_path, "barrier-schemeX-nIndiv300-nSites3000")
#' # Load the population habitat
#' outer <- read.table(paste0(input, ".outer"))
#' # Each hole is a ring (simple closed polygon) and the holes don't overlap
#' hole1 <- data.frame(V1 = c(2., 5., 5., 2., 2.), V2 = c(2., 2., 5., 5., 2.))
#' hole2 <- data.frame(V1 = c(6.5, 10., 8., 6.5), V2 = c(2.5, 5., 5., 2.5))
#'
#' # Create the new grid  with holes
#' grid.make(outer = outer, holes = list(hole1, hole2), nDemes = 300)
#' @export
grid.make <- function(outer, holes = NULL, plotpath = NULL, nDemes) {
    data_path <- file.path(tempdir(), "data_path")
    dir.create(data_path, showWarnings = FALSE)
    on.exit(unlink(data_path, recursive = TRUE, force = TRUE), add = TRUE)
    datapath <- file.path(data_path,"in")
    outerfile <- paste0(datapath,".outer")
    write_for_eems(outer,outerfile)
    mcmcpath <-  file.path(tempdir(), "intermediate_path")
    dir.create(mcmcpath, showWarnings = FALSE)
    on.exit(unlink(mcmcpath, recursive = TRUE, force = TRUE), add = TRUE)
    cpp_write_grid(datapath, mcmcpath, nDemes)
    if (is.null(holes)){
        demes <- read.table(file.path(mcmcpath, "demes.txt"))
        edges <- read.table(file.path(mcmcpath, "edges.txt"))
        if (!is.null(plotpath)) {
            plot_basic_popgrid(demes, edges, plotpath)
        }
        return(list("demes" = demes, "edges" = edges))
    }
    else {
        habitat_with_holes <- create_habitat_with_holes(outer, holes)
        outlist <- remove_demes_outside_habitat(habitat_with_holes, mcmcpath, plotpath)
        return(outlist)
    }
}
        
    
#' Create a habitat with holes
#' @param outer A two-column matrix of coordinates of boundary points (longitude, latitude).
#' @param holes List of holes. Each hole is a two-column matrix of coordinates of boundary points.
#' (longitude, latitude).
#' @returns A spatial polygon object with the outer boundary `outer` and the holes in `holes`.  
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
#' # Create the new habitat with holes
#' create_habitat_with_holes(outer, list(hole1, hole2))
#' @noRd
create_habitat_with_holes <- function(outer, holes) {
  polys <- list(Polygon(outer, FALSE))

  for (hole in holes) {
    polys <- c(polys, Polygon(hole, TRUE))
  }

  SpatialPolygons(list(Polygons(polys, "1")))
}

#' Check whether a point lies inside a habitat
#'
#' @param point Numeric vector of length 2 (x, y).
#' @param habitat A `SpatialPolygons` habitat, possibly with holes.
#' @returns `TRUE` if the point is inside the habitat and not inside a hole.
#'   Points exactly on the outer boundary count as inside; points exactly on a
#'   hole's boundary count as in the hole (`FALSE`).
#' @noRd
point_is_in_habitat <- function(point, habitat) {
  point <- matrix(point, nrow = 1)
  point <- SpatialPoints(point)
  point <- point[habitat]
  length(point) == 1
}

#' New 1-based indices for the demes that are kept
#'
#' @param in_habitat Logical vector, one element per deme.
#' @returns Integer vector with consecutive indices for kept demes and `NA` for dropped ones.
#' @noRd
reindex_demes_sequentially <- function(in_habitat) {
  indices_in_habitat <- cumsum(in_habitat)
  indices_in_habitat[!in_habitat] <- NA_integer_
  indices_in_habitat
}

#' Drop demes that have no new index
#'
#' @param demes Data frame or matrix of deme coordinates, one row per deme.
#' @param new_indices Output of `reindex_demes_sequentially()`.
#' @returns `demes` without the rows whose new index is `NA`. Subsetting uses
#'   `drop = TRUE`, so a matrix with a single kept row becomes a vector; a data
#'   frame (as passed by `remove_demes_outside_habitat()`) keeps its shape.
#' @noRd
reindex_demes <- function(demes, new_indices) {
  demes[!is.na(new_indices), ]
}

#' Renumber edges after demes were dropped
#'
#' @param edges Two-column data frame or matrix of 1-based deme indices.
#' @param new_indices Output of `reindex_demes_sequentially()`.
#' @returns `edges` with renumbered endpoints. Edges touching a dropped deme are removed.
#' @noRd
reindex_edges <- function(edges, new_indices) {
  edges[, 1] <- new_indices[edges[, 1]]
  edges[, 2] <- new_indices[edges[, 2]]
  na.omit(edges)
}

#' Save a simple picture of a population grid as a PNG
#'
#' Writes `<gridpath>.png` (8 x 6 in, 600 dpi) using [grDevices::bitmap()], which
#' needs Ghostscript. Any devices opened here are closed on exit.
#'
#' @param demes Two-column matrix of deme coordinates.
#' @param edges Two-column matrix of 1-based deme indices.
#' @param gridpath Output path without the `.png` extension.
#' @param col.grid,col.demes Colours for edges and demes.
#' @param ... Further arguments passed to [grDevices::bitmap()].
#' @returns Called for its side effect.
#' @noRd
plot_basic_popgrid <- function(demes, edges, gridpath, col.grid = "green", col.demes = "red", ...) {
  # Ensure graceful exit closing graphics devices.
    initial_ndev <- length(grDevices::dev.list())
    on.exit({
        while (!is.null(grDevices::dev.list()) &&
               length(grDevices::dev.list()) > initial_ndev) {
                   dev.off()
               }
    }, add = TRUE)
    
    bitmap(
        paste0(gridpath, ".png"),
        type = "png16m", res = 600, units = "in", height = 6, width = 8, ...
    )
    
    plot(demes, col = col.demes, pch = 19, xlab = "", ylab = "")
    
    for (i in seq_len(nrow(edges))) {
        a <- edges[i, 1]
        b <- edges[i, 2]
        lines(demes[c(a, b), ], col = col.grid)
    }
    dev.off()
}

#' Remove demes that lie outside of the habitat
#'
#' If `plotpath` is provided, this function creates a file `plotpath.png`,
#' which visualizes the custom grid with demes in red and edges in green.
#' 
#' @param habitat A SpatialPolygons habitat (possibly with holes).
#' @param mcmcpath An EEMS output directory which contains demes.txt and edges.txt.
#' @param plotpath Output filename for the visualisation PNG file. Optional.
#' @returns A two-entry list of `demes` and `edges`, both two-column matrices.
#' @examples
#' # The input is an EEMS run with the default regular triangular grid without holes
#' extdata <- system.file("extdata", package = "reems")
#' eems_run_with_default_habitat <- file.path(extdata, "EEMS-barrier")
#' # The output filepath (in the temporary directory for our example);
#' # three files with the same name but different extension to be created
#' outdir <- file.path(tempdir(), "path_out")
#' dir.create(outdir, showWarnings = FALSE)
#' custom_grid_path_with_holes <- file.path(outdir, "gridpath-with-holes")
#'
#' # Load the population habitat
#' outer <- read.table(file.path(eems_run_with_default_habitat, "outer.txt"))
#' # Each hole is a ring (simple closed polygon) and the holes don't overlap
#' hole1 <- data.frame(V1 = c(2., 5., 5., 2., 2.), V2 = c(2., 2., 5., 5., 2.))
#' hole2 <- data.frame(V1 = c(6.5, 10., 8., 6.5), V2 = c(2.5, 5., 5., 2.5))
#'
#' # Create the new habitat with holes
#' new_habitat_with_holes <- create_habitat_with_holes(outer, list(hole1, hole2))
#'
#' remove_demes_outside_habitat(
#'   habitat = new_habitat_with_holes,
#'   mcmcpath = eems_run_with_default_habitat,
#' )
#' # Delete the output directory to tidy up. 
#' unlink(outdir, recursive = TRUE, force = TRUE)
#' @noRd
remove_demes_outside_habitat <- function(habitat, mcmcpath, plotpath = NULL) {
  demes <- read.table(file.path(mcmcpath, "demes.txt"))
  edges <- read.table(file.path(mcmcpath, "edges.txt"))

  in_habitat <- apply(demes, 1, point_is_in_habitat, habitat)
  new_indices <- reindex_demes_sequentially(in_habitat)

  demes <- reindex_demes(demes, new_indices)
  edges <- reindex_edges(edges, new_indices)

  demes <- as.matrix(demes)
  edges <- as.matrix(edges)

  if (!is.null(plotpath)) {
      plot_basic_popgrid(demes, edges, plotpath)
  }
  return(list("demes" = demes, "edges" = edges)) 
}
