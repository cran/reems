#' Draw a matrix as a heatmap in matrix orientation
#'
#' Assumes a square matrix (non-square input is silently truncated or errors).
#' The matrix is drawn turned 180 degrees from how it prints: row 1 at the
#' bottom and column 1 on the right, so the diagonal runs top-left to
#' bottom-right. Single-row or single-column input is drawn with element 1 at
#' the top.
#'
#' @param x A matrix (or something coercible to one).
#' @param col Anchor colours for `mypalette()`; `NULL` uses the default "Reds".
#' @param colscale Range of values for the colour scale; defaults to the range of `x`.
#' @returns The palette from `mypalette()` (`colors` and `levels`), for drawing a key.
#' @noRd
myheatmap <- function(x, col = NULL, colscale = NULL) {
  if (!is.matrix(x)) x <- as.matrix(x)
  r <- nrow(x)
  c <- ncol(x)
  if (c == 1) {
    x <- matrix(rev(x[, 1]), nrow = 1)
  } else if (r == 1) {
    x <- matrix(rev(x[1, ]), ncol = 1)
  } else {
    x <- t(x)[rev(seq(r)), ]
  }
  if (is.null(colscale)) {
    colscale <- range(x, na.rm = TRUE)
  }
  palette <- mypalette(colors = col, colscale = colscale)
  image(x, col = palette$colors, breaks = palette$levels, axes = FALSE)
  palette
}

#' Residuals between observed and fitted dissimilarities between individuals
#'
#' Reads the observed `<datapath>.diffs`, and for each chain the fitted deme-level
#' dissimilarities `rdistJtDhatJ.txt` together with `ipmap.txt`, and maps them back
#' to individuals. Chains are skipped unless they have `rdistJtDobsJ.txt`,
#' `rdistJtDhatJ.txt` and `rdistoDemes.txt`, although only `rdistJtDhatJ.txt`
#' (and the unchecked `ipmap.txt`) are read. Note that the sum is still divided
#' by `length(mcmcpath)`, not by the number of chains used. If every chain is
#' skipped, the result is the observed dissimilarities with an `NA` diagonal
#' (an empty `mcmcpath` returns `NULL`).
#'
#' @param datapath Path of the EEMS input data, without extension.
#' @param mcmcpath Character vector of EEMS output directories, one per chain.
#' @returns An `nIndiv` x `nIndiv` matrix of observed minus fitted dissimilarities,
#'   with `NA` on the diagonal, or `NULL` if `mcmcpath` is empty.
#' @noRd
heatmap.resid <- function(datapath, mcmcpath) {
  mcmcpath <- mcmcpath
  nchains <- length(mcmcpath)
  message("Heatmap of n-by-n matrix of residuals (observed - fitted):")
  Diffs <- as.matrix(read.table(paste0(datapath, ".diffs"), header = FALSE))
  nIndiv <- nrow(Diffs)
  nChains <- 0
  Delta <- matrix(0, nIndiv, nIndiv)
  for (path in mcmcpath) {
    if (
      file.exists(file.path(path, "rdistJtDobsJ.txt")) &&
        file.exists(file.path(path, "rdistJtDhatJ.txt")) &&
        file.exists(file.path(path, "rdistoDemes.txt"))
    ) {
      message(path)
      nChains <- nChains + 1
      ipmap <- scan(file.path(path, "ipmap.txt"), what = numeric(), quiet = TRUE)
      Sizes <- as.numeric(table(ipmap))
      n <- length(ipmap) # number of samples
      o <- length(Sizes) # number of observed demes
      J <- Matrix::spMatrix(n, o, i = seq(n), j = ipmap, x = rep(1, n)) # indicator matrix
      J <- as.matrix(J)
      JtDhatJ <- as.matrix(read.table(file.path(path, "rdistJtDhatJ.txt"),
        header = FALSE
      ))
      Delta <- Delta + J %*% JtDhatJ %*% t(J)
    }
  }
  if (nchains == 0) {
    return(NULL)
  }
  Delta <- Delta / nchains
  Delta <- Delta - diag(diag(Delta))
  resid <- Diffs - Delta
  diag(resid) <- NA # The diagonal entries would always be zero
  resid
}

#' A function to plot a heatmap of the residual pairwise dissimilarities, abs(observed - fitted)
#'
#' Given a set of EEMS output directories, this function generates a heatmap of the n-by-n matrix
#' of residual dissimilarities between pairs of individuals. The residuals are the differences
#' between the observed and the fitted dissimilarities.
#' The function also saves the residual matrix to a file called \code{plotpath-eems-resid.RData}.
#' In both the residual matrix and the corresponding heat map, individuals are are in the same
#' order as in the input files \code{datapath.coord} and \code{datapath.diffs}. 
#' @param datapath Path of the EEMS input data without extension; \code{<datapath>.diffs}
#' is read. The Diffs matrix is not copied by \code{runeems} to the output directory.
#' @param heatmap.cols The heatmap color palette as a vector of colors, ordered from low to high.
#' Defaults to the sequential "Reds" palette in the RColorBrewer package.
#' @param heatmap.colscale A fixed range for the heatmap colors. The default is NULL, so the color
#' space is the observed range of the residuals.
#' @param out.png A logical value which, if set,  forces output graphics to be generated as PNGs
#' (if `TRUE`) or PDFs (if `FALSE`). Defaults to `TRUE`.
#' @inheritParams eems.plots
#' @returns `NULL`, invisibly; called for its side effects (plots and an `.RData` file).
#' @examples
#' # Use the provided example or supply the path to your own EEMS run
#' extdata_path <- system.file("extdata", package = "reems")
#' eems_dataset <- file.path(extdata_path, "EEMS-barrier")
#' eems_results <- file.path(extdata_path, "EEMS-barrier")
#' # Create a temporary output directory for this example 
#' outdir <- file.path(tempdir(), "path_out")
#' dir.create(outdir, showWarnings = FALSE)
#' name_figures <- file.path(outdir, "EEMS-barrier")
#'
#' eems.resid.heatmap(
#'   datapath = eems_dataset,
#'   mcmcpath = eems_results,
#'   plotpath = name_figures,
#'   heatmap.cols = c("gray99", "red"),
#'   out.png = FALSE
#' )
#' # Delete the output directory to tidy up. 
#' unlink(outdir, recursive = TRUE, force = TRUE)
#' @seealso \code{\link{eems.plots}, \link{eems.voronoi.samples}, \link{eems.posterior.draws},
#' \link{eems.population.grid}}
#' @noRd
eems.resid.heatmap <- function(datapath,
                               mcmcpath,
                               plotpath,
                               plot.width = 7,
                               plot.height = 7,
                               out.png = TRUE,
                               res = 600,
                               heatmap.cols = NULL,
                               heatmap.colscale = NULL) {
  load.required.package(
    package = "Matrix",
    required.by = "eems.resid.heatmap"
  )

  save.params <- list(height = plot.height, width = plot.width, res = res, out.png = out.png)

  # A vector of EEMS output directories, for the same dataset.
  # Assume that if eemsrun.txt exists, then all EEMS output files exist.
  mcmcpath <- mcmcpath[file.exists(file.path(mcmcpath, "eemsrun.txt"))]
  if (!length(mcmcpath)) {
    stop("Please provide one existing EEMS output directory, mcmcpath")
  }

  message("Processing the following EEMS output directories :")
  message(mcmcpath)

  eems.resid <- heatmap.resid(datapath, mcmcpath)
  save(eems.resid, file = paste0(plotpath, "-eems-resid.RData"))

  key <- plot_with_graphics_device(
      paste0(plotpath, "-eems-resid"),
      save.params,
      myheatmap(abs(eems.resid), col = heatmap.cols, colscale = heatmap.colscale),
      par.args = list(las = 1, font.main = 1, mar = c(0, 0, 0, 0) + 0.1)
  )


  if (out.png) {
    save.params$height <- 6
    save.params$width <- 1.5
  } else {
    save.params$height <- 12
    save.params$width <- 3
  }

  plot_with_graphics_device(
      paste0(plotpath, "-eems-resid-key"),
      save.params,
      myfilled.legend(
          levels = key$levels, col = key$colors,
          key.axes = axis(4, tick = FALSE, hadj = 1, line = 4, cex.axis = 2),
          key.title = mtext("abs(r)", side = 3, cex = 2.5, line = 1.5, font = 1)
      ),
      par.args = list(las = 1, font.main = 1, mar = c(1, 1, 5, 8))
  )
  invisible(NULL)
}
