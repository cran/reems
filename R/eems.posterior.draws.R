#' Plotting smoothed Voronoi diagrams of effective migration and diversity rates
#'
#' Given a set of EEMS output directories, this function takes random draws from the posterior
#' distribution of the migration and diversity rate parameters. Each draw is visualized as two
#' smoothed Voronoi diagrams; the migration diagram is saved to a file ending in \code{mvoronoiXX},
#' the diversity diagram is saved to a file ending in \code{qvoronoiXX} where \code{XX} is a
#' numeric id. Specify the number of times to draw from the posterior with the argument
#' \code{post.draws}. If \code{post.draws = 10}, then \code{eems.posterior.draws} will generate
#' plots with id \code{XX = 1} to \code{XX = 10}.
#' This function differs from [eems.voronoi.samples()] by plotting smoothed contour diagrams
#' rather than raw Voronoi plots. 
#'
#' Note about the implementation: \code{eems.voronoi.samples} samples randomly from the posterior
#' draws saved during the execution of EEMS, after burn-in and thinning.
#' @param post.draws Number of times to sample from the posterior. The default is 1.
#' @param  out.png A logical value which, if set,  forces output graphics to be generated as PNGs
#' (if `TRUE`) or PDFs (if `FALSE`). Defaults to `TRUE`.
#' @inheritParams eems.plots
#' @returns None  
#' @references Light A and Bartlein PJ (2004). The End of the Rainbow? Color Schemes for Improved
#' Data Graphics. EOS Transactions of the American Geophysical Union, 85(40), 385.
#' @examples
#' # Use the provided example or supply the path to your own EEMS run.
#' extdata_path <- system.file("extdata", package = "reems")
#' eems_results <- file.path(extdata_path, "EEMS-example")
#' # Create a temporary output directory for this example 
#' outdir <- file.path(tempdir(), "path_out")
#' dir.create(outdir, showWarnings = FALSE)
#' name_figures <- file.path(outdir, "EEMS-example")
#'
#' # Plot a series of Voronoi diagrams for the EEMS model parameters:
#' # the effective migration rates (m) and the effective diversity rates (q).
#' eems.posterior.draws(
#'   mcmcpath = eems_results,
#'   plotpath = paste0(name_figures, "-posterior-draws"),
#'   longlat = TRUE,
#'   post.draws = 2,
#'   out.png = FALSE
#' )
#' # Delete the output directory to tidy up. 
#' unlink(outdir, recursive = TRUE, force = TRUE)
#' @seealso \code{\link{eems.voronoi.samples}}
#' @export

eems.posterior.draws <- function(mcmcpath,
                                 plotpath,
                                 longlat,
                                 post.draws = 1,
                                 plot.width = 10,
                                 plot.height = 10,
                                 out.png = TRUE,
                                 res = 600,
                                 xpd = TRUE,
                                 add.grid = FALSE,
                                 col.grid = "gray80",
                                 lwd.grid = 1,
                                 add.demes = FALSE,
                                 col.demes = "black",
                                 pch.demes = 19,
                                 min.cex.demes = 1,
                                 max.cex.demes = 3,
                                 add.outline = FALSE,
                                 col.outline = "gray90",
                                 lwd.outline = 2,
                                 projection.in = NULL,
                                 projection.out = NULL,
                                 add.map = FALSE,
                                 col.map = "gray60",
                                 lwd.map = 2,
                                 eems.colors = NULL,
                                 add.colbar = FALSE,
                                 m.colscale = NULL,
                                 q.colscale = NULL,
                                 add.title = FALSE,
                                 m.plot.xy = NULL,
                                 q.plot.xy = NULL) {
  plot.params <- list(
    eems.colors = eems.colors, m.colscale = m.colscale, q.colscale = q.colscale,
    add.map = add.map, add.grid = add.grid, add.outline = add.outline, add.demes = add.demes,
    col.map = col.map, col.grid = col.grid, col.outline = col.outline, col.demes = col.demes,
    lwd.map = lwd.map, lwd.grid = lwd.grid, lwd.outline = lwd.outline, pch.demes = pch.demes,
    min.cex.demes = min.cex.demes, proj.in = projection.in, add.colbar = add.colbar,
    max.cex.demes = max.cex.demes, proj.out = projection.out, add.title = add.title
  )
  plot.params <- check.plot.params(plot.params)

  # A vector of EEMS output directories, for the same dataset.
  # Assume that if eemsrun.txt exists, then all EEMS output files exist.
  mcmcpath <- mcmcpath[file.exists(file.path(mcmcpath, "eemsrun.txt"))]
  if (!length(mcmcpath)) {
    stop("Please provide at least one existing EEMS output directory, mcmcpath")
  }

  dimns <- read.dimns(mcmcpath[1], longlat)

  save.params <- list(height = plot.height, width = plot.width, res = res, out.png = out.png)

  plot_with_graphics_device(
      paste0(plotpath, "-mvoronoi"),
      save.params,
      for (draw in seq(post.draws)) {
                                        # Choose one output directory at random
          mrates.raster <- random.eems.contour(sample(mcmcpath, 1), dimns, longlat,
                                               plot.params,
                                               is.mrates = TRUE, plot.xy = m.plot.xy
                                               )
      },
      par.args = list(las = 1, font.main = 1)
  )
                                        # Graphics updated to here
   plot_with_graphics_device(
       paste0(plotpath, "-qvoronoi"),
       save.params,
       for (draw in seq(post.draws)) {
                                        # Choose one output directory at random
           qrates.raster <- random.eems.contour(sample(mcmcpath, 1), dimns, longlat,
                                                plot.params,
                                                is.mrates = FALSE, plot.xy = m.plot.xy
                                                )
       },
       par.args = list(las = 1, font.main = 1)
   )
invisible(NULL)
}
