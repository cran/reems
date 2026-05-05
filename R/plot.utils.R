save.graphics <- function(plotpath, plot.params, def.out = NULL, ...) {
    if(is.null(plot.params$out.png)) {
        out <- def.out
    } else {
        out <- plot.params$out.png
    }
    plotpath <- path.expand(plotpath)
  if (out) {
    png(paste0(plotpath, "%02d.png"),
      res = plot.params$res, units = "in",
      height = plot.params$height,
      width = plot.params$width, ...
    )
  } else {
    pdf(paste0(plotpath, "%02d.pdf"),
      height = plot.params$height,
      width = plot.params$width,
      onefile = FALSE, ...
    )
  }
}

plot_with_graphics_device <- function(filename, save.params, expr, def.out = NULL, par.args = list(), ...) {
  save.graphics(plotpath = filename, plot.params = save.params, def.out = def.out, ...)
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(oldpar)
    grDevices::dev.off()
  }, add = TRUE)

  do.call(graphics::par, par.args)
  force(expr)
}

