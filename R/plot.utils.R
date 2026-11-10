#' Open a PNG or PDF graphics device for an EEMS plot
#'
#' Uses PNG if `plot.params$out.png` is `TRUE` (or, when that is `NULL`, if
#' `def.out` is `TRUE`), and PDF otherwise. If both are `NULL` it errors. A `%02d`
#' page counter is appended to the file name, so each new page goes to its own
#' file.
#'
#' @param plotpath File path prefix for the output, without extension.
#' @param plot.params List of plot settings; uses `out.png`, `res`, `height` and `width`.
#' @param def.out Default for `out.png` when `plot.params$out.png` is `NULL`.
#' @param ... Further arguments passed to [grDevices::png()] or [grDevices::pdf()].
#' @returns Called for its side effect of opening a device.
#' @noRd
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

#' Evaluate plotting code inside a temporary graphics device
#'
#' Opens a device with `save.graphics()`, applies `par.args` to it, evaluates
#' `expr`, and closes the device, even if `expr` fails. The `par()` settings are
#' only changed on the temporary device, so the caller's device is untouched.
#'
#' @param filename File path prefix passed to `save.graphics()`.
#' @param save.params Plot settings passed to `save.graphics()` as `plot.params`.
#' @param expr Plotting code. It is evaluated lazily, after the device is open.
#' @param def.out Default PNG/PDF choice passed to `save.graphics()`.
#' @param par.args List of graphical parameters passed to [graphics::par()].
#' @param ... Further arguments passed to the graphics device.
#' @returns The value of `expr`.
#' @noRd
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

