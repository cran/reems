#' Default EEMS colour scheme
#'
#' A diverging DarkOrange-to-Blue palette with a near-white midpoint, built
#' from two 'dichromat' schemes. Orange marks rates below the mean and blue
#' marks rates above it.
#'
#' @returns A character vector of 13 hex colours, ordered from dark orange to dark blue.
#' @noRd
default.eems.colors <- function() {
  c(
    # orange sequence, from dark to light
    "#994000", "#CC5800", "#FF8F33", "#FFAD66", "#FFCA99", "#FFE6CC",
    # very slightly off-white
    "#FBFBFB",
    # blue sequence, from light to dark
    "#CCFDFF", "#99F8FF", "#66F0FF", "#33E4FF", "#00AACC", "#007A99"
  )
}

#' Check whether values are valid R colors
#'
#' @param x Vector of any of the three kinds of R color specifications for
#'   [grDevices::col2rgb()]: color names, hex strings (`"#rrggbb"`,
#'   `"#rrggbbaa"`, `"#rgb"`, `"#rgba"`), or positive palette indices.
#'   May be `NULL`.
#' @returns `FALSE` if `x` is `NULL`; otherwise a logical vector, `TRUE` where
#'   `col2rgb()` accepts the element. Following `col2rgb()`, `NA`, `"NA"` and
#'   `"transparent"` are accepted (as transparent); real numbers are truncated
#'   to palette indices (`1.5` is accepted) and zero or negative ones are not;
#'   factors are treated as character. The result is named by the values of an
#'   unnamed character `x`, or by `names(x)` if it has them; numeric and factor
#'   input is unnamed. A zero-length `x` gives `list()`.
#' @noRd
is.color <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  # Check that a string, or a vector of strings, represents a valid color
  sapply(x, function(x) {
    tryCatch(is.matrix(col2rgb(x)), error = function(e) FALSE)
  })
}

#' Turn a user-supplied colour scale into a range
#'
#' @param x Numeric vector of values that the colour scale should cover.
#' @returns `range(x)`, or `c(-1, 1)` if all values in `x` are equal.
#' @noRd
set.colscale <- function(x) {
  if (min(x) < max(x)) range(x) else c(-1, 1)
}

#' Build a colour ramp and matching break points for a heatmap
#'
#' @param colors Character vector of anchor colours to interpolate. Defaults to
#'   RColorBrewer's 9-colour "Reds" palette.
#' @param colscale Numeric vector of values to cover. If it has `n + 1` values they
#'   are used (sorted) as the breaks; otherwise `n + 1` equally spaced breaks
#'   between its minimum and maximum are used.
#' @param n Number of colours to interpolate.
#' @returns A list with `colors` (`n` colours) and `levels` (`n + 1` break points).
#'   Note that `colscale` must not be `NULL`: in that case `levels` is never assigned
#'   locally and the returned element is the function [base::levels()].
#' @noRd
mypalette <- function(colors = NULL, colscale = NULL, n = 299) {
  if (is.null(colors)) {
    # Use RColorBrewer's 9-color "Reds" palette.
    colors <- c(
      "#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A",
      "#EF3B2C", "#CB181D", "#A50F15", "#67000D"
    )
  }
  colors <- colorRampPalette(colors)(n = n)

  if (!is.null(colscale)) {
    if (length(colscale) != (n + 1)) {
      levels <- seq(from = min(colscale), to = max(colscale), length.out = n + 1)
    } else {
      levels <- sort(colscale)
    }
  }
  list(colors = colors, levels = levels)
}

#' Break points for an EEMS contour plot
#'
#' @param values Numeric values to plot (posterior mean rates on the log10 scale).
#' @param nlevels Number of colour levels.
#' @param colscale Extra values that the scale must cover (required, may be `NULL`), usually
#'   `plot.params$m.colscale` or `plot.params$q.colscale`.
#' @returns A numeric vector of `nlevels + 1` equally spaced breaks spanning both
#'   `values` (padded by 0.001 each side) and `colscale`.
#' @noRd
eems.colscale <- function(values, nlevels, colscale) {
  max_val <- max(values + 0.001, colscale)
  min_val <- min(values - 0.001, colscale)
  seq(from = min_val, to = max_val, length = nlevels + 1)
}
