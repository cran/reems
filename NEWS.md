# reems 0.1.1

* `eems.plots()` now uses the distance metric recorded in `eemsrun.txt` to assign interpolation points to Voronoi tiles. Previously it always used Euclidean distance, even for `greatcirc` runs.
* `eems()` now defaults to `distance = "greatcirc"`, as documented. The previous default `"greatcircle"` was not recognised, so default runs silently used Euclidean distance. **Default results will change.** Coordinates must be (longitude, latitude) in degrees; otherwise pass `distance = "euclidean"`. Output directories labelled `greatcircle` were run with Euclidean distance and now give an error in `eems.plots()`; change their label to `euclidean` to plot them.
* `eems()` and `eems.from.files()` now give an error at the start of a run if `distance` is not exactly `"euclidean"` or `"greatcirc"`. Previously any other value was silently run as Euclidean.
* With `distance = "greatcirc"`, `eems()` and `eems.from.files()` now give an error if any coordinate cannot be (longitude, latitude) in degrees, such as projected coordinates. Swapped (latitude, longitude) columns are only caught when a value is out of range.

# reems 0.1.0

* Initial CRAN submission.
