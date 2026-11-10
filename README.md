
# reems

This R package implements the method of Estimating Effective Migration Surfaces (EEMS) described by Petkova, D., Novembre, J. & Stephens, M. Visualizing spatial population structure with estimated effective migration surfaces. Nat Genet 48, 94–100 (2016). https://doi.org/10.1038/ng.3464.

It provides the function `eems` to run an EEMS analysis, and the function `eems.plots` to visualize the estimated effective migration and diversity surfaces, as well as some other, more rarely used functions.

Its current stable version can be installed directly from CRAN, in the usual way. 
```R
install.packages("reems")
```
The CRAN package page of this package is to be found at <https://CRAN.R-project.org/package=reems>.

As this package provides a  wrapper around the original  C++ implementation of EEMS, installation of the package requires compilation of C++ source code.
This can lead to a lot of noisy console output, including several notes and warnings. This is expected and harmless, and as long as the installation process finishes successfully this is nothing to worry about. 

