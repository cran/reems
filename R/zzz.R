.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "reems is an R wrapper for the EEMS spatial population genetics method.\n",
    "It also integrates the R plotting functions bundled with the original EEMS command-line utility.\n",
    "reems uses DarkOrange to Blue color scheme, with 'white' as the midpoint color.\n",
    "It combines two color schemes from the 'dichromat' package, which itself is based on\n",
    "a collection of color schemes for scientific data graphics:\n",
    "\tLight A and Bartlein PJ (2004).\n",
    "\tThe End of the Rainbow? Color Schemes for Improved Data Graphics.\n",
    "\tEOS Transactions of the American Geophysical Union, 85(40), 385.\n",
    "See also http://geog.uoregon.edu/datagraphics/color_scales.htm\n\n\n"
  )
}
