\name{reems}
\alias{rEEMS}
\docType{package}
\title{
Estimating Effective Migration Surfaces (EEMS)  
}
\description{
    Provides several functions to run EEMS analyses and to visualise the
    results of EEMS. For an exmplanation of and introduction to the EEMS
    method, see Petkova, D., Novembre, J. & Stephens, M. (2016) <doi:10.1038/ng.3464>.

    \code{\link{eems}} is the main entrypoint, running EEMS analyses on
    single nucleotide polymorphism data allele frequeny available as R matrices.

    \code{\link{eems.from.files}} runs an EEMS analysis from input data
    available as tabular files, and correesponds precisely to the
    original EEMS utility available at <https://github.com/dipetkov/eems>.   
    
    \code{\link{eems.plots}} takes the output of one or more EEMS runs
    and produces contour plots of the effective migration and diversity rates,
    as well as scatterplots of observed vs fitted genetic
    dissimilarities and trace plots to assess convergence.
    
    \code{\link{eems.population.grid}} plots the population grid, with all edges in the same color,
    to visualize the grid before estimating migration and diversity rates.
    
    \code{\link{eems.voronoi.samples}} and \code{\link{eems.posterior.draws}} both take random samples
    from the posterior distribution of the migration and diversity rates and plot
    them to visualize the posterior variance (in a slightly different way).
    }
\keyword{ package }
