#' Run an EEMS analysis
#'
#' This function runs an EEMS analysis with given parameters, which include file paths for
#' retrieving the input files (datapath.coord, datapath.diffs and datapath.outer) and
#' writing the output files.
#' It is an exact replicate of the original EEMS command-line function. 
#'
#' @param seed The random seed. Defaults to the current system time in seconds. 
#' @param datapath Full path to a set of three files: datapath.coord, datapath.diffs and datapath.outer.
#' @param mcmcpath Full path to a filename prefix in an output directory with write permission. 
#' @param prevpath Full path to previous output directory, i.e., the mcmcpath in a previous EEMS run. Optional.
#' @param gridpath Full path to a set of two files: gridpath.demes and gridpath.edges. Optional.
#' @param nDemes Number of demes, roughly. EEMS constructs a regular triangular grid with circa nDemes vertices.
#' @param nIndiv Number of samples. Should match the size of the dissimilarity matrix in datapath.diffs.
#' @param nSites Number of SNPs used to compute the observed dissimilarity matrix in datapath.diffs.
#' @param diploid Logical value that indicates whether the species is diploid (TRUE) or haploid (FALSE). Defaults to TRUE.
#' @param distance Distance metric. Either `euclidean` or `greatcirc`, that is, great circle distance.
#' Defaults to 'euclidean'. 'greatcirc' requires (longitude, latitude) coordinates in degrees,
#' and gives an error if any coordinate is out of range, e.g. for projected coordinates.
#' Swapped (latitude, longitude) columns cannot always be detected.
#' @param numMCMCIter Number of Markov Chain Monte Carlo iterations.
#' @param numBurnIter Number of burn-in iterations to be discarded before sampling from posterior. 
#' @param numThinIter Number of thinning iterations to be discarded between sampling from posterior. 
#' @param mSeedsProposalS2 Variance of normal proposals to update the seeds of the migration tiles. Defaults to 0.01.
#' @param qSeedsProposalS2 Variance of normal proposals to update the seeds of the diversity tiles. Defaults to 0.1. 
#' @param mEffctProposalS2 Variance of normal proposals to update the log10 rates of the migration tiles. Defaults to 0.1.
#' @param qEffctProposalS2 Variance of normal proposals to update the log10 rates of the diversity tiles. Defaults to 0.001.
#' @param mrateMuProposalS2 Variance of normal proposals to update the overall mean migration rate, on the log10 scale.
#' Defaults to 0.01.
#' @param qVoronoiPr With probability qVoronoiPr, update diversity Voronoi;
#' with probability 1-qVoronoiPr, update migration Voronoi. Defaults to 0.25.
#' @param qrateShape Shape hyperparameter for the diversity rates variance, qrateS2 ~ invgamma(qrateShape, qrateScale).
#' Defaults to 0.001
#' @param mrateShape Shape hyperparameter for the migration rates variance, mrateS2 ~ invgamma(mrateShape, mrateScale).
#' Defaults to 0.001.
#' @param sigmaShape Shape hyperparameter for the scaling factor sigma^2 ~ invgamma(sigmaShape, sigmaScale).
#' Defaults to 0.001.
#' @param qrateScale Scale hyperparameter for the diversity rates variance, qrateS2 ~ invgamma(qrateShape, qrateScale).
#' Defaults to 1.0.
#' @param mrateScale Scale hyperparameter for the migration rates variance, mrateS2 ~ invgamma(mrateShape, mrateScale).
#' Defaults to 1.0.
#' @param sigmaScale Scale hyperparameter for the scaling factor sigma^2 ~ invgamma(sigmaShape, sigmaScale).
#' Defaults to 1.0.
#' @param negBiProb Success probability for the number of Voronoi tiles ~ negbinom(negBiSize, negBiProb). Defaults to 0.67.
#' @param negBiSize Size for the number of Voronoi tiles ~ negbinom(negBiSize, negBiProb). Defaults to 10.
#' @returns None
#' @references Petkova, D., Novembre, J. & Stephens, M.
#' Visualizing spatial population structure with estimated effective migration surfaces.
#' Nat Genet 48, 94–100 (2016). https://doi.org/10.1038/ng.3464
#' @examples
#' # We use example input from Petkova et al. (2016), found in the '/extdata' directory
#' data_path <- system.file("extdata", package = "reems")
#' input <- file.path(data_path, "barrier-schemeX-nIndiv300-nSites3000")
#' # The example puts the output in a temporary directory.
#' mcmcdir <- file.path(tempdir(), "eems_out")
#' dir.create(mcmcdir, showWarnings = FALSE)
#' 
#'
#' # Run an example EEMS analysis with a small number of iterations to ensure quick termination. 
#' 
#' eems.from.files(
#'   datapath = input,
#'   mcmcpath = mcmcdir,
#'   nDemes = 200,
#'   nIndiv = 300,
#'   nSites = 3000,
#'   diploid = FALSE,
#'   numMCMCIter = 200,
#'   numBurnIter = 100,
#'   numThinIter = 99,
#' )
#' # Delete the output directory to tidy up. 
#' unlink(mcmcdir, recursive = TRUE, force = TRUE)
#' @seealso \code{\link{eems.plots}, \link{eems}}
#' @export
eems.from.files <- function(
                            seed = unclass(Sys.time()),
                            datapath,
                            mcmcpath,
                            prevpath = "",
                            gridpath = "",
                            nDemes,
                            nIndiv,
                            nSites,
                            diploid = TRUE,
                            distance = "euclidean",
                            numMCMCIter,
                            numBurnIter,
                            numThinIter,
                            mSeedsProposalS2 = 0.01,
                            qSeedsProposalS2 = 0.1,
                            mEffctProposalS2 = 0.1,
                            qEffctProposalS2 = 0.001,
                            mrateMuProposalS2 = 0.01,
                            qVoronoiPr = 0.25,
                            qrateShape = 0.001,
                            mrateShape = 0.001,
                            sigmaShape = 0.001,                            
                            qrateScale = 1.0,
                            mrateScale = 1.0,
                            sigmaScale = 1.0,
                            negBiProb = 0.67,
                            negBiSize = 10) {
    if (identical(distance, "greatcirc")) {
        check.greatcirc.coords(
            coordinates = read.coord.file(paste0(datapath, ".coord")),
            outer = read.coord.file(paste0(datapath, ".outer")),
            demes = if (nzchar(gridpath)) read.coord.file(paste0(gridpath, ".demes"))
        )
    }
    testing = FALSE
    p <- as.list(environment())
    cpp_eems(p)
}

#' Check that coordinates can be (longitude, latitude) in degrees
#'
#' Great circle distance reads the first column as longitude and the second as
#' latitude, in degrees. Projected coordinates, or some swapped columns, fall
#' outside longitude \[-180, 360\] or latitude \[-90, 90\]. Swapped columns that
#' are both within 90 cannot be detected.
#'
#' @param ... Named two-column matrices, such as `coordinates` and `outer`.
#'   `NULL` entries are skipped.
#' @returns `NULL`, invisibly. Stops naming the first input out of range.
#' @noRd
check.greatcirc.coords <- function(...) {
    inputs <- list(...)
    for (name in names(inputs)) {
        x <- inputs[[name]]
        if (is.null(x)) next
        x <- as.matrix(x)
        if (any(x[, 1] < -180 | x[, 1] > 360 | x[, 2] < -90 | x[, 2] > 90, na.rm = TRUE)) {
            stop("distance = \"greatcirc\" needs (longitude, latitude) coordinates in degrees, but some ",
                 name, " are outside longitude [-180, 360] / latitude [-90, 90]. ",
                 "Use distance = \"euclidean\" for projected coordinates.", call. = FALSE)
        }
    }
    invisible(NULL)
}

#' Read a two-column coordinate file in EEMS input format
#'
#' @param path Path to a whitespace-separated file with two numbers per line.
#' @returns A two-column numeric matrix.
#' @noRd
read.coord.file <- function(path) {
    matrix(scan(path, what = numeric(), quiet = TRUE), ncol = 2, byrow = TRUE)
}

#' Run an EEMS analysis from data in R format
#'
#' This function runs an EEMS analysis with given parameters in the input format for `conStruct`,
#' which include R matrices for allele frequency data and coordinates.  
#' In addition to taking R objects as inputs,
#' it also provides additional default values over the original `EEMS` command-line utility.
#' Furthermore, it allows for running more than one chain, either in parallel or sequentially,
#' and returns a vector of output directories as input to [eems.plots()].  
#'
#' @param coords The coordinate matrix, a two-column matrix with a line for every sample.
#' @param freqs An allele frequency  matrix in `conStruct` format, with a column for every site and a row for every sample.
#' Choosing one main allele, it tabulates the average genotypes of the sample at that locus.  
#' @param outer Matrix of coordinates of the outer polygon.
#' Defaults to a polygon larger than the convex hull of the supplied coordinates to mitigate EEMS boundary effects,
#' which is created by [outer.buffered()].
#' @param buffer The rough distance of the boundary from the convex hull. Defaults to 0.2 times the length of the hull.
#' Unused if `outer` is specified.
#' @param nDemes Number of demes, roughly. EEMS constructs a regular triangular grid with circa nDemes vertices.
#' Defaults to 6 times the number of individuals, aiming for sufficient resolution. 
#' @param nChains Number of chains to be run. Defaults to 1.
#' @param parallel Logical values that indicates whether chains should be run in parallel. Defaults to FALSE.
#' @param nWorkers If `parallel` is TRUE, then the number of workers used is the minimum of `nChains` and `nWorkers`.
#' Defaults to the number of available cores.
#' @param distance Distance metric. Either `euclidean` or `greatcirc`, that is, great circle distance.
#' Defaults to 'greatcirc', which is more accurate at larger scales, and requires
#' (longitude, latitude) coordinates in degrees. Gives an error if any coordinate is out of range,
#' e.g. for projected coordinates; use 'euclidean' for those.
#' Swapped (latitude, longitude) columns cannot always be detected.
#' @param numMCMCIter Number of Markov Chain Monte Carlo iterations. Defaults to 2000000.
#' @param numBurnIter Number of burn-in iterations to be discarded before sampling from posterior. Defaults to 1000000.
#' @param numThinIter Number of thinning iterations to be discarded between sampling from posterior. Defaults to 9999.
#' @param demes Matrix of coordinates of demes; optional, and usually unnecessary. Only in conjunction with `edges`.
#' @param edges Matrix of custom input grid; optional, and usually unnecessary. Only in conjunction with `demes`. 
#' @inheritParams eems.from.files
#' @returns A vector of output directories which can be given as input to [eems.plots()]. 
#' @references Petkova, D., Novembre, J. & Stephens, M.
#' Visualizing spatial population structure with estimated effective migration surfaces.
#' Nat Genet 48, 94–100 (2016). https://doi.org/10.1038/ng.3464
#' @examples
#' # The example puts the output in a temporary directory.
#' mcmcdir <- file.path(tempdir(), "eems_out")
#' dir.create(mcmcdir, showWarnings = FALSE)
#' mcmcpath <- file.path(mcmcdir,"example")
#'
#' # Run an example EEMS analysis with a small number of iterations to ensure quick termination. 
#' 
#' eems(
#'   freqs = ex.freqs,
#'   coords = ex.coords,
#'   mcmcpath = mcmcpath,   
#'   numMCMCIter = 200,
#'   numBurnIter = 100,
#'   numThinIter = 99,
#' )
#' # Delete the output directory to tidy up. 
#' unlink(mcmcdir, recursive = TRUE, force = TRUE)
#' @seealso \code{\link{eems.plots}, \link{eems.from.files}}
#' @export
eems <- function(
                 seed = unclass(Sys.time()),
                 coords,
                 freqs,
                 outer = NULL,
                 buffer = NULL,
                 mcmcpath,
                 demes = NULL,
                 edges = NULL,
                 prevpath = "",
                 nChains = 1,
                 parallel = FALSE,
                 nWorkers = NULL,
                 nDemes = NULL,
                 diploid = TRUE,
                 distance = "greatcirc",
                 numMCMCIter = 2000000,
                 numBurnIter = 1000000,
                 numThinIter = 9999,
                 mSeedsProposalS2 = 0.01,
                 qSeedsProposalS2 = 0.1,
                 mEffctProposalS2 = 0.1,
                 qEffctProposalS2 = 0.001,
                 mrateMuProposalS2 = 0.01,
                 qVoronoiPr = 0.25,
                 qrateShape = 0.001,
                 mrateShape = 0.001,
                 sigmaShape = 0.001,                            
                 qrateScale = 1.0,
                 mrateScale = 1.0,
                 sigmaScale = 1.0,
                 negBiProb = 0.67,
                 negBiSize = 10) {
    if (is.null(outer)) {
        outer <- outer.buffered(coords,buffer)
    }
    if (identical(distance, "greatcirc")) {
        check.greatcirc.coords(coordinates = coords, outer = outer, demes = demes)
    }
    nIndiv <- nrow(freqs)
    nSites <- ncol(freqs)
    if (is.null(nDemes)) {
        nDemes <- 6*nIndiv 
    }
    data_path <- file.path(tempdir(), "data_path")
    dir.create(data_path, showWarnings = FALSE)
    on.exit(unlink(data_path, recursive = TRUE, force = TRUE), add = TRUE)
    datapath <- file.path(data_path,"in")
    coordfile <- paste0(datapath, ".coord")
    diffsfile <- paste0(datapath,".diffs")
    outerfile <- paste0(datapath,".outer")
    write_for_eems(coords,coordfile)
    write_for_eems(geno2diffs(freqs),diffsfile)
    write_for_eems(outer,outerfile)
    write_for_eems(coords,coordfile)    
    testing = FALSE

    if (!is.null(demes) && !is.null(edges)) {
        gridpath <- datapath
        demesfile <- paste0(gridpath, ".demes")
        edgesfile <- paste0(gridpath, ".edges")
        write_for_eems(demes, demesfile)
        write_for_eems(edges, edgesfile)
    }
    else {
        gridpath = ""
    }
    p <- as.list(environment())
    if (parallel){
        if(is.null(nWorkers)) {
            nWorkers <- min(nChains, future::availableCores(constraints = "connections-16"))
        }
        old_future_plan <- future::plan()
        on.exit(future::plan(old_future_plan), add = TRUE)
        future::plan(future::multisession, workers = nWorkers)
        paths <- future.apply::future_sapply(1:nChains, function(i) {
        pf <- p
        pf$mcmcpath <- paste0(p$mcmcpath, "-chain", i)
        dir.create(pf$mcmcpath, showWarnings = FALSE)
        pf$seed <- p$seed + i
        capture.output(cpp_eems(pf), file = file.path(pf$mcmcpath, "eems.log"))
        return(pf$mcmcpath)
        }, future.seed = TRUE)
        return(paths)
    }
    else {
        sapply(1:nChains, function(i) {
        pf <- p
        pf$mcmcpath <- paste0(p$mcmcpath, "-chain", i)
        pf$seed <- p$seed + i
        cpp_eems(pf)
        return(pf$mcmcpath)
        })
    }
}
