/* Created by Felix Weitkämper on 2026-04-11 */

#include <Rcpp.h>
#include "util.h"
#include "runeems_snps.h"
#include "habitat.h"
#include "graph.h"
#include <string>

extern string dist_metric;

// [[Rcpp::export]]
void cpp_eems(const Rcpp::List p){

  Params params;
  params.seed = static_cast<long>(Rcpp::as<double>(p["seed"]));
  params.diploid = Rcpp::as<bool>(p["diploid"]);
  params.testing = Rcpp::as<bool>(p["testing"]);
  params.datapath = Rcpp::as<std::string>(p["datapath"]);
  params.mcmcpath = Rcpp::as<std::string>(p["mcmcpath"]);
  params.prevpath = Rcpp::as<std::string>(p["prevpath"]);
  params.gridpath = Rcpp::as<std::string>(p["gridpath"]);
  params.nDemes = Rcpp::as<int>(p["nDemes"]);
  params.nIndiv = Rcpp::as<int>(p["nIndiv"]);
  params.nSites = Rcpp::as<int>(p["nSites"]);
  dist_metric = Rcpp::as<std::string>(p["distance"]);
  params.numMCMCIter = Rcpp::as<int>(p["numMCMCIter"]);
  params.numBurnIter = Rcpp::as<int>(p["numBurnIter"]);
  params.numThinIter = Rcpp::as<int>(p["numThinIter"]);
  params.mSeedsProposalS2 = Rcpp::as<double>(p["mSeedsProposalS2"]);
  params.qSeedsProposalS2 = Rcpp::as<double>(p["qSeedsProposalS2"]);
  params.mEffctProposalS2 = Rcpp::as<double>(p["mEffctProposalS2"]);
  params.qEffctProposalS2 = Rcpp::as<double>(p["qEffctProposalS2"]);
  params.mrateMuProposalS2 = Rcpp::as<double>(p["mrateMuProposalS2"]);
  params.qVoronoiPr = Rcpp::as<double>(p["qVoronoiPr"]);
  params.qrateShape_2 = Rcpp::as<double>(p["qrateShape"]);
  params.mrateShape_2 = Rcpp::as<double>(p["mrateShape"]);
  params.sigmaShape_2 = Rcpp::as<double>(p["sigmaShape"]);
  params.qrateScale_2 = Rcpp::as<double>(p["qrateScale"]);
  params.mrateScale_2 = Rcpp::as<double>(p["mrateScale"]);
  params.sigmaScale_2 = Rcpp::as<double>(p["sigmaScale"]);
  params.negBiProb = Rcpp::as<double>(p["negBiProb"]);
  params.negBiSize = Rcpp::as<int>(p["negBiSize"]);
  run_eems(params);
}


// [[Rcpp::export]]
void cpp_write_grid(const std::string &datapath, const std::string &mcmcpath, const int nDemes)
{
  Graph graph;
  Habitat habitat;
  habitat.generate_outer(datapath, mcmcpath);
  graph.generate_only_grid(mcmcpath, habitat, nDemes);
}
