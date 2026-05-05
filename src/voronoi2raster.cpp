/*
 * Modified by Felix Weitkämper on 2026-04-11.
 * Remove functions that overlap with util.cpp and include those from there.
 */

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>

// via the depends attribute we tell Rcpp to create hooks for
// RcppEigen so that the build process will know what to do
//
// [[Rcpp::depends(RcppEigen)]]

#include "util.h" 
const double pi_180 = M_PI / 180.0;

// Compute one contour, by filling in each of the pixels/marks
Eigen::VectorXd compute_contours(const Eigen::MatrixXd &marks, const Eigen::VectorXd &now_rates,
				 const Eigen::MatrixXd &now_seeds, const std::string &distm) {
  int nmrks = marks.rows();
  Eigen::VectorXd raster = Eigen::VectorXd::Zero(nmrks);
  Eigen::MatrixXd dists = pairwise_dist(marks, now_seeds, distm);
  for ( int row = 0, closest = 0 ; row < nmrks ; row++ ) {
    dists.row(row).minCoeff( &closest );
    raster(row) = now_rates( closest );
  }
  return raster;
}
// Compute the average contour, by calling compute_contours repeatedly
//
// [[Rcpp::export]]
Rcpp::List tiles2contours_standardize(const Eigen::VectorXd &tiles,
                                      const Eigen::VectorXd &rates,
                                      const Eigen::MatrixXd &seeds,
                                      const Eigen::MatrixXd &marks,
                                      const std::string &distm) {
  bool use_weighted_mean = true;
  int nmrks = marks.rows();
  Eigen::VectorXd zval = Eigen::VectorXd::Zero(nmrks);
  Eigen::VectorXd ones = Eigen::VectorXd::Ones(nmrks);
  // Vector to store the migration rates m (or diversity rates q).
  Eigen::VectorXd raster = Eigen::VectorXd::Zero(nmrks);
  // Vectors to store the number of times (m > 0) and (m < 0).
  Eigen::VectorXd pr_gt0 = Eigen::VectorXd::Zero(nmrks);
  Eigen::VectorXd pr_lt0 = Eigen::VectorXd::Zero(nmrks);
  for ( int iter = 0, pos = 0 ; iter < tiles.size() ; iter++ ) {
    int now_tiles = (int)tiles(iter);
    Eigen::VectorXd now_rates = rates.segment(pos, now_tiles);
    Eigen::MatrixXd now_seeds = seeds.block(pos, 0, now_tiles, 2);
    if (use_weighted_mean) {
      zval = compute_contours(marks, now_rates, now_seeds, distm);
      zval = zval.array() - zval.mean();
    } else {
      now_rates = now_rates.array() - now_rates.mean();
      zval = compute_contours(marks, now_rates, now_seeds, distm);
    }
    pr_gt0 += (zval.array() > 0.0).select(ones, 0.0);
    pr_lt0 += (zval.array() < 0.0).select(ones, 0.0);
    raster += zval;
    pos += now_tiles;
  }
  // Do not divide by the number of iterations here
  // as we might be combing results from several output directories.
  return Rcpp::List::create(Rcpp::Named("zvals") = raster,
                            Rcpp::Named("prgt0") = pr_gt0,
                            Rcpp::Named("prlt0") = pr_lt0);
}
// Compute the average contour, by calling compute_contours repeatedly,
// this time without normalizing the rates to have mean 0. Without normalizing,
// it doesn't make sense to keep track of the number of times (m > 0) and (m < 0).
//
// [[Rcpp::export]]
Rcpp::List tiles2contours(const Eigen::VectorXd &tiles,
                          const Eigen::VectorXd &rates,
                          const Eigen::MatrixXd &seeds,
                          const Eigen::MatrixXd &marks,
                          const std::string &distm) {
  Eigen::VectorXd raster = Eigen::VectorXd::Zero(marks.rows());
  for ( int iter = 0, pos = 0 ; iter < tiles.size() ; iter++ ) {
    int now_tiles = (int)tiles(iter);
    Eigen::VectorXd now_rates = rates.segment(pos, now_tiles);
    Eigen::MatrixXd now_seeds = seeds.block(pos, 0, now_tiles, 2);
    raster += compute_contours(marks, now_rates, now_seeds, distm);
    pos += now_tiles;
  }
  // Do not divide by the number of iterations here
  // as we might be combing results from several output directories.
  return Rcpp::List::create(Rcpp::Named("zvals") = raster);
}
