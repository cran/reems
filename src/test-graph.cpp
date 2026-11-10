/*
 * Unit tests for Habitat and Graph.
 * Habitat::generate_outer reads `datapath.outer` and both classes write their
 * outputs to `mcmcpath`, so each test works in a fresh R temporary directory.
 */

// Project headers (Eigen, Boost, Rcpp) must come before testthat.h
#include "graph.h"
#include <testthat.h>

// Creates a temporary directory holding `<dir>/in.outer` with the given ring.
// Returns the directory; the datapath to pass to Habitat is `<dir>/in`.
static string write_outer(const MatrixXd &outer) {
  Rcpp::Function tempfile("tempfile");
  string dir = Rcpp::as<string>(tempfile("reems-cpp-test"));
  std::filesystem::create_directories(dir);
  dlmwrite(dir + "/in.outer", outer);
  return dir;
}

static MatrixXd square(const double side) {
  MatrixXd outer(5, 2);
  outer << 0,    0,
           0,    side,
           side, side,
           side, 0,
           0,    0;
  return outer;
}

context("habitat") {

  test_that("a square habitat has the expected bounding box and area") {
    string dir = write_outer(square(10));
    Habitat habitat;
    habitat.generate_outer(dir + "/in", dir);
    expect_true((habitat.get_xmin() == 0 && habitat.get_xmax() == 10));
    expect_true((habitat.get_ymin() == 0 && habitat.get_ymax() == 10));
    expect_true(std::abs(habitat.get_area() - 100.0) < 1e-10);
    std::filesystem::remove_all(dir);
  }

  test_that("in_point distinguishes inside from outside") {
    string dir = write_outer(square(10));
    Habitat habitat;
    habitat.generate_outer(dir + "/in", dir);
    expect_true(habitat.in_point(5, 5));
    expect_false(habitat.in_point(15, 5));
    expect_false(habitat.in_point(-1, -1));
    std::filesystem::remove_all(dir);
  }
}

context("graph") {

  test_that("the triangular grid is connected and every deme lies in the habitat") {
    string dir = write_outer(square(10));
    Habitat habitat;
    habitat.generate_outer(dir + "/in", dir);
    Graph graph;
    graph.generate_only_grid(dir, habitat, 50);

    int nDemes = graph.get_num_total_demes();
    expect_true(nDemes > 1);
    expect_true(graph.get_num_edges() > 0);
    expect_true(graph.is_connected());

    int alpha, beta;
    for (int e = 0; e < graph.get_num_edges(); e++) {
      graph.get_edge(e, alpha, beta);
      expect_true(alpha != beta);
      expect_true((alpha >= 0 && alpha < nDemes));
      expect_true((beta >= 0 && beta < nDemes));
    }
    std::filesystem::remove_all(dir);
  }

  test_that("a denser grid has more demes") {
    string dir = write_outer(square(10));
    Habitat habitat;
    habitat.generate_outer(dir + "/in", dir);
    Graph sparse, dense;
    sparse.generate_only_grid(dir, habitat, 20);
    dense.generate_only_grid(dir, habitat, 200);
    expect_true(dense.get_num_total_demes() > sparse.get_num_total_demes());
    std::filesystem::remove_all(dir);
  }
}
