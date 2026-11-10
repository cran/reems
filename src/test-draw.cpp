/*
 * Unit tests for the Boost-based random number generator in draw.h.
 */

// Project headers (Eigen, Boost, Rcpp) must come before testthat.h
#include "draw.h"
#include <testthat.h>

context("draw") {

  test_that("the same seed gives the same sequence") {
    Draw a, b;
    a.initialize(123);
    b.initialize(123);
    for (int i = 0; i < 20; i++) {
      expect_true(a.runif() == b.runif());
      expect_true(a.rnorm(0.0, 1.0) == b.rnorm(0.0, 1.0));
    }
  }

  test_that("different seeds give different sequences") {
    Draw a, b;
    a.initialize(1);
    b.initialize(2);
    bool differ = false;
    for (int i = 0; i < 10; i++) {
      if (a.runif() != b.runif()) { differ = true; }
    }
    expect_true(differ);
  }

  test_that("draws stay inside their support") {
    Draw draw;
    draw.initialize(42);
    for (int i = 0; i < 1000; i++) {
      double u = draw.runif();
      expect_true((u >= 0.0 && u < 1.0));

      int k = draw.runif_int(3, 7);
      expect_true((k >= 3 && k <= 7));

      double t = draw.rtrnorm(0.0, 1.0, 0.5);
      expect_true((t >= -0.5 && t <= 0.5));

      expect_true(draw.rinvgam(3.0, 1.5) > 0.0);
      expect_true(draw.rnegbin(10, 0.67) >= 0);
    }
  }

  test_that("rnorm has roughly the requested mean") {
    Draw draw;
    draw.initialize(7);
    double sum = 0.0;
    int n = 20000;
    for (int i = 0; i < n; i++) { sum += draw.rnorm(5.0, 4.0); }
    // Standard error of the mean is 2 / sqrt(20000) ~ 0.014
    expect_true(std::abs(sum / n - 5.0) < 0.1);
  }
}
