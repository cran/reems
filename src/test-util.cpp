/*
 * Unit tests for the numerical helpers in util.h.
 * Reference values were computed in R (see the comments next to each constant).
 */

// Project headers (Eigen, Boost, Rcpp) must come before testthat.h
#include "util.h"
#include <testthat.h>

// Defined in util.cpp but only the two-argument overload is declared in util.h
MatrixXd resistance_distance(const MatrixXd &M);

static const double tol = 1e-10;

context("util: matrix predicates and determinants") {

  test_that("logdet matches log(det) for an SPD matrix") {
    MatrixXd A(3, 3);
    A << 4, 1, 0,
         1, 3, 1,
         0, 1, 2;
    expect_true(std::abs(logdet(A) - std::log(A.determinant())) < tol);
  }

  test_that("isposdef separates positive definite from indefinite matrices") {
    MatrixXd A = MatrixXd::Identity(3, 3) * 2.0;
    MatrixXd B(2, 2);
    B << 1, 2,
         2, 1;   // eigenvalues 3 and -1
    expect_true(isposdef(A));
    expect_false(isposdef(B));
  }

  test_that("isdistmat accepts squared Euclidean distances and rejects non-distances") {
    MatrixXd X(3, 2);
    X << 0, 0,
         1, 0,
         0, 2;
    MatrixXd D = euclidean_dist(X, X);
    expect_true(isdistmat(D));

    MatrixXd notZeroDiag = D;
    notZeroDiag(0, 0) = 1.0;
    expect_false(isdistmat(notZeroDiag));

    MatrixXd asym = D;
    asym(0, 1) += 0.5;
    expect_false(isdistmat(asym));
  }
}

context("util: distances") {

  // euclidean_dist returns *squared* distances; callers only use it to find the closest point
  test_that("euclidean_dist gives squared distances (3-4-5 triangle)") {
    MatrixXd X(1, 2), Y(2, 2);
    X << 0, 0;
    Y << 3, 4,
         0, 0;
    MatrixXd D = euclidean_dist(X, Y);
    expect_true((D.rows() == 1 && D.cols() == 2));
    expect_true(std::abs(D(0, 0) - 25.0) < tol);
    expect_true(std::abs(D(0, 1)) < tol);
  }

  test_that("greatcirc_dist gives a quarter of the equator for 90 degrees of longitude") {
    MatrixXd X(1, 2), Y(1, 2);
    X << 0, 0;
    Y << 90, 0;
    double quarter = 0.25 * boost::math::double_constants::pi * Earth_radiusX2;
    expect_true(std::abs(greatcirc_dist(X, Y)(0, 0) - quarter) < 1e-6);
  }

  test_that("greatcirc_dist is symmetric with a zero diagonal") {
    MatrixXd X(3, 2);
    X << 10, 50,
         -5, 40,
         30, -20;
    MatrixXd D = greatcirc_dist(X, X);
    expect_true((D - D.transpose()).cwiseAbs().maxCoeff() < 1e-6);
    expect_true(D.diagonal().cwiseAbs().maxCoeff() < 1e-6);
  }

  test_that("pairwise_dist dispatches on the metric name") {
    MatrixXd X(1, 2), Y(1, 2);
    X << 0, 0;
    Y << 90, 0;
    expect_true(pairwise_dist(X, Y, "greatcirc").isApprox(greatcirc_dist(X, Y)));
    expect_true(pairwise_dist(X, Y, "euclidean").isApprox(euclidean_dist(X, Y)));
  }

  test_that("resistance_distance on a path graph adds resistances in series") {
    // Path 0 - 1 - 2 with unit conductances
    MatrixXd M = MatrixXd::Zero(3, 3);
    M(0, 1) = M(1, 0) = 1.0;
    M(1, 2) = M(2, 1) = 1.0;
    MatrixXd R = resistance_distance(M);
    expect_true(std::abs(R(0, 1) - 1.0) < tol);
    expect_true(std::abs(R(1, 2) - 1.0) < tol);
    expect_true(std::abs(R(0, 2) - 2.0) < tol);
    expect_true(R.diagonal().cwiseAbs().maxCoeff() < tol);
  }
}

context("util: log densities") {

  test_that("dnegbinln matches R's dnbinom up to the normalising constant") {
    // R: dnbinom(3, 10, 1 - 0.67, log = TRUE) - dnbinom(0, 10, 1 - 0.67, log = TRUE)
    double ref = 4.1921948465609846;
    expect_true(std::abs(dnegbinln(3, 10, 0.67) - dnegbinln(0, 10, 0.67) - ref) < tol);
    // R: lgamma(13) - lgamma(4) + 3 * log(0.67)
    expect_true(std::abs(dnegbinln(3, 10, 0.67) - 16.994022326642455) < tol);
  }

  test_that("dnegbinln is -Inf outside its support") {
    expect_true(dnegbinln(-1, 10, 0.5) == -Inf);
    expect_true(dnegbinln(3, 10, 1.0) == -Inf);
  }

  test_that("dinvgamln matches the unnormalised inverse gamma density") {
    // R: -(3 + 1) * log(2) - 1.5 / 2
    expect_true(std::abs(dinvgamln(2.0, 3.0, 1.5) - (-3.5225887222397811)) < tol);
    expect_true(dinvgamln(-1.0, 3.0, 1.5) == -Inf);
  }

  test_that("dtrnormln matches the truncated normal density up to 0.5 * log(2 * pi)") {
    // R: log(dnorm(0.05, 0.01, sqrt(0.002)) /
    //        (pnorm(0.1, 0.01, sqrt(0.002)) - pnorm(-0.1, 0.01, sqrt(0.002)))) + 0.5 * log(2 * pi)
    expect_true(std::abs(dtrnormln(0.05, 0.01, 0.002, 0.1) - 2.7367728410326508) < tol);
    expect_true(dtrnormln(0.2, 0.01, 0.002, 0.1) == -Inf);
  }
}

context("util: vector and matrix manipulation") {

  test_that("split parses whitespace-separated numbers") {
    VectorXd v = split("1.5  2\t-3");
    expect_true(v.size() == 3);
    expect_true((v(0) == 1.5 && v(1) == 2.0 && v(2) == -3.0));
  }

  // removeElem/removeRow fill the gap with the last element/row (order is not preserved)
  test_that("removeElem and insertElem edit vectors in place") {
    VectorXd v(3);
    v << 1, 2, 3;
    removeElem(v, 1);
    expect_true((v.size() == 2 && v(0) == 1 && v(1) == 3));
    insertElem(v, 7);
    expect_true((v.size() == 3 && v(2) == 7));
  }

  test_that("removeRow and insertRow edit matrices in place") {
    MatrixXd m(3, 2);
    m << 1, 2,
         3, 4,
         5, 6;
    removeRow(m, 0);
    expect_true((m.rows() == 2 && m(0, 0) == 5 && m(1, 1) == 4));
    VectorXd row(2);
    row << 8, 9;
    insertRow(m, row);
    expect_true((m.rows() == 3 && m(2, 0) == 8 && m(2, 1) == 9));
  }

  test_that("slice extracts elements and submatrices by index") {
    VectorXd v(4);
    v << 10, 20, 30, 40;
    VectorXi I(2);
    I << 3, 1;
    VectorXd s = slice(v, I);
    expect_true((s.size() == 2 && s(0) == 40 && s(1) == 20));

    MatrixXd A(3, 3);
    A << 1, 2, 3,
         4, 5, 6,
         7, 8, 9;
    VectorXi R(2), C(1);
    R << 0, 2;
    C << 1;
    MatrixXd B = slice(A, R, C);
    expect_true((B.rows() == 2 && B.cols() == 1 && B(0, 0) == 2 && B(1, 0) == 8));
  }
}
