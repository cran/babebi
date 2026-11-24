library(testthat)
library(babebi)

test_that("fit_model returns a babebi_fit object with expected components", {
  dat <- data.frame(
    id = rep(1:4, each = 4),
    time = rep(c("pre", "pre", "post", "post"), times = 4),
    rater = rep(c("r1", "r2"), times = 8),
    y = c(
      3.0, 3.2, 3.8, 4.0,
      2.9, 3.1, 3.5, 3.7,
      3.4, 3.6, 4.0, 4.1,
      3.1, 3.3, 3.9, 4.0
    )
  )

  fit <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 500,
    seed = 123
  )

  expect_s3_class(fit, "babebi_fit")
  expect_identical(
    names(fit),
    c(
      "beta_hat",
      "gamma_hat",
      "ci_low",
      "ci_high",
      "p_pos",
      "bf10",
      "posterior_draws",
      "design"
    )
  )

  expect_true(is.numeric(fit$beta_hat))
  expect_length(fit$beta_hat, 1)

  expect_true(is.numeric(fit$gamma_hat))
  expect_length(fit$gamma_hat, 1)

  expect_true(is.numeric(fit$ci_low))
  expect_length(fit$ci_low, 1)

  expect_true(is.numeric(fit$ci_high))
  expect_length(fit$ci_high, 1)

  expect_true(is.numeric(fit$p_pos))
  expect_length(fit$p_pos, 1)

  expect_true(is.numeric(fit$bf10))
  expect_length(fit$bf10, 1)

  expect_s3_class(fit$posterior_draws, "data.frame")
  expect_identical(names(fit$posterior_draws), c("beta", "gamma"))
  expect_equal(nrow(fit$posterior_draws), 500)

  expect_type(fit$design, "list")
  expect_identical(
    names(fit$design),
    c("N", "delta", "bias_r2", "time_levels", "rater_levels")
  )
})

test_that("fit_model returns numerically coherent estimates on the example dataset", {
  dat <- data.frame(
    id = rep(1:4, each = 4),
    time = rep(c("pre", "pre", "post", "post"), times = 4),
    rater = rep(c("r1", "r2"), times = 8),
    y = c(
      3.0, 3.2, 3.8, 4.0,
      2.9, 3.1, 3.5, 3.7,
      3.4, 3.6, 4.0, 4.1,
      3.1, 3.3, 3.9, 4.0
    )
  )

  fit <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 1000,
    seed = 123
  )

  expect_equal(fit$beta_hat, 0.70, tolerance = 1e-10)
  expect_equal(fit$gamma_hat, -0.05, tolerance = 1e-10)

  expect_true(fit$ci_low < fit$ci_high)
  expect_true(fit$p_pos >= 0 && fit$p_pos <= 1)
  expect_true(is.finite(fit$bf10))
  expect_true(fit$bf10 >= 0)

  expect_equal(fit$design$N, 4)
  expect_equal(fit$design$delta, 0.675, tolerance = 1e-10)
  expect_equal(fit$design$bias_r2, 0.175, tolerance = 1e-10)
})

test_that("fit_model is reproducible when seed is fixed", {
  dat <- data.frame(
    id = rep(1:4, each = 4),
    time = rep(c("pre", "pre", "post", "post"), times = 4),
    rater = rep(c("r1", "r2"), times = 8),
    y = c(
      3.0, 3.2, 3.8, 4.0,
      2.9, 3.1, 3.5, 3.7,
      3.4, 3.6, 4.0, 4.1,
      3.1, 3.3, 3.9, 4.0
    )
  )

  fit1 <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 500,
    seed = 999
  )

  fit2 <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 500,
    seed = 999
  )

  expect_equal(fit1$beta_hat, fit2$beta_hat)
  expect_equal(fit1$gamma_hat, fit2$gamma_hat)
  expect_equal(fit1$ci_low, fit2$ci_low)
  expect_equal(fit1$ci_high, fit2$ci_high)
  expect_equal(fit1$p_pos, fit2$p_pos)
  expect_equal(fit1$bf10, fit2$bf10)
  expect_equal(fit1$posterior_draws, fit2$posterior_draws)
})

test_that("fit_model accepts character column names", {
  dat <- data.frame(
    subj = rep(1:4, each = 4),
    occasion = rep(c("pre", "pre", "post", "post"), times = 4),
    judge = rep(c("r1", "r2"), times = 8),
    score = c(
      3.0, 3.2, 3.8, 4.0,
      2.9, 3.1, 3.5, 3.7,
      3.4, 3.6, 4.0, 4.1,
      3.1, 3.3, 3.9, 4.0
    )
  )

  fit <- fit_model(
    data = dat,
    id = "subj",
    time = "occasion",
    rater = "judge",
    outcome = "score",
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 200,
    seed = 123
  )

  expect_s3_class(fit, "babebi_fit")
  expect_equal(nrow(fit$posterior_draws), 200)
})

test_that("fit_model fails when n_draws is not a positive integer", {
  dat <- data.frame(
    id = rep(1:4, each = 4),
    time = rep(c("pre", "pre", "post", "post"), times = 4),
    rater = rep(c("r1", "r2"), times = 8),
    y = c(
      3.0, 3.2, 3.8, 4.0,
      2.9, 3.1, 3.5, 3.7,
      3.4, 3.6, 4.0, 4.1,
      3.1, 3.3, 3.9, 4.0
    )
  )

  expect_error(
    fit_model(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      n_draws = 0
    ),
    "`n_draws` must be a positive integer"
  )

  expect_error(
    fit_model(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      n_draws = 10.5
    ),
    "`n_draws` must be a positive integer"
  )
})

test_that("fit_model fails when seed is not an integer", {
  dat <- data.frame(
    id = rep(1:4, each = 4),
    time = rep(c("pre", "pre", "post", "post"), times = 4),
    rater = rep(c("r1", "r2"), times = 8),
    y = c(
      3.0, 3.2, 3.8, 4.0,
      2.9, 3.1, 3.5, 3.7,
      3.4, 3.6, 4.0, 4.1,
      3.1, 3.3, 3.9, 4.0
    )
  )

  expect_error(
    fit_model(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      seed = 1.5
    ),
    "`seed` must be a single integer value"
  )
})

test_that("fit_model fails when there are insufficient residual degrees of freedom", {
  dat <- data.frame(
    id = rep(1, each = 4),
    time = c("pre", "pre", "post", "post"),
    rater = c("r1", "r2", "r1", "r2"),
    y = c(1, 2, 3, 4)
  )

  expect_error(
    fit_model(
      data = dat,
      id = id,
      time = time,
      rater = rater,
      outcome = y,
      time_order = c("pre", "post"),
      rater_order = c("r1", "r2"),
      n_draws = 100,
      seed = 123
    ),
    "Insufficient degrees of freedom for variance estimation"
  )
})

# Regression test for the Cholesky correction in babebi 0.1.1

test_that("posterior draws reproduce the theoretical covariance matrix", {
  dat <- data.frame(
    id = rep(1:4, each = 4),
    time = rep(c("pre", "pre", "post", "post"), times = 4),
    rater = rep(c("r1", "r2"), times = 8),
    y = c(
      3.0, 3.2, 3.8, 4.0,
      2.9, 3.1, 3.5, 3.7,
      3.4, 3.6, 4.0, 4.1,
      3.1, 3.3, 3.9, 4.0
    )
  )

  fit <- fit_model(
    data = dat,
    id = id,
    time = time,
    rater = rater,
    outcome = y,
    time_order = c("pre", "post"),
    rater_order = c("r1", "r2"),
    n_draws = 50000,
    seed = 123
  )

  # Rater-specific change scores.
  d1 <- c(0.8, 0.6, 0.6, 0.8)
  d2 <- c(0.8, 0.6, 0.5, 0.7)

  N <- length(d1)

  # Pooled residual variance under the implemented model.
  sigma2 <- (
    sum((d1 - mean(d1))^2) +
      sum((d2 - mean(d2))^2)
  ) / (2 * N - 2)

  # Theoretical covariance matrix of the normal approximation.
  V_expected <- (sigma2 / N) * matrix(
    c(1, -1, -1, 2),
    nrow = 2
  )

  # Numerical stabilisation used in fit_model().
  diag(V_expected) <- diag(V_expected) + 1e-10

  # Empirical covariance matrix of the posterior draws.
  V_observed <- stats::cov(fit$posterior_draws)

  # Scale-independent discrepancies between covariance matrices.
  discrepancy <- abs(V_observed - V_expected) /
    sqrt(outer(diag(V_expected), diag(V_expected)))

  # Allow for Monte Carlo sampling variability.
  expect_lt(max(discrepancy), 0.05)
})
