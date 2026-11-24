# babebi 0.1.1

## Bug fixes

- Corrected the orientation of the upper-triangular
  Cholesky factor in `fit_model()` when generating
  multivariate normal posterior draws.

- Posterior draws now reproduce the covariance matrix
  specified by the implemented normal approximation.
  This correction affects posterior interval estimates,
  posterior tail probabilities and posterior density plots.

## Testing

- Added a regression test to verify the covariance
  structure of posterior draws.

- Verified that the new regression test detects the error
  in version 0.1.0 and passes with the corrected
  implementation.

The statistical model, least-squares point estimates,
BIC-based Bayes factors, design calibration and Monte
Carlo simulation procedure remain unchanged.

# babebi 0.1.0

- Initial CRAN release.
