#-----------------------------------------------------
# 7. Data generation functions. Useful for simulations
#-----------------------------------------------------
# 7. 1. Generate data
# Generates data based on a linear model with specified n, rho2 and j
# n: size of the data set
# rho2: Population squared correlation coefficient
# j: predictors
generate_data <- function(n = 100, rho2 = 0.4, j = 1){

  # Population means and variances
  mus <- rep(0, j+1)
  vars <- rep(1, j+1)

  # Population variance-covariance matrix
  var_cov <- matrix(data = 0, nrow = j+1, ncol = j+1,
                    dimnames = list(c(paste("X", 1:j, sep = ""), "Y"),
                                    c(paste("X", 1:j, sep = ""), "Y"))) # j predictors + 1 dependent variable
  diag(var_cov) <- vars # add variances to var_cov matrix

  # Correlation predictors - dependent variable
  cor_xy <- sqrt(rho2 / j)
  var_cov[1:j, ncol(var_cov)] <- var_cov[nrow(var_cov), 1:j] <- cor_xy # add correlations to var_cov matrix

  # Generate data
  data <- MASS::mvrnorm(n = n, mu = mus, Sigma = var_cov)

  # Generated data as data frame:
  data <- as.data.frame(data)

  # Result
  return(data)

}
