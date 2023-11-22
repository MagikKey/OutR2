#-----------------------------------------------------
# 7. Data generation functions. Useful for simulations
#-----------------------------------------------------
# 7. 3. Generate correlation data
# Generates data based on a correlation
# n: size of the data set
# r: desired correlation
generate_cor_data <- function(n, r = 0){

  # r: specified correlation

  # Predictors
  j <- 1
  # Population means and variances
  mus <- rep(0, j+1)
  vars <- rep(1, j+1)

  # Population variance-covariance matrix
  sigma <- matrix(NA, nrow = j+1, ncol = j+1,
                  dimnames = list(c(paste("X", 1:j, sep = ""), "Y"),
                                  c(paste("X", 1:j, sep = ""), "Y")))
  diag(sigma) <- vars # add variances
  sigma[1:j, ncol(sigma)] <- sigma[nrow(sigma), 1:j] <- r # add correlation

  # Generate data frame with given correlation
  data <- data.frame(mvrnorm(n = n, mu = mus, Sigma = sigma))

  # X and Y
  X <- data$X1
  Y <- data$Y

  # Correlation and R2
  cor <- cor(X, Y)
  r2 <- cor^2

  # Return results
  return(list(data =  data,
              cor = cor,
              r2 = r2))
}
