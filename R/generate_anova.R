#-----------------------------------------------------
# 7. Data generation functions. Useful for simulations
#-----------------------------------------------------
# 7. 2. Generate ANOVA data
# Generates data based on a linear model - ANOVA with simulated eta-squared
# n: size of the data set. J groups will be the same size, j/2
# mu_j1: population mean of group 1
# mu_j2: population mean of group 2
# var_j1: population variance of group 1
# var_j2: population variance of group 2
generate_anova <- function(n, J = 2, mu_j = c(-0.5, 0.5), var_j = c(1, 1)){

  # Conditions
  n_j <- n / J

  # Group 1
  x_j1 <- rep(0, n_j)
  y_j1 <- rnorm(n_j, mean = mu_j[1], sd = sqrt(var_j[1]))
  # Group 2
  x_j2 <- rep(1, n_j)
  y_j2 <- rnorm(n_j, mean = mu_j[2], sd = sqrt(var_j[2]))

  # Compute X and Y
  x <- c(x_j1, x_j2)
  y <- c(y_j1, y_j2)

  # Compute population mean
  mu_js <- c(mean(y_j1), mean(y_j2)) # mus group 1 and 2
  mu_y <- mean(y) # mu y

  # Generate data frame with simulated data
  data <- data.frame(id = 1:n, X = x, Y = y)

  # Eta-squared
  # Compute simulated eta-squared:
  eta2_sim <- n_j * sum( (mu_js - mu_y)^2 ) / sum( (y - mu_y)^2 )
  eta2_sim

  # Results
  return(list(data = data,
              eta2 = eta2_sim))

}

# # PENDING: generate_anova: extensions to j groups
# # Create population data based on j groups, n observations per group and differences among groups
# generate_anova <- function(difference, j, n_group, plot = F){
#
#   # Conditions
#   # j: number of groups
#   # n_group: number of observations per group
#   # difference: effect of the grouping variable
#
#   # Generate population data
#   group_means <- c(0, difference, difference*2) # group means based on population effect size
#   group_sd <- 1 # standard deviation within groups
#
#   # Groups
#   g1 <- rnorm(n_group, group_means[1], group_sd)
#   g2 <- rnorm(n_group, group_means[2], group_sd)
#   g3 <- rnorm(n_group, group_means[3], group_sd)
#   group <-  rep(c("1", "2", "3"), each = n_group)
#
#   # Population data
#   pop <- data.frame(id=1:(n_group*3), Y = c(g1, g2, g3), group = group)
#
#   # # Check computed values with effect size package
#   # ANOVA
#   anova_pop <- aov(formula = Y ~ group, data = pop)
#
#   # Eta squared
#   eta <- eta_squared(model = anova_pop, partial = F)$Eta2
#   # Omega squared
#   omega <- omega_squared(model = anova_pop, partial = F)$Omega2
#   # MSE and R2
#   mse <- mean( (pop$Y - anova_pop$fitted.values)^2 )
#   r2 <- rsq(y = pop$Y, yhat = anova_pop$fitted.values)
#
#   # Visualize data
#
#   if(plot){
#     boxplot(Y ~ group, data = pop, col="skyblue")
#   }
#
#   return(list(population = pop, eta2 = eta, omega2 = omega, r2 = r2, pop_mse = mse))
#
# }
