#------------------------------
# 3. rsq_cv: cross-validated R2
#------------------------------
# data: data set or sample where the model was fitted
# glmfit: fitted model
# k: number of subsets or folds
# balanced: generate balanced folds when working with categorical predictors
# J: number of groups
rsq_cv <- function(data, glmfit, k = 10, balanced = F, J = 2){


  ##### Training residual sum of squares, Tr_RSS #####
  y <- glmfit$y # observed
  yhat <- glmfit$fitted.values # predicted
  # Tr_RSS
  Tr_RSS <- sum( (y - yhat)^2 )


  ##### Cross-validation residual sum of squares, Cv_RSS #####
  # Sample size
  n <- nrow(data)

  # Leave-one-out, k = n
  # Regression shortcut
  if(k == n){

    # Leverage statistic for the ith observation
    H <- lm.influence(glmfit)$hat

    # Cross validated residual sum of squares, Cv_RSS
    Cv_RSS <- sum( ( residuals(glmfit) / (1 - H) )^2 )

  } else {

    # Split the data in k folds
    data_folds <- split_cv(data = data, k = k, balanced = balanced, J = J)

    # Compute cross-validated residual sum of squares in each fold / validation set;
    # Also, compute mixed cross-validated-training residual sum of squares, Cv_Tr_RSS
    # Cv_Tr_RSS is computed for the bias corrected version of cross-validation
    Cv_RSS_k <- numeric(k)
    Cv_Tr_RSS_k <- numeric(k)

    # Compute cross-validated R2 averaging over k folds
    R2_k <- numeric(k) # object to store R2 in each k fold

    for (i in 1:k){

      # Training and validation set
      train_set_k <- data_folds[data_folds$fold != i, ]; train_set_k$fold <- NULL
      val_set_k <- data_folds[data_folds$fold == i, ]; val_set_k$fold <- NULL

      # Fit the model in the training set
      fit_k <- glm(formula = as.formula(glmfit$formula), data = train_set_k, family = "gaussian")

      # Predictions on the k validation set: compute validation error
      y_val_k <- val_set_k[, as.character(glmfit$formula)[2]] # Observed
      yhat_val_k <- predict(fit_k, newdata = val_set_k) # Predicted
      yhat_valtrain_k <- predict(fit_k, newdata = data) # Predicted mixed cross-validation-training

      # Compute RSS in k validation set;
      # and mixed cross-validated-training RSS
      Cv_RSS_k[i] <- sum( (y_val_k - yhat_val_k)^2 )
      Cv_Tr_RSS_k[i] <- sum( (y - yhat_valtrain_k)^2 ) * ( length(y_val_k) / length(y) )

      # Compute R2 in fold k
      R2_k[i] <- rsq(y = y_val_k, yhat = yhat_val_k)

    }

    # Compute cross-validated residual sum of squares over all folds, Cv_RSS
    Cv_RSS <- sum(Cv_RSS_k)

    # Compute bias corrected cross-validated residual sum of squares over all folds, Cv_RSS_s
    Cv_Tr_RSS <- sum(Cv_Tr_RSS_k) # mixed cross-validated-training residual sum of squares
    # Cv_RSS_s
    Cv_RSS_s <- Cv_RSS + Tr_RSS - Cv_Tr_RSS

  }


  ##### R squared #####
  TSS <- sum( ( y - mean(y) )^2 ) # Total sum of squares
  MST_out <- ( (TSS / (n - 1)) * ((n + 1) / n) ) # Out-of-sample Mean Squared Total

  # Cross validated R2, R2_Cv
  MSE_Cv <- Cv_RSS / n # Cross-validated Mean Squared Error
  R2_Cv <- (MST_out - MSE_Cv) / MST_out

  if(k != n) {

    # Bias corrected cross validated R2, R2_Cvs
    MSE_Cv_s <- Cv_RSS_s / n # Corrected Cross-validated Mean Squared Error
    R2_Cv_s <- (MST_out - MSE_Cv_s) / MST_out

    # Cross validated R2 averaging over k folds, R2_ave
    R2_ave <- mean(R2_k)

  } else {

    # Bias corrected cross validated R2, R2_Cvs
    R2_Cv_s <- NA

    # Cross validated R2 averaging over k folds, R2_ave
    R2_ave <- NA

  }

  # Return results
  return(list(R2_Cv = R2_Cv,
              R2_Cv_s = R2_Cv_s,
              R2_ave = R2_ave,
              MSE_Cv = MSE_Cv))

}
