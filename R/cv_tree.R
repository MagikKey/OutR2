# 6. 2. cv_tree: Prediction error estimates via cross-validation in decision trees
# data: data set were the tree model was fitted
# fit: fitted tree using the tree package
# k: number of subsets or folds
cv_tree <- function(data, fit, k = k, cost = "mse"){

  # Control
  if(k <= 1 | k > nrow(data)) return(cat("Error: k must take a value between 2 and n (sample size)"))

  # Cost function
  if (cost == "mse") cost_f = function(y, yhat) mean((y - yhat)^2)

  # Call
  Call <- fit$call

  # First step: separate data in k folds
  fold_data <- split_cv(data = data, k = k)

  # Second step: compute validation error
  cv_error <- numeric(k)
  for (i in 1:k){

    # Training and validation set
    train_set <- fold_data[fold_data$fold != i, ]
    val_set <- fold_data[fold_data$fold == i, ]

    # Fit the model in the train set
    Call$data <- train_set[,colnames(train_set)!="fold"]
    fit_k <- eval(Call)
    # Predictions on the validation set: compute validation error
    cost_k <- cost_f(y = val_set$Y, yhat = predict(fit_k, val_set[,colnames(train_set)!="fold"]))
    cv_error[i] <- cost_k
  }

  # Compute estimated test error each k-folds cv
  cv_error <- sum(cv_error) / k

  # Return
  return(cv_error = cv_error)

}
