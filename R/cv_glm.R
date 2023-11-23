#----------------------------------------------------------------------------------------------
# 4. cv_glm: estimates the prediction error via cross validation for a fitted generalized model
#----------------------------------------------------------------------------------------------
# PENDING. Make a more general version?
# data: data set were the model was fitted
# glmfit: fitted model, class glm()
# k: number of subsets or folds
# cutoff: cutoff point for computing classification in logistic regression. By default, cutoff is set to 0.5
# cost: cost function
cv_glm <- function(data, glmfit, k = 2, rpcv = 1, cost = "mse"){

  # Control
  if(k <= 1 | k > nrow(data)) return(cat("Error: k must take a value between 2 and n (sample size)"))
  if (class(glmfit)[1] != "glm") return(cat("Error: model has to be fitted as a generalized linear model: glm()"))
  if(!is.null(seed)) set.seed(seed)

  # Cost function
  if (cost == "mse") cost_f = function(y, yhat) mean((y - yhat)^2)
  if (cost == "rss") cost_f = function(y, yhat) sum((y - yhat)^2)
  if (cost == "deviance") cost_f = function(y, yhat) -2*(sum(y*log(yhat) + (1-y)*log(1-yhat)))
  if (cost == "logloss") cost_f = function(y, yhat) -1/length(y)*(sum(y*log(yhat) + (1-y)*log(1-yhat)))
  if (cost == "h.rate") cost_f = function(y, yhat) mean(abs(y-yhat) < cutoff)

  # Leave-one-out regression shortcut
  if(k==nrow(data) & glmfit$family$family == "gaussian"){
    h <- lm.influence(glmfit)$hat
    cv_error <- mean((residuals(glmfit)/(1-h))^2)
    return(list(err_cv=cv_error))
  }

  # First step: separate data in k folds
  fold_data <- split_cv(data=data, k=k)

  cv_rep <- numeric(rpcv)
  for (rp in 1:rpcv){

    # Second step: compute validation error
    Call <- glmfit$call
    cv_error <- numeric(k)
    for (i in 1:k){
      # Training and validation set
      train_set <- fold_data[fold_data$fold!=i, ]
      val_set <- fold_data[fold_data$fold==i, ]
      # Fit the model in the train set
      Call$data <- train_set
      glm_k <- eval(Call, envir=env)
      # Predictions on the validation set: compute validation error
      cost_i <- cost_f(val_set$Y, predict(glm_k,
                                          val_set, type="response"))
      cv_error[i] <- cost_i
      # if (cost == "deviance") val_error[i] <- cost_i*k else val_error[i] <- cost_i
    }

    # Compute estimated test error each k-folds cv
    cv_error <- sum(cv_error)/k
    cv_rep[rp] <- cv_error

  }
  # Compute final estimated test error
  cv_error <- mean(cv_rep)

  # if (cost == "deviance") val_error <- sum(val_error) else

  return(list(err_cv=cv_error))

}
