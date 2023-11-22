#--------------------------------------------------------------------------------------------
# 5. boot_glm: estimates the prediction error via bootstraping for a fitted generalized model
#--------------------------------------------------------------------------------------------
# PENDING, make it more general?
# data: data set were the model was fitted
# glmfit: fitted model, class glm()
# k: number of bootstrap samples
# cutoff: cutoff point for computing classification in logistic regression. By default, cutoff is set to 0.5
# cost: cost function
boot_glm <- function(data, glmfit, kboot = 100, cost = "mse"){

  # Control
  if (class(glmfit)[1] != "glm") return(cat("Error: model has to be fitted as a generalized linear model: glm()"))
  if(!is.null(seed)) set.seed(seed)

  # Cost function
  if (cost == "mse"){
    cost_f = function(y, yhat) mean((y - yhat)^2)
    cost_fi = function(y, yhat) (y - yhat)^2
  }
  if (cost == "rss") cost_f = function(y, yhat) sum((y - yhat)^2)
  if (cost == "deviance") cost_f = function(y, yhat) -2*sum(y*log(yhat) + (1-y)*log(1-yhat))
  if (cost == "h.rate") cost_f = function(y, yhat) mean(abs(y-yhat) < cutoff)

  # Model evaluated
  Call <- glmfit$call # glm model

  # Index data for resampling
  nb <- nrow(data)
  ind <- 1:nb

  # Store prediction error estimations
  # Train error
  tr_error <- cost_f(data$Y,
                     predict(glmfit, data, type = "response"))

  # Optimistic bootstrap
  bt_error <- numeric(kboot)
  # Leave-one-out bootstrap
  rr_loob <- matrix(NA, nrow=nb, ncol=kboot)

  for (b in 1:kboot){
    # bth bootstrap sample and validation set
    # Index for boot sample: train set observations
    b_in <-  sample(ind, nb, replace=T)
    # Observations not included in boot sample
    b_val <- ind[!(ind %in% unique(b_in))]

    # Fit the model in boot sample
    Call$data <- data[b_in,]
    glm_b <- eval(Call, envir=env)

    # Optimistic bootstrap
    # Original train data/sample acts as validation set
    # Predictions on the validation set: compute boot error
    bt_error[b] <- cost_f(data$Y, predict(glm_b,
                                          data, type = "response"))
    # Loo bootstrap
    # Compute error in observations not included in bootstrap sample
    rr_loob[b_val, b] <- cost_fi(data[b_val,]$Y,
                                 predict(glm_b, data[b_val,], type = "response"))
  }
  # Optimistic bootstrap
  boot_error <- mean(bt_error)

  # Loo bootstrap
  # Average prediction error on the ith observation with all boot samples
  rr_ib <- apply(rr_loob, 1, mean, na.rm=T)[!is.nan(apply(rr_loob, 1, mean, na.rm=T))]
  # Average prediction error through all observations
  Err1 <- mean(rr_ib)

  # 0.632 bootstrap
  b.632 <- 0.368*tr_error + 0.632*Err1

  # 0.632+ bootstrap
  y <- data$Y
  fx <- glmfit$fitted.values
  # Compute gamma based on no-information error rate
  ga_df <- NULL
  for(i in 1:nb){
    y <- c(y[-1], y[1])
    ga_df <- rbind(ga_df, cbind(fx,y))
  }
  ga <- cost_f(ga_df[,2], ga_df[,1])

  # Compute R
  # Control
  Err1_ <- min(Err1, ga)
  if(Err1_ > tr_error & ga > tr_error){
    R <- (Err1_-tr_error)/(ga-tr_error)
  } else {
    R <- 0
  }

  # Compute omega
  w <- 0.632/(1-(0.368*R))
  b.632p <- (1-w)*tr_error + w*Err1

  return(list(op_boot = boot_error,
              loo_boot = Err1,
              b.632 = b.632,
              b.632plus = b.632p))

}
