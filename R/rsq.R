#--------------------------------
# 2. rsq: regular or in-sample R2
#--------------------------------
# y: dependent variable
# yhat: predicted values
rsq <- function(y, yhat){

  # Total sum of squares
  TSS <- sum( ( y - mean(y) )^2 )

  # Residual sum of squares
  RSS <- sum( (y - yhat)^2 )

  # R2
  res <- (TSS - RSS) / TSS

  return(res)

}
