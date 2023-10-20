#---------------------------------------
# 1. split_cv: split data into k subsets
#---------------------------------------
# data: data set to be splitted into k-subsets
# k: number of subsets or folds
# J: groups
# balanced: generates k-subsets balanced for all J groups. Currently prepared for J = 2.
split_cv <- function(data, k = 10, J = 2, balanced = F){

  # Control
  if(k <= 1 | k > nrow(data)) return(cat("Error: k must take a value between 2 and n (sample size)"))

  # Create folds data
  n <- nrow(data)

  if(k == n) {

    folds <- 1:n

  } else if(balanced) {

    # Generate folds balanced by group
    Rm <- rep(1:k,  length.out = (n %% (k * J))) # Remaining
    Rm_J <- sample( cut(1:length(Rm), breaks = J, labels = F) ) # Remaining for each group

    folds <- c()
    for(i in 1:J){

      # Add folds per J
      if(length(Rm) == 0){

        folds_J <- sample( rep(1:k, each = (n %/% (k * J))), replace = F)

      } else {

        folds_J <- sample( c(rep(1:k, each = (n %/% (k * J))),
                             Rm[Rm_J == i]), replace = F)

      }

      folds <- c(folds, folds_J)

    }

  } else {

    # Generate k random folds
    folds <- sample( cut(1:n, breaks = k, labels = F) ) # Random sampling of folds

  }

  # Add fold column to data
  data$fold <- folds

  # Ordered data by fold
  data_folds <- data[order(data$fold),]

  return(data_folds = data_folds)

}
