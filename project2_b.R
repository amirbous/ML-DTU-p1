## machine learning fall 2023
# linear regression b

rm(list = ls())

##library for cross validation

library(caret)

##reading modified data

data <- read.csv(file.path("resources", "obesity_final.csv"), sep = ";", dec = ".")

colnames(data)

## selecting features for the linear regression model
## Attribute to predict is the weight of an individual


# (Age, Height) -> (Weight)

tmp_x <- data[, (names(data) %in% c("Age", "Height",
                                "family_history_with_overweight", "MTRANS"))]
tmp_x <- as.matrix(tmp_x)
X <- cbind(rep(1, length(data$Age)), tmp_x)
y <- as.numeric(data$Weight)

N <- nrow(data)  # Number of samples
M <- ncol(X)  # Number of features
attributeNames <- colnames(X)


K <- 10
set.seed(4280)

CV <- list()
CV$which <- createFolds(y, k = K, list = F)

CV$TrainSize <- c()
CV$TestSize <- c()


KK <- 10
T <- M + 1
## Model 1, simple linear regression

# Initialize variables


temp <- rep(NA, M * T * KK)
w <- array(temp, c(M, T, KK))
Error_train2 <- matrix(rep(NA, times = T * KK), nrow = T)
Error_test2 <- matrix(rep(NA, times = T * KK), nrow = T)

w_rlr <- matrix(rep(NA, times = M * K), nrow = M)
Error_train_rlr <- rep(NA, K)
Error_test_rlr <- rep(NA, K)
w_noreg <- matrix(rep(NA, times = M * K), nrow = M)
mu <- matrix(rep(NA, times = (M - 1) * K), nrow = K)
sigma <- matrix(rep(NA, times = (M - 1) * K), nrow = K)
Error_train <- rep(NA, K)
Error_test <- rep(NA, K)

for (k in 1:K) {
  paste("Crossvalidation fold ", k, "/", K, sep = "")
  
  # Extract the training and test set
  X_train <- X[CV$which != k, ]
  y_train <- y[CV$which != k]
  X_test <- X[CV$which == k, ]
  y_test <- y[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  # Use 10-fold cross-validation to get the best liner regression model
  KK <- 10
  
  CV2 <- list()
  CV2$which <- createFolds(y_train, k = KK, list = F)
  CV2$TrainSize <- c()
  CV2$ValidateSize <- c()
  
  
  for (kk in 1:KK) {
    X_train_inner <- X_train[CV2$which != kk, ]
    y_train_inner <- y_train[CV2$which != kk]
    X_validate <- X_train[CV2$which == kk, ]
    y_validate <- y_train[CV2$which == kk]
    
    CV2$TrainSize[kk] <- length(y_train)
    CV2$ValidateSize[kk] <- length(y_validate)
    
    Xty_inner <- t(X_train_inner) %*% y_train_inner
    XtX_inner <- t(X_train_inner) %*% X_train_inner
    
  }
  
  # Define lambda for testing
  #lambda <- 0.5
  
  # Assuming your data is already standardized and scaled, you don't need to standardize it again.
  # Simply copy the subsets.
  
  X_train_inner <- X_train[CV2$which != kk, ]
  X_test_inner <- X_train[CV2$which == kk, ]
  
  # Estimate w for the optimal value of lambda
  Xty <- t(X_train) %*% y_train
  XtX <- t(X_train) %*% X_train

  
  w_rlr[, k] <- solve(XtX) %*% Xty
  
  # evaluate training and test error performance for optimal selected value of lambda
  Error_train_rlr[k] <- sum((y_train - X_train %*% w_rlr[, k])^2)
  Error_test_rlr[k] <- sum((y_test - X_test %*% w_rlr[, k])^2)
  
  # Compute squared error without regularization
  # Adds a small value to diagonal to avoid a singular matrix
  w_noreg[, k] <- solve(XtX + (diag(M) * 1e-10)) %*% Xty
  Error_train[k] <- sum((y_train - X_train %*% w_noreg[, k])^2)
  Error_test[k] <- sum((y_test - X_test %*% w_noreg[, k])^2)
  
  # Compute squared error without using the input data at all
  #Error_train_nofeatures[k] <- sum((y_train - mean(y_train))^2)
  #Error_test_nofeatures[k] <- sum((y_test - mean(y_train))^2)
  
  if (k == K) {
    dev.new()
    # Display result for cross-validation fold
    w_mean <- apply(w, c(1, 2), mean)
    
    # Plot weights as a function of the regularization strength (not offset)
    par(mfrow = c(1, 2))
    par(cex.main = 1.5) # Define size of title
    par(cex.lab = 1) # Define size of axis labels
    par(cex.axis = 1) # Define size of axis labels
    par(mar = c(5, 4, 3, 1) + .1) # Increase margin size to allow for larger axis labels

  
    
  }
}

# Display Results
writeLines("Linear regression without feature selection:")
writeLines(paste("- Training error: ", sum(Error_train) / sum(CV$TrainSize)))
writeLines(paste("- Test error", sum(Error_test) / sum(CV$TestSize)))
writeLines(paste("- R^2 train:     %8.2f\n", (sum(Error_train_nofeatures) - sum(Error_train)) / sum(Error_train_nofeatures)))
writeLines(paste("- R^2 test:     %8.2f\n", (sum(Error_test_nofeatures) - sum(Error_test)) / sum(Error_test_nofeatures)))

writeLines("Regularized Linear regression:")
writeLines(paste("- Training error:", sum(Error_train_rlr) / sum(CV$TrainSize)))
writeLines(paste("- Test error:", sum(Error_test_rlr) / sum(CV$TestSize)))
writeLines(paste("- R^2 train: ", (sum(Error_train_nofeatures) - sum(Error_train_rlr)) / sum(Error_train_nofeatures)))
writeLines(paste("- R^2 test:", (sum(Error_test_nofeatures) - sum(Error_test_rlr)) / sum(Error_test_nofeatures)))


writeLines("Weights in last fold :")
for (m in 1:M) {
  writeLines(paste(attributeNames[m], w_rlr[m, k]))
}


