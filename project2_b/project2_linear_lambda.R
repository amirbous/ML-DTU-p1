## better choice of lambda

# Regularization


rm(list = ls()) # Clear work space
library(caret)

source("project2_b/read_data.R")

# Include an additional attribute corresponding to the offset
# X <- cbind(rep(1, N), X)
# M <- M[1, 1] + 1
# attributeNames <- c("offset", attributeNames)

# Cross-validation

# Create cross-validation partition for evaluation of performance of optimal model
K <- 10

# Set seed for reproducibility
set.seed(123)

CV <- list()
CV$which <- createFolds(y, k = K, list = F)

# Set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Values of lambda
lambda_tmp <- 10^((-10:-2)/8)

# Initialize variables

KK <- 10 # Inner loop
T <-  length(lambda_tmp)
temp <- rep(NA, M * T * KK)
w <- array(temp, c(M, T, KK))
Error_train2 <- matrix(rep(NA, times = T * KK), nrow = T)
Error_test2 <- matrix(rep(NA, times = T * KK), nrow = T)
lambda_opt <- rep(NA, K)
w_rlr <- matrix(rep(NA, times = M * K), nrow = M)
Error_train_rlr <- rep(NA, K)
Error_test_rlr <- rep(NA, K)
w_noreg <- matrix(rep(NA, times = M * K), nrow = M)
mu <- matrix(rep(NA, times = (M - 1) * K), nrow = K)
sigma <- matrix(rep(NA, times = (M - 1) * K), nrow = K)
Error_train <- rep(NA, K)
Error_test <- rep(NA, K)
Error_train_nofeatures <- rep(NA, K)
Error_test_nofeatures <- rep(NA, K)

for (k in 1:K) {
  paste("Crossvalidation fold ", k, "/", K, sep = "")
  
  # Extract the training and test set
  X_train <- X[CV$which != k, ]
  y_train <- y[CV$which != k]
  X_test <- X[CV$which == k, ]
  y_test <- y[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  # Use 10-fold cross-validation to estimate optimal value of lambda
  KK <- 10
  
  CV2 <- list()
  CV2$which <- createFolds(y_train, k = KK, list = F)
  CV2$TrainSize <- c()
  CV2$TestSize <- c()
  
  
  for (kk in 1:KK) {
    X_train2 <- X_train[CV2$which != kk, ]
    y_train2 <- y_train[CV2$which != kk]
    X_test2 <- X_train[CV2$which == kk, ]
    y_test2 <- y_train[CV2$which == kk]
    
    CV2$TrainSize[kk] <- length(y_train)
    CV2$TestSize[kk] <- length(y_test2)
    
    Xty2 <- t(X_train2) %*% y_train2
    XtX2 <- t(X_train2) %*% X_train2
    
    for (t in 1:length(lambda_tmp)) {
      # Learn parameter for current value of lambda for the given inner CV_fold
      lambdaI <- lambda_tmp[t] * diag(M)
      
      # Don't regularize bias
      lambdaI[1, 1] <- 0 
      
      w[, t, kk] <- solve(XtX2 + lambdaI) %*% Xty2
      
      # Evaluate training and test performance
      Error_train2[t, kk] <- sum((y_train2 - X_train2 %*% w[, t, kk])^2)
      Error_test2[t, kk] <- sum((y_test2 - X_test2 %*% w[, t, kk])^2)
    }
  }
  
  # Select optimal value of lambda
  ind_opt <- which.min(apply(Error_test2, 1, sum) / sum(CV2$TestSize))
  lambda_opt[k] <- lambda_tmp[ind_opt]
  
  # Assuming your data is already standardized and scaled, you don't need to standardize it again.
  # Simply copy the subsets.
  
  X_train2 <- X_train[CV2$which != kk, ]
  X_test2 <- X_train[CV2$which == kk, ]
  
  # Estimate w for the optimal value of lambda
  Xty <- t(X_train) %*% y_train
  XtX <- t(X_train) %*% X_train
  
  lambdaI <- lambda_opt[k] * diag(M)
  lambdaI[1, 1] <- 0 # don't regularize bias
  
  w_rlr[, k] <- solve(XtX + lambdaI) %*% Xty
  
  # evaluate training and test error performance for optimal selected value of lambda
  Error_train_rlr[k] <- sum((y_train - X_train %*% w_rlr[, k])^2)
  Error_test_rlr[k] <- sum((y_test - X_test %*% w_rlr[, k])^2)
  
  # Compute squared error without regularization
  # Adds a small value to diagonal to avoid a singular matrix
  w_noreg[, k] <- solve(XtX + (diag(M) * 1e-10)) %*% Xty
  Error_train[k] <- sum((y_train - X_train %*% w_noreg[, k])^2)
  Error_test[k] <- sum((y_test - X_test %*% w_noreg[, k])^2)
  
  # Compute squared error without using the input data at all
  Error_train_nofeatures[k] <- sum((y_train - mean(y_train))^2)
  Error_test_nofeatures[k] <- sum((y_test - mean(y_train))^2)
  
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
    plot(log(lambda_tmp), w_mean[2, ],
         xlab = "log(lambda)",
         ylab = "Coefficient Values", main = paste("Weights, fold ", k, "/", K),
         ylim = c(min(w_mean[-1, ]), max(w_mean[-1, ]))
    )
    
    
    colors_vector <- colors()[c(1, 50, 26, 59, 101, 126, 151, 551, 71, 257, 506, 634, 639, 383)]
    
    for (i in 3:M) {
      points(log(lambda_tmp), w_mean[i, ], col = rainbow(T)[i])
      lines(log(lambda_tmp), w_mean[i, ], col = rainbow(T)[i])
    }
    print(log(apply(Error_train2, 1, sum) / sum(CV2$TrainSize)))
    print(log(apply(Error_test2, 1, sum) / sum(CV2$TestSize)))
    plot(log(lambda_tmp), log(apply(Error_train2, 1, sum) / sum(CV2$TrainSize)),
         xlab = "log(lambda)", ylab = "log(Error)",
         main = paste0("Optimal lambda: 1e", log10(lambda_opt[k])),
         ylim = c((min(log(apply(Error_train2, 1, sum) / sum(CV2$TrainSize)))),
                     (max(log(apply(Error_train2, 1, sum) / sum(CV2$TrainSize))))
          )
     )
    
    lines(log(lambda_tmp), log(apply(Error_train2, 1, sum) / sum(CV2$TrainSize)))
    points(log(lambda_tmp), log(apply(Error_test2, 1, sum) / sum(CV2$TestSize)), col = "red")
    lines(log(lambda_tmp), log(apply(Error_test2, 1, sum) / sum(CV2$TestSize)), col = "red")
    
    legend("bottomright", legend = c("Training", "Test"), col = c("black", "red"), lty = 1,
           cex=0.5)
    
  }
}
# Display Results
# writeLines("Linear regression without feature selection:")
# writeLines(paste("- Training error: ", sum(Error_train) / sum(CV$TrainSize)))
# writeLines(paste("- Test error", sum(Error_test) / sum(CV$TestSize)))
# writeLines(paste("- R^2 train:     %8.2f\n", (sum(Error_train_nofeatures) - sum(Error_train)) / sum(Error_train_nofeatures)))
# writeLines(paste("- R^2 test:     %8.2f\n", (sum(Error_test_nofeatures) - sum(Error_test)) / sum(Error_test_nofeatures)))
# 
# writeLines("Regularized Linear regression:")
# writeLines(paste("- Training error:", sum(Error_train_rlr) / sum(CV$TrainSize)))
# writeLines(paste("- Test error:", sum(Error_test_rlr) / sum(CV$TestSize)))
# writeLines(paste("- R^2 train: ", (sum(Error_train_nofeatures) - sum(Error_train_rlr)) / sum(Error_train_nofeatures)))
# writeLines(paste("- R^2 test:", (sum(Error_test_nofeatures) - sum(Error_test_rlr)) / sum(Error_test_nofeatures)))
# 
# 
# writeLines("Weights in last fold :")
# for (m in 1:M) {
#   writeLines(paste(attributeNames[m], w_rlr[m, k]))
# }




