## machine learning fall 2023
# linear regression b

rm(list = ls())

##library for cross validation

library(caret)
library(pracma)


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

#all_w <- array(temp, c(M, T, KK))



ws_final <- matrix(rep(NA, times = K * M), nrow = M)
error_train_final <- c()
error_test_final <- c()
Error_train_rlr <- rep(NA, K)
Error_test_rlr <- rep(NA, K)
w_noreg <- matrix(rep(NA, times = M * K), nrow = M)


y_validate <- c()
y_train_inner <- c()
X_train_inner <- matrix(rep(NA, times = K * M), nrow = M)

X_validate <- matrix(rep(NA, times = K * M), nrow = M)


## constructing the model:
# Standard linear regression

# lambdas = 

for (k in 1:K) {
  paste("Crossvalidation fold ", k, "/", K, sep = "")
  MSE_min <- 0
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
  
  ## find omega vector that gives the min 
  
  X_train_default <- X_train[CV2$which != 1,]
  y_train_default <- y_train[CV2$which != 1]
  X_validate_default <- X_train[CV2$which == 1, ]
  y_validate_default <- y_train[CV2$which == 1]
  
  Xty_default <- t(X_train_default) %*% y_train_default
  XtX_default <- t(X_train_default) %*% X_train_default
  
  w_optimal <- solve(XtX_default) %*% Xty_default
  error_vector <- c()
  for (kx in 1:(length(y_validate_default))){
    
    error_vector[kx] = y_validate_default[kx] - dot(w_optimal, X_validate_default[kx, ])
    
  }
  
  MSE_min <- (sum(error_vector^2))/length(y_validate_default)
  
  
  ## extracting the best model in the inner loop
  
  for (kk in 1:KK) {
    
    ## in this inner loop, the goal is 
    y_validate <- y_train[CV2$which == kk]
    X_train_inner <- X_train[CV2$which != kk, ]
    y_train_inner <- y_train[CV2$which != kk]
    X_validate <- X_train[CV2$which == kk, ]
    
    
    CV2$TrainSize[kk] <- length(y_train_inner)
    CV2$ValidateSize[kk] <- length(y_validate)
    
    Xty_inner <- t(X_train_inner) %*% y_train_inner
    XtX_inner <- t(X_train_inner) %*% X_train_inner
    
    w_curr <- solve(XtX_inner) %*% Xty_inner
    error_curr <- c()
    for (kx in 1:(length(X_validate[, 1]))){
      error_curr <- append(error_curr, y_validate[kx] - dot(w_curr, X_validate[kx, ]))
    }
    MSE_curr <- (sum(error_curr^2))/length(y_validate)
    
    if (MSE_curr <= MSE_min) {
      MSE_min <- MSE_curr
      w_optimal <- w_curr
    }
  }
  ws_final[, k] <- w_optimal 
  error_train_final[k] <- MSE_min
  error_test_final[k] <- sum((y_test - X_test %*% ws_final[, k])^2)/length(y_test)
  
  
  
  
  # Assuming your data is already standardized and scaled, you don't need to standardize it again.
  # Simply copy the subsets.
  
  # Estimate w for the optimal value of lambda
  
  
  
  
  
  # Compute squared error without regularization
  # Adds a small value to diagonal to avoid a singular matrix
  #w_noreg[, k] <- solve(XtX + (diag(M) * 1e-10)) %*% Xty
  
  # Compute squared error without using the input data at all
  #Error_train_nofeatures[k] <- sum((y_train - mean(y_train))^2)
  #Error_test_nofeatures[k] <- sum((y_test - mean(y_train))^2)
  
  #if (k == K) {
  # dev.new()
  # Display result for cross-validation fold
  #w_mean <- apply(w, c(1, 2), mean)
  
  # Plot weights as a function of the regularization strength (not offset)
  #par(mfrow = c(1, 2))
  #par(cex.main = 1.5) # Define size of title
  #par(cex.lab = 1) # Define size of axis labels
  #par(cex.axis = 1) # Define size of axis labels
  #par(mar = c(5, 4, 3, 1) + .1) # Increase margin size to allow for larger axis labels
  
  
  
  # }
  
  writeLines("Weights in last fold :")
  for (m in 1:M) {
    writeLines(paste(attributeNames[m], ws_final[m, k]))
  }
  
  
}

plot(c(1:10), error_train_final, type = "o", pch = 19,
     ylim = c(0,1), col = "red", xlab = "fold number")
lines(c(1:10), error_test_final, type = "o", pch = 19, col = "blue")

# Display Results




