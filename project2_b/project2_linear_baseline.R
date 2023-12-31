# Clear work space
library(caret)
rm(list = ls()) # Clear work space
##reading the data
source("project2_b/read_data.R")

library(MASS)

# Package for Cross-Validation
library(caret)

chosen_inner_model <- 0
inner_MSE_min <- 0

test_error_baseline <- rep(NA, times = K)
training_error_baseline <- rep(NA, times = K)
  
  ## Constructig the model
  for (k in 1:K) { 
    # Extract training and test set
    y_train <- y[CV$which != k]
    y_test <- y[CV$which == k]
    CV$TrainSize[k] <- length(y_train)
    CV$TestSize[k] <- length(y_test)
  
    KK <- 10
    
    CV2 <- list()
    CV2$which <- createFolds(y_train, k = KK, list = F)
    CV2$TrainSize <- c()
    CV2$TestSize <- c()
    
    for (kk in 1:KK) {
      print(paste("Crossvalidation fold ", k, "/", K, sep = ""))
      print(paste("Nested Cross Crossvalidation fold ", kk, "/", KK, sep = ""))
      
      y_train2 <- y_train[CV2$which != kk]
      y_test2 <- y_train[CV2$which == kk]
      
      CV2$TrainSize[kk] <- length(y_train)
      CV2$TestSize[kk] <- length(y_test2)
      
     result_model <- mean(y_train2) 

      MSE_validate <- as.numeric(sum((y_test2 - result_model)^2)/length(y_test2))
      if (k == 1) {
        inner_MSE_min <- MSE_validate
        chosen_inner_model <- result_model
      }
      if (MSE_validate < inner_MSE_min) {
        inner_MSE_min <- MSE_validate
        chosen_inner_model <- result_model
      }
    }
    training_error_baseline[k] <- as.numeric(sum((y_train - chosen_inner_model)^2)/length(y_train))
    test_error_baseline[k] <- as.numeric(sum((y_test - chosen_inner_model)^2)/length(y_test))
  }

plot(c(1:K), log(training_error_baseline),
     xlab = "iteration", ylab = "log(Error)", col = "red",
     ylim = c(min(c(log(training_error_baseline),log(test_error_baseline))), max(c(log(training_error_baseline),log(test_error_baseline)))
))
lines(c(1:K), log(test_error_baseline), col = "black")
points(c(1:K), log(test_error_baseline), col = "black")
lines(c(1:K), log(training_error_baseline), col = "red")

legend("right", legend = c("Test", "Train"), col = c("black", "red"), lty = 1,
       cex=0.5)
