rm(list = ls()) # Clear work space
library(caret)

##reading the data
source("project2_b/read_data.R")


library(neuralnet) 
library(MASS)

# Package for Cross-Validation
library(caret)


chosen_inner_model <- NA 
inner_MSE_min <- 0
training_error <- rep(NA, times = 10)
test_error <- rep(NA, times = 10)


## Constructig the model
for (k in 1:K) { 
  print(paste("Crossvalidation fold ", k, "/", K, sep = ""))
  chosen_mode <- NA
  # Extract training and test set
  X_train <- X[CV$which != k, ]
  y_train <- y[CV$which != k]
  X_test <- X[CV$which == k, ]
  y_test <- y[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  

  
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

    
    
  
  
    tmp_df = data.frame(y_train2, X_train2[, 1], X_train2[, 2], X_train2[, 3], X_train2[, 4],
                        X_train2[, 5], X_train2[, 6],  X_train2[, 7], X_train2[, 8])
    names(tmp_df) = c("target", "Gender", "Height", "Age", "SMOKE",
                      "MTRANS", "family_history_with_overweight",
                      "FAVC", "FCVC")
  
    result_model <- neuralnet(target ~ Gender + Age + family_history_with_overweight + 
                          FAVC + FCVC + SMOKE + MTRANS, rep = 1,
                    data = tmp_df, hidden = 2, stepmax=1e5, threshold=0.04, 
                    lifesign = "full",
                    linear.output = TRUE) 
    
  
    prediction_validate <- predict(result_model, X_test2)
    
    MSE_validate <- sum((y_test2 - prediction_validate)^2)/length(y_test2)
    if (k == 1) {
      inner_MSE_min <- MSE_validate
      chosen_inner_model <- result_model
    }
    if (MSE_validate < inner_MSE_min) {
      inner_MSE_min <- MSE_validate
      chosen_inner_model <- result_model
    }
  }
  prediction_model_train <- predict(chosen_inner_model, X_train)
  prediction_model_test <- predict(chosen_inner_model, X_test)
  training_error[k] <- sum((y_train - prediction_model_train)^2)/length(y_train)
  test_error[k] <- sum((y_test - prediction_model_test)^2)/length(y_test)
  
}

plot(c(1:10), log(training_error),
     xlab = "iteration", ylab = "log(Error)", col = "black",
     main = paste0("Optimal lambda: 1e", log10(lambda_opt[k])),
     ylim = c((min(log(apply(Error_train2, 1, sum) / sum(CV2$TrainSize)))),(
       max(log(apply(Error_test2, 1, sum) / sum(CV2$TestSize))))
     )
)
lines(c(1:10), log(testing_error), col = "red")
legend("bottomright", legend = c("Training", "Test"), col = c("black", "red"), lty = 1,
       cex=0.5)



## Model




