rm(list = ls()) # Clear work space
library(caret)

##reading the data
source("project2_b/read_data.R")
source("project2_b/train_neural_network.R")

library(neuralnet) 
library(MASS)

# Package for Cross-Validation
library(caret)



chosen_inner_model <- NA 
inner_MSE_min <- 0



NHiddenUnits <- c(3:8)

Error_per_hidden <- rep(NA, times = length(NHiddenUnits))
optimal_number_of_layer <- rep(NA, times = K)
# Variable for classification error

## loop over hidden layers  

test_error <- rep(NA, times = length(NHiddenUnits))
training_error <- rep(NA, times = length(NHiddenUnits))

X_tensor <- as.matrix(X)
y_tensor <- as.array(y)

## Constructig the model

for(h in NHiddenUnits){
  error_hidden_test <- 0
  error_hidden_train <- 0
  
  model <- function() {  
    nn_sequential(
      nn_linear(M, h),
      nn_tanh(),
      nn_linear(h, 1)
    )
  }
  loss_fn <- nn_mse_loss()

for (k in 1:K) { 

  # Extract training and test set
  X_train <- X_tensor[CV$which != k, ]
  y_train <- y_tensor[CV$which != k]
  X_test <- X_tensor[CV$which == k, ]
  y_test <- y_tensor[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  KK <- 5
  
  CV2 <- list()
  CV2$which <- createFolds(y_train, k = KK, list = F)
  CV2$TrainSize <- c()
  CV2$TestSize <- c()
  
  X_train_tensor <- torch_tensor(X_train)
  y_train_tensor <- torch_tensor(y_train)
  for (kk in 1:KK) {
    print(paste("Number of hidden units  ",h, "/", length(NHiddenUnits) + 2, sep = ""))
    print(paste("Crossvalidation fold ", k, "/", K, sep = ""))
    print(paste("Nested Cross Crossvalidation fold ", kk, "/", KK, sep = ""))
    X_train2 <- X_train_tensor[CV2$which != kk, ]
    y_train2 <- y_train_tensor[CV2$which != kk]
    X_test2 <- X_train_tensor[CV2$which == kk, ]
    y_test2 <- y_train_tensor[CV2$which == kk]
    
    
    
    CV2$TrainSize[kk] <- length(y_train)
    CV2$TestSize[kk] <- length(y_test2)
      #fit neural network to training set
      #result_model <- train_neural_net(model, loss_fn, as.matrix(X_train2), y_train2,
      #                                 max_iter = 10000, n_replicates = 3)
      
      result_model <- train_neural_net(model, loss_fn, as.matrix(X_train2), y_train2,
                                       max_iter = 5000, n_replicates = 2)
      
      
      prediction_result <- matrix(as.array(result_model$net(X_test2)))
      MSE_validate <- as.numeric(sum((y_test2 - prediction_result)^2)/length(y_test2))
      
      ##These if statement are to get the optimal parameter
      if (k == 1) {
        inner_MSE_min <- MSE_validate
        chosen_inner_model <- result_model
      }
      if (MSE_validate <= inner_MSE_min) {
        inner_MSE_min <- MSE_validate
        chosen_inner_model <- result_model
      }
    }
  
  prediction_model_train <- as.numeric(chosen_inner_model$net(X_train))
  prediction_model_test <- as.numeric(chosen_inner_model$net(X_test))
  error_hidden_test <- error_hidden_test + as.numeric(sum((y_test - prediction_model_test)^2)/length(y_test))
  error_hidden_train <- error_hidden_train + as.numeric(sum((y_train - prediction_model_train)^2)/length(y_train))

}
 
  training_error[h-2] <- error_hidden_train/K
  test_error[h-2] <- error_hidden_test/K
}



plot(prediction_model_test, y_test)


plot(c(1:length(NHiddenUnits)), log(training_error),
     xlab = "iteration", ylab = "log(Error)", col = "red",
     ylim = c(min(c(log(training_error),log(test_error))), max(c(log(training_error),log(test_error))))
)
lines(c(1:length(NHiddenUnits)), log(test_error), col = "black")
points(c(1:length(NHiddenUnits)), log(test_error), col = "black")
lines(c(1:length(NHiddenUnits)), log(training_error), col = "red")

legend("right", legend = c("test", "train"), col = c("black", "red"), lty = 1,
       cex=0.5)



