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


NHiddenUnits <- c(1:10)
Generalised_test_err_hidden <- rep(NA, times = 10)
Generalised_train_err_hidden <- rep(NA, times = 10)

# Variable for classification error




## loop over hidden layers  

for (i in 1:length(NHiddenUnits)){
  test_error <- rep(NA, times = 10)
  training_error <- rep(NA, times = 10)
  model <- function() {  
    nn_sequential(
      nn_linear(M, i),
      nn_tanh(),
      nn_linear(i, 1),
      nn_sigmoid()
    )
  }
  loss_fn <- nn_bce_loss()
  
  ## Constructig the model
  for (k in 1:K) { 
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
      print(paste("Number of hidden units  ", i, "/", K, sep = ""))
      print(paste("Crossvalidation fold ", k, "/", K, sep = ""))
      print(paste("Nested Cross Crossvalidation fold ", kk, "/", KK, sep = ""))
      
      X_train2 <- X_train[CV2$which != kk, ]
      y_train2 <- y_train[CV2$which != kk]
      X_test2 <- X_train[CV2$which == kk, ]
      y_test2 <- y_train[CV2$which == kk]
      
      CV2$TrainSize[kk] <- length(y_train)
      CV2$TestSize[kk] <- length(y_test2)
    
      #tmp_df = data.frame(y_train2, X_train2[, 1], X_train2[, 2], X_train2[, 3], X_train2[, 4],
    #                      X_train2[, 5], X_train2[, 6],  X_train2[, 7], X_train2[, 8])
      #names(tmp_df) = c("target", "Gender", "Height", "Age", "SMOKE",
       #                 "MTRANS", "family_history_with_overweight",
      #                  "FAVC", "FCVC")
    
      #result_model <- neuralnet(target ~ Gender + Age + family_history_with_overweight + 
       #                     FAVC + FCVC + SMOKE + MTRANS, rep = 1,
      #                data = tmp_df, hidden = 2, stepmax=1e5, threshold=0.04, 
      #                lifesign = "full",
      #                linear.output = TRUE) 
      
    
     # prediction_validate <- predict(result, X_test2)
      
      result_model <- train_neural_net(model, loss_fn, X_train2, y_train2,
                                       max_iter = 10000, n_replicates = 3)
      
      X_test2 <- as.matrix(X_test2)
      prediction_result <- matrix(as.array(result_model$net(X_test2)))
      
      MSE_validate <- as.numeric(sum((y_test2 - prediction_result)^2)/length(y_test2))
      if (k == 1) {
        inner_MSE_min <- MSE_validate
        chosen_inner_model <- result_model
      }
      if (MSE_validate < inner_MSE_min) {
        inner_MSE_min <- MSE_validate
        chosen_inner_model <- result_model
      }
    }
    
    X_test <- as.matrix(X_train)
    X_train <- as.matrix(X_train)
    
    
    prediction_model_train <- matrix(as.array(result_model$net(X_train)))
    prediction_model_test <- matrix(as.array(result_model$net(X_test)))
    training_error[k] <- as.numeric(sum((y_train - prediction_model_train)^2)/length(y_train))
    test_error[k] <- as.numeric(sum((y_test - prediction_model_test)^2)/length(y_test))
  }
  Generalised_test_err_hidden[i] <- sum(test_error) / K
  Generalised_train_err_hidden[i] <- sum(training_error) / K
}


plot(c(1:10), log(Generalised_train_err_hidden),
     xlab = "iteration", ylab = "log(Error)", col = "red",
     ylim = c(min(log(Generalised_train_err_hidden)), max(log(Generalised_test_err_hidden))),
)
lines(c(1:10), log(Generalised_test_err_hidden), col = "black")
points(c(1:10), log(Generalised_test_err_hidden), col = "black")
lines(c(1:10), log(Generalised_train_err_hidden), col = "red")



legend("right", legend = c("Training", "Test"), col = c("black", "red"), lty = 1,
       cex=0.5)






