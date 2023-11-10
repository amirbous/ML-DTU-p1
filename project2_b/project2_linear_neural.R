rm(list = ls()) # Clear work space
library(caret)

source("project2_b/read_data.R")
source('project2_b/train_neural_network.R')
source("project2_b/dbplot.R")


# install.packages("torch")
# install.packages("torch")
library(neuralnet) 
library(MASS)

# Package for Cross-Validation
library(caret)


NHiddenUnits <- 1

Error <- rep(NA, times = K)
Outer_model <- rep(NA, times = K)

model <- function() { 
  nn_sequential(
    nn_linear(M, NHiddenUnits),
    nn_tanh(),
    nn_linear(NHiddenUnits, 1),
    nn_sigmoid()
  )
}

loss_fn <- nn_bce_loss()


# For each cross-validation fold
for (k in 1:K) { 
  print(paste("Crossvalidation fold ", k, "/", K, sep = ""))
  
  # Extract training and test set
  X_train <- X[CV$which != k, ]
  y_train <- y[CV$which != k]
  X_test <- X[CV$which == k, ]
  y_test <- y[CV$which == k]
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  
  # Build Neural Network 

  
  ##inner protocol for constructing the model
  tmp_df = data.frame(y_train, X_train[, 1], X_train[, 2], X_train[, 3], X_train[, 4],
                      X_train[, 5], X_train[, 6],  X_train[, 7], X_train[, 8])
  names(tmp_df) = c("target", "Gender", "Height", "Age", "SMOKE",
                    "MTRANS", "family_history_with_overweight",
                    "FAVC", "FCVC")
  result_model <- neuralnet(target ~ Gender + Age + family_history_with_overweight + 
                        FAVC + FCVC + SMOKE + MTRANS,
                  data = tmp_df, hidden = 3, 
                  linear.output = TRUE) 
  
  
  
  prediction_model <- predict(result_model, X_test)
  
  MSE <- sum((y_test - prediction_model)^2)/length(y_test)
  
}

