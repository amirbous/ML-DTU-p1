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
  
  tmp_df = data.frame(c("target", y_train), c("Gender", X_train[, 1]), 
                      c("Age", X_train[, 2]), c("family_history_with_overweight", X_train[, 3]),
                      c("FAVC", X_train[, 4]), c("FCVC", X_train[, 5]), c("SMOKE", X_train[, 5]),
                      c("MTRANS", X_train[, 6])
  )
  result <- neuralnet(target ~ Gender + Age + family_history_with_overweight + 
                        FAVC + FCVC + SMOKE + MTRANS,
                  data = tmp_df, hidden = 1, 
                  linear.output = TRUE) 
  
  print(result)
  print(paste("Best loss:", result$final_loss))
  
  # Predict model on test data
  y_sigmoid = result$net(X_test)
  y_test_est <- as.integer(y_sigmoid > 0.5)
  
  # Compute error rate
  Error[k] <- sum(as.integer(y_test) != y_test_est)
}

# Print the error rate
print(paste("Error rate: ", sum(Error) / sum(CV$TestSize) * 100, "%", sep = ""))

# Display the decision boundary (given for last cross-validation fold)
predictionFunction <- function(X_train, net) {
  X_train <- as.matrix(X_train)
  probs <- matrix(as.array(net(X_train)), nrow = sqrt(dim(X_train)[1]), byrow = FALSE)
  probs
}

dbplot(X, attributeNames, predictionFunction, y = y,
       contourLevels = 0.5, contourCols = "white", net = result$net)
