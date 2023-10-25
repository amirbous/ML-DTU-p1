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

X <- data[, (names(data) %in% c("Age", "Height",
                                "family_history_with_overweight", "MTRANS"))]
X <- as.matrix(X)
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

T = 1
temp <- rep(NA, M * T * KK)
w <- array(temp, c(M, T, KK))
Error_train2 <- matrix(rep(NA, times = T * KK), nrow = T)
Error_test2 <- matrix(rep(NA, times = T * KK), nrow = T)
lambda_opt_inner_loop <- rep(NA, K)
w_rlr <- matrix(rep(NA, times = M * K), nrow = M)
Error_train_rlr <- rep(NA, K)
Error_test_rlr <- rep(NA, K)
mu <- matrix(rep(NA, times = (M - 1) * K), nrow = K)
sigma <- matrix(rep(NA, times = (M - 1) * K), nrow = K)
Error_train <- rep(NA, K)
Error_test <- rep(NA, K)

