#create outer folds

amir_dir <-"/Users/amirbouslama/Documents/DTU_ml/ML-DTU-p1"
swati_dir <-"/Users/swati/Desktop/MachineLearning/1/ML-DTU-p1/ML-DTU-p1"

amir_data_path<- "resources/obesity_final.csv"
swati_data_path <- "ObesityDataSet_raw_and_data_sinthetic.csv"


# setwd(amir_dir)
# data <- read.csv(amir_data_path)

setwd(amir_dir)
data <- read.csv(amir_data_path, sep = ";")
na.omit(data)

# Define the feature matrix X and the target variable y
X <- data[, (names(data) %in% c("Gender", "Height", "Age", "SMOKE",
                                "MTRANS", "family_history_with_overweight",
                                "FAVC", "FCVC"))]
X <- as.matrix(X)
y <- data$Weight
N <- nrow(data)  # Number of samples
M <- ncol(X)  # Number of features
attributeNames <- colnames(X)

set.seed(123)

K <- 5

CV <- list()
CV$which <- createFolds(y, k = K, list = F)

CV$TrainSize <- c()
CV$TestSize <- c()