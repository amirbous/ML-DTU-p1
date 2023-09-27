
amir_dir <-"/Users/amirbouslama/Documents/DTU_ml/ML-DTU-p1"
swati_di <-"/Users/swati/Desktop/MachineLearning/Project 1"

amir_data_path<- "resources/obesity_raw.csv"
swati_data_path <- "ObesityDataSet_raw_and_data_sinthetic.csv"


setwd(amir_dir)
data <- read.csv(amir_data_path)


# Load the required libraries
library(dplyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(ggpubr)
library(tidyverse)
library(gdata)
# Read the CSV file
#data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")

# View the data
View(data)
colnames(data)

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# Summary Statistics
# Checking summary of the data
summary(data)

# Converting all categorical variables to factors
data <- data %>%
  mutate(FAVC=as.factor(FAVC), # Frequent consumption of high caloric food
         CAEC=as.factor(CAEC), # Consumption of food between meals
         SMOKE=as.factor(SMOKE), # Smoking habit
         CALC=as.factor(CALC), # Consumption of alcohol
         SCC=as.factor(SCC), # Calories consumption monitoring
         MTRANS=as.factor(MTRANS),  # Transportation used
         Gender=as.factor(Gender), # Gender of the individual
         Obesity.Type=as.factor(NObeyesdad),
         FHWO=as.factor(family_history_with_overweight)) #Making a short form for it as well for convenience

# Checking summary of the data again
table(summary(data))
table(data$Gender, data$MTRANS)
table(data$Gender, data$CALC)
table(data$CAEC)
table(data$CALC)
table(data$MTRANS)
table(data$NObeyesdad)
table(data$Gender, data$FAVC)

# Measures of Centrality
# Calculate the mean for numeric variables
mean_values <- data %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

# Printing the result
mean_values

# Calculating the mode for categorical variables
mode_values <- data %>%
  summarise_if(is.factor, mode)

# Defining mode calculation function
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Calculating mode for factor columns in the data frame
mode_values <- data %>%
  summarise_if(is.factor, find_mode)

# Printing the result
mode_values

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

# Exploratory Data Analysis

# # Selecting continuous variables from the dataset
# continuous_vars <- data %>%
#   select(Age, Height, Weight, FCVC, NCP, FAF, TUE, CH2O)
# 
# # Calculating the correlation matrix
# correlation_matrix <- cor(continuous_vars)

# Calculating the correlation matrix for numeric variables
cor_matrix <- cor(data %>% select_if(is.numeric), use = "pairwise.complete.obs")

# Creating a heat map
heatmap(cor_matrix, 
        col = colorRampPalette(c("steelblue", "purple", "orange"))(100),
        main = "Correlation Heatmap of Continuous Variables",
        xlab = "Continuous Variables",
        ylab = "Continuous Variables",
        )
# PCA (Principal Component Analysis)
# Perform PCA on numeric variables
pca_result <- PCA(data %>% select_if(is.numeric), scale.unit = TRUE, graph = TRUE)

# PCA method 2
pca <- prcomp(data %>% select_if(is.numeric), scale=TRUE)
fviz_pca_biplot(pca, geom = "point", label = "var", col.var = "black", col.ind = data$NObeyesdad, repel = TRUE) + 
  ggtitle("PCA Biplot After Scaling of Continuous Variables")




## Normalisation (to normal distribution)

data$Age <- scale(data$Age) 
data$Height <- scale(data$Height)
data$Weight <- scale(data$Weight)


## binary attributes 

data$family_history_with_overweight <-ifelse(data$family_history_with_overweight=="yes",1,0)


data$FAVC <-ifelse(data$FAVC=="yes",1,0)
data$SCC <-ifelse(data$SCC=="yes",1,0)
data$SMOKE <-ifelse(data$SMOKE=="yes",1,0)
data$FHWO <-ifelse(data$FHWO=="yes",1,0)

unique(data$Gender)
data$Gender <- data$Gender <-ifelse(data$Gender=="Female",0,1)
## Metrics for rows with classes

#Alcohol consumption
unique(data$CALC)
fac <- factor(data$CALC, levels = c("no", "Sometimes", "Frequently", "Always"))
data$CALC <- as.numeric(fac) - 1

#means of transportation
unique(data$MTRANS)
fac <- factor(data$MTRANS, levels = c("Walking", "Bike", "Public_Transportation", "Motorbike", "Automobile"))
fac <- as.numeric(fac) - 1
data$MTRANS <- ifelse(fac >= 3, fac - 1, fac)

# food between meals
unique <- unique(data$CAEC)
fac <- factor(data$CAEC, levels = c("no", "Sometimes", "Frequently", "Always"))
data$CAEC <- as.numeric(fac) - 1
View(data)
## Probabilities and frequencies


hist(data$Age)
hist(data$Height)
hist(data$Weight)
barplot(table(data$Gender) / length(data$Gender))
barplot(table(data$family_history_with_overweight) / length(data$family_history_with_overweight))

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------


