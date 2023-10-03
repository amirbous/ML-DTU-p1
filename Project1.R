
amir_dir <-"/Users/amirbouslama/Documents/DTU_ml/ML-DTU-p1"
swati_dir <-"/Users/swati/Desktop/MachineLearning/Project 1"

amir_data_path<- "resources/obesity_raw.csv"
swati_data_path <- "ObesityDataSet_raw_and_data_sinthetic.csv"


# setwd(amir_dir)
# data <- read.csv(amir_data_path)

setwd(swati_dir)
data <- read.csv(swati_data_path)

# Checking for missing values
which(is.na(data))

# Load the required libraries
library(knitr)
library(dplyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(ggpubr)
library(tidyverse)
library(gdata)
library(gridExtra)
library(RColorBrewer)

# Read the CSV file
data <- read.csv("ObesityDataSet_raw_and_data_sinthetic.csv")

# View the data
# View(data)
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
         FHWO=as.factor(family_history_with_overweight)) %>% #Making a short form for it as well for convenience
  select(-c(NObeyesdad, family_history_with_overweight))

# Checking summary of the data again
table(summary(data))
table(data$Gender, data$MTRANS)
table(data$Gender, data$CALC)
table(data$CAEC)
table(data$CALC)
table(data$MTRANS)
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

# Initial analysis for discrepancies in obesity levels using BMI calculation based assignment
colnames(data)

# Adding a new column for BMI by using height and weight values
data$BMI <- data$Weight / (data$Height^2)

# Analyzing obesity levels based on BMI
# Defining aand assigning BMI categories to each record
data <- data %>%
  mutate(BMI.Status = case_when(BMI < 18.5 ~ "Insufficient_Weight",
                                18.5 <= BMI & BMI < 25.0 ~ "Normal_Weight",
                                25.0 <= BMI & BMI < 30.0 ~ "Overweight",
                                30.0 <= BMI & BMI < 35.0 ~ "Obesity_Type_I",
                                35.0 <= BMI & BMI < 40 ~ "Obesity_Type_II",
                                35.0 <= BMI ~ "Obesity_Type_III"))


# Define a mapping function for comparison of pre-assigned obesity and empirically estimated BMI levels
map_obesity_type_to_bmi_status <- function(obesity_type, bmi_status) {
  ifelse(obesity_type %in% c("Overweight_Level_I", "Overweight_Level_II"), "Overweight", as.character(obesity_type))
}

# Creating adjusted bmi status based on the matches
data$Adjusted_BMI_Status <- map_obesity_type_to_bmi_status(data$Obesity.Type, data$BMI.Status)
table(data$Adjusted_BMI_Status)

# Filtering out all the records where BMI.Status and Adjust.BMI.Status do not match
filtered_data <- data %>%
  filter(BMI.Status != Adjusted_BMI_Status)

head(filtered_data)
colnames(filtered_data)

filtered_data_2 <- filtered_data %>% select(Obesity.Type, BMI.Status, BMI, Adjusted_BMI_Status)
kable(list(filtered_data_2), format = "rst")

list(filtered_data_2)

# Checking for missing values
which(is.na(data))

# Convert BMI.Status and Adjusted BMI status as factors
data <- data %>%
  mutate(Adjusted_BMI_Status = as.factor(Adjusted_BMI_Status),
         BMI.Status = as.factor(BMI.Status))

# Identify records where Adjusted BMI status doesn't match BMI Status
discrepancy_indices <- which(data$BMI.Status != data$Adjusted_BMI_Status)

# Update Obesity Type for records with discrepancies
data$Obesity.Type[discrepancy_indices] <- data$BMI.Status[discrepancy_indices]

# Checking for missing values
which(is.na(data))

# Checking the new assignment of obesity types
table(data$Obesity.Type)

# View(data %>% filter(Obesity.Type == "Overweight" | Obesity.Type == "Overweight_Level_II"))

# Adjusted BMI status, BMI, and BMI.status columns are no longer needed so removing them
data <- data %>%
  select(-c(Adjusted_BMI_Status))


# Correcting the sequence of obesity levels in factor
data$Obesity.Type <- factor(data$Obesity.Type, 
                            levels = c("Insufficient_Weight", "Normal_Weight", 
                                       "Overweight_Level_I", "Overweight_Level_II", 
                                       "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))


# Correcting the sequence of obesity levels in factor
data$BMI.Status <- factor(data$BMI.Status, 
                            levels = c("Insufficient_Weight", "Normal_Weight", 
                                       "Overweight", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III"))

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

# Descriptive Statistics

# Global Aesthetics
gen_col <- c("Male" = "grey", "Female" = "#990000")
par(cex=1.5)

# Extract demographics only from the data
demographics <- data %>%
  select("Gender", "Age", "Height", "Weight")

head(demographics)

# Visualizing weight distribution by Gender using histogram
demo_plot_gender <- ggplot(demographics, aes(x = Weight, fill = Gender)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Weight distribution by Gender", x = "Weight (kg)", y = "Frequency", size=0.5) +
  scale_fill_manual(values = c("Male" = "grey", "Female" = "#990000")) +
  theme_minimal()

# Add theme to the plot
demo_plot_gender + theme(
  plot.title = element_text(size = 16, face = "bold"),
  axis.title.x = element_text(size = 14, face = "bold"),
  axis.title.y = element_text(size = 14, face = "bold"),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 14, face = "bold"))

# Visualizing weight distribution by Age and Gender using boxplots

# Create age groups (you can customize the breaks and labels as needed)
demographics$Age_Group <- cut(demographics$Age, breaks = c(0, 18, 30, 40, 50, 60, 100), labels = c("0-18", "19-30", "31-40", "41-50", "51-60", "61+"))

# Plot the weight distribution by age groups and gender
demo_plot_age <- ggplot(demographics, aes(x = Age_Group, y = Weight, fill = Gender)) +
  geom_boxplot() +  # Using boxplots to show distribution
  labs(title = "Weight distribution by Age Group and Gender", x = "Age Group", y = "Weight (kg)") +
  scale_fill_manual(values = c("Male" = "grey", "Female" = "#990000")) +
  theme_minimal()

# Add theme to the plot
demo_plot_age + theme(
  plot.title = element_text(size = 16, face = "bold"),
  axis.title.x = element_text(size = 14, face = "bold"),
  axis.title.y = element_text(size = 14, face = "bold"),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 12),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 14, face = "bold"))

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

## Normalisation of matrix (to ensure normal distribution)

data$Age <- scale(data$Age) 
data$Height <- scale(data$Height)
data$Weight <- scale(data$Weight)

## Binary attributes
data$FHWO <-ifelse(data$FHWO=="yes",1,0)
data$FAVC <-ifelse(data$FAVC=="yes",1,0)
data$SCC <-ifelse(data$SCC=="yes",1,0)
data$SMOKE <-ifelse(data$SMOKE=="yes",1,0)

unique(data$Gender)
data$Gender <- data$Gender <-ifelse(data$Gender=="Female",0,1)

## Metrics for rows with classes

# Alcohol consumption
unique(data$CALC)
fac <- factor(data$CALC, levels = c("no", "Sometimes", "Frequently", "Always"))
data$CALC <- as.numeric(fac) - 1

# Means of transportation
unique(data$MTRANS)
fac <- factor(data$MTRANS, levels = c("Walking", "Bike", "Public_Transportation", "Motorbike", "Automobile"))
fac <- as.numeric(fac) - 1
data$MTRANS <- ifelse(fac >= 3, fac - 1, fac)

# Food between meals
unique(data$CAEC)
fac <- factor(data$CAEC, levels = c("no", "Sometimes", "Frequently", "Always"))
data$CAEC <- as.numeric(fac) - 1

## Probabilities and frequencies after normalisation

# Setting the canvas for histograms
# par(mfrow = c(3, 1), cex=1)

par(cex=1.5)

# Histograms for distribution of age, height and weight variables
hist(data$Age, col = "#990000", border="white", main="Age distribution after normalisation",
     xlab="Age (years)")

hist(data$Height, col = "#990000", border="white", main="Height distribution after normalisation",
     xlab="Height (meters)")


hist(data$Weight, col = "#990000", border="white", main="Weight distribution after normalisation",
     xlab="Weight (kg)")


# Setting the canvas for barplots
par(mfrow = c(1, 2), cex=1)

# Create barplots for distribution of gender and family history of obesity
barplot(table(data$Gender) / length(data$Gender), col = c("#990000", "grey"), border="white", 
        main="Gender distribution in normalized data",
        sub="0 means females, 1 means males")

barplot(table(data$FHWO) / length(data$FHWO), col = c("grey", "#990000"), border="white", 
        main="Family history of obesity: distribution in normalized data",
        sub="1 means family history of obesity, 0 mean no such family history")

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

# Exploratory Data Analysis

# Calculating the correlation matrix for numeric variables
cor_matrix <- cor(data %>% select_if(is.numeric), use = "pairwise.complete.obs")

# Creating a heat map
heatmap(cor_matrix, 
        col = colorRampPalette(c("white", "#990000", "black"))(100),
        main = "Correlation Heatmap of Continuous Variables",
        cexRow=1.25,
        cexCol=1.25)

# Principal Component Analysis (PCA)
pca <- prcomp(data %>% select_if(is.numeric) %>% select(-c(BMI)), scale=FALSE)

# Define a palette with seven distinct colors
# display.brewer.all()
custom_palette <- brewer.pal(7, "YlGnBu")

# PCA visualisation
biplot1 <- fviz_pca_biplot(pca, geom.ind = "point", geom.var=c("text", "arrow"), col.var = "#990000", 
                palette = custom_palette, col.ind = as.factor(data$BMI.Status), repel = TRUE, 
                title = "PCA Biplot")

biplot1 + theme(text = element_text(size = 14))


biplot2 <- fviz_pca_var(pca, geom = c("point", "text"), label = "var", col.var = "#990000", 
                        repel = TRUE, title = "PCA Variables Plot")

biplot2 + theme(text = element_text(size = 14))

# Arrange the plots side by side
grid.arrange(biplot1, biplot2, ncol = 2)

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
