## machine learning fall 2023
# linear regression b

rm(list = ls())


##reading modified data

data <- read.csv(file.path("resources", "obesity_final.csv"), sep = ";", dec = ".")

head(data)

## selecting features for the linear regression model
## Attribute to predict is the weight of an individual



