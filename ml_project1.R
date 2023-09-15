###########################################################################
## set working directory

working_directory = "~/Documents/DTU_ml/ml_fall2023_project1"
setwd(working_directory)

## Read raw data

df <- read.table("resources/obesity_raw.csv", header=TRUE, sep=",", 
                 as.is=TRUE)
