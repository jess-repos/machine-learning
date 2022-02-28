# --- INDEX STARTS AS 1 ---

# Data Preprocessing

# installing packages

# install.packages('caTools')
library(caTools)

# importing dataset

dataset <- read.csv('Data.csv')
dataset

# taking care of missing data

dataset$Age <- ifelse(
  is.na(dataset$Age), # if there's no value
  ave(dataset$Age, # return mean of the column values
      FUN = function(x) mean(x, na.rm = TRUE)), 
  dataset$Age # else return the original column value
  )

dataset$Salary <- ifelse(
  is.na(dataset$Salary), # if there's no value
  ave(dataset$Salary, # return mean of the column values
      FUN = function(x) mean(x, na.rm = TRUE)), 
  dataset$Salary # else return the original column value
  )


# encoding categorical data
dataset$Country <- factor(
  dataset$Country, # select column to encode 
  levels = c('France', 'Spain', 'Germany'), # what values to encode
  labels = c(1,2,3) # replacement values
)

dataset$Purchased <- factor(
  dataset$Purchased, 
  levels = c('No', 'Yes'),
  labels = c(0, 1)
)

dataset

# splitting dataset into training set and test set

set.seed(123)
split <- sample.split(
  Y = dataset$Purchased, # dependent variable
  SplitRatio = 0.8 # percentage of observations 80% for the training set 20% to the test set
)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)
