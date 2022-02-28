# --- INDEX STARTS AS 1 ---

# Data Preprocessing


# importing dataset

dataset <- read.csv('Salary_Data.csv')

# splitting dataset into training set and test set

# install.packages('caTools')
library(caTools)

set.seed(123)
split <- sample.split(
  Y = dataset$Salary, # dependent variable
  SplitRatio = 2/3 # percentage of observations 2/3 for the training set 1/3 to the test set (recommended = 0.75)
)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)