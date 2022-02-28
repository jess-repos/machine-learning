# automatic backward elimination function

# backwardElimination <- function(x, sl) {
#   numVars = length(x)
#   for (i in c(1:numVars)){
#     regressor = lm(formula = Profit ~ ., data = x)
#     maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
#     if (maxVar > sl){
#       j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
#       x = x[, -j]
#     }
#     numVars = numVars - 1
#   }
#   return(summary(regressor))
# }
# 
# SL = 0.05
# dataset = dataset[, c(1,2,3,4,5)]
# backwardElimination(training_set, SL)

# installing packages

# install.packages('caTools')
# install.packages('ggplot2')

library(ggplot2)
library(caTools)

# importing dataset

dataset <- read.csv('50_Startups.csv')



# encoding categorical values (State)

states <- unique(dataset$State) # get all unique values on State variable

dataset$State <- factor(
  dataset$State, # select column to encode 
  levels = states, # what values to encode
  labels = c(1: length(states)) # replacement values(range 1 - length of states)
)

dataset
# splitting dataset into training set and test set

set.seed(123)
split <- sample.split(
  Y = dataset$Profit, # dependent variable
  SplitRatio = 0.8 # percentage of observations 80% for the training set 20% to the test set (recommended = 0.75)
)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

training_set
test_set

# fitting multiple linear regression to the training set

regressor <- lm(
  # other way to write <-  formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State
  formula = Profit ~ ., # '.' means all the independent variables
  data = training_set,
)

# show regressor summary (3 stars mean high statistical significance)
# regressor automatically create dummy variables for encoded categorical variables
# lower P-value has more impact to the dependent variable
summary(regressor)

# rewrite regressor using the using backward elimination method using the dataset as a whole

# 1. select significance level (0.05)
# 2. fit model with all possible predictors <-  formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State
regressor <- lm(
  formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
  data = dataset,
)
summary(regressor)
# 3. consider the predictor with the highest P-value (State)
# 4. remove the predictor (State)
# 5. fit the model without the variable <- formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
regressor <- lm(
  formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
  data = dataset,
)
summary(regressor)
# 6. consider the predictor with the highest P-value (Administration)
# 7. remove the predictor (Administration)
# 8. fit the model without the variable <- formula = Profit ~ R.D.Spend + Marketing.Spend,
regressor <- lm(
  formula = Profit ~ R.D.Spend + Marketing.Spend,
  data = dataset,
)
summary(regressor)
# 9. consider the predictor with the highest P-value (Marketing.Spend)
# 10. remove the predictor (Marketing.spend)
# 11. fit the model without the variable <- formula = Profit ~ R.D.Spend,
regressor <- lm(
  formula = Profit ~ R.D.Spend,
  data = dataset,
)
summary(regressor)
# 12. model is ready

# use the training set for the model demonstration

regressor <- lm(
  formula = Profit ~ R.D.Spend,
  data = dataset,
)
summary(regressor)

# predicting test set results

y_pred <- predict(
  regressor,
  newdata = test_set
)

y_pred
