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

# rewrite regressor using the variable with the most statistical significance based on summary

regressor <- lm(
  formula = Profit ~ R.D.Spend, 
  data = training_set,
)
summary(regressor)

# predicting test set results

# create a new set from test set without the Profit variable(profit is what we are predicting)

new_set <- test_set[,1:4]
new_set

y_pred <- predict(
  regressor,
  newdata = new_set
)

y_pred

# visualizing training set results

ggplot() + 
  geom_point(
    aes(
      # dataset of what we are training
      x = training_set$R.D.Spend,  
      y = training_set$Profit
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      # use regression for training 
      x = training_set$R.D.Spend, 
      predict(regressor, newdata = training_set)
    ),
    colour = 'blue'
  ) + 
  ggtitle('R.D.Spend vs Profit (Training Set)') + 
  xlab('R.D.Spend') + 
  ylab('Profit')

# visualizing actual test set results

ggplot() + 
  geom_point(
    aes(
      # dataset of what we are training
      x = test_set$R.D.Spend,  
      y = test_set$Profit
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      # use regression for training 
      x = training_set$R.D.Spend, 
      predict(regressor, newdata = training_set)
    ),
    colour = 'blue'
  ) + 
  ggtitle('R.D.Spend vs Profit (Test Set)') + 
  xlab('R.D.Spend') + 
  ylab('Profit')

# visualizing predicted test set results from new set

ggplot() + 
  geom_point(
    aes(
      # dataset of what we are training
      x = test_set$R.D.Spend,  
      y = y_pred
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      # use regression for training 
      x = training_set$R.D.Spend, 
      predict(regressor, newdata = training_set)
    ),
    colour = 'blue'
  ) + 
  ggtitle('R.D.Spend vs Profit Prediction (New Set)') + 
  xlab('R.D.Spend') + 
  ylab('Profit')