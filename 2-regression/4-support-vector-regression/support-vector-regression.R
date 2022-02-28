# support vector regression

# importing libraries

# svm support vector model
# install.packages('e1071')
library(e1071)

library(ggplot2)

# importing dataset

dataset <- read.csv('Position_Salaries.csv')

str(dataset)

# keeping only necessary columns 

dataset <- dataset[2:3]

str(dataset)

# not splitting into training set and test set for this example

# set.seed(123)
# split <- sample.split(
#   Y = dataset$Variable, # dependent variable
#   SplitRatio = 0.8 # percentage of observations 80% for the training set 20% to the test set
# )
# 
# training_set <- subset(dataset, split == TRUE)
# test_set <- subset(dataset, split == FALSE)

# feature scaling
# most libraries don't require us to apply it manually

# training_set[,2:3] <- scale(training_set[,2:3])
# test_set[,2:3] <- scale(test_set[,2:3])

# fitting SVR to the dataset

# choose eps-regression type
regressor <- svm(
  formula = Salary ~ .,
  data = dataset,
  type = 'eps-regression'
  
)

summary(regressor)

# predicting a new result

y_pred <- predict(
  regressor, 
  data.frame(Level = 6.5)
)

y_pred

# predicting/visualizing svr model results

prediction <- predict(regressor, newdata = dataset)

ggplot() +
  geom_point(
    aes(
      x = dataset$Level, # independent variable
      y = dataset$Salary # dependent variable
    ),
    colour = 'red' 
  ) + 
  geom_line(
    aes(
      x = dataset$Level,
      y = prediction
    ),
    colour = 'blue' 
  ) + 
  ggtitle('Salary Prediction (SVR)') +
  xlab('Leevel') +
  ylab('Salary')
