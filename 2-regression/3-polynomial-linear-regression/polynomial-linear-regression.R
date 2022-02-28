# polynomial linear regression

# importing libraries

library(ggplot2)

# importing dataset

dataset <- read.csv('Position_Salaries.csv')

str(dataset)

# keeping only necessary columns 

dataset <- dataset[2:3]

str(dataset)

# polynomial regression doesn't require feature scaling and splitting into 
# training set and test set for this example

# comparing to other linear regression models

# fitting linear regression to the dataset

lin_reg <- lm(
  formula = Salary ~ .,
  data = dataset
)

summary(lin_reg)

# fitting polynomial regression to the dataset

# create new column with squared/cubed values of Level column

dataset$Level2 <- dataset$Level ^ 2
dataset$Level3 <- dataset$Level ^ 3
dataset$Level4 <- dataset$Level ^ 4
dataset$Level5 <- dataset$Level ^ 5
dataset$Level6 <- dataset$Level ^ 6

poly_reg <- lm(
  formula = Salary ~ .,
  data = dataset,
  
)

summary(poly_reg)

# predicting/visualizing linear regression results

lin_pred <- predict(lin_reg, newdata = dataset)

ggplot() +
  geom_point(
    aes(
      x = dataset$Level,
      y = dataset$Salary
    ),
    colour = 'red' 
  ) + 
  geom_line(
    aes(
      x = dataset$Level,
      y = lin_pred
    ),
    colour = 'blue' 
  ) + 
  ggtitle('Salary Prediction by Level (Linear Regression)') +
  xlab('Levels') +
  ylab('Salary')

# predicting/visualizing polynomial regression results

poly_pred <- predict(poly_reg, newdata = dataset)

ggplot() +
  geom_point(
    aes(
      x = dataset$Level,
      y = dataset$Salary
    ),
    colour = 'red' 
  ) + 
  geom_line(
    aes(
      x = dataset$Level,
      y = poly_pred
    ),
    colour = 'blue' 
  ) + 
  ggtitle('Salary Prediction by Level (Polynomial Regression)') +
  xlab('Levels') +
  ylab('Salary')

# predicting a new result with linear regression

l_pred <- predict(
  lin_reg, newdata = data.frame(Level = 6.5)
)

l_pred

# predicting a new result with polynomial regression

p_pred <- predict(
  poly_reg, 
  newdata = data.frame(
    Level = 6.5, 
    Level2 = 6.5 ^ 2, 
    Level3 = 6.5 ^ 3, 
    Level4 = 6.5 ^ 4, 
    Level5 = 6.5 ^ 5, 
    Level6 = 6.5 ^ 6
  )
)

p_pred