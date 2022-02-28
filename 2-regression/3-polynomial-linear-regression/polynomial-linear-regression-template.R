# polynomial linear regression template

# importing libraries

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

# fitting regression to the dataset

# create new column with squared/cubed values of Level column

dataset$Level2 <- dataset$Level ^ 2
dataset$Level3 <- dataset$Level ^ 3
dataset$Level4 <- dataset$Level ^ 4
dataset$Level5 <- dataset$Level ^ 5
dataset$Level6 <- dataset$Level ^ 6

regressor <- lm(
  formula = Salary ~ .,
  data = dataset,
  
)

summary(regressor)

# predicting a new result

p_pred <- predict(
  regressor, 
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

# predicting/visualizing regression model results

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
  ggtitle('Title (Regression Model)') +
  xlab('X Label') +
  ylab('Y Label')

# improving visualization (smoother curve)

# incrementing sequence from 1.0 to 10.0 by 0.1 [1.0, 1.1, 1.2, 1.3 ...]

x_grid <- seq(min(dataset$Level), max(dataset$Level), 0.1)

new_data <- data.frame(
  Level = x_grid,
  Level2 = x_grid ^ 2,
  Level3 = x_grid ^ 3,
  Level4 = x_grid ^ 4,
  Level5 = x_grid ^ 5,
  Level6 = x_grid ^ 6
)
  
seq_pred <- predict(regressor, newdata = mew_data)

new_data$Salary <- seq_pred

new_data[c('Level', 'Salary')]

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
      x = x_grid,
      y = seq_pred
    ),
    colour = 'blue' 
  ) + 
  ggtitle('Title (Regression Model)') +
  xlab('X Label') +
  ylab('Y Label')

