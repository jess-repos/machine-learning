

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

# fitting simple linear regression into training set

regressor <- lm(
  formula = Salary ~ YearsExperience,
  data = training_set
)

# coefficient 3 stars = high statistical significance

summary(regressor)

# predicting the test set results

y_pred <- predict(regressor, newdata = test_set)

# visualization

# install.packages('ggplot2')
library(ggplot2)

# visualizing training set results

ggplot() + 
  geom_point(
    aes(
      # dataset of what we are training
      x = training_set$YearsExperience,  
      y = training_set$Salary
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      # use regression for training 
      x = training_set$YearsExperience, 
      predict(regressor, newdata = training_set)
    ),
    colour = 'blue'
  ) + 
  ggtitle('Salary vs Experience (Training Set)') + 
  xlab('Years of Experience') + 
  ylab('Salary')

# visualizing test set results

ggplot() + 
  geom_point(
    aes(
      # dataset of what we are predicting
      x = test_set$YearsExperience,  
      y = test_set$Salary
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      # use the regression learned when training the model
      x = training_set$YearsExperience,
      predict(regressor, newdata = training_set) 
    ),
    colour = 'blue'
  ) + 
  ggtitle('Salary vs Experience (Test Set)') + 
  xlab('Years of Experience') + 
  ylab('Salary')
