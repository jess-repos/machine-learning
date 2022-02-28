# importing libraries

library(caTools)
library(ggplot2)

# importing dataset

dataset <- read.csv('Salary.csv')

str(dataset)

# splitting dataset into training and test set

set.seed(123)
split <- sample.split(
  Y = dataset$Salary,
  SplitRatio = 2/3
)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)


# fitting linear regression into training set

regressor <- lm(
  formula = Salary ~ YearsExperience,
  data = training_set
)

summary(regressor)

# predicting the test set results

prediction <- predict(regressor, newdata = test_set)

prediction

# visualizing training set results

ggplot() + 
  geom_point(
    aes(
      x = training_set$YearsExperience,  
      y = training_set$Salary
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      x = training_set$YearsExperience, 
      predict(regressor, newdata = training_set)
    ),
    colour = 'blue'
  ) + 
  ggtitle('Salary vs Years of Experience (Training Set)') + 
  xlab('Years of Experience') + 
  ylab('Salary')

# visualizing test set results

ggplot() + 
  geom_point(
    aes(
      x = test_set$YearsExperience,  
      y = test_set$Salary
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      x = training_set$YearsExperience,
      predict(regressor, newdata = training_set) 
    ),
    colour = 'blue'
  ) + 
  ggtitle('Salary vs Experience (Test Set)') + 
  xlab('Years of Experience') + 
  ylab('Salary')


# visualizing test prediction results

ggplot() + 
  geom_point(
    aes(
      x = test_set$YearsExperience,  
      y = prediction
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      x = training_set$YearsExperience,
      predict(regressor, newdata = training_set) 
    ),
    colour = 'blue'
  ) + 
  ggtitle('Salary vs Experience (Test Set)') + 
  xlab('Years of Experience') + 
  ylab('Salary')
