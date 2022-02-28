# installing packages

library(caTools)
library(ggplot2)

# inporting dataset

dataset <- read.csv('ths_data.csv')

str(dataset)


# splitting dataset into training set and test set

set.seed(321654987)
split <- sample.split(
  dataset$humidity,
  SplitRatio = 0.9
)

training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# building the model (backward elimination)

# automatic backward elimination function

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = humidity ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}
 
significance_level = 0.05
backwardElimination(training_set, significance_level)

regressor <- lm(
  formula = humidity ~ temperature,
  data = training_set
)

summary(regressor)

y_pred <- predict(
  regressor, 
  newdata = test_set
)

cor(dataset$humidity,  dataset$temperature)

ggplot() + 
  geom_point(
    aes(
      # dataset of what we are visualizing
      x = test_set$temperature,  
      y = test_set$humidity
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      # use regression from training 
      x = training_set$temperature, 
      predict(regressor, newdata = training_set)
    ),
    colour = 'blue'
  ) + 
  ggtitle('temperature vs humidity (Test Set)') + 
  xlab('temperature') + 
  ylab('humidity')

ggplot() + 
  geom_point(
    aes(
      # dataset of what we are visualizing
      x = test_set$temperature,  
      y = y_pred
    ),
    colour = 'red'
  ) + 
  geom_line(
    aes(
      # use regression from training 
      x = training_set$temperature, 
      predict(regressor, newdata = training_set)
    ),
    colour = 'blue'
  ) + 
  ggtitle('temperature vs humidity (Predicted Values)') + 
  xlab('temperature') + 
  ylab('humidity')
