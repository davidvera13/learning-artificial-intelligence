# Data preprocessing
# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# training set and test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# fitting simple linear regression to the training set
regressor = lm(
  formula = Salary ~ YearsExperience,
  data = training_set)

# allow to show information, including p value that shows correlation strength between 
# dependent and independent variable
summary(regressor)

# predicting the test set results
y_pred = predict(regressor, newdata = test_set)


# showing training set results
# install.packages('ggplot2')
library(ggplot2)

# Visualizing the Training set results
#ggplot() +
#  geom_point(
#    aes(x = training_set$YearsExperience, y = training_set$Salary),
#    colour = 'red') +
#  geom_line(
#    aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
#    colour = 'blue') +
#  ggtitle('Salary vs Experience (Training set)') +
#  xlab('Years of experience') +
#  ylab('Salary')

# Visualizing the Test set results: line will be the same as the training set graph... 
#ggplot() +
#  geom_point(
#    aes(x = test_set$YearsExperience, y = test_set$Salary),
#    colour = 'red') +
#  geom_line(
#    aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
#    colour = 'blue') +
#  ggtitle('Salary vs Experience (Test set)') +
#  xlab('Years of experience') +
#  ylab('Salary')

# Visualizing the Training set results
# using geom smooth...
ggplot() +
  geom_point(
    aes(x = training_set$YearsExperience, y = training_set$Salary),
    colour = 'red') +
  geom_line(
    aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
    colour = 'blue') +
  geom_smooth(
    data= training_set, 
    aes(x=YearsExperience, y= Salary), 
    method = "lm") +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualizing the Test set results: line will be the same as the training set graph... 
# using geom smooth...
ggplot() +
  geom_point(
    aes(x = test_set$YearsExperience, y = test_set$Salary),
    colour = 'red') +
  geom_line(
    aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
    colour = 'blue') +
  geom_smooth(
    data= training_set, 
    aes(x=YearsExperience, y= Salary), 
    method = "lm") +  
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')