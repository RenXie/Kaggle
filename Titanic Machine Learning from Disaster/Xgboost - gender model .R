library(caTools)
library(xgboost)
library(caret)


train_data <- cbind(x_train,y_train)
train_data <- as.data.frame(train_data)


set.seed(123)
split = sample.split(train_data$y_train, SplitRatio = 0.8)
training_set = subset(train_data, split == TRUE)
validation_set = subset(train_data, split == FALSE)

# Fitting XGBoost to the Training set
# install.packages('xgboost')
classifier = xgboost(data = as.matrix(training_set[-40]), label = training_set$y_train, nrounds = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(validation_set[-40]))
y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix
cm = table(validation_set[, 40], y_pred)
cm

# Applying k-Fold Cross Validation
folds = createFolds(training_set$y_train, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-40]), label = training_set$y_train, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-40]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 40], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))


#Submission
x_test <- as.data.frame(x_test)
test_result = predict(classifier, newdata = as.matrix(x_test))
test_result = (test_result >= 0.5)
Survival <- as.integer(test_result)
submission <- read.csv('gender_submission.csv')
submission$Survived <- Survival
write.csv(submission, 'Submission.csv')



