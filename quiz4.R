## question 1

library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)

vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)

set.seed(33833)

q1rf = train(y ~ ., method = 'rf', data = vowel.train)
q1gbm = train(y ~ ., method = 'gbm', data = vowel.train)

pred_rf = predict(q1rf, vowel.test)
pred_gbm = predict(q1gbm, vowel.test)

mean(vowel.test$y == pred_rf)
mean(vowel.test$y  == pred_gbm)

confusionMatrix(pred_rf, vowel.test$y)$overall[1] # 0.5930736
confusionMatrix(pred_gbm, vowel.test$y)$overall[1] # 0.512987

q1combine = as.data.frame(cbind(pred_rf, pred_gbm, vowel.test$y))
q1combine$pred_match = ifelse(q1combine$pred_rf == q1combine$pred_gbm, 1, 0)
q1combine$pred_match_correct = ifelse(q1combine$pred_match == 1 & q1combine$pred_rf == q1combine$V3, 1, 0)

sum(q1combine$pred_match_correct)/sum(q1combine$pred_match)


## question 2

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

q2rf = train(diagnosis ~ ., method = 'rf', data = training)
q2gbm = train(diagnosis ~ ., method = 'gbm', data = training)
q2lda = train(diagnosis ~ ., method = 'lda', data = training)

q2_pred_rf = predict(q2rf, testing)
q2_pred_gbm = predict(q2gbm, testing)
q2_pred_lda = predict(q2lda, testing)

confusionMatrix(q2_pred_rf, testing$diagnosis)$overall[1] # 0.7926829
confusionMatrix(q2_pred_gbm, testing$diagnosis)$overall[1] # 0.7804878
confusionMatrix(q2_pred_lda, testing$diagnosis)$overall[1] # 0.7682927

stacked = data.frame(testing$diagnosis, q2_pred_rf, q2_pred_gbm, q2_pred_lda)

q2rfstack = train(testing.diagnosis ~ ., method = 'rf', data = stacked)
stackpred = predict(q2rfstack, testing)

confusionMatrix(stackpred, testing$diagnosis)$overall[1] # 0.804878

# question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

lasso = train(CompressiveStrength ~ ., data = training, method = 'lasso')
library(elasticnet)
plot.enet(lasso$finalModel, xvar = "penalty", use.color = TRUE)
# concrete is the last variable to be lasso'd

# question 4
library(lubridate) # For year() function below
dat = read.csv("./gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)
ts_model = bats(tstrain)
pred_fc = forecast(ts_model, level = 95, h = nrow(testing))

mean(pred_fc$lower < testing$visitsTumblr & testing$visitsTumblr < pred_fc$upper)
#0.9617021

# question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
mod_svm <- svm(CompressiveStrength ~ ., data = training)
pred_svm <- predict(mod_svm, testing)
accuracy(pred_svm, testing$CompressiveStrength)
