library(randomForest)

adult.rf <- randomForest(income ~ ., data = adult.train, ntree = 1000)
adult.rf.pred.prob <- predict(adult.rf, select(adult.test, -income), type = 'prob')
adult.rf.pred <- predict(adult.rf, select(adult.test, -income), type = 'class')
adult.rf.pred.table <- table(adult.rf.pred, adult.test$income)
sum(diag(adult.rf.pred.table))/sum(adult.rf.pred.table)
