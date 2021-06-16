library(e1071)

adult.svm <- svm(income ~ ., data = adult.train, decision.values = TRUE)
adult.svm.pred.prob <- attributes(predict(adult.svm, select(adult.test, -income), decision.values = TRUE))$decision.values
adult.svm.pred <- predict(adult.svm, select(adult.test, -income))
adult.svm.pred.table <- table(adult.svm.pred, adult.test$income)
sum(diag(adult.svm.pred.table))/sum(adult.svm.pred.table)