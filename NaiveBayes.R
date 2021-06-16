library(e1071)

adult.nb <- naiveBayes(x = adult.train[, 1:11], y = adult.train$income, decision.values = TRUE)
adult.nb.pred <- predict(adult.nb, select(adult.test, -income))
adult.nb.pred.table <- table(adult.nb.pred, adult.test$income)
sum(diag(adult.nb.pred.table))/sum(adult.nb.pred.table)