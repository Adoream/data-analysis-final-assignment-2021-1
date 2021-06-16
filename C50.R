library(C50)

adult.c50 <- C5.0(income ~ ., data = adult.train,  trials = 10)
adult.c50.pred.prob <- predict(adult.c50, select(adult.test, -income), type = 'prob')
adult.c50.pred <- predict(adult.c50, select(adult.test, -income), type = 'class')
adult.c50.pred.table <- table(adult.c50.pred, adult.test$income)
sum(diag(adult.c50.pred.table))/sum(adult.c50.pred.table)