library(rpart)
library(rpart.plot)

adult.cart <- rpart(income ~ ., data = adult.train, method = 'class')
rpart.plot(adult.cart)
adult.cart.pred.prob <- predict(adult.rpart, select(adult.test, -income), type = 'prob')
adult.cart.pred <- predict(adult.rpart, select(adult.test, -income), type = 'class')
adult.cart.pred.table <- table(adult.rpart.pred, adult.test$income)
sum(diag(adult.cart.pred.table))/sum(adult.cart.pred.table)