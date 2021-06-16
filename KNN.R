library(kknn)

adult.knn <- kknn(income ~ ., train = adult.train, test = adult.test, k = 7, distance = 2)
adult.knn.pred <- fitted(adult.knn)
adult.knn.pred.table <- table(adult.knn.pred, adult.test$income)
sum(diag(adult.knn.pred.table))/sum(adult.knn.pred.table)