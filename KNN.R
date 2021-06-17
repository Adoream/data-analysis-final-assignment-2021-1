library(kknn)

adult.knn.numeric <- NULL
for (i in 1:50) {
  adult.knn.numeric.for <- kknn(income ~ ., train = adult.train, test = adult.test, k = i, distance = 2)
  adult.knn.numeric.for.pred <- fitted(adult.knn.numeric.for)
  adult.knn.numeric.for.pred.table <- table(adult.knn.numeric.for.pred, adult.test$income)
  adult.knn.numeric[i] <- sum(diag(adult.knn.numeric.for.pred.table)) / sum(adult.knn.numeric.for.pred.table)
}
rm(adult.knn.numeric.for, i)
ggplot(data.frame(1:50, adult.knn.numeric[1:50]), aes(x = 1:50, y = adult.knn.numeric[1:50])) + geom_line(size = 0.8)

adult.knn <- kknn(income ~ ., train = adult.train, test = adult.test, k = 50, distance = 2)
adult.knn.pred <- fitted(adult.knn)
adult.knn.pred.table <- table(adult.knn.pred, adult.test$income)
sum(diag(adult.knn.pred.table))/sum(adult.knn.pred.table)