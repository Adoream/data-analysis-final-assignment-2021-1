library(MASS)

#create model and pred
adult.lda <- lda(income ~ ., data = adult.data)
summary(adult.lda)
adult.lda.pred <- predict(adult.lda,adult.test)
adult.lda.pred.table <- table(adult.test$income, adult.lda.pred$class)
adult.lda.pred.table
sum(diag(adult.lda.pred.table))/sum(adult.lda.pred.table)
