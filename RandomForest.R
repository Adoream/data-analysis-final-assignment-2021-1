library(randomForest)

#test tree
adult.rf <- randomForest(income ~ ., data = adult.train, ntree = 1000)
adult.rf.pred.prob <- predict(adult.rf, select(adult.test, -income), type = 'prob')
adult.rf.pred <- predict(adult.rf, select(adult.test, -income), type = 'class')
adult.rf.pred.table <- table(adult.rf.pred, adult.test$income)
sum(diag(adult.rf.pred.table))/sum(adult.rf.pred.table)

#best number of mtry
n <- length(names(adult.train))
rate <- 1
for(i in 1:13){
  set.seed(2333)
  adult.rf.test <- randomForest(income ~ ., data = adult.train, mtry = i, ntree=500)
  rate[i]<-mean(adult.rf.test$err.rate)
}
rate

#best number of ntree
set.seed(2333)
adult.rf.test<-randomForest(income~., data=adult.train, mtry=3, ntree=1000)
plot(adult.rf.test)
legend(800,0.02, "IS_LIUSHI=0", cex=0.9, bty="n")    
legend(800,0.0245, "total", cex=0.09, bty="n")

#best tree
adult.rf <- randomForest(income ~ ., data = adult.train, mtry=3, ntree=300)
varImpPlot(adult.rf)
adult.rf.pred.prob <- predict(adult.rf, select(adult.test, -income), type = 'prob')
adult.rf.pred <- predict(adult.rf, select(adult.test, -income), type = 'class')
adult.rf.pred.table <- table(adult.rf.pred, adult.test$income)
sum(diag(adult.rf.pred.table))/sum(adult.rf.pred.table)