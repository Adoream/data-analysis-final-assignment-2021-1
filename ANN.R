library(nnet)

adult.nn <- nnet(income ~ ., data = adult.train, size = 40, maxit = 500, MaxNWts = 2601)
adult.nn.pred <- predict(adult.nn, select(adult.test, -income), type = 'raw')
adult.nn.pred.tr <- rep('<=50K', length(adult.nn.pred))
adult.nn.pred.tr[adult.nn.pred.tr >= .5] <- '>50K'
adult.nn.pred.table <- table(adult.nn.pred.tr, adult.test$income)
sum(diag(adult.nn.pred.table))/sum(adult.nn.pred.table)
