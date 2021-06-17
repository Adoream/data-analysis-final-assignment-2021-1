library(lightgbm)

adult.lgb.data <- as.data.frame(sapply(adult.data, as.numeric))
adult.lgb.data.matrix <- sparse.model.matrix(income ~ .-1, data = adult.lgb.data)
adult.lgb.data.matrix.train <- adult.lgb.data.matrix[adult.index, ]
adult.lgb.data.matrix.test <- adult.lgb.data.matrix[-adult.index, ]
adult.lgb.d.train <- lgb.Dataset(adult.lgb.data.matrix.train, label = select(adult.lgb.data[adult.index, ], income))

adult.lgb <- lgb.train(
  data = adult.lgb.d.train,
  objective = "binary", 
  nrounds = 300,
  num_leaves = 512,
  learning_rate = 0.1, 
  verbose = 0
)

adult.lgb.pred <- predict(adult.lgb, adult.lgb.data.matrix.test)
adult.lgb.pred.table <- table(adult.lgb.pred, adult.test$income)
sum(diag(adult.lgb.pred.table))/sum(adult.lgb.pred.table)
