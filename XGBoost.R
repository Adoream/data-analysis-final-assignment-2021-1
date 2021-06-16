library(Matrix)
library(xgboost)

adult.xgb.matrix.train <- sparse.model.matrix(income ~ ., data = adult.train)
adult.xgb.matrix.train.Y <- as.numeric(adult.train$income) - 1
adult.xgb.matrix.test <- sparse.model.matrix(income ~ ., data = adult.test)
adult.xgb <- xgboost(
  data = adult.xgb.matrix.train,
  label = adult.xgb.matrix.train.Y,
  max.depth = 4,
  eta = 0.5,
  lambda = 0.7,
  min_child_weight = 10,
  eval_metric = 'auc',
  nthread = 3,
  nround = 100,
  objective = "binary:logistic"
)
adult.xgb.pred.prob <- predict(adult.xgb, adult.xgb.matrix.test, type = 'prob')
adult.xgb.pred <- predict(adult.xgb, adult.xgb.matrix.test)
adult.xgb.pred.table <- table(ifelse(adult.xgb.pred > 0.5, 1, 0), as.numeric(na.omit(adult.test)$income))
sum(diag(adult.xgb.pred.table))/sum(adult.xgb.pred.table)

