library(e1071)

adult.svm.linear <- svm(income ~ ., data = adult.train, decision.values = TRUE, kernel = 'linear')
adult.svm.linear.pred.prob <- attributes(predict(adult.svm.linear, select(adult.test, -income), decision.values = TRUE))$decision.values
adult.svm.linear.pr <- prediction(adult.svm.linear.pred.prob, adult.test$income)
adult.svm.linear.prf <- performance(adult.svm.linear.pr, measure = "tpr", x.measure = "fpr")
adult.svm.linear.dd <- data.frame(FP = adult.svm.linear.prf@x.values[[1]], TP = adult.svm.linear.prf@y.values[[1]])

adult.svm.polynomial <- svm(income ~ ., data = adult.train, decision.values = TRUE, kernel = 'polynomial')
adult.svm.polynomial.pred.prob <- attributes(predict(adult.svm.polynomial, select(adult.test, -income), decision.values = TRUE))$decision.values
adult.svm.polynomial.pr <- prediction(adult.svm.polynomial.pred.prob, adult.test$income)
adult.svm.polynomial.prf <- performance(adult.svm.polynomial.pr, measure = "tpr", x.measure = "fpr")
adult.svm.polynomial.dd <- data.frame(FP = adult.svm.polynomial.prf@x.values[[1]], TP = adult.svm.polynomial.prf@y.values[[1]])

adult.svm.radial <- svm(income ~ ., data = adult.train, decision.values = TRUE, kernel = 'radial')
adult.svm.radial.pred.prob <- attributes(predict(adult.svm.radial, select(adult.test, -income), decision.values = TRUE))$decision.values
adult.svm.radial.pr <- prediction(adult.svm.radial.pred.prob, adult.test$income)
adult.svm.radial.prf <- performance(adult.svm.radial.pr, measure = "tpr", x.measure = "fpr")
adult.svm.radial.dd <- data.frame(FP = adult.svm.radial.prf@x.values[[1]], TP = adult.svm.radial.prf@y.values[[1]])


adult.svm.sigmoid <- svm(income ~ ., data = adult.train, decision.values = TRUE, kernel = 'sigmoid')
adult.svm.sigmoid.pred.prob <- attributes(predict(adult.svm.sigmoid, select(adult.test, -income), decision.values = TRUE))$decision.values
adult.svm.sigmoid.pr <- prediction(adult.svm.sigmoid.pred.prob, adult.test$income)
adult.svm.sigmoid.prf <- performance(adult.svm.sigmoid.pr, measure = "tpr", x.measure = "fpr")
adult.svm.sigmoid.dd <- data.frame(FP = adult.svm.sigmoid.prf@x.values[[1]], TP = adult.svm.sigmoid.prf@y.values[[1]])

adult.roc <- ggplot() + 
  geom_line(data = adult.svm.linear.dd, aes(x = FP, y = TP, color = 'SVM - Linear')) + 
  geom_line(data = adult.svm.polynomial.dd, aes(x = FP, y = TP, color = 'SVM - Polynomial')) + 
  geom_line(data = adult.svm.radial.dd, aes(x = FP, y = TP, color = 'SVM - Radial')) +
  geom_line(data = adult.svm.sigmoid.dd, aes(x = FP, y = TP, color = 'SVM - Sigmoid')) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle('ROC Curve') + 
  labs(x = 'False Positive Rate', y = 'True Positive Rate') 

adult.roc + scale_colour_manual(name = 'Classifier', values = c(
  'SVM - Linear' = '#F4A8D7', 
  'SVM - Polynomial' = '#DEB8EE',
  'SVM - Radial' = '#BEC8FC', 
  'SVM - Sigmoid' = '#98D9FF'
))

adult.auc <- rbind(performance(adult.svm.linear.pr, measure = 'auc')@y.values[[1]],
                   performance(adult.svm.polynomial.pr, measure = 'auc')@y.values[[1]],
                   performance(adult.svm.radial.pr, measure = 'auc')@y.values[[1]],
                   performance(adult.svm.sigmoid.pr, measure = 'auc')@y.values[[1]])
rownames(adult.auc) <- (c('SVM - Linear', 'SVM - Polynomial', 'SVM - Radial', 'SVM - Sigmoid'))
colnames(adult.auc) <- 'Area Under ROC Curve'
round(adult.auc, 4)

adult.svm.radial.pred <- predict(adult.svm.radial, select(adult.test, -income))
adult.svm.radial.pred.table <- table(adult.svm.radial.pred, adult.test$income)
sum(diag(adult.svm.radial.pred.table))/sum(adult.svm.radial.pred.table)