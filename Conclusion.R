library(plyr)
library(ROCR)

# ANN
adult.nn.pr <- prediction(adult.nn.pred, adult.test$income)
adult.nn.prf <- performance(adult.nn.pr, measure = "tpr", x.measure = "fpr")
adult.nn.dd <- data.frame(FP = adult.nn.prf@x.values[[1]], TP = adult.nn.prf@y.values[[1]])

# CART
adult.cart.pr <- prediction(adult.cart.pred.prob[, 2], adult.test$income)
adult.cart.prf <- performance(adult.cart.pr, measure = "tpr", x.measure = "fpr")
adult.cart.dd <- data.frame(FP = adult.cart.prf@x.values[[1]], TP = adult.cart.prf@y.values[[1]])

# Random Forest
adult.rf.pr <- prediction(adult.rf.pred.prob[, 2], adult.test$income)
adult.rf.prf <- performance(adult.rf.pr, measure = "tpr", x.measure = "fpr")
adult.rf.dd <- data.frame(FP = adult.rf.prf@x.values[[1]], TP = adult.rf.prf@y.values[[1]])

# SVM
adult.svm.linear.pr <- prediction(adult.svm.linear.pred.prob, adult.test$income)
adult.svm.linear.prf <- performance(adult.svm.linear.pr, measure = "tpr", x.measure = "fpr")
adult.svm.dd <- data.frame(FP = adult.svm.linear.prf@x.values[[1]], TP = adult.svm.linear.prf@y.values[[1]])

# C50
adult.c50.pr <- prediction(adult.c50.pred.prob[, 2], adult.test$income)
adult.c50.prf <- performance(adult.c50.pr, measure = "tpr", x.measure = "fpr")
adult.c50.dd <- data.frame(FP = adult.c50.prf@x.values[[1]], TP = adult.c50.prf@y.values[[1]])

# KNN
adult.knn.pr <- prediction(as.numeric(adult.knn.pred), adult.test$income)
adult.knn.prf <- performance(adult.knn.pr, measure = "tpr", x.measure = "fpr")
adult.knn.dd <- data.frame(FP = adult.knn.prf@x.values[[1]], TP = adult.knn.prf@y.values[[1]])

# Naive Bayes
adult.nb.pr <- prediction(as.numeric(adult.nb.pred), adult.test$income)
adult.nb.prf <- performance(adult.knn.pr, measure = "tpr", x.measure = "fpr")
adult.nb.dd <- data.frame(FP = adult.knn.prf@x.values[[1]], TP = adult.knn.prf@y.values[[1]])

# XGBoost
adult.xgb.pr <- prediction(adult.xgb.pred.prob, adult.test$income)
adult.xgb.prf <- performance(adult.xgb.pr, measure = "tpr", x.measure = "fpr")
adult.xgb.dd <- data.frame(FP = adult.xgb.prf@x.values[[1]], TP = adult.xgb.prf@y.values[[1]])

# LDA
adult.lda.pr <- prediction(as.numeric(adult.lda.pred$x), adult.test$income)
adult.lda.prf <- performance(adult.lda.pr, measure = "tpr", x.measure = "fpr")
adult.lda.dd <- data.frame(FP = adult.lda.prf@x.values[[1]], TP = adult.lda.prf@y.values[[1]])

adult.roc <- ggplot() + 
  geom_line(data = adult.nn.dd, aes(x = FP, y = TP, color = 'Neural Networks')) + 
  geom_line(data = adult.cart.dd, aes(x = FP, y = TP, color = 'CART')) + 
  geom_line(data = adult.rf.dd, aes(x = FP, y = TP, color = 'Random Forest')) +
  geom_line(data = adult.svm.dd, aes(x = FP, y = TP, color = 'Support Vector Machine')) +
  geom_line(data = adult.c50.dd, aes(x = FP, y = TP, color = 'C5.0 Classification')) +
  geom_line(data = adult.knn.dd, aes(x = FP, y = TP, color = 'K-Nearest Neighbors')) +
  geom_line(data = adult.nb.dd, aes(x = FP, y = TP, color = 'Naive Bayes')) +
  geom_line(data = adult.xgb.dd, aes(x = FP, y = TP, color = 'XGBoost')) +
  geom_line(data = adult.lda.dd, aes(x = FP, y = TP, color = 'LDA')) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle('ROC Curve') + 
  labs(x = 'False Positive Rate', y = 'True Positive Rate') 

adult.roc + scale_colour_manual(name = 'Classifier', values = c(
  'Neural Networks' = '#F4A8D7', 
  'CART' = '#DEB8EE',
  'Random Forest' = '#BEC8FC', 
  'Support Vector Machine' = '#98D9FF',
  'C5.0 Classification' = '#70E4FA',
  'K-Nearest Neighbors' = '#59EEE7',
  'Naive Bayes' = '#69F6CF',
  'XGBoost' = '#8EF9B0',
  'LDA' = '#E2FB75'
))

adult.auc <- rbind(performance(adult.nn.pr, measure = 'auc')@y.values[[1]],
             performance(adult.cart.pr, measure = 'auc')@y.values[[1]],
             performance(adult.rf.pr, measure = 'auc')@y.values[[1]],
             performance(adult.svm.pr, measure = 'auc')@y.values[[1]],
             performance(adult.c50.pr, measure = 'auc')@y.values[[1]],
             performance(adult.knn.pr, measure = 'auc')@y.values[[1]],
             performance(adult.nb.pr, measure = 'auc')@y.values[[1]],
             performance(adult.xgb.pr, measure = 'auc')@y.values[[1]],
             performance(adult.lda.pr, measure = 'auc')@y.values[[1]])
rownames(adult.auc) <- (c('Neural Networks', 'Decision Tree', 'Random Forest',
                    'Support Vector Machine', 'C5.0 Classification', 
                    'K-Nearest Neighbors', 'Naive Bayes', 'XGBoost', 'LDA'))
colnames(adult.auc) <- 'Area Under ROC Curve'
round(adult.auc, 4)