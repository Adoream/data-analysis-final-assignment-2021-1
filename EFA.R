library(GGally)
library(psych)
library(corrplot)
#data
adult.efa.data <- as.data.frame(sapply(adult.data, as.numeric))
adult.efa.data$income <- NULL
adult.efa.data <- scale(adult.efa.data)
boxplot(adult.efa.data)
str(adult.efa.data)
corrplot(adult.efa.data)
#best number of factor
fa.parallel(adult.efa.data, n.obs = 240, fa = "both", n.iter = 100, main = "平行分析碎石图")
#fa
adult.efa.fa <- fa(adult.efa.data, nfactors = 2, rotate = "none", fm = "pa")
adult.efa.fa
adult.efa.fa <- fa(adult.efa.fa, nfactors = 2, rotate = "varimax", fm="ml")
adult.efa.fa
fa.diagram(adult.efa.fa,simple = TRUE, digits = 3)
