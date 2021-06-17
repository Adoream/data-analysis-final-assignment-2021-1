library(GGally)
library(psych)
library(corrplot)

#data
adult.efa.data <- as.data.frame(sapply(adult.data, as.numeric))
adult.efa.data <- cor(adult.efa.data)
boxplot(adult.efa.data)
str(adult.efa.data)
#best number of factor
fa.parallel(adult.efa.data , n.obs = 240, fa = "both", n.iter = 100, main = "平行分析碎石图")
#fa
adult.efa.fa <- fa(correlations, nfactors = 3, rotate = "none", fm = "wls")
adult.efa.fa
fa.diagram(adult.efa.fa,simple = TRUE, digits = 3)
