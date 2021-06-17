library(arules) 
library(arulesViz)

#apriori for maxlen = 2
adult.apriori.2 <- apriori(data = adult.data, parameter = list(support = 0.12, confidence = 0.2333, minlen = 2, maxlen = 2))
summary(adult.apriori.2)

#show lift TOP10
inspect(sort(adult.apriori.2,by="lift")[1:10])

#about high income
adult.apriori.highIncome <- subset(adult.apriori.2, rhs %in% c("income=>50K"))
inspect(sort(adult.apriori.highIncome,by="lift")[1:5])
plot(sort(adult.apriori.highIncome,by="lift")[1:5], method = "group")

#about low income
adult.apriori.lowIncome <- subset(adult.apriori.2, rhs %in% c("income=<=50K"))
inspect(sort(adult.apriori.lowIncome,by="lift")[1:5])
plot(sort(adult.apriori.lowIncome,by="lift")[1:5], method = "group")

#apriori for maxlen = 3
adult.apriori.noIncome <- adult.data
adult.apriori.noIncome$income <- NULL
adult.apriori.3 <- apriori(data = adult.apriori.noIncome, parameter = list(support = 0.12, confidence = 0.2333, minlen = 2, maxlen = 3))
summary(adult.apriori.3)

#show lift TOP5
inspect(sort(adult.apriori.3,by="lift")[1:5])
