library(arules) 
library(arulesViz)

#aporiori for maxlen = 2
adult.apriori.2 <- apriori(data = adult.data, parameter = list(support = 0.12, confidence = 0.2333, minlen = 2, maxlen = 2))
summary(adult.apriori)

#show lift TOP10
inspect(sort(adult.apriori,by="lift")[1:10])

#aporiori for maxlen = 3
adult.apriori.3 <- apriori(data = adult.data, parameter = list(support = 0.12, confidence = 0.2333, minlen = 2, maxlen =3))
summary(adult.apriori)

#show lift TOP3
inspect(sort(adult.apriori.3,by="lift")[1:3])


#about high income
adult.apriori.highIncome <- subset(adult.apriori.2, rhs %in% c("income=>50K"))
adult.apriori.highIncome <- subset(adult.apriori.highIncome, subset = !(lhs %in% c("age=[1,15)")))
inspect(sort(adult.apriori.highIncome,by="lift")[1:5])
plot(sort(adult.apriori.highIncome,by="lift")[1:5], method = "group")

#about low income
adult.apriori.lowIncome <- subset(adult.apriori.2, rhs %in% c("income=<=50K"))
adult.apriori.lowIncome <- subset(adult.apriori.lowIncome, subset = !(lhs %in% c("age=[1,15)")))
inspect(sort(adult.apriori.lowIncome,by="lift")[1:5])
plot(sort(adult.apriori.lowIncome,by="lift")[1:5], method = "group")

