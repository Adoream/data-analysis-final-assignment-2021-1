library(arules) 
library(arulesViz)

#aporiori
adult.apriori <- apriori(data = adult.data, parameter = list(support = 0.12, confidence = 0.2333, minlen = 2))
summary(adult.apriori)

#show lift TOP20
inspect(sort(adult.apriori,by="lift")[1:20])

#about high income
adult.apriori.rule <- apriori(Adult, parameter = list(supp = 0.12, conf = 0.2333), appearance = list(rhs= "income=large", default="lhs"))
inspect(adult.apriori.rule)
#about low income
adult.apriori.rule2 <- apriori(Adult, parameter = list(supp = 0.12, conf = 0.2333), appearance = list(rhs= "income=small", default="lhs"))
inspect(sort(adult.apriori.rule2,by="lift")[1:20])

#scatterplot
plot(sort(adult.apriori,by="lift")[1:20])
#group
plot(sort(adult.apriori,by="lift")[1:20], method = "group")
#graph
plot(sort(adult.apriori,by="lift")[1:20], method = "graph")



