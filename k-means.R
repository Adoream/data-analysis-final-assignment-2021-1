library(fmsb)
library(ggplot2)
library(ggfortify)
#numeric
adult.kmeans.data <- adult.data
adult.kmeans.data$native_country <- NULL
G <- matrix(0,48840,1)
  #workclass
G[which(adult.kmeans.data$workclass=="Other/Unknown")] <- 0
G[which(adult.kmeans.data$workclass=="Self-Employed")] <- 1
G[which(adult.kmeans.data$workclass=="Government")] <- 2
G[which(adult.kmeans.data$workclass=="Private")] <- 3
adult.kmeans.data$workclass <- NULL
adult.kmeans.data$workclass <- G
  #marital_status
G[which(adult.kmeans.data$marital_status=="Separated")] <- 0
G[which(adult.kmeans.data$marital_status=="Widowed")] <- 1
G[which(adult.kmeans.data$marital_status=="Single")] <- 2
G[which(adult.kmeans.data$marital_status=="Divorced")] <- 3
G[which(adult.kmeans.data$marital_status=="Married")] <- 4
adult.kmeans.data$marital_status <- NULL
adult.kmeans.data$marital_status <- G
  #occupation
G[which(adult.kmeans.data$occupation=="Other/Unknown")] <- 0
G[which(adult.kmeans.data$occupation=="Service")] <- 1
G[which(adult.kmeans.data$occupation=="Blue-Collar")] <- 2
G[which(adult.kmeans.data$occupation=="Sales")] <- 3
G[which(adult.kmeans.data$occupation=="White-Collar")] <- 4
G[which(adult.kmeans.data$occupation=="Professional")] <- 5
adult.kmeans.data$occupation <- NULL
adult.kmeans.data$occupation <- G
  #occupation
G[which(adult.kmeans.data$occupation=="Other/Unknown")] <- 0
G[which(adult.kmeans.data$occupation=="Service")] <- 1
G[which(adult.kmeans.data$occupation=="Blue-Collar")] <- 2
G[which(adult.kmeans.data$occupation=="Sales")] <- 3
G[which(adult.kmeans.data$occupation=="White-Collar")] <- 4
G[which(adult.kmeans.data$occupation=="Professional")] <- 5
adult.kmeans.data$occupation <- NULL
adult.kmeans.data$occupation <- G
  #race
G[which(adult.kmeans.data$race=="Other/Unknown")] <- 0
G[which(adult.kmeans.data$race=="Service")] <- 1
G[which(adult.kmeans.data$race=="Blue-Collar")]
adult.kmeans.data$race <- NULL
adult.kmeans.data$race <- G
  #race
G[which(adult.kmeans.data$race=="Other")] <- 0
G[which(adult.kmeans.data$race=="Black")] <- 1
G[which(adult.kmeans.data$race=="White")] <- 2
G[which(adult.kmeans.data$race=="Black")] <- 3
G[which(adult.kmeans.data$race=="Asian-Pac-Islander")] <- 4
adult.kmeans.data$race <- NULL
adult.kmeans.data$race <- G
  #sex
G[which(adult.kmeans.data$sex=="Male")] <- 0
G[which(adult.kmeans.data$sex=="Female")] <- 1
adult.kmeans.data$sex <- NULL
adult.kmeans.data$sex <- G
  #income
G[which(adult.kmeans.data$income=="<=50K")] <- 0
G[which(adult.kmeans.data$income==">50K")] <- 1
adult.kmeans.data$income <- NULL
adult.kmeans.data$income <- G



#best number of group
 #plot
adult.kmeans.wss <- (nrow(adult.kmeans.data) - 1) * sum(apply(adult.kmeans.data, 2, var))
for (i in 2:15) 
  adult.kmeans.wss[i] <- sum(kmeans(adult.kmeans.data, centers = i)$withinss)
plot(1:15, adult.kmeans.wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

 #data
adult.kmeans.result <- rep(0,60)
for (i in 1:60) {
  fit_km <- kmeans(adult.kmeans.data, centers = i)
  adult.kmeans.result[i] <- fit_km$betweenss/fit_km$totss
  rm(fit_km)
}
round(adult.kmeans.result, 2)

#kmeans
adult.kmeans <- kmeans(adult.kmeans.data, 4, nstart = 24)
print(adult.kmeans)

#kmeans-plot
autoplot(adult.kmeans, data = adult.kmeans.data, label = TRUE,label.size = 3)

#Radar Chart
adult.kmeans.max <- apply(adult.kmeans$centers, 2, max)
adult.kmeans.min <- apply(adult.kmeans$centers, 2, min)
adult.kmeans.df <- data.frame(rbind(adult.kmeans.max, adult.kmeans.min, adult.kmeans$centers))
radarchart(df = adult.kmeans.df, seg = 3, plty = 1:3, vlcex = 1, plwd = 3)


