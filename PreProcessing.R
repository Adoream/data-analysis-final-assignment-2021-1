library(plyr)
library(dplyr)
library(ggplot2)

# Loading Train Data
adult.temp.train <- read.csv(
  "data/adult/adult.data",
  sep = ",",
  fill = F,
  strip.white = T,
  col.names = c(
    "age", "workclass", "fnlwgt", "education",
    "education_num", "marital_status", "occupation", "relationship", "race", "sex",
    "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"
  )
)

# Loading Test Data
adult.temp.test <- read.csv(
  "data/adult/adult.test",
  sep = ",",
  fill = F,
  strip.white = T,
  col.names = c(
    "age", "workclass", "fnlwgt", "education",
    "education_num", "marital_status", "occupation", "relationship", "race", "sex",
    "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"
  )
)

adult.data <- rbind(adult.temp.train, adult.temp.test)

adult.data$education <- NULL
adult.data$fnlwgt <- NULL
adult.data$relationship <- NULL

adult.int.var <- colnames(adult.data)[which(sapply(adult.data, class) == 'integer')]
adult.fac.var <- colnames(adult.data)[which(sapply(adult.data, class) == 'factor')]
adult.data <- as.data.frame(lapply(adult.data, function(x) ifelse(x == "?", "Unknown", as.character(x))))
for(i in adult.int.var) {
  adult.data[i] <- as.numeric(unlist(adult.data[i]))
}
rm(i)

# histogram of age by income group
ggplot(adult.data) + aes(x=as.numeric(age), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')

# histogram of age by gender group
ggplot(adult.data) + aes(x=as.numeric(age), group=sex, fill=sex) + 
  geom_histogram(binwidth=1, color='black')

# Combine Work
summary(adult.data$workclass)
adult.data$workclass <- gsub("^Federal-gov", "Government", adult.data$workclass)
adult.data$workclass <- gsub("^Local-gov", "Government", adult.data$workclass)
adult.data$workclass <- gsub("^State-gov", "Government", adult.data$workclass)
adult.data$workclass <- gsub("^Self-emp-inc", "Self-Employed", adult.data$workclass)
adult.data$workclass <- gsub("^Self-emp-not-inc", "Self-Employed", adult.data$workclass)
adult.data$workclass <- gsub("^Never-worked", "Other", adult.data$workclass)
adult.data$workclass <- gsub("^Without-pay", "Other", adult.data$workclass)
adult.data$workclass <- gsub("^Other", "Other/Unknown", adult.data$workclass)
adult.data$workclass <- gsub("^Unknown", "Other/Unknown", adult.data$workclass)
adult.data$workclass <- as.factor(adult.data$workclass)
summary(adult.data$workclass)

adult.data.workclass.table <- table(adult.data[adult.data$workclass == 'Government',]$income)["<=50K"]
adult.data.workclass.table <- c(adult.data.workclass.table, table(adult.data[adult.data$workclass == 'Government',]$income)[">50K"])
adult.data.workclass.table <- c(adult.data.workclass.table, table(adult.data[adult.data$workclass == 'Other/Unknown',]$income)["<=50K"])
adult.data.workclass.table <- c(adult.data.workclass.table, table(adult.data[adult.data$workclass == 'Other/Unknown',]$income)[">50K"])
adult.data.workclass.table <- c(adult.data.workclass.table, table(adult.data[adult.data$workclass == 'Private',]$income)["<=50K"])
adult.data.workclass.table <- c(adult.data.workclass.table, table(adult.data[adult.data$workclass == 'Private',]$income)[">50K"])
adult.data.workclass.table <- c(adult.data.workclass.table, table(adult.data[adult.data$workclass == 'Self-Employed',]$income)["<=50K"])
adult.data.workclass.table <- c(adult.data.workclass.table, table(adult.data[adult.data$workclass == 'Self-Employed',]$income)[">50K"])
adult.data.workclass.table <- as.numeric(adult.data.workclass.table)
adult.data.workclass.industry <- rep(levels(adult.data$workclass), each = 2)
adult.data.workclass.income <- rep(c('<=50K', '>50K'), 4)
adult.data.workclass.df <- data.frame(adult.data.workclass.industry, adult.data.workclass.income, adult.data.workclass.table)
print(adult.data.workclass.df)
adult.data.workclass.df <- ddply(adult.data.workclass.df, .(adult.data.workclass.industry), transform, percent = adult.data.workclass.table/sum(adult.data.workclass.table) * 100)
adult.data.workclass.df <- ddply(adult.data.workclass.df, .(adult.data.workclass.industry), transform, pos = (cumsum(adult.data.workclass.table) - 0.5 * adult.data.workclass.table))
adult.data.workclass.df$label <- paste0(sprintf("%.0f", adult.data.workclass.df$percent), "%")
ggplot(adult.data.workclass.df, aes(x = adult.data.workclass.industry, y = adult.data.workclass.table, fill = adult.data.workclass.income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 3, position=position_stack(vjust = 0)) + 
  ggtitle('Income by Industry')

# create a dataframe for income, education_num, count
adult.data.education.df <- data.frame(table(adult.data$income, adult.data$education_num))
names(adult.data.education.df) <- c('income', 'education_num', 'count')
adult.data.education.df <- ddply(adult.data.education.df, .(education_num), transform, percent = count/sum(count) * 100)
adult.data.education.df <- ddply(adult.data.education.df, .(education_num), transform, pos = (cumsum(count) - 0.5 * count))
adult.data.education.df$label <- paste0(sprintf("%.0f", adult.data.education.df$percent), "%")
print(adult.data.education.df)
ggplot(adult.data.education.df, aes(x = education_num, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2, position=position_stack(vjust = 0)) + 
  ggtitle('Income Level with Years of Education')

# Combine Occupation
summary(adult.data$occupation)
adult.data$occupation <- gsub("Adm-clerical", "White-Collar", adult.data$occupation)
adult.data$occupation <- gsub("Craft-repair", "Blue-Collar", adult.data$occupation)
adult.data$occupation <- gsub("Exec-managerial", "White-Collar", adult.data$occupation)
adult.data$occupation <- gsub("Farming-fishing", "Blue-Collar", adult.data$occupation)
adult.data$occupation <- gsub("Handlers-cleaners", "Blue-Collar", adult.data$occupation)
adult.data$occupation <- gsub("Machine-op-inspct", "Blue-Collar", adult.data$occupation)
adult.data$occupation <- gsub("Other-service", "Service", adult.data$occupation)
adult.data$occupation <- gsub("Priv-house-serv", "Service", adult.data$occupation)
adult.data$occupation <- gsub("Prof-specialty", "Professional", adult.data$occupation)
adult.data$occupation <- gsub("Protective-serv", "Service", adult.data$occupation)
adult.data$occupation <- gsub("Tech-support", "Service", adult.data$occupation)
adult.data$occupation <- gsub("Transport-moving", "Blue-Collar", adult.data$occupation)
adult.data$occupation <- gsub("Unknown", "Other/Unknown", adult.data$occupation)
adult.data$occupation <- gsub("Armed-Forces", "Other/Unknown", adult.data$occupation)
adult.data$occupation <- as.factor(adult.data$occupation)
summary(adult.data$occupation)

adult.data.occupation.df <- data.frame(table(adult.data$income, adult.data$occupation))
names(adult.data.occupation.df) <- c('income', 'occupation', 'count')
adult.data.occupation.df <- ddply(adult.data.occupation.df, .(occupation), transform, percent = count/sum(count) * 100)
adult.data.occupation.df <- ddply(adult.data.occupation.df, .(occupation), transform, pos = (cumsum(count) - 0.5 * count))
adult.data.occupation.df$label <- paste0(sprintf("%.0f", adult.data.occupation.df$percent), "%")
print(adult.data.occupation.df)
ggplot(adult.data.occupation.df, aes(x = occupation, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2, position=position_stack(vjust = 0)) + 
  ggtitle('Income Level with Different Occupations')

# Combine Married
summary(adult.data$marital_status)
adult.data$marital_status <- gsub("Married-AF-spouse", "Married", adult.data$marital_status)
adult.data$marital_status <- gsub("Married-civ-spouse", "Married", adult.data$marital_status)
adult.data$marital_status <- gsub("Married-spouse-absent", "Married", adult.data$marital_status)
adult.data$marital_status <- gsub("Never-married", "Single", adult.data$marital_status)
adult.data$marital_status <- as.factor(adult.data$marital_status)
summary(adult.data$marital_status)

adult.data.marital.df <- data.frame(table(adult.data$income, adult.data$marital_status))
names(adult.data.marital.df) <- c('income', 'marital_status', 'count')
adult.data.marital.df <- ddply(adult.data.marital.df, .(marital_status), transform, percent = count/sum(count) * 100)
adult.data.marital.df <- ddply(adult.data.marital.df, .(marital_status), transform, pos = (cumsum(count) - 0.5 * count))
adult.data.marital.df$label <- paste0(sprintf("%.0f", adult.data.marital.df$percent), "%")
print(adult.data.marital.df)
ggplot(adult.data.marital.df, aes(x = marital_status, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2, position=position_stack(vjust = 0)) + 
  ggtitle('Income Level with Marital Status')

adult.data.race.df <- data.frame(table(adult.data$income, adult.data$race))
names(adult.data.race.df) <- c('income', 'race', 'count')
print(adult.data.race.df)
adult.data.race.df <- ddply(adult.data.race.df, .(race), transform, percent = count/sum(count) * 100)
adult.data.race.df <- ddply(adult.data.race.df, .(race), transform, pos = (cumsum(count) - 0.5 * count))
adult.data.race.df$label <- paste0(sprintf("%.0f", adult.data.race.df$percent), "%")
adult.data.race.df$label[adult.data.race.df$race == 'Other'] <- NA
adult.data.race.df$label[adult.data.race.df$race == 'Amer-Indian-Eskimo'] <- NA
ggplot(adult.data.race.df, aes(x = race, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2, position=position_stack(vjust = 0)) + 
  ggtitle('Income Level by Race')

set.seed(23333)
adult.index <- sample(nrow(adult.data), nrow(adult.data) * 0.7)
adult.train <- adult.data[adult.index, ]
adult.test <- adult.data[-adult.index, ]

data("Adult")