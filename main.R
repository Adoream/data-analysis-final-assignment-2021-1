library(dplyr)
library(ggplot2)

# 导入数据
adult <- read.table(
  'data/adult.data', 
  sep = ',',
  fill = F,
  strip.white = T,
  col.names = c(
    'age', 'workclass', 'fnlwgt', 'education', 
    'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
    'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income'
  )
)

adult$education <- NULL
adult$fnlwgt <- NULL
adult$relationship <- NULL

# Age - Income
ggplot(adult) + aes(x = as.numeric(age), group = income, fill = income) +
  geom_histogram(binwidth = 1, color = 'black')

# Age - Sex
ggplot(adult) + aes(x = as.numeric(age), group = sex, fill = sex) + 
  geom_histogram(binwidth = 1, color = 'black')

summary(adult$workclass)

# Combine Work
levels(adult$workclass)[1] <- 'Unknown'
adult$workclass <- gsub('^Federal-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^Local-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^State-gov', 'Government', adult$workclass) 
adult$workclass <- gsub('^Self-emp-inc', 'Self-Employed', adult$workclass)
adult$workclass <- gsub('^Self-emp-not-inc', 'Self-Employed', adult$workclass)
adult$workclass <- gsub('^Never-worked', 'Other', adult$workclass)
adult$workclass <- gsub('^Without-pay', 'Other', adult$workclass)
adult$workclass <- gsub('^Other', 'Other/Unknown', adult$workclass)
adult$workclass <- gsub('^Unknown', 'Other/Unknown', adult$workclass)
adult$workclass <- as.factor(adult$workclass)
summary(adult$workclass)

# Combine Occupation
levels(adult$occupation)[1] <- 'Unknown'
adult$occupation <- gsub('Adm-clerical', 'White-Collar', adult$occupation)
adult$occupation <- gsub('Craft-repair', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Exec-managerial', 'White-Collar', adult$occupation)
adult$occupation <- gsub('Farming-fishing', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Handlers-cleaners', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Machine-op-inspct', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Other-service', 'Service', adult$occupation)
adult$occupation <- gsub('Priv-house-serv', 'Service', adult$occupation)
adult$occupation <- gsub('Prof-specialty', 'Professional', adult$occupation)
adult$occupation <- gsub('Protective-serv', 'Service', adult$occupation)
adult$occupation <- gsub('Tech-support', 'Service', adult$occupation)
adult$occupation <- gsub('Transport-moving', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Unknown', 'Other/Unknown', adult$occupation)
adult$occupation <- gsub('Armed-Forces', 'Other/Unknown', adult$occupation)
adult$occupation <- as.factor(adult$occupation)
summary(adult$occupation)

# Combine Married
adult$marital_status <- gsub('Married-AF-spouse', 'Married', adult$marital_status)
adult$marital_status <- gsub('Married-civ-spouse', 'Married', adult$marital_status)
adult$marital_status <- gsub('Married-spouse-absent', 'Married', adult$marital_status)
adult$marital_status <- gsub('Never-married', 'Single', adult$marital_status)
adult$marital_status <- as.factor(adult$marital_status)
summary(adult$marital_status)

adult$capital_gain <- NULL
adult$capital_loss <- NULL
adult$native_country <- NULL

set.seed(23333)
adult.index <- sample(nrow(adult), nrow(adult) * 0.7)
adult.train <- adult[adult.index, ]
adult.test <- adult[-adult.index, ]