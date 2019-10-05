# Read Data
data <- read.csv("CTG.csv", header = TRUE)
str(data)
data$NSP <- as.factor(data$NSP)
table(data$NSP)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# naiveBayes
library(e1071)
set.seed(222)
NB <- naiveBayes(NSP~., data=train)
summary(NB)
NB

#time Elapsed
time_NB <- system.time(naiveBayes(NSP~., data = train,controls=ctree_control(mincriterion=0.9, minsplit=100)))
time_NB

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(NB, train)
confusionMatrix(p1, train$NSP)
tab1 <- table(p1, train$NSP)
1-sum(diag(tab1))/sum(tab1)

# # Prediction & Confusion Matrix - test data
p2 <- predict(NB, test)
CM_NB <- confusionMatrix(p2, test$NSP)
tab2 <- table(p2, test$NSP)
MCE_NB <- 1-sum(diag(tab2))/sum(tab2)
time_NB
CM_NB
MCE_NB
