# Read Data
data <- read.csv("CTG.csv", header = TRUE)
str(data)
data$NSP <- as.factor(data$NSP)
table(data$NSP)
#plot(data)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Support Vector Machine
library(e1071)
library(rgl)
set.seed(222)
support_vector_machine <- svm(NSP~., data=train)
summary(support_vector_machine)
support_vector_machine
plot(support_vector_machine, data=train, LB~Tendency)


#time Elapsed
time_svm <- system.time(svm(NSP~., data = train))
time_svm

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(support_vector_machine, train)
confusionMatrix(p1, train$NSP)
tab1 <- table(p1, train$NSP)
1-sum(diag(tab1))/sum(tab1)

# # Prediction & Confusion Matrix - test data
p2 <- predict(support_vector_machine, test)
CM_svm <- confusionMatrix(p2, test$NSP)
tab2 <- table(p2, test$NSP)
MCE_svm <- 1-sum(diag(tab2))/sum(tab2)
CM_svm
