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

# C50
library(C50)
set.seed(222)
tree <- C5.0(NSP~., data=train,controls=ctree_control(mincriterion=0.9, minsplit=100))
summary(tree)
tree
plot(tree)

#time Elapsed
tm <- system.time(C5.0(NSP~., data = train,controls=ctree_control(mincriterion=0.9, minsplit=100)))
tm

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(tree, train)
confusionMatrix(p1, train$NSP)
tab1 <- table(p1, train$NSP)
1-sum(diag(tab1))/sum(tab1)

# # Prediction & Confusion Matrix - test data
p2 <- predict(tree, test)
CM_C50_GA <- confusionMatrix(p2, test$NSP)
tab2 <- table(p2, test$NSP)
MCE_C50_GA <- 1-sum(diag(tab2))/sum(tab2)

