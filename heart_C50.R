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

# decission tree with C50
library(C50)
set.seed(222)
tree_C5.0 <- C5.0(NSP~., data = train)
tree_C5.0
summary(tree_C5.0)
plot(tree_C5.0)

tm <- system.time(C5.0(NSP~., data = train))
tm

attributes(tree_C5.0)

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(tree_C5.0, train)
tab1 <- table(p1, train$NSP)
confusionMatrix(p1, train$NSP)
1-sum(diag(tab1))/sum(tab1)


# # Prediction & Confusion Matrix - test data
p2 <- predict(tree_C5.0, test)
tab2 <- table(p2, test$NSP)
CM_C5.0 <- confusionMatrix(p2, test$NSP)
1-sum(diag(tab2))/sum(tab2)

