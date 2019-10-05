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

#Decission Tree
library(party)
set.seed(222)
tree <- ctree(NSP~., data=train,controls=ctree_control(mincriterion=0.9, minsplit=100))
summary(tree)
tree
plot(tree)

#time Elapsed
tm <- system.time(ctree(NSP~., data = train,controls=ctree_control(mincriterion=0.9, minsplit=100)))
tm

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(tree, train)
confusionMatrix(p1, train$NSP)
tab1 <- table(p1, train$NSP)
1-sum(diag(tab1))/sum(tab1)

# # Prediction & Confusion Matrix - test data
p2 <- predict(tree, test)
confusionMatrix(p2, test$NSP)
tab2 <- table(p2, test$NSP)
1-sum(diag(tab2))/sum(tab2)




data$NSP <- as.factor(data$NSP)
table(data$NSP)
