# Read Data
data <- read.csv("CTG.csv", header = TRUE)
str(data)
data <- subset(data, select = -c(AC,FM,DL,MSTV,Nmax,Nzeros))
data$NSP <- as.factor(data$NSP)
table(data$NSP)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train_GA <- data[ind==1,]
test_GA <- data[ind==2,]

# decission tree_C5.0_GA with C50
library(C50)
set.seed(222)
tree_C5.0_GA <- C5.0(NSP~., data = train_GA)
tree_C5.0_GA
summary(tree_C5.0_GA)
plot(tree_C5.0_GA)

tm <- system.time(C5.0(NSP~., data = train_GA))
tm

attributes(tree_C5.0_GA)

# Prediction & Confusion Matrix - train_GA data
library(caret)
p1 <- predict(tree_C5.0_GA, train_GA)
tab1 <- table(p1, train_GA$NSP)
confusionMatrix(p1, train_GA$NSP)
1-sum(diag(tab1))/sum(tab1)


# # Prediction & Confusion Matrix - test_GA data
p2 <- predict(tree_C5.0_GA, test_GA)
tab2 <- table(p2, test_GA$NSP)
CM_C5.0_GA <- confusionMatrix(p2, test_GA$NSP)
1-sum(diag(tab2))/sum(tab2)

