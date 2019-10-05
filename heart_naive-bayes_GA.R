# Read Data
data <- read.csv("CTG.csv", header = TRUE)
data <- subset(data, select = -c(AC,FM,DL,MSTV,Nmax,Nzeros))
str(data)
data$NSP <- as.factor(data$NSP)
table(data$NSP)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train_GA <- data[ind==1,]
test_GA <- data[ind==2,]

# naiveBayes
library(e1071)
set.seed(222)
NB_GA <- naiveBayes(NSP~., data=train_GA)
summary(NB_GA)
NB_GA

#time Elapsed
time_NB_GA <- system.time(naiveBayes(NSP~., data = train_GA,controls=ctree_control(mincriterion=0.9, minsplit=100)))
time_NB_GA

# Prediction & Confusion Matrix - train_GA data
library(caret)
p1 <- predict(NB_GA, train_GA)
confusionMatrix(p1, train_GA$NSP)
tab1 <- table(p1, train_GA$NSP)
1-sum(diag(tab1))/sum(tab1)

# # Prediction & Confusion Matrix - test_GA data
p2 <- predict(NB_GA, test_GA)
CM_NB_GA <- confusionMatrix(p2, test_GA$NSP)
tab2 <- table(p2, test_GA$NSP)
MCE_NB_GA <- 1-sum(diag(tab2))/sum(tab2)
time_NB_GA
CM_NB_GA
MCE_NB_GA
