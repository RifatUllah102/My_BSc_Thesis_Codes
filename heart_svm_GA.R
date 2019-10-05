# Read Data
data <- read.csv("CTG.csv", header = TRUE)
data <- subset(data, select = -c(AC,FM,DL,MSTV,Nmax,Nzeros))
str(data)
data$NSP <- as.factor(data$NSP)
table(data$NSP)
#plot(data)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train_GA<- data[ind==1,]
test_GA <- data[ind==2,]

# Support Vector Machine
library(e1071)
library(rgl)
set.seed(222)
support_vector_machine_GA <- svm(NSP~., data=train_GA)
summary(support_vector_machine)
support_vector_machine
plot(support_vector_machine, data=train_GA, LB~Tendency)


#time Elapsed
time_svm <- system.time(svm(NSP~., data = train_GA))
time_svm

# Prediction & Confusion Matrix - train_GA_GAdata
library(caret)
p1 <- predict(support_vector_machine, train_GA)
confusionMatrix(p1, train_GA$NSP)
tab1 <- table(p1, train_GA$NSP)
1-sum(diag(tab1))/sum(tab1)

# # Prediction & Confusion Matrix - test_GA data
p2 <- predict(support_vector_machine_GA, test_GA)
CM_svm_GA <- confusionMatrix(p2, test_GA$NSP)
tab2 <- table(p2, test_GA$NSP)
MCE_svm_GA <- 1-sum(diag(tab2))/sum(tab2)

CM_svm_GA
MCE_svm_GA
