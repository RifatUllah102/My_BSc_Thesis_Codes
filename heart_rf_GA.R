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

# Random Forest
library(randomForest)
set.seed(222)
rf_GA <- randomForest(NSP~., data=train_GA,
                   ntree = 300,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)
print(rf_GA)
attributes(rf_GA)

# Prediction & Confusion Matrix - train_GA data
library(caret)
p1 <- predict(rf_GA, train_GA)
confusionMatrix(p1, train_GA$NSP)
tab1 <- table(p1, train_GA$NSP)
1-sum(diag(tab1))/sum(tab1)

# Prediction & Confusion Matrix - test_GA data
p2 <- predict(rf_GA, test_GA)
CM_rf_GA <- confusionMatrix(p2, test_GA$NSP)
tab2 <- table(p2, test_GA$NSP)
MCE_rf_GA <- 1-sum(diag(tab2))/sum(tab2)
CM_rf_GA

# Error rate of Random Forest
plot(rf_GA)

# Tune mtry
t <- tuneRF(train_GA[,-22], train_GA[,22],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace = TRUE,
       improve = 0.05)

# No. of nodes for the trees
hist(treesize(rf_GA),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rf_GA,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf_GA)
varUsed(rf_GA)

# Partial Dependence Plot
partialPlot(rf_GA, train_GA, ASTV, "2")

# Extract Single Tree
getTree(rf_GA, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf_GA, train_GA$NSP)

