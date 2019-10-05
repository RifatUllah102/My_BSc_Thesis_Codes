# Data
getwd()
data <- read.csv("CTG.csv", header = TRUE)


str(data)

# Min-Max Normalization
data$LB <- (data$LB - min(data$LB))/(max(data$LB) - min(data$LB))
data$AC <- (data$AC - min(data$AC))/(max(data$AC) - min(data$AC))
data$FM <- (data$FM - min(data$FM))/(max(data$FM)-min(data$FM))
data$UC <- (data$UC - min(data$UC))/(max(data$UC) - min(data$UC))
data$DL <- (data$DL - min(data$DL))/(max(data$DL) - min(data$DL))
data$DS <- (data$DS - min(data$DS))/(max(data$DS) - min(data$DS))
data$DP <- (data$DP - min(data$DP))/(max(data$DP)-min(data$DP))
data$ASTV <- (data$ASTV - min(data$ASTV))/(max(data$ASTV) - min(data$ASTV))
data$MSTV <- (data$MSTV - min(data$MSTV))/(max(data$MSTV) - min(data$MSTV))
data$ALTV <- (data$ALTV - min(data$ALTV))/(max(data$ALTV)-min(data$ALTV))
data$MLTV <- (data$MLTV - min(data$MLTV))/(max(data$MLTV) - min(data$MLTV))
data$Width <- (data$Width - min(data$Width))/(max(data$Width) - min(data$Width))
data$Min <- (data$Min - min(data$Min))/(max(data$Min) - min(data$Min))
data$Max <- (data$Max - min(data$Max))/(max(data$Max) - min(data$Max))
data$Nmax <- (data$Nmax - min(data$Nmax))/(max(data$Nmax) - min(data$Nmax))
data$Nzeros <- (data$Nzeros - min(data$Nzeros))/(max(data$Nzeros) - min(data$Nzeros))
data$Mode <- (data$Mode - min(data$Mode))/(max(data$Mode) - min(data$Mode))
data$Mean <- (data$Mean - min(data$Mean))/(max(data$Mean) - min(data$Mean))
data$Median <- (data$Median - min(data$Median))/(max(data$Median) - min(data$Median))
data$Variance <- (data$Variance - min(data$Variance))/(max(data$Variance) - min(data$Variance))
data$Tendency <- (data$Tendency - min(data$Tendency))/(max(data$Tendency) - min(data$Tendency))
data$NSP <- (data$NSP - min(data$NSP))/(max(data$NSP) - min(data$NSP))

str(data)
# Data Partition
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training <- data[ind==1,]
testing <- data[ind==2,]

# Neural Networks
library(neuralnet)
set.seed(333)


nn <- neuralnet(NSP~LB+AC+FM+UC+DL+DS+DP+ASTV+MSTV+ALTV+MLTV+Width+Min+Max+Nmax+Nzeros+Mode+Mean+Median+Variance+Tendency,
               data = training,
               hidden = c(3),
               stepmax = 1000000000,
               threshold = 0.04,
               linear.output = FALSE)
plot(nn)

time_nn <- system.time(neuralnet(NSP~LB+AC+FM+UC+DL+DS+DP+ASTV+MSTV+ALTV+MLTV+Width+Min+Max+Nmax+Nzeros+Mode+Mean+Median+Variance+Tendency,
                                 data = training,
                                 hidden = c(3),
                                 stepmax = 1000000000,
                                 threshold = 0.04,
                                 linear.output = FALSE))

time_nn
# Prediction
output <- compute(nn, training[,c(1:21)])
head(output$net.result)
head(training[1,])

# Confusion Matrix & Misclassification Error - training data
output <- compute(nn, training[,c(1:21)])
p1 <- output$net.result


library(caret)
pred1 <- ifelse(p1<0.3, 1, c(ifelse(p1>0.7, 3,2)))


tab1 <- table(pred1, training$NSP)
tab1
dimnames(tab1) = list(c(1,2,3),c(1,2,3))
tab1
confusionMatrix(tab1)
1-sum(diag(tab1))/sum(tab1)






# Confusion Matrix & Misclassification Error - testing data
output <- compute(nn, testing[,1:21])
p2 <- output$net.result

pred2 <- ifelse(p2<0.3, 1, c(ifelse(p2>0.7, 3,2)))
tab2 <- table(pred2, testing$NSP)
tab2
dimnames(tab2) = list(c(1,2,3),c(1,2,3))
CM_nn<- confusionMatrix(t(tab2))
CM_nn
CM_nn$byClass
MCE_nn <- 1-sum(diag(tab2))/sum(tab2)
MCE_nn


