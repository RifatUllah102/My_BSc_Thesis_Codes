# Data
getwd()
data <- read.csv("CTG.csv", header = TRUE)
data <- subset(data, select = -c(AC,FM,DL,MSTV,Nmax,Nzeros))

str(data)

# Min-Max Normalization
data$LB <- (data$LB - min(data$LB))/(max(data$LB) - min(data$LB))
#data$AC <- (data$AC - min(data$AC))/(max(data$AC) - min(data$AC))
#data$FM <- (data$FM - min(data$FM))/(max(data$FM)-min(data$FM))
data$UC <- (data$UC - min(data$UC))/(max(data$UC) - min(data$UC))
#data$DL <- (data$DL - min(data$DL))/(max(data$DL) - min(data$DL))
data$DS <- (data$DS - min(data$DS))/(max(data$DS) - min(data$DS))
data$DP <- (data$DP - min(data$DP))/(max(data$DP)-min(data$DP))
data$ASTV <- (data$ASTV - min(data$ASTV))/(max(data$ASTV) - min(data$ASTV))
#data$MSTV <- (data$MSTV - min(data$MSTV))/(max(data$MSTV) - min(data$MSTV))
data$ALTV <- (data$ALTV - min(data$ALTV))/(max(data$ALTV)-min(data$ALTV))
data$MLTV <- (data$MLTV - min(data$MLTV))/(max(data$MLTV) - min(data$MLTV))
data$Width <- (data$Width - min(data$Width))/(max(data$Width) - min(data$Width))
data$Min <- (data$Min - min(data$Min))/(max(data$Min) - min(data$Min))
data$Max <- (data$Max - min(data$Max))/(max(data$Max) - min(data$Max))
#data$Nmax <- (data$Nmax - min(data$Nmax))/(max(data$Nmax) - min(data$Nmax))
#data$Nzeros <- (data$Nzeros - min(data$Nzeros))/(max(data$Nzeros) - min(data$Nzeros))
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
training_GA <- data[ind==1,]
testing_GA <- data[ind==2,]

# Neural Networks
library(neuralnet)
set.seed(333)
nn_GA <- neuralnet(NSP~LB+UC+DS+DP+ASTV+ALTV+MLTV+Width+Min+Max+Mode+Mean+Median+Variance+Tendency,
                data = training_GA,
                hidden = c(3),
                stepmax = 1000000000,
                threshold = 0.04,
                linear.output = FALSE)

plot(nn_GA)
time_nn_GA <- system.time(neuralnet(NSP~LB+UC+DS+DP+ASTV+ALTV+MLTV+Width+Min+Max+Mode+Mean+Median+Variance+Tendency,
                                    data = training_GA,
                                    hidden = c(3),
                                    stepmax = 1000000000,
                                    threshold = 0.04,
                                    linear.output = FALSE))






time_nn_GA
# Prediction
output <- compute(nn_GA, training_GA[,c(1:15)])
head(output$net.result)
head(training_GA[1,])

# Confusion Matrix & Misclassification Error - training_GA data
output <- compute(nn_GA, training_GA[,c(1:15)])
p1 <- output$net.result


library(caret)
pred1 <- ifelse(p1<0.3, 1, c(ifelse(p1>0.7, 3,2)))


tab1 <- table(pred1, training_GA$NSP)
tab1
dimnames(tab1) = list(c(1,2,3),c(1,2,3))
tab1
confusionMatrix(tab1)
1-sum(diag(tab1))/sum(tab1)






# Confusion Matrix & Misclassification Error - testing_GA data
output <- compute(nn_GA, testing_GA[,1:15])
p2 <- output$net.result

pred2 <- ifelse(p2<0.3, 1, c(ifelse(p2>0.7, 3,2)))
tab2 <- table(pred2, testing_GA$NSP)
tab2
dimnames(tab2) = list(c(1,2,3),c(1,2,3))
CM_nn_GA<- confusionMatrix(t(tab2))
CM_nn_GA
CM_nn_GA$byClass
MCE_nn_GA <- 1-sum(diag(tab2))/sum(tab2)
MCE_nn_GA


