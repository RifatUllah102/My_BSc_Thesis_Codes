library(party)
library(C50)
library(e1071)
library(randomForest)
library(neuralnet)
library(caret)
library(rpart)
library(psych)
library(GA)
library(devtools)
library(ggbiplot)

# Read heart_data
heart_data <- read.csv("CTG.csv", header = TRUE)
str(heart_data)
heart_data$NSP <- as.factor(heart_data$NSP)
table(heart_data$NSP)


# heart_data Partition
set.seed(123)
ind <- sample(2, nrow(heart_data), replace = TRUE, prob = c(0.7, 0.3))
train <- heart_data[ind==1,]
test <- heart_data[ind==2,]

#corelation of training heart_data
pairs.panels(train[,-22],
             gap =0,
             bg = c("yellow","red")[train$NSP],
             pch=21)

cor_matrix <- cor(train[,-22], method = "pearson", use = "complete.obs")
cor_matrix
UM <- upperTriangle(cor_matrix, diag=FALSE)
UM
Diab_correlation_mean <- mean(UM)
Diab_correlation_mean


#principle component analysis 
pc <- prcomp(train[,-22],
             center = TRUE,
             scale. = TRUE)

summary(pc)
attributes(pc)
pc$center
pc$scale
print(pc)

pairs.panels(pc$x,
             gap =0,
             bg = c("red","yellow","green","blue")[train$NSP],
             pch=21)

g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = train$NSP,
              ellipses = TRUE,
              circle = TRUE,
              ellipses.prob = 0.68)

g <- g + scale_color_continuous(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')

print(g)

#prediction component
pred_train <- predict(pc,train)
pred_train <- heart_data.frame(pred_train, train$Outcome)

pred_test <- predict(pc, test)
pred_test <- heart_data.frame(pred_test, test$Outcome)

