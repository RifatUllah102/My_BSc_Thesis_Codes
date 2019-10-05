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
library(gdata)
library(psych)
library(ROCR)
library(pROC)
par(new= TRUE)
par(mfrow = c(2,3))

#ANN
output <- compute(nn, testing[1:21])
p_ann <- output$net.result
p_ann <- ifelse(p_ann<0.3, 1, c(ifelse(p_ann>0.7, 3,2)))
performance_ann <- as.numeric(p_ann)
multiclass_roc <- multiclass.roc(testing$NSP,performance_ann)
multiclass_roc

auc <- "90.04%"

roc <- multiclass_roc[['rocs']]
plot.roc(roc[[1]], col = "red", main = "ANN")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)



#Random Forest
p_rf <- predict(rf, test)
performance_rf <- as.numeric(p_rf)
multiclass_roc <- multiclass.roc(test$NSP,performance_rf)
roc <- multiclass_roc[['rocs']]
multiclass_roc
auc <- "91.69%"
plot.roc(roc[[1]], col = "red", main = "Random Forest")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)




#C50
p_C50 <- predict(tree_C5.0, test)
performance_C50 <- as.numeric(p_C50)
multiclass_roc <- multiclass.roc(test$NSP,performance_C50)
multiclass_roc
auc <- "88.20%"
roc <- multiclass_roc[['rocs']]
plot.roc(roc[[1]], col = "red", main = "C5.0")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)

#svm
p_svm <- predict(support_vector_machine, test)
performance_C50 <- as.numeric(p_svm)
multiclass_roc <- multiclass.roc(test$NSP,performance_svm)
multiclass_roc
auc <- "88.02%"
roc <- multiclass_roc[['rocs']]
plot.roc(roc[[1]], col = "red", main = "SVM")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)






#Naive_bayes
p_NB <- predict(NB, test, "raw")
performance_NB <- as.numeric(p_NB[,3])
multiclass_roc <- multiclass.roc(test$NSP,performance_NB)
multiclass_roc
auc <- "84.45%"
roc <- multiclass_roc[['rocs']]
plot.roc(roc[[1]], col = "red", main = "Naive Bayes")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)

plot(0,type='n',axes=FALSE,ann=FALSE)

legend(x=0.6, y= 0.5, legend = c("Normal","Suspect","Pathologic"), col = c("red","green","blue"), lwd = 2, lty = 1, cex = 1.5)




















#Genetic Algorithm


par(mfrow = c(2,3))
#ANN
output <- compute(nn_GA, testing_GA[1:15])
p_ann <- output$net.result
p_ann <- ifelse(p_ann<0.3, 1, c(ifelse(p_ann>0.7, 3,2)))
performance_ann <- as.numeric(p_ann)
multiclass_roc <- multiclass.roc(testing_GA$NSP,performance_ann)
multiclass_roc
auc = "87.72%"

roc <- multiclass_roc[['rocs']]

plot.roc(roc[[1]], col = "red", main = "ANN")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)


#Random Forest
p_rf <- predict(rf_GA, test_GA)
performance_rf <- as.numeric(p_rf)
multiclass_roc <- multiclass.roc(test_GA$NSP,performance_rf)
multiclass_roc
auc <- "90.82%"
roc <- multiclass_roc[['rocs']]
plot.roc(roc[[1]], col = "red", main = "Random Forest")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)

#C50
p_C50 <- predict(tree_C5.0_GA, test_GA)
performance_C50 <- as.numeric(p_C50)
multiclass_roc <- multiclass.roc(test_GA$NSP,performance_C50)
multiclass_roc
auc <- "87.95%"
roc <- multiclass_roc[['rocs']]
plot.roc(roc[[1]], col = "red", main = "C5.0")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)

#svm
p_svm <- predict(support_vector_machine_GA, test_GA)
performance_svm <- as.numeric(p_svm)
multiclass_roc <- multiclass.roc(test_GA$NSP,performance_svm)
multiclass_roc 
auc <- "87.51%"
roc <- multiclass_roc[['rocs']]
plot.roc(roc[[1]], col = "red", main = "SVM")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)






#Naive_bayes
p_NB <- predict(NB_GA, test_GA, "raw")
performance_NB <- as.numeric(p_NB[,3])
multiclass_roc <- multiclass.roc(test_GA$NSP,performance_NB)
multiclass_roc
auc <- "81.97%"
roc <- multiclass_roc[['rocs']]
plot.roc(roc[[1]], col = "red", main = "Naive Bayes")
par(new = TRUE)
plot.roc(roc[[2]], col = "green")
par(new = TRUE)
plot.roc(roc[[3]], col = "blue")
legend(.5,.35,auc,title = "AUC", cex = 1.3)

plot(0,type='n',axes=FALSE,ann=FALSE)

legend(x=0.6, y= 0.5, legend = c("Normal","Suspect","Pathologic"), col = c("red","green","blue"), lwd = 2, lty = 1, cex = 1.5)





#missclasification error
MCE <- matrix(c(MCE_C50, MCE_C50_GA, MCE_NB,MCE_NB_GA,MCE_rf,MCE_rf_GA,MCE_svm,MCE_svm_GA,MCE_nn,MCE_nn_GA),
              nrow = 2,
              dimnames = list(c("Not Genetic Algorithm","Genetic Algorithm"),
                              c("C50","Naive Bayes","Random Forest","SVM", "ANN")
              ))
par(mfcol=c(1,2))
barplot(MCE, beside = TRUE, legend.text = TRUE, col = c("pink","lightyellow"), 
        xlab = "Models", ylab = "Missclassification Error", ylim = c(0,0.35),
        main = "MCE comparison")


#Accuracy
Accuracy <- matrix(c(CM_C50$overall['Accuracy'], 
                     CM_C50_GA$overall['Accuracy'], 
                     CM_NB$overall['Accuracy'],
                     CM_NB_GA$overall['Accuracy'],
                     CM_rf$overall['Accuracy'],
                     CM_rf_GA$overall['Accuracy'],
                     CM_svm$overall['Accuracy'],
                     CM_svm_GA$overall['Accuracy'],
                     CM_nn$overall['Accuracy'],
                     CM_nn_GA$overall['Accuracy']),
              nrow = 2,
              dimnames = list(c("Not Genetic Algorithm","Genetic Algorithm"),
                              c("C50","Naive Bayes","Random Forest","SVM", "ANN")
              ))
Accuracy <- round(Accuracy*100,2)
Accuracy


barplot(Accuracy, beside = TRUE, col = c("Violet","skyblue"), 
        xlab = "Models", ylab = "Model Accuracy", ylim = c(0,90),
        main = "Accuracy")
legend(x=01, y= 90, legend = c("Not Genetic Algorithm","Genetic Algorithm"), fill = c("Violet","skyblue"))








#extra

#Accuracy
Accuracy <- matrix(c(CM_C50$overall['Accuracy'], 
                     CM_C50_GA$overall['Accuracy'], 
                     CM_NB$overall['Accuracy'],
                     CM_NB_GA$overall['Accuracy'],
                     CM_rf$overall['Accuracy'],
                     CM_rf_GA$overall['Accuracy'],
                     CM_svm$overall['Accuracy'],
                     CM_svm_GA$overall['Accuracy'],
                     CM_nn$overall['Accuracy'],
                     CM_nn_GA$overall['Accuracy']),
                   nrow = 2,
                   dimnames = list(c("Before Genetic Algorithm","After Genetic Algorithm"),
                                   c("C50","NB","RF","SVM", "ANN")
                   ))
Accuracy <- round(Accuracy*100,2)
Accuracy

par(mfcol=c(1,2))
barplot(Accuracy, beside = TRUE, col = c("Violet","skyblue"), 
        xlab = "Models", ylab = "Model Accuracy in parcentage", ylim = c(0,100),
        main = "Accuracy")
legend(x=3, y= 20, legend = c("Before Genetic Algorithm","After Genetic Algorithm"), fill = c("Violet","skyblue"))



#missclasification error
MCE <- matrix(c(MCE_C50, MCE_C50_GA, MCE_NB,MCE_NB_GA,MCE_rf,MCE_rf_GA,MCE_svm,MCE_svm_GA,MCE_nn,MCE_nn_GA),
              nrow = 2,
              dimnames = list(c("Not Genetic Algorithm","Genetic Algorithm"),
                              c("C50","NB","RF","SVM", "ANN")
              ))

MCE <- round(MCE*100,2)

barplot(MCE, beside = TRUE, col = c("lightgreen","lightblue2"), 
        xlab = "Models", ylab = "Missclassification Error in parcentage",ylim = c(0,20),
        main = "MCE comparison")

legend(x=3, y= 4, legend = c("Before Genetic Algorithm","After Genetic Algorithm"), fill = c("lightgreen","lightblue2"))





tpr_fpr <- matrix(c(CM_C50$byClass['Sensitivity'], 
                    CM_C50$byClass['Specificity'], 
                    CM_NB$byClass['Sensitivity'], 
                    CM_NB$byClass['Specificity'],
                    CM_rf$byClass['Sensitivity'], 
                    CM_rf$byClass['Specificity'],
                    CM_svm$byClass['Sensitivity'], 
                    CM_svm$byClass['Specificity'],
                    CM_nn$byClass['Sensitivity'], 
                    CM_nn$byClass['Specificity']),
                  nrow = 2,
                  dimnames = list(c("Before Genetic Algorithm","After Genetic Algorithm"),
                                  c("C50","NB","RF","SVM", "ANN")
                  ))
tpr_fpr <- round(tpr_fpr*100,2)

tpr_fpr_GA <- matrix(c(CM_C50_GA$byClass['Sensitivity'], 
                       CM_C50_GA$byClass['Specificity'], 
                       CM_NB_GA$byClass['Sensitivity'], 
                       CM_NB_GA$byClass['Specificity'],
                       CM_rf_GA$byClass['Sensitivity'], 
                       CM_rf_GA$byClass['Specificity'],
                       CM_svm_GA$byClass['Sensitivity'], 
                       CM_svm_GA$byClass['Specificity'],
                       CM_nn_GA$byClass['Sensitivity'], 
                       CM_nn_GA$byClass['Specificity']),
                     nrow = 2,
                     dimnames = list(c("Before Genetic Algorithm","After Genetic Algorithm"),
                                     c("C50","NB","RF","SVM", "ANN")
                     ))
tpr_fpr_GA <- round(tpr_fpr_GA*100,2)



barplot(tpr_fpr, beside = TRUE, col = c("blueviolet","aquamarine2"), 
        xlab = "Models", ylab = "parformance in parcentage", ylim = c(0,100),
        main = "Performance before Genetic Algorithm")
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("blueviolet","aquamarine2"))



barplot(tpr_fpr, beside = TRUE, col = c("orchid","cyan"), 
        xlab = "Models", ylab = "parformance in parcentage", ylim = c(0,100),
        main = "Performance after Genetic Algorithm")
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("orchid","cyan"))




























par(mfrow = c(2,3))




tpr_fpr_nn <- matrix(c(CM_nn$byClass[1,1], 
                        CM_nn$byClass[1,2], 
                        CM_nn$byClass[2,1], 
                        CM_nn$byClass[2,2],
                        CM_nn$byClass[3,1], 
                        CM_nn$byClass[3,2]),
                      nrow = 2,
                      dimnames = list(c("Sensitivity","Specificity"),
                                      c("Normal","Suspect","Pathologic")
                      ))

tpr_fpr_nn <- round(tpr_fpr_nn*100,2)



barplot(tpr_fpr_nn, beside = TRUE, col = c("blueviolet","aquamarine2"), 
        main = "ANN", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("blueviolet","aquamarine2"))


###########################


###############

tpr_fpr_rf <- matrix(c(CM_rf$byClass[1,1], 
                       CM_rf$byClass[1,2], 
                       CM_rf$byClass[2,1], 
                       CM_rf$byClass[2,2],
                       CM_rf$byClass[3,1], 
                       CM_rf$byClass[3,2]),
                     nrow = 2,
                     dimnames = list(c("Sensitivity","Specificity"),
                                     c("Normal","Suspect","Pathologic")
                     ))

tpr_fpr_rf <- round(tpr_fpr_rf*100,2)



barplot(tpr_fpr_rf, beside = TRUE, col = c("blueviolet","aquamarine2"), 
        main = "Random Forest", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("blueviolet","aquamarine2"))


###########################

##############################


tpr_fpr_C50 <- matrix(c(CM_C50$byClass[1,1], 
                        CM_C50$byClass[1,2], 
                        CM_C50$byClass[2,1], 
                        CM_C50$byClass[2,2],
                        CM_C50$byClass[3,1], 
                        CM_C50$byClass[3,2]),
                      nrow = 2,
                      dimnames = list(c("Sensitivity","Specificity"),
                                      c("Normal","Suspect","Pathologic")
                      ))

tpr_fpr_C50 <- round(tpr_fpr_C50*100,2)



barplot(tpr_fpr_C50, beside = TRUE, col = c("blueviolet","aquamarine2"), 
        main = "C5.0", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("blueviolet","aquamarine2"))



###############

###############

tpr_fpr_svm <- matrix(c(CM_svm$byClass[1,1], 
                       CM_svm$byClass[1,2], 
                       CM_svm$byClass[2,1], 
                       CM_svm$byClass[2,2],
                       CM_svm$byClass[3,1], 
                       CM_svm$byClass[3,2]),
                     nrow = 2,
                     dimnames = list(c("Sensitivity","Specificity"),
                                     c("Normal","Suspect","Pathologic")
                     ))

tpr_fpr_svm <- round(tpr_fpr_svm*100,2)



barplot(tpr_fpr_svm, beside = TRUE, col = c("blueviolet","aquamarine2"), 
        main = "SVM", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("blueviolet","aquamarine2"))


###########################

###############

tpr_fpr_NB <- matrix(c(CM_NB$byClass[1,1], 
                       CM_NB$byClass[1,2], 
                       CM_NB$byClass[2,1], 
                       CM_NB$byClass[2,2],
                       CM_NB$byClass[3,1], 
                       CM_NB$byClass[3,2]),
                     nrow = 2,
                     dimnames = list(c("Sensitivity","Specificity"),
                                     c("Normal","Suspect","Pathologic")
                     ))

tpr_fpr_NB <- round(tpr_fpr_NB*100,2)



barplot(tpr_fpr_NB, beside = TRUE, col = c("blueviolet","aquamarine2"), 
        main = "Naive Bayes", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("blueviolet","aquamarine2"))


###########################
plot(0,type='n',axes=FALSE,ann=FALSE)
legend(x=0.6, y= .5, legend = c("Sensitivity","Specificity"), fill = c("blueviolet","aquamarine2"),cex = 1.5)



#########Genetic Algorith#######
par(mfrow = c(2,3))




tpr_fpr_nn_GA <- matrix(c(CM_nn_GA$byClass[1,1], 
                          CM_nn_GA$byClass[1,2], 
                          CM_nn_GA$byClass[2,1], 
                          CM_nn_GA$byClass[2,2],
                          CM_nn_GA$byClass[3,1], 
                          CM_nn_GA$byClass[3,2]),
                        nrow = 2,
                        dimnames = list(c("Sensitivity","Specificity"),
                                        c("Normal","Suspect","Pathologic")
                        ))

tpr_fpr_nn_GA <- round(tpr_fpr_nn_GA*100,2)



barplot(tpr_fpr_nn_GA, beside = TRUE, col = c("royalblue","violetred"), 
        main = "ANN", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("royalblue","violetred"))


###########################


###############

tpr_fpr_rf_GA <- matrix(c(CM_rf_GA$byClass[1,1], 
                          CM_rf_GA$byClass[1,2], 
                          CM_rf_GA$byClass[2,1], 
                          CM_rf_GA$byClass[2,2],
                          CM_rf_GA$byClass[3,1], 
                          CM_rf_GA$byClass[3,2]),
                        nrow = 2,
                        dimnames = list(c("Sensitivity","Specificity"),
                                        c("Normal","Suspect","Pathologic")
                        ))

tpr_fpr_rf_GA<- round(tpr_fpr_rf_GA*100,2)



barplot(tpr_fpr_rf_GA, beside = TRUE, col = c("royalblue","violetred"), 
        main = "Random Forest", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("royalblue","violetred"))


###########################

##############################


tpr_fpr_C50_GA<- matrix(c(CM_C50_GA$byClass[1,1], 
                          CM_C50_GA$byClass[1,2], 
                          CM_C50_GA$byClass[2,1], 
                          CM_C50_GA$byClass[2,2],
                          CM_C50_GA$byClass[3,1], 
                          CM_C50_GA$byClass[3,2]),
                        nrow = 2,
                        dimnames = list(c("Sensitivity","Specificity"),
                                        c("Normal","Suspect","Pathologic")
                        ))

tpr_fpr_C50_GA<- round(tpr_fpr_C50_GA*100,2)



barplot(tpr_fpr_C50_GA, beside = TRUE, col = c("royalblue","violetred"), 
        main = "C5.0", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("royalblue","violetred"))



###############

###############

tpr_fpr_svm_GA<- matrix(c(CM_svm_GA$byClass[1,1], 
                          CM_svm_GA$byClass[1,2], 
                          CM_svm_GA$byClass[2,1], 
                          CM_svm_GA$byClass[2,2],
                          CM_svm_GA$byClass[3,1], 
                          CM_svm_GA$byClass[3,2]),
                        nrow = 2,
                        dimnames = list(c("Sensitivity","Specificity"),
                                        c("Normal","Suspect","Pathologic")
                        ))

tpr_fpr_svm_GA<- round(tpr_fpr_svm_GA*100,2)



barplot(tpr_fpr_svm_GA, beside = TRUE, col = c("royalblue","violetred"), 
        main = "SVM", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("royalblue","violetred"))


###########################

###############

tpr_fpr_NB_GA<- matrix(c(CM_NB_GA$byClass[1,1], 
                         CM_NB_GA$byClass[1,2], 
                         CM_NB_GA$byClass[2,1], 
                         CM_NB_GA$byClass[2,2],
                         CM_NB_GA$byClass[3,1], 
                         CM_NB_GA$byClass[3,2]),
                       nrow = 2,
                       dimnames = list(c("Sensitivity","Specificity"),
                                       c("Normal","Suspect","Pathologic")
                       ))

tpr_fpr_NB_GA<- round(tpr_fpr_NB_GA*100,2)



barplot(tpr_fpr_NB_GA, beside = TRUE, col = c("royalblue","violetred"), 
        main = "Naive Bayes", ylab = "parformance in parcentage", ylim = c(0,100))
legend(x=01, y= 98, legend = c("Sensitivity","Specificity"), fill = c("royalblue","violetred"))


###########################
plot(0,type='n',axes=FALSE,ann=FALSE)
legend(x=0.6, y= .5, legend = c("Sensitivity","Specificity"), fill = c("royalblue","violetred"), cex = 1.5)
