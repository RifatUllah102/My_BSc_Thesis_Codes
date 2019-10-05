# Read Data
data <- read.csv("CTG.csv", header = TRUE)
str(data)

model <- glm(NSP~LB+AC+FM+UC+DL+DS+DP+ASTV+MSTV+ALTV+MLTV+Width+Min+Max+Nmax+Nzeros+Mode+Mean+Median+Variance+Tendency, data = data) 
summary(model)

#install.packages("GA")
library("GA")

x <- model.matrix(model)
y <- model.response(model.frame(model))

fitness <- function(string){
  inc <- which(string == 1)
  X <- cbind(1,x[,inc])
  mod <- glm.fit(X,y)
  class(mod) <- "glm"
  -AIC(mod)
}


GA <- ga("binary",fitness = fitness, nBits = ncol(x), names = colnames(x), monitor = plot)
summary(GA)

par(mfrow=c(1,2))
plot(GA, main="Fitness of CTG feature")
plot(GA, main="Fitness of Diabetic feature")
