
setwd("C:\\Users\\GSUProf\\Dropbox\\STAT 7134 Regression\\Data set")
library(car)
#setwd("C:\\Users\\achatterjee\\Downloads")

data_mlr <- as.matrix(read.csv("NFL_data.csv",header=TRUE,sep=","))[,c(1,3,8,9)]
games <- data_mlr[,1]
pass_yard <- data_mlr[,2]
rush_plays <- data_mlr[,3]
opp_rush <- data_mlr[,4]

x <- data_mlr[,-1]
p <- ncol(x)
n <- nrow(x)

colnames(x) <- c("pass_yard", "rush_plays", "opp_rush")

pairs(~pass_yard+rush_plays+opp_rush,data=data_mlr, 
   main="Scatterplot Matrix")

model <- lm (games ~ pass_yard+rush_plays+opp_rush)
summary(model)

cor(x)

det(cor(x))

### VIF ###

part.reg <- rep(NA, p)
for (i in 1:p) {
part.reg[i] <- summary(lm(x[,i]~x[,-i]))$r.squared }
vifct <- 1/(1-part.reg)
vif(model)

#### CN and CI ####
xpx <- t(x)%*%x

############ Example 2: Gasoline Milleage Data ########

data_gasoline <- as.matrix(read.csv("data-table-B3_gasoline milleage.csv", header=TRUE, sep=","))[,-12]

x <- data_gasoline[-c(23,25),-1]

cor(x)
det(cor(x))

xpx <- t(x)%*%x
eigen(xpx)

CIndx <- max(eigen(xpx)[[1]])/eigen(xpx)[[1]]

which.max(max(eigen(xpx)[[1]])/eigen(xpx)[[1]])




