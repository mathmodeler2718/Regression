setwd("C:\\Users\\GSUProf\\Dropbox\\STAT 7134 Regression\\Data set")
#setwd("C:\\Users\\achatterjee\\Downloads")

## Set up the data
data_mlr <- read.csv("Hald Cement.csv",header=TRUE,sep=",")
resp <- data_mlr[,1]
x1 <- data_mlr[,2]
x2 <- data_mlr[,3]
x3 <- data_mlr[,4]
x4 <- data_mlr[,5]

x <- data_mlr[,-1]
p <- ncol(x)+1
n <- nrow(x)

cor(data_mlr)

#### Stepwise method
### The stepAIC function performs stepwise variable selection approach but the 
### the criterion can be either AIC or BIC. 
library(MASS)
fit <- lm(resp~x1+x2+x3+x4, data=data_mlr)
step1 <- stepAIC(fit, direction="forward")  ## forward selection
step1$anova # display results

fit2 <- lm(resp~x1+x2+x3+x4, data=data_mlr)
step2 <- stepAIC(fit2, direction="backward")  ## backward selection
step2$anova

fit3 <- lm(resp~x1+x2+x3+x4, data=data_mlr)
step3 <- stepAIC(fit3, direction="both")    ## stepwise selection
step3$anova


#### The "addterm" and "dropterm" function adds (or drops) one variable at a time
### the selection criterion can be set as "F" to implement partial F test. 

#### backward selection with dropterm
## starts with full model
fullmodel <- lm(resp~x1+x2+x3+x4, data=data_mlr)
dropterm( fullmodel, test = "F" )

model1 <- lm(resp~x1+x2+x4, data=data_mlr)
dropterm(model1, test = "F" )

model2 <- lm(resp~x1+x2, data=data_mlr)
dropterm(model2, test = "F" )

#### forward selection with addterm
## starts with null model
nullmodel <- lm(resp~1, data=data_mlr)
addterm( nullmodel, scope=fullmodel, test = "F" )

model1 <- lm(resp~x4, data=data_mlr)
addterm(model1, scope=fullmodel, test = "F" )

model2 <- lm(resp~x1+x4, data=data_mlr)
addterm(model2, scope=fullmodel, test = "F" )

model3 <- lm(resp~x1+x2+x4, data=data_mlr)
addterm(model3, scope=fullmodel, test = "F" )

#### stepwise with addterm and dropterm
### we can start with either full or null model. 
nullmodel <- lm(resp~1, data=data_mlr)
addterm( nullmodel, scope=fullmodel, test = "F" )

model1 <- lm(resp~x4, data=data_mlr)
addterm(model1, scope=fullmodel, test = "F" )

model2 <- lm(resp~x1+x4, data=data_mlr)
dropterm(model2, test = "F" )
addterm(model2, scope=fullmodel, test = "F" )

model3 <- lm(resp~x1+x2+x4, data=data_mlr)
dropterm(model3, test = "F" )

model4 <- lm(resp~x1+x2, data=data_mlr)
addterm(model4, scope=fullmodel, test = "F" )


#### This part of the code is to apply Mallows Cp as the model selection criterion. 
### You need to install the package "leaps". 
 
install.packages("leaps")
library(leaps)

out1 <- leaps( x=data_mlr[,2:5], y=data_mlr[,1], names=colnames(data_mlr)[2:5], method="Cp") 
## The other moethods are "adjr2" and "r2".

plot(out1$size, out1$Cp, log = "y", xlab = "p", ylab = expression (C[p]))
 lines(out1$size, out1$size)

### we can consider all the competing models with small Cp values and then use some other 
### model selction criterion or a cross validation method to select the best among them. 
comp.model <- out1$Cp < 10  ## competing models
comp.model <- cbind(out1$which[comp.model,], out1$Cp[comp.model])

### The other option would be selecting the one with the lowest Cp or with the 
##  smaler number of parameters.
best <- out1$Cp == min(out1$Cp)
best <- out1$which[best, ]
