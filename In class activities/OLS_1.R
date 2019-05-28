## In-class activity: January19,2017 ###

### Example 1

## The original data are in the form of
## the time taken (in minutes) for a production run, Y , and 
## the number of items produced, X , for 20 randomly selected orders

### Read dataset in R

setwd("C:\\Users\\GSUProf\\Dropbox\\STAT 7134 Regression\\Data set")
#setwd("C:\\Users\\achatterjee\\Downloads")

data <- as.matrix(read.table("production.txt",header=TRUE))

x <- data[,2]
y <- data[,3]

#### scatterplot
plot(x,y)

cor(x,y)

model <- lm(y~x)

summary(model)
abline(model)

### model is a list

b0 <- model[[1]][1]
b1 <- model[[1]][2]

### predicted responses

y.predicted <- b0+b1*x

points(x,y.predicted, col='red')

residual <- y-y.predicted

## standardized residual 

stnd.residual <- residual/sqrt(var(residual))
stnd.residual1 <- residual/sqrt(y.predicted)

### can be calculated by "rstandard(model)" ###

sum(residual)

### Goodness of Fit ###
R2 <- 1-(sum(residual^2)/sum((y-mean(y))^2))

### Plot of residual vs x ###

plot(x,residual)
abline(h=0, col="red")

########################################################

###### EXAMPLE 2 

install.packages("car")
library(car)

##Prestige data 

data <- Prestige

## Response variable: prestige

education <- data[,1]
income <- data[,2]
prestige <- data[,4]

## scatter plot

op <- par(mfrow=c(1,2))
plot(education, prestige)
abline(model.edu, col='red')
plot(income, prestige)
abline(model.inc, col='red')
par(op)

## correlation analysis 
cor(education, prestige)
cor(income, prestige)

## OLS Model ##
model.edu <- lm(prestige~education)
model.inc <- lm(prestige~income)

summary(model.edu)
summary(model.inc)

### predicted response 

b0.edu <- model.edu[[1]][1]
b1.edu <- model.edu[[1]][2]

b0.inc <- model.inc[[1]][1]
b1.inc <- model.inc[[1]][2]

ypred.edu <- b0.edu+b1.edu*education
ypred.inc <- fitted.values(model.inc)

### residuals 

res.edu <- prestige-ypred.edu
res.inc <- residuals(model.inc)

##### Residual Plot
op <- par(mfrow=c(1,2))
plot(education, res.edu)
abline(h=0, col='red')
plot(income, res.inc)
abline(h=0, col='red')
par(op)

###### Normality check 
op <- par(mfrow=c(1,2))
qqnorm(res.inc)
qqline(res.inc, col="red")
qqnorm(res.edu)
qqline(res.edu, col="red")
par(op)

######## Outlier Check 

stnd.res.edu <- res.edu/sqrt(var(res.edu)) ## standard residual
stnd.res.inc <- rstandard(model.inc)

summary(stnd.res.edu)
summary(stnd.res.inc)

### add the residual to the output data 
out.edu <- cbind(education,prestige,ypred.edu,stnd.res.edu)
out.inc <- cbind(income,prestige,ypred.inc,stnd.res.inc)

#### Delete datapoints with high stndandard residuals
#### The residual plot suggets, deleting data points with 
#### income higher than 1500

incdata.rev <- out.inc[out.inc[,1]<15000,]

income.rev <- incdata.rev[,1]
prestige.rev <- incdata.rev[,2]

## scatter plot

op <- par(mfrow=c(1,2))
plot(income, prestige)
#abline(model.inc, col='red')
plot(income.rev, prestige.rev)
#abline(model.inc, col='red')
par(op)

## correlation analysis 
cor(income, prestige)
cor(income.rev, prestige.rev)


## OLS Model ##
model.inc.rev <- lm(prestige.rev~income.rev)

summary(model.inc.rev)

### residuals 
res.inc.rev <- residuals(model.inc.rev)
stdres.inc.rev <- rstandard(model.inc.rev)

##### Residual Plot
op <- par(mfrow=c(1,2))
plot(income, res.inc)
abline(h=0, col='red')
plot(income.rev, res.inc.rev)
abline(h=0, col='red')
par(op)

###### Normality check 
qqnorm(stdres.inc.rev)
qqline(stdres.inc.rev, col="red")

##For this second part of the R-code we consider education as covariate##

############### ANOVA ################
anova(model.edu)

### T-CI for model parameters ###
 ### 1. slope parameter
 
 MSE <- sum(res.edu^2)/(length(education)-2)
 x.bar <- mean(education)
 sxx <- sum((education-x.bar)^2)
 
 alpha <- 0.05
 t.crtcl <- qt(alpha/2, df=length(education)-2,lower.tail=FALSE) ## t-critical value
 
 lb1 <- b1.edu-t.crtcl*sqrt(MSE/sxx)
 ub1 <- b1.edu+t.crtcl* sqrt(MSE/sxx)

 ### 2. Prediction Intervals ###
  ## A. Prediction of average value 
 
  PV_mean <- rep(NA, length(education))
  lb_mean <- rep(NA, length(education)) 
  ub_mean <- rep(NA, length(education))
  
  for (i in 1:length(education)){
   PV_mean[i] <-  MSE*(1/length(education)+(education[i]-x.bar)^2/sxx)
   lb_mean[i] <- b0.edu+b1.edu*education[i]-t.crtcl*sqrt(PV_mean[i])
   ub_mean[i] <- b0.edu+b1.edu*education[i]+t.crtcl*sqrt(PV_mean[i]) }
   
   ## B. Prediction of actual values 
   
  PV_av <- rep(NA, length(education))
  lb_av <- rep(NA, length(education)) 
  ub_av <- rep(NA, length(education))
  
  for (i in 1:length(education)){
   PV_av[i] <-  MSE*(1+1/length(education)+(education[i]-x.bar)^2/sxx)
   lb_av[i] <- b0.edu+b1.edu*education[i]-t.crtcl*sqrt(PV_av[i])
   ub_av[i] <- b0.edu+b1.edu*education[i]+t.crtcl*sqrt(PV_av[i]) }
  
  ######## Plot of prediction intervals ##########
  
    plot(education, prestige)
    abline(model.edu, col='red')
    lines(education, lb_mean, col="blue")
    lines(education, ub_mean, col="blue")
    lines(education, lb_av, col="green")
    lines(education, ub_av, col="green")
    




