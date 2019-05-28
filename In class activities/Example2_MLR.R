## In-class activity: February 23,2017 ###

### Example 1

## The original data are in the form of
## the time taken (in minutes) for a production run, Y , and 
## the number of items produced, X , for 20 randomly selected orders

### Read dataset in R

setwd("C:\\Users\\GSUProf\\Dropbox\\STAT 7134 Regression\\Data set")
#setwd("C:\\Users\\achatterjee\\Downloads")

data_mlr <- as.matrix(read.csv("NFL_data.csv",header=TRUE,sep=","))[,c(1,3,8,9)]
games <- data_mlr[,1]
pass_yard <- data_mlr[,2]
rush_plays <- data_mlr[,3]
opp_rush <- data_mlr[,4]

x <- cbind(rep(1,length(games)),data_mlr[,-1])
p <- ncol(x)
n <- nrow(x)

########## 1. Scatter Plot Matrix ################
pairs(~games+pass_yard+rush_plays+opp_rush,data=data_mlr, 
   main="Scatterplot Matrix")
   
###### 2. Multiple linear regression model ########
model <- lm (games ~ pass_yard+rush_plays+opp_rush)
summary(model)

###### 3. ANOVA ########
anova(model)

###### 4. Partial F test #########
reduced <- lm(games ~ pass_yard+opp_rush)  #### without rush_palys
full <- lm(games ~ pass_yard+rush_plays+opp_rush) ### with all the covariates

anova(reduced,full)

######## 5. Drop pass_yard ########
model2 <- lm(games ~ rush_plays+opp_rush)
summary(model2)

######## 6. CIs for model parameters #######
require(MASS)

res <- residuals(model)
b <- c(model[[1]][1],model[[1]][2],model[[1]][3],model[[1]][4])

xpx_inv <- ginv(t(x)%*%x)
MSE <- sum(res^2)/(n-p)
C_jj <- diag(xpx_inv)

alpha <- 0.05
t.crtcl <- qt(alpha/2, df=n-p,lower.tail=FALSE) ## t-critical value

b_CIs <- matrix(NA, nrow=p, ncol=2)

for (i in 1:p) {
b_CIs[i,1] <- b[i]-t.crtcl*sqrt(MSE*C_jj[i])
b_CIs[i,2] <- b[i]+t.crtcl*sqrt(MSE*C_jj[i]) }

####### 7. Joint Confidence Region for X2 and X3 #######
install.packages("ellipse")
require(ellipse)

reduced_23 <-  lm(games ~ rush_plays+opp_rush)
plot(ellipse(reduced_23, which = c('rush_plays', 'opp_rush'), level = 0.95), type = 'l')
points(reduced_23$coefficients['rush_plays'], reduced_23$coefficients['opp_rush'])

####### 8. Joint Confidence Region for X1 and X3 #######

reduced_13 <-  lm(games ~ pass_yard+opp_rush)
plot(ellipse(reduced_13, which = c('pass_yard', 'opp_rush'), level = 0.95), type = 'l')
points(reduced_13$coefficients['pass_yard'], reduced_13$coefficients['opp_rush'])

op <- par(mfrow=c(1,2))
plot(ellipse(reduced_23, which = c('rush_plays', 'opp_rush'), level = 0.95), type = 'l')
points(reduced_23$coefficients['rush_plays'], reduced_23$coefficients['opp_rush'])

plot(ellipse(reduced_13, which = c('pass_yard', 'opp_rush'), level = 0.95), type = 'l')
points(reduced_13$coefficients['pass_yard'], reduced_13$coefficients['opp_rush'])
par(op)
