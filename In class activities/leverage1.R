
###### Leverage and influence #########

setwd("C:\\Users\\GSUProf\\Dropbox\\STAT 7134 Regression\\Data set")
#setwd("C:\\Users\\achatterjee\\Downloads")

cdata <- read.csv("crime_leverage.csv",header=TRUE,sep=",")
summary(cdata)
state <- cdata[,2]
crime <- cdata[,3]
murder <- cdata[,4]
pctmetro <- cdata[,5]
pctwhite <- cdata[,6]
pcths <- cdata[,7]
poverty <- cdata[,8]
single1 <- cdata[,9]

variable.names <- c("pctmetro","pctwhite","pcths","poverty","single1")
row.names(cdata) <- state

#### scatter plot 
pairs(~crime+pctmetro+pctwhite+pcths+poverty+single1,data=cdata, 
   main="Scatterplot Matrix")

   op <- par(mfrow=c(3,2))
for(i in 1:5)  { 
plot(cdata[,i+4],crime,xlab=variable.names[i],ylim=c(0,3500))
points(cdata[row.names(cdata)==" hi ",i+4],crime[row.names(cdata)==" hi "],col='red')
points(cdata[row.names(cdata)==" dc ",i+4],crime[row.names(cdata)==" dc "],col='blue')
text(cdata[,i+4],crime, labels=row.names(cdata), pos = 4)  }
par(op)

##### Linear model 
model <- lm(crime~pctmetro+pctwhite+pcths+poverty+single1)

#### leverage
 lev <- hat(model.matrix(model))
 plot(lev, xlim=c(0,70))
 cut_off <- 2*mean(lev)
 abline(h=cut_off,lty=2)
 text(c(1:length(crime)),lev, labels=row.names(cdata), pos = 4)
 
  ### or ##
 x <- cbind(rep(1,length(crime)),pctmetro,pctwhite,pcths,poverty,single1)
 H <- x%*%solve(t(x)%*%x)%*%t(x)
 diag(H)
 
 cdata[lev>cut_off,]
 ind <- which(lev>cut_off)
 

## Residual analysis 
res <- residuals(model)
r <- rstandard(model)
rstud <- rstudent(model)

op <- par(mfrow=c(3,2))
for(i in 1:5)  { 
#plot(cdata[,i+4],res,xlab=variable.names[i])
plot(cdata[,i+4],rstud,col='red',xlab=variable.names[i])
points(cdata[,i+4],r,col='blue')
text(cdata[,i+4],rstud, labels=row.names(cdata), pos = 4)  }
par(op)

op <- par(mfrow=c(3,2))
for(i in 1:5)  { 
#plot(cdata[,i+4],res,xlab=variable.names[i])
plot(cdata[,i+4],rstud-r,col='red',xlab=variable.names[i])
text(cdata[,i+4],rstud-r, labels=row.names(cdata), pos = 4)  }
par(op)
 
#### Cook's Distance
  cook <- cooks.distance(model)
  plot(cook)
  abline(h=1,lty=2)
 text(c(1:length(crime)),cook, labels=row.names(cdata), pos = 4)
 
  ind1 <- which(cook > 1)
 
 op <- par(mfrow=c(2,1))
  plot(lev, xlim=c(0,70))
 cut_off <- 2*mean(lev)
 abline(h=cut_off,lty=2)
 text(c(1:length(crime)),lev, labels=row.names(cdata), pos = 4)
 
 plot(cook)
  abline(h=1,lty=2)
 text(c(1:length(crime)),cook, labels=row.names(cdata), pos = 4)
 par(op)
  
 combnd <- cbind(cook,lev,rstud)
 
 ### plotting leverage, r-student residual and cook's D 
  plot(lev,r,xlim=c(0,0.7),ylim=c(-4,4))
  points(lev,r,cex=10*sqrt(cook)/max(cook))
  abline(v=cut_off,lty=2)
  abline(h=c(-2,0,2), lty=2)
  text(lev,r, labels=row.names(cdata), pos = 4)

cook[51]
p <- 6
df.res <- length(crime)-p
pf(cook[51],p,df.res,lower.tail=TRUE)
 
#####  DFBETAS
 dbetas <- dfbetas(model)
 abs(dbetas)>=2/sqrt(length(crime))
 plot(c(1:length(crime)),dbetas[,c(2,3)],xlim=c(min(dbetas[,2]),max(dbetas[,2])+1))
 text(dbetas[,2],dbetas[,3], labels=row.names(cdata), pos = 4)
 
op <- par(mfrow=c(3,2))
for(i in 1:5)  { 
plot(c(1:length(crime)),dbetas[,i+1],xlab=variable.names[i],xlim=c(0,54))
abline(h=2/sqrt(length(crime)))
abline(h=-2/sqrt(length(crime)))
text(c(1:length(crime)),dbetas[,i+1], labels=row.names(cdata), pos = 4)  }
par(op)

##### Delete DC
model1 <- lm(crime[-51]~pctmetro[-51]+pctwhite[-51]+pcths[-51]+poverty[-51]+single1[-51]) 
model2 <- lm(crime[-c(11,51)]~pctmetro[-c(11,51)]+pctwhite[-c(11,51)]+pcths[-c(11,51)]+poverty[-c(11,51)]+single1[-c(11,51)])

model3 <- lm(crime[-c(11,51)]~pctmetro[-c(11,51)]+pctwhite[-c(11,51)]+poverty[-c(11,51)]+single1[-c(11,51)])
model.final <- lm(crime[-c(11,51)]~pctmetro[-c(11,51)]+poverty[-c(11,51)]+single1[-c(11,51)])
 
 model1 <- lm(crime~pctmetro+poverty+single1)
summary(model1)
 








##### scatter plot


 plot(pctmetro,crime)
 points(pctmetro[ind],crime[ind], col='red')
  
 plot(poverty,crime)
 points(poverty[ind],crime[ind], col='red')
 
 plot(single1,crime)
 points(single1[ind],crime[ind], col='red')
 
 #### standardized residual
 r <- stdres(model1)

  
### Plot without influence and leverage 
 plot(pctmetro,crime)
 abline(lm(crime~pctmetro), lwd=2, lty=1, col=1)
 abline(lm(crime[-51]~pctmetro[-51]), lwd=2, lty=1, col=2)
 
#####  DFBETAS
 dbetas <- dfbetas(model1)

 plot(dbetas[,c(2,3)])
 text(pctmetro,r, labels=row.names(cdata), pos = 4)
 
  
  
  
  
 
  
 
 
 