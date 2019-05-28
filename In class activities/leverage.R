
###### Leverage and influence #########

setwd("C:\\Users\\GSUProf\\Dropbox\\STAT 7134 Regression\\Data set")
#setwd("C:\\Users\\achatterjee\\Downloads")

cdata <- as.matrix(read.csv("crime_leverage.csv",header=TRUE,sep=","))
summary(cdata)
state <- cdata[,2]
crime <- cdata[,3]
murder <- cdata[,4]
pctmetro <- cdata[,5]
pctwhite <- cdata[,6]
pcths <- cdata[,7]
poverty <- cdata[,8]
single1 <- cdata[,9]

#### scatter plot 
pairs(~crime+pctmetro+pctwhite+pcths+poverty+single1,data=cdata, 
   main="Scatterplot Matrix")

##### Linear model 
model <- lm(crime~pctmetro+pctwhite+pcths+poverty+single1)

model1 <- lm(crime~pctmetro+poverty+single1)
summary(model1)

#### leverage
 lev <- hat(model.matrix(model1))
 plot(lev)
 cut_off <- 2*mean(lev)
 abline(h=cut_off,lty=2)
 identify(1,length(crime),lev,state)
 
  ### or ##
 x <- cbind(rep(1,length(crime)),pctmetro,poverty,single1)
 H <- x%*%solve(t(x)%*%x)%*%t(x)
 diag(H)
 
 cdata[lev>cut_off,]
 ind <- which(lev>cut_off)
 
##### scatter plot
 plot(pctmetro,crime)
 points(pctmetro[ind],crime[ind], col='red')
  
 plot(poverty,crime)
 points(poverty[ind],crime[ind], col='red')
 
 plot(single1,crime)
 points(single1[ind],crime[ind], col='red')
 
 #### standardized residual
 r <- stdres(model1)
 
#### Cook's Distance
  cook <- cooks.distance(model1)
  plot(cook)
  ind1 <- which(cook > 1)
  
  combnd <- cbind(cdata, cook, r)
  
  plot(lev,r,xlim=c(0,0.7),ylim=c(-4,4))
  points(lev,r,cex=10*sqrt(cook)/max(cook))
  abline(v=cut_off,lty=2)
  abline(h=c(-2,0,2), lty=2)
  text(lev,r, labels=row.names(cdata), pos = 4)

  
### Plot without influence and leverage 
 plot(pctmetro,crime)
 abline(lm(crime~pctmetro), lwd=2, lty=1, col=1)
 abline(lm(crime[-51]~pctmetro[-51]), lwd=2, lty=1, col=2)
 
#####  DFBETAS
 dbetas <- dfbetas(model1)
 plot(dbetas[,c(2,3)])
 text(lev,r, labels=row.names(cdata), pos = 4)
 
  
  
  
  
 
  
 
 
 