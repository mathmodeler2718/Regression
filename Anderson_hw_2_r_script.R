#Matthew Anderson
#HW #2 
#10.26.2018

#here I am setting my working directory on my computer
setwd("~/VCU/Biomedical_Data_1/HW2")

#this library will enable the read_csv
library(readr)

#importing the gfr csv file
gfr <- read_csv("~/VCU/Biomedical_Data_1/HW2/gfr.csv", col_names = FALSE)

#getting each column and saving it as a vector 
dpta<-as.vector(as.numeric(t(gfr[,1])))
systc<-as.vector(t(gfr[,2]))
systc_inv<-as.vector(t(gfr[,3]))
systc_ln<-as.vector(t(gfr[,4]))

library(car)

#assesment of DTPA GFR
qqPlot(dpta, distribution="norm", pch=1, envelope = .95, id=FALSE,
 col.lines="Black", main="DTPA QQ Plot", ylab='DPTA GFR mL/min/1.73 m2', xlab='Normal Quantiles')

#Test for normality 
shapiro.test(dpta)

#assesment of Cystatin C
qqPlot(systc, distribution="norm", pch=1, envelope = .95, id=FALSE,
       col.lines="black", main="Cystatin C QQ Plot", ylab='Cystatin C levels mg/L', xlab='Normal Quantiles')

shapiro.test(systc)

#assesment of Inverse Cystatin C
qqPlot(systc_inv, distribution="norm", pch=1, envelope = .95, id=FALSE,
       col.lines="Black", main="Inverse Cystatin C QQ Plot", ylab='Inverse Cystatin C levels mg/L', xlab='Normal Quantiles')

shapiro.test(systc_inv)

#assesment of Ln Cystatin
qqPlot(systc_ln, distribution="norm", pch=1, envelope = .95, id=FALSE,
       col.lines="Black", main="Ln Cystatin C QQ Plot", ylab='Ln Cystatin C levels mg/L', xlab='Normal Quantiles')

shapiro.test(systc_ln)

#scatter plots 
plot(systc,dpta, main="Scatterplot of DPTA and Cystatin C ", xlab="Cystatin C levels mg/L",
     ylab="DPTA GFR mL/min/1.73 m2", pch = 20)

cor.test(systc,dpta, method = "spearman", alternative = "two.sided")

plot(systc_inv,dpta, main="Scatterplot of DPTA and Inverse Cystatin C ", xlab="Inv Cystatin C levels mg/L", 
     ylab="DPTA GFR mL/min/1.73 m2", pch = 20)

cor.test(systc_inv,dpta, method = "pearson", alternative = "two.sided")

plot(systc_ln,dpta, main="Scatterplot of DPTA and Ln Cystatin C ", xlab="Ln Cystatin C levels mg/L", 
     ylab="DPTA GFR mL/min/1.73 m2", pch = 20)

cor.test(systc_ln,dpta, method = "pearson", alternative = "two.sided")

#simple linear regression
systc_lr<- lm(dpta ~ systc)
systc_resid<-resid(systc_lr)
summary(systc_lr)
plot(systc, systc_resid, ylab="Residuals", xlab="Cystatin C", main="Residual Plot Cystatin C") 
abline(0, 0)   

systc_inv_lr<- lm(dpta ~ systc_inv)
systc_inv_resid<-resid(systc_inv_lr)
summary(systc_inv_lr)
plot(systc_inv, systc_inv_resid, ylab="Residuals", xlab="Inv Cystatin C", main="Residual Plot Inverse Cystatin C") 
abline(0, 0) 

systc_ln_lr<- lm(dpta ~ systc_ln)
systc_ln_resid<-resid(systc_ln_lr)
summary(systc_ln_lr)

plot(systc_ln, systc_ln_resid, ylab="Residuals", xlab="Ln Cystatin C", main="Residual Plot Ln Cystatin C") 
abline(0, 0) 

uc<- quantile(systc_ln, .75) + 1.5*IQR(systc_ln)


