#Matthew Anderson
#HW #3
#11.7.2018


#this library will enable the read_csv
library(readr)
hospital <- read_csv("~/VCU/Biomedical_Data_1/HW3/hospital.csv")

library(car)
library(plyr)

#full model using lm function 
full_model<-lm(hospital$InfctRsk~hospital$Stay + hospital$Age + hospital$Culture + hospital$Xray
                 + hospital$Beds +  factor(hospital$MedSchool) + factor(hospital$Region)
                 + hospital$Census + hospital$Nurses)

summary(full_model)
vif(full_model)


#assesment of normality of the response variable infection risk 
#plot qq plot of residuals of response variable 
plot(full_model, which=1)
plot(full_model, which=2)

resid(full_model) #List of residuals
plot(density(resid(full_model))) #A density plot
qqnorm(resid(full_model)) # A quantile normal plot - good for checking normality
qqline(resid(full_model))

#Test for normality of response variable
shapiro.test(resid(full_model))

#Correlation between predictors to determine colinearity 
corrs<-cor(hospital,method = "spearman")
corrs[lower.tri(corrs)] <- 0
corrs[,10:11]

cor.test(hospital$Census,hospital$Beds,method = "spearman")
cor.test(hospital$Nurses,hospital$Beds,method = "spearman")
cor.test(hospital$Nurses,hospital$Census,method = "spearman")
#looks like there is high corelation between (census,beds), and (Nurses,Beds), (Nurses Census)
#want to remove at least 2 of them, but which ones to remove? 

#lets look at the vif
vif(full_model)



#step wise regression 

#I have a hunch to take out beds and censuss to account for vif and colinearity 
library(MASS)

stepwise_forward_reg<- stepAIC(full_model, direction="forward")
stepwise_forward_reg$anova 

stepwise_backward_reg <- stepAIC(full_model, direction="backward")
stepwise_backward_reg$anova # display results

stepwise_both_reg <- stepAIC(full_model, direction="both")
stepwise_both_reg$anova # display results

best_model<-lm(hospital$InfctRsk ~ hospital$Stay + hospital$Culture + hospital$Xray + 
                 factor(hospital$MedSchool) + factor(hospital$Region) + hospital$Nurses)


library(leaps)
cp<-leaps( x=hospital[,3:11], y=hospital[,2], names=names(hospital)[3:11], method="Cp")
cp$Cp

best_model<-lm(hospital$InfctRsk ~ hospital$Stay + hospital$Culture + hospital$Xray + 
                 factor(hospital$MedSchool) + factor(hospital$Region) + hospital$Nurses)
AIC(best_model)

summary(best_model)
anova(best_model)
vif(best_model)

best_model_residual<-resid(best_model)

plot(hospital$InfctRsk, best_model_residual, ylab="Residuals", xlab="infection risk", main="Hospital Infection Risk") 
abline(0, 0)   


full_model_residual<-resid(full_model)

plot(hospital$InfctRsk, full_model_residual, ylab="Residuals", xlab="infection risk", main="Hospital Infection Risk") 
abline(0, 0)   

#before
summary(full_model)
df_before<-data.frame("R_sqaured" = .568, "R_squared_adj" =.521, "AIC" =AIC(full_model),
                      "MSE"=mean(full_model$residuals^2))

#after  
summary(best_model)

df_after <- data.frame("R_sqaured" = .5629, "R_squared_adj" =.5293, "AIC" =AIC(best_model),
                "MSE"=mean(best_model$residuals^2))

