        ################################
##### Logistic Regression ######
################################

#1. call data
#read the csv file directly from the website
admit<-read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

#take a look at the first few rows of the data
admit[1:10,]

#2. run Logistic regression model
#admit$rank <- factor(admit$rank)
admitlogit <- glm(admit ~ gre + gpa + factor(rank), data = admit, family = "binomial")
summary(admitlogit)
anova(admitlogit,test="Chi")

#3, interprete logistic regression result
exp(admitlogit$coef)

##4. Plotting the probability plots

#gre_range <- seq(from=min(admit[,2]), to=max(admit[,2]), by=0.01)
gpa_range <- seq(from=min(admit[,3]), to=max(admit[,3]), by=0.01)
#gpa_mean <- mean(admit[,3])
gre_mean <- mean(admit[,3])

### Predicted probabilities for rank 1

pred.logits1 <- admitlogit$coef[1]+admitlogit$coef[2]*gre_mean+ admitlogit$coef[3]*gpa_range

### Predicted probabilities for rank 2
pred.logits2 <- (admitlogit$coef[1]+admitlogit$coef[4])+admitlogit$coef[2]*gre_mean+ admitlogit$coef[3]*gpa_range

### Predicted probabilities for rank 3
pred.logits3 <- (admitlogit$coef[1]+admitlogit$coef[5])+admitlogit$coef[2]*gre_mean+ admitlogit$coef[3]*gpa_range

### Predicted probabilities for rank 4
pred.logits4 <- (admitlogit$coef[1]+admitlogit$coef[6])+admitlogit$coef[2]*gre_mean+ admitlogit$coef[3]*gpa_range

# Compute the probibilities (this is what will actually get plotted):
rank1_probs <- exp(pred.logits1)/(1 + exp(pred.logits1))
rank2_probs <- exp(pred.logits2)/(1 + exp(pred.logits2))
rank3_probs <- exp(pred.logits3)/(1 + exp(pred.logits3))
rank4_probs <- exp(pred.logits4)/(1 + exp(pred.logits4))

### Plot
plot(gpa_range,rank1_probs,ylim=c(0,1),
     type='l', 
     lwd=2, 
     lty=1, 
     col="gold", 
     xlab="gpa", ylab="P(outcome)", main="Predicted Probability")
     
lines(gpa_range,rank2_probs,ylim=c(0,1),
     type='l', 
     lwd=2, 
     lty=1, 
     col="blue", 
     xlab="gpa", ylab="P(outcome)", main="Predicted Probability") 

lines(gpa_range,rank3_probs,ylim=c(0,1),
     type='l', 
     lwd=2, 
     lty=1, 
     col="red", 
     xlab="gre", ylab="P(outcome)", main="Predicted Probability")   
     
lines(gpa_range,rank4_probs,ylim=c(0,1),
     type='l', 
     lwd=2, 
     lty=1, 
     col="green", 
     xlab="gpa", ylab="P(outcome)", main="Predicted Probability") 

abline(h=0.5, lty=2)

##################################
###### Model with interaction 
##################################

admitlogit2 <- glm(admit ~ gre + gpa + factor(rank)+gre*factor(rank)+gpa*factor(rank), data = admit, family = "binomial")
summary(admitlogit2)
anova(admitlogit2,test="Chi")

#3, interprete logistic regression result
exp(admitlogit2$coef)

##4. Plotting the probability plots

#gre_range <- seq(from=min(admit[,2]), to=max(admit[,2]), by=0.01)
#gpa_range <- seq(from=min(admit[,3]), to=max(admit[,3]), by=0.01)
#gpa_mean <- mean(admit[,3])

### Predicted probabilities for rank 1

pred.logits1 <- admitlogit2$coef[1]+admitlogit2$coef[2]*gre_mean+ admitlogit2$coef[3]*gpa_range

### Predicted probabilities for rank 2
pred.logits2 <- (admitlogit2$coef[1]+admitlogit2$coef[4])+(admitlogit2$coef[2]+admitlogit2$coef[7])*gre_mean+(admitlogit2$coef[3]+admitlogit2$coef[10])*gpa_range

### Predicted probabilities for rank 3
pred.logits3 <- (admitlogit2$coef[1]+admitlogit2$coef[5])+(admitlogit2$coef[2]+admitlogit2$coef[8])*gre_mean+(admitlogit2$coef[3]+admitlogit2$coef[11])*gpa_range

### Predicted probabilities for rank 4
pred.logits4 <- (admitlogit2$coef[1]+admitlogit2$coef[6])+(admitlogit2$coef[2]+admitlogit2$coef[9])*gre_mean+(admitlogit2$coef[3]+admitlogit2$coef[12])*gpa_range

# Compute the probibilities (this is what will actually get plotted):
rank1_probs <- exp(pred.logits1)/(1 + exp(pred.logits1))
rank2_probs <- exp(pred.logits2)/(1 + exp(pred.logits2))
rank3_probs <- exp(pred.logits3)/(1 + exp(pred.logits3))
rank4_probs <- exp(pred.logits4)/(1 + exp(pred.logits4))


### Plot
plot(gpa_range,rank1_probs,ylim=c(0,1),
     type='l', 
     lwd=2, 
     lty=1, 
     col="gold", 
     xlab="gpa", ylab="P(outcome)", main="Predicted Probability")
     
lines(gpa_range,rank2_probs,ylim=c(0,1),
     type='l', 
     lwd=2, 
     lty=1, 
     col="blue", 
     xlab="gpa", ylab="P(outcome)", main="Predicted Probability") 

lines(gpa_range,rank3_probs,ylim=c(0,1),
     type='l', 
     lwd=2, 
     lty=1, 
     col="red", 
     xlab="gpa", ylab="P(outcome)", main="Predicted Probability")   
     
lines(gpa_range,rank4_probs,ylim=c(0,1),
     type='l', 
     lwd=2, 
     lty=1, 
     col="green", 
     xlab="gpa", ylab="P(outcome)", main="Predicted Probability") 

abline(h=0.5, lty=2)





