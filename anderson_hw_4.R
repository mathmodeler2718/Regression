#Matthew Anderson
#HW #4
#11.29.2018

#this library will enable the read_csv
library(readr)
library(car)
monkeys <- read_csv("~/VCU/Biomedical_Data_1/HW4/monkeys.csv", col_names = TRUE)
monkeys<-as.data.frame(monkeys)



#first I want to just do a visual inspection of the data

boxplot(monkeys$Response ~ monkeys$Group,
        main="Immune Function by Treatment",
        ylab="Immune Function",
        ylim=c(9, 14),
        col="grey",
        boxwex=0.3)


# 1 way ANOVA Model 
model1 <- lm(monkeys$Response ~ monkeys$Group, data = monkeys)
anova(model1)
summary(model1)

#assumptions of normality by checking QQ plot of residuals 
plot(model1, which = 1) 
qqnorm(resid(model1)) 
qqline(resid(model1))

#Test for normality of response variable
shapiro.test(resid(model1))

#Test for homogeniety of varaince 
leveneTest(Response ~ Group, center = mean, data = monkeys)


# 2 way ANOVA Model

#here I am creating two variables that assing a 0 or 1 to either containing rhigf or rhgh
monkeys$rhgh  <- ifelse(monkeys$Group=="Control"|monkeys$Group=="rhIGF-I",0,1)  
monkeys$rhigf<- ifelse(monkeys$Group=="Control"|monkeys$Group=="rhGH",0,1)   

#running the model with the interaction term
model2<-lm(Response ~ rhigf + rhgh + rhigf*rhgh, data = monkeys)
anova(model2)

#contrast and summary
Anova(model2, contrasts = list(topic=contr.sum, sys=contr.sum, type=3))
summary(model2)

#It looks like they get the same p value as the first model



































