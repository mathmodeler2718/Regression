     ##### Indicator Variable #####
  #### Variable description ####
  
  ## This data frame contains the following columns:
 ### 1. type (qualitative explanatory)  
 ### Type of occupation. A factor with the following levels: prof, professional and managerial; wc, white-collar; bc, blue-collar.
 ### 2. income (Quantitative explanatory)
 ### Percent of males in occupation earning $3500 or more in 1950.
 ### education (Quantitative explanatory)
 ### Percent of males in occupation in 1950 who were high-school graduates.
 ### prestige (Response)
 ### Percent of raters in NORC study rating occupation as excellent or good in prestige.  
 
 
install.packages("car")
library(car)

##Prestige data 

data_use <- Prestige

 ### Cleaning the data: Deleting rows with NA
 data_use <- data_use[!is.na(data_use[,6]),]
 education <- data_use[,1]
 income <- data_use[,2]
 prestige <- data_use[,4]
 type <- data_use[,6]
 percent_women <- data_use[,3]
 
 ##### scatter plot 
 pairs(~prestige+education+income+percent_women,data=data_use, 
   main="Scatterplot Matrix")
 
 #### Scatter plot with indicator variable  
 plot(prestige,education, pch=unclass(type))  # different symbol
 legend("bottomright", legend=levels(type), pch=c(1:3))
 
 plot(prestige,income, pch=unclass(type))  # different symbol
 legend("bottomright", legend=levels(type), pch=c(1:3))
  
 ###  Model Fitting
 
 model1 <- lm(prestige~education+income+percent_women+factor(type))
 summary(model1)
 
 ### percent of women is not significant 
 model2 <- lm(prestige~education+income+factor(type))
 summary(model2)
 
 ### H0: concurrent model
  ### H0: beta_prof=0, beta_wc=0; testing the hypothesis that type is not an important variable
  
  concrnt_model <- lm(prestige~education+income+percent_women)
  anova(red_model,concrnt_model)
  
  ### Intercept model 
  full_model <- lm(prestige~education+income+factor(type)+education*factor(type)+income*factor(type))
  
  ### H0: parallel line
  
  parl_model <- lm(prestige~education+income+factor(type))
  anova(full_model,parl_model)
  
 
 
 
 
 
 
 
 
  