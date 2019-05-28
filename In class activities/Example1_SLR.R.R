
#### Simple Linear Regression Example #####
### We will use the computer repair data. In this study a random sample of 
### service call records for a computer repair operation were examined and 
### the length of each call (in minutes) and the number of components repaired 
### or replaced were recorded.

minutes <- c(23, 29, 49, 64, 74, 87, 96, 97, 109, 119, 149, 145, 154, 166)
units <- c(1,2,3,4,4,5,6,6,7,8,9,9,10, 10)

## scatterplot 

plot(units,minutes)

model = lm(minutes ~ units)

model ## to display the coefficients

summary(model) ## to display the complete analysis. 
abline(model) ## To add the fitted regression line

abline(lm(minutes~0+units),lty="dotted", col="red")

############ Error sum of squares ############
b0 <- seq(-2,10, by =0.5)
b1 <- seq(-1,20,by=0.1)
S <- matrix(NA, nrow=length(b0), ncol=length(b1))

for(i in 1:length(b0)){
 for (j in 1:length(b1)){
  S[i,j] <- sum((minutes-b0[i]-b1[j]*units)^2)}}

##### plot S to find the global minima ######

 install.packages("plot3D")
 library(plot3D)

persp3D(z=S, b0, b1)
persp3d(b0,b1,S)




