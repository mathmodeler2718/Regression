
 ######## This R-code generates normally distributed and exponentially
 ######## distributed errors. 
 n <- 50
 x <- seq(0,2,length=n)
 y <- 3+2*x
 error<-rnorm(n,0,1)## normally distributed error
 y.new <- y+error
 

 error1 <- rexp(n,rate=1)### esponential errors
 y.new1 <- y+error1

 op <- par(mfrow=c(2,1))
 plot(x,y,ylim=c(min(y.new),max(y.new)))
 points(seq(0,2,length=n),y+error,col="red")

 plot(x,y,ylim=c(min(y.new1),max(y.new1)))
 points(seq(0,2,length=n),y+error1,col="blue")

 par(op)

 #############################

 ###### Linear and polynomial regression ######

 n <- 50
 x <- seq(0,2,length=n)
 b0 <- 3
 b1 <- 0.3
 b2 <- 1

 y <- b0+b1^2*x ##linear model

 y1 <- b0+log(b1)*x ## linear model

 y2 <- b0+b1*x+b2*x^2 ## polynomial model

 op <- par(mfrow=c(3,1))

 plot(x,y,main="b0+b1^2*x")

 plot(x,y1, col='red', main="b0+log(b1)*x")

 plot(x,y2, col="blue", main="b0+b1*x^2")
 par(op)
 

 
 

 
 

 
