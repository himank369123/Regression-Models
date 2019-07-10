library(ggplot2)
library(UsingR)
data(diamond)
fit<-lm(data=diamond,price~carat)
rm(g)
g<-ggplot(data=diamond,aes(x=carat,y=price))
g<-g+geom_point(aes(diamond$carat,diamond$price))
g<-g+geom_smooth(method="lm")

g

plot(x=diamond$carat,y=diamond$price,pch=16)
abline(fit,lwd=2)
yhat<-predict(fit)
x=diamond$carat;y=diamond$price
n<-length(y)
for(i in 1:n){
        lines(c(x[i],x[i]),c(y[i],yhat[i]),col="red",lwd=2 )
}
e<-resid(fit)
plot(x,e)
abline(h=0)
for (i in 1:n){
        lines(c(x[i],x[i]),c(0,e[i]),col="red")
}


##not correct model. but does explain a linear trend. no model is useless.

x<-runif(100,-3,3)
y<-x+sin(x)+rnorm(100,0.1,0.1)
fit<-lm(y~x)
rm(g)
g<-ggplot(data=data.frame(x=x,y=y),aes(x,y))
g<-g+geom_point(size=3,col="black",alpha=0.2)
g<-g+geom_point(size=2,col="red",alpha=0.3)
g<-g+geom_smooth(method="lm")
g

#now we can use residual to highlight a trend that is not explained by linearity by residual plot:

e<-resid(fit)
plot(x,e)
abline(h=0)

#this explains the sin trend using residual variation from linear model.


#Heteroscedasticity is unequal variance which can be emphasized using residual plots.
#If data follows heterosedasticity linear model results may become less precise.
#to correct heteroscedasticity we can:
                                        #1)Redifine variables. for eg numbers to rates or proportions.                
                                        #2)use weighted regression
                                        #3)use transformation on predictor variable
#basically linear model predicts good at low x values but more variation at big x values.
x<-runif(100,0,0.6)
y<-x+rnorm(100,mean=0,sd=.001*x)
plot(x,y)
fit<-lm(y~x)
abline(fit)
e<-resid(fit)
plot(x,e)
abline(h=0)




