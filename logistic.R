##LOGISTIC REGRESSION:
####used for binary outccomes. eg win/loss alive/dead, etc
####in linear expected outcome = b0+b1x
####in logistic regression n=log(p/1-p)=b0+b1x
#### therefore the coefficients represent the change in log of odds for 1 unit increase in predictor.
#### p=e^n/1+e^n
#### THe outcomes follow bernoulli distribution with mean u.
#### Link fuction is log of the odds. n=log(u/1-u). called logit
#### then inverse logit is e^n/1+e^n
#### has an s-curve with fucntion e^n/1+e^n

head(ravensData)
fit<-glm(data=ravensData,ravenWin~ravenScore,family="binomial")
summary(fit)
##actual points
g<-ggplot(data=ravensData,aes(ravenScore,ravenWin))
g<-g+geom_point()
g<-g+geom_smooth(method="glm",method.args=list(family="binomial"))
g
##fitted curve.
plot(ravensData$ravenScore,fit$fitted,pch=19,col='blue')


coef(fit)
#0.105 change in log odds for every score more.
exp(coef(fit))
##11% or 1.11 increse in odds. for every score more.

