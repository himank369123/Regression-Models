##POISOON REGRSSION:
#### THe outcome follow poission dist with mean u
#### link function is log(u)
#### log(u)=b0+b1x
#### e(b0+b1x)=u where u is mean or geometric mean for outocomes.

library(ggplot2)
gaData$julian<-julian(gaData$date)
head(gaData)
g<-ggplot(data=gaData,aes(x=julian,y=visits))
g<-g+geom_point()
g
##fitting linear model:
lmfit<-lm(data=gaData,visits~julian)
summary(lmfit)
#suggests 0.028% increase in visits per day.
g<-g+geom_smooth(method="lm",color='red',se=F,size=2)
g

## now fitting poisson regression model:
glmfit<-glm(data=gaData,visits~julian,family="poisson")
summary(glmfit)
exp(coef(glmfit))
## 0.002% incrase in visits per day.    
g<-g+geom_smooth(method="glm",method.args=list(family="poisson"),se=F,size=2)
g
## we can see a sligt curve in case of glm


##we can model proportions too. 
##for example proportion of webhits originating from simply statistics compared to toal visits.
## then we want to model log(simplystats/visits)=b0+b1x
##it turns out to be normal model +offset . where offset is log of denominator or total count.
glmfit2<-glm(data=gaData,simplystats~julian,offset=log(visits+1),family="poisson")
summary(glmfit2)
exp(coef(glmfit2))
plot(gaData$julian,glmfit2$fitted,col="blue",pch=19)
points(gaData$julian,glmfit$fitted,col="red",pch=19)
##glmfit vs glmfit2

##actual points of proportion vs model(line)
plot(gaData$julian,gaData$simplystats/(gaData$visits+1),col="grey")
lines(gaData$julian,glmfit2$fitted/(gaData$visits+1),col="blue")
        