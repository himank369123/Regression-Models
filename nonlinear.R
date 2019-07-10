#when data doesnt follow a complete linear trend we must resort to non linear regression. for E.G:
library(tidyverse)
library(caret)
data('Boston',package = "MASS")
head(Boston)
training.samples<-Boston$medv %>% createDataPartition(p=0.8,list=F)
train.data<-Boston[training.samples,]
test.data<-Boston[-training.samples,]
ggplot(train.data, aes(lstat, medv) ) +
        geom_point() +
        stat_smooth()
#clearly the data isnt linear.
#Linear model:
head(train.data)
lmfit<-lm(data=train.data,medv~lstat)
summary(lmfit)
g<-ggplot(data=train.data,aes(lstat,medv))
g<-g+geom_point(col='blue',size=2)
g<-g+geom_smooth(method="lm",se=F,color='red',lwd=2)
g
#Linear model performance:
predictions<-predict(lmfit,newdata=test.data)
r2<-R2(predictions,test.data$medv)
rmse<-RMSE(predictions,test.data$medv)
linear<-cbind(r2,rmse)
linear
#Non Linear regression:
# 1) Polynomial regression
# 2) log transformation
# 3) spline regression
#1) POLYNOMIAL REGRESSION:
pn2fit<-lm(data=train.data,medv~lstat+I(lstat^2))
#OR
pn2fit<-lm(data=train.data,medv~poly(lstat,2,raw=FALSE))

summary(pn2fit)
#pvalues indicate both degrees significant
g<-ggplot(data=train.data,aes(lstat,medv))
g<-g+geom_point(size=2,col='blue')
g<-g+geom_smooth(method="lm",formula = y~poly(x,2,raw=FALSE),se=FALSE,col='red',size=2)
g
predictions<-predict(pn2fit,newdata = test.data)
##model performance:
r2<-R2(predictions,test.data$medv)
rmse<-RMSE(predictions,test.data$medv)
twoorderpol<-cbind(r2,rmse)
twoorderpol
##similarly for 6 order polynomial:
pn6fit<-lm(data=train.data,medv~poly(lstat,6,raw=FALSE))
summary(pn6fit)
#pvalues indicate only degrees upto 5 are significant i.e can discard 6th degree term.
g<-ggplot(data=train.data,aes(lstat,medv))
g<-g+geom_point(size=2,col='blue')
g<-g+geom_smooth(method="lm",formula = y~poly(x,6,raw=FALSE),se=FALSE,col='red',size=2)
g
predictions<-predict(pn6fit,newdata = test.data)
##model performance:
r2<-R2(predictions,test.data$medv)
rmse<-RMSE(predictions,test.data$medv)
sixorderpol<-cbind(r2,rmse)
sixorderpol

#2) Log transformation:
logfit<-lm(data=train.data,medv~log(lstat))
summary(logfit)
g<-ggplot(data=train.data,aes(lstat,medv))
g<-g+geom_point(size=2,col='blue')
g<-g+geom_smooth(method="lm",formula = y~log(x),se=FALSE,col='red',size=2)
g
predictions<-predict(logfit,newdata = test.data)
##model performance:
r2<-R2(predictions,test.data$medv)
rmse<-RMSE(predictions,test.data$medv)
logtrans<-cbind(r2,rmse)
logtrans

#3) spline regression:
#use knots to model non linear trends.
library(splines)
knots<-quantile(train.data$lstat,p=c(0.25,0.5,0.75))
splinefit<-lm(medv~bs(lstat,knots=knots),data=train.data)
summary(splinefit)
g<-ggplot(data=train.data,aes(lstat,medv))
g<-g+geom_point(size=2,col='blue')
g<-g+geom_smooth(method="lm",formula = y~bs(x,df=3),se=FALSE,col='red',size=2)
g
predictions<-predict(splinefit,newdata = test.data)
##model performance:
r2<-R2(predictions,test.data$medv)
rmse<-RMSE(predictions,test.data$medv)
spline<-cbind(r2,rmse)
spline
##mgcv provides generalized additive models to fit spline regression autommatically without specifying knots

library(mgcv)
mgcvfit<-gam(medv~s(lstat),data=train.data)
summary(mgcvfit)
g<-ggplot(data=train.data,aes(lstat,medv))
g<-g+geom_point(size=2,col='blue')
g<-g+geom_smooth(method="gam",formula = y~s(x),se=FALSE,col='red',size=2)
g
predictions<-predict(mgcvfit,test.data)
r2<-R2(predictions,test.data$medv)
rmse<-RMSE(predictions,test.data$medv)
gam<-cbind(r2,rmse)
gam

df<-rbind(linear,twoorderpol,sixorderpol,logtrans,spline,gam)
rownames(df)<-c('linear','twoorderpol','sixorderpol','logtrans','spline','gam')
df

#clearly spline regression,general additive model and polynomial regression outperformed linear model and logtransformation.
