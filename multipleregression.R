library(datasets)
data(swiss)
names(swiss)
fit<-lm(data=swiss,Fertility~.)
summary(fit)
#1% increase in agriculture is associated with 0.17% decrease in fertility keeping others constant.
#t vlaue tests Ho hypothesis if agriculture coefficient is 0. ie 0.17211-0/S.E
#p value give significance. in this case significant at 5% level. reject NUll hypothesis.

fit1<-lm(data=swiss,Fertility~Agriculture)
fit1
#this suggests 1% increase in agriculture is associated with 0.19 % increase in fertility.
#this is opposite of what observed before. this is simpson's paradox.
#therefore multiple regression is accurate when all necessary variables are included.

### FACTORE IN MULTIPLE REGRESSION:
#FACTORS GIVE RELATIVE COEFFICIENTS. THE INTERCEPT IS COEFF FOR ONE VARIABLE. AND OTHER COEFFS ARE RELATIVE TO IT.

data("InsectSprays")
library(ggplot2)
g<-ggplot(data=InsectSprays,aes(y=count,x=spray,fill=spray))
g<-g+geom_violin(colour="black",size=2)
g
fit<-lm(data=InsectSprays,count~spray)
summary(fit)
#14.5 is mean for spray A. 
#0.833 is the diffrenece between spray B and spray A
#SPRAY X = SPRAY A + COEFF
#p value tests wheather a spray mean is equal to mean of SPRAY A

#remove intercept by -1 in lm
#here p value shows if a spray mean is equal to 0
fit2<-lm(data=InsectSprays,count~spray-1)
summary(fit2)
#adjustment in multiple regression means accounting for cofounding variable(s)
# we can change reference level by using relevel()
################################################################################
# y=b+mx1
data(swiss)
swiss
library(dplyr)
#creating dummy variable for catholic.
swiss<-mutate(swiss,Catholicbin=1*(Catholic>50))
swiss$Catholicbin<-factor(swiss$Catholicbin)
fit<-lm(data=swiss,Fertility~Agriculture)
intercept<-coef(fit)[1]
slope<-coef(fit)[2]
g<-ggplot(data=swiss,aes(x=Agriculture,y=Fertility,color=Catholicbin))
g<-g+geom_point(size=5,colour="black")+geom_point(size=3)
g<-g+geom_abline(intercept = intercept,slope=slope,size=2)
g
#####################
# y=b+m1x1+m2x2
fit<-lm(data=swiss,Fertility~Agriculture+Catholicbin)
summary(fit)
coef(fit)
#fertility increases by 0.124 for every 1 increase in agriculture.
#p values suggest that agriculture isnt statistically significant, catholicbin is statistically significant.
#here the slope is same. catholicbin1 represents change in intercept for catholicbin1. i.e change in fertility from protestent to catholic
g<-ggplot(data=swiss,aes(x=Agriculture,y=Fertility,color=Catholicbin))
g<-g+geom_point(size=5,color='black')+geom_point(size=3)
g<-g+geom_abline(intercept = coef(fit)[1],slope=coef(fit)[2],color='black',size=2)+geom_abline(intercept = coef(fit)[1],slope=coef(fit)[2],color='salmon',size=1)
g<-g+geom_abline(intercept = coef(fit)[1]+coef(fit)[3],slope=coef(fit)[2],color='black',size=2)+geom_abline(intercept = coef(fit)[1]+coef(fit)[3],slope=coef(fit)[2],color='cyan',size=1)
g
############################
# if we want different slopes different intercepts we need  an asteric (*) in model lm.
# this shows interaction between x1 & x2. if slopes are different then there is interaction.
#interaction would mean that agriculture has different affect on fertility depending on wheather they are catholic or protestent.
# y=b+m1x1+m2x2+m3x1x2
fit<-lm(data=swiss,Fertility~Agriculture*factor(Catholicbin))
summary(fit)
coef(fit)
#fertility changes by 0.096 for every unit increase in agriculture for catholicbin0
#fertility changes by 0.096+0.089 for every unit increase in agriculture for catholicbin1 
#p values suggest that none of the predictors are statistically significant and even the interaction isnt.
g<-g<-ggplot(data=swiss,aes(x=Agriculture,y=Fertility,color=Catholicbin))
g<-g+geom_point(size=5,color='black')+geom_point(size=3)
g<-g+geom_abline(intercept = coef(fit)[1],slope=coef(fit)[2],color='black',size=2)+geom_abline(intercept = coef(fit)[1],slope=coef(fit)[2],color='salmon',size=1)
g<-g+geom_abline(intercept = coef(fit)[1]+coef(fit)[3],slope=coef(fit)[2]+coef(fit)[4],color='black',size=2)+geom_abline(intercept = coef(fit)[1]+coef(fit)[3],slope=coef(fit)[2]+coef(fit)[4],color='cyan',size=1)
g

#note: interaction simply means the effect of a third variable on outcome for a predictor.
#meanig, does the outcome change for same value of predictor but different interaction variable value.