#model selection
##In Multiple linear regression model selection can be a huge question
## a model with two categories same slope but different intercepts.i.e. multiple regresiion
##a model with two categories different slopes and different intercepts i.e with interaction

##which variables to include?
## including unnecessary(corelated) variables inflates variation. check with vif in car library
## excluding necessary variables creates bias.

data(swiss)
library(car)
fit<-lm(data=swiss,Fertility~.)
vif(fit)
## this means.by the inclusion of agriculture the variance inflates by 2.28 times than it would if agriculture was uncorelated.
##note vif for infant.mortality is only 1 since its mostly uncorelated with other variables.
##therefore always include variables with less variance inflation factor (Vif)
## we can conduct nested variable models and then anova over them as below:
fit1<-lm(data=swiss,Fertility~Agriculture)
fit2<-lm(data=swiss,Fertility~Agriculture+Education+Examination)
fit3<-lm(data=swiss,Fertility~.)
anova(fit1,fit2,fit3)
##this suggests that DF 2 variables added in each model.
##pr(>F) indicated that the variables are significant hence important for model.
##use annova between nested models to see significance.
