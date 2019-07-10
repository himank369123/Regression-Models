#GLM
######The outcomes are exponentially distributed which inclues binomial,normal and poisson while in
###### linear models outcomes are only nomrally distributed
######Glm contains exponential distribution,linear predictor,link function.

##LOGISTIC REGRESSION:
####used for binary outccomes. eg win/loss alive/dead, etc
####in linear expected outcome = b0+b1x
####in logistic regression n=log(p/1-p)=b0+b1x
#### p=e^n/1+e^n
#### THe outcomes follow bernoulli distribution with mean u.
#### Link fuction is log of the odds. n=log(u/1-u). called logit
#### then inverse logit is e^n/1+e^n
#### has an s-curve with fucntion e^n/1+e^n

##POISOON REGRSSION:
#### THe outcome follow poission dist with mean u
#### link function is log(u)
#### log(m)=b0+b1x
#### e(b0+b1x)=m wher m is mean or geometric mean for outocomes.


var(rpois(1000,lambda = 3))


