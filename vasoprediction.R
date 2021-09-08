#ISYE 6420 FALL 2020 FINAL
#QUESTION 1 VASOCONSTRICTION

#installing necessary libraries
library(tidyverse)
library(rjags)
library(coda)

#Creating dataframe with given data
y <- c(1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,
0,1,0,0,0,0,1,0,1,0,1,0,1,0,0,1,1,1,0,0,1)

v <- c(3.7, 3.5, 1.25, 0.75, 0.8, 0.7, 0.6, 1.1, 0.9, 0.9, 0.8, 0.55, 0.6, 1.4, 
       0.75, 2.3, 3.2, 0.85, 1.7, 1.8, 0.4, 0.95, 1.35, 1.5, 1.6, 0.6, 1.8, 0.95, 
       1.9, 1.6, 2.7, 2.35, 1.1, 1.1, 1.2, 0.8, 0.95, 0.75, 1.3)

r <- c(0.825, 1.09, 2.5, 1.5, 3.2, 3.5, 0.75, 1.7, 0.75, 0.45, 0.57, 2.75, 3, 2.33, 3.75, 1.64, 1.6, 1.415,
1.06, 1.8, 2, 1.36, 1.35, 1.36, 1.78, 1.5, 1.5, 1.9, 0.95, 0.4, 0.75, 0.3, 1.83, 2.2, 2, 3.33, 1.9, 1.9, 1.625)

vaso <- data.frame(y,v,r)

head(vaso)

#Q1 part a, Transform covariates v and r as x1 =log(10×v), x2 =log(10×r).

vaso$v <- log(10* vaso$v)

vaso$r <- log10(10* vaso$r)

head(vaso)

#Q1 part b, Estimate posterior means for coefficients in the logit model. Use noninformative priors on all coefficients.

mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
        logit(p[i]) = int + b[1]*v[i] + b[2]*r[i] 
   
    }
    int ~ dnorm(0.0, 1.0/25.0)
    for (j in 1:2) {
        b[j] ~ ddexp(0.0, sqrt(2.0)) # has variance 1.0
    }
} "

set.seed(92)
data_jags = list(y=vaso$y, v = vaso$v, r = vaso$r)

params = c("int", "b")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)

mod1_sim = coda.samples(model=mod1,
                        variable.names=params,
                        n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

## calculate DIC
dic1 = dic.samples(mod1, n.iter=1e3)
dic1

#Results of Model Summarized
summary(mod1_sim)

#Posterior means for coefficients
(pm_coef = colMeans(mod1_csim))

#Q1 part c, For a subject with v = r = 1.5, find the probability of vasoconstriction.

p1 = 1.0 / (1.0 + exp(-(pm_coef["int"] +  pm_coef[1]*(log(10*1.5)) +  pm_coef[2]*(log(10*1.5)))))

p1


#Q1 part d, Compare with the result of probit model. Which has smaller deviance?

#Probit Model
mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
        probit(p[i]) = int + b[1]*v[i] + b[2]*r[i] 
   
    }
    int ~ dnorm(0.0, 1.0/25.0)
    for (j in 1:2) {
        b[j] ~ ddexp(0.0, sqrt(2.0)) # has variance 1.0
    }
} "


mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)

update(mod2, 1e3)

mod2_sim = coda.samples(model=mod2,
                        variable.names=params,
                        n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

## calculate DIC
dic2 = dic.samples(mod2, n.iter=1e3)
dic2

#Results of Model Summarized
summary(mod2_sim)

#Posterior means for coefficients
(pm_coef2 = colMeans(mod2_csim))

#Prediction with probit coefficients
p2 = 1.0 / (1.0 + exp(-(pm_coef["int"] +  pm_coef2[1]*(log(10*1.5)) +  pm_coef2[2]*(log(10*1.5)))))

p2
