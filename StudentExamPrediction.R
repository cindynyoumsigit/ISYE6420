#Cindy Nyoumsi ISYE 6420 Fall 2020 Project

#Description: In this project, 
#My goal will be to predict whether a student has failed a class or not based on the students weekly study time in hours and their final grades. 
#I will be using a logit model to predict the probability of failure1 or no failure 0
#Datasource: https://www.kaggle.com/uciml/student-alcohol-consumption?select=student-merge.R
#
#
#
#
#Beginning of Code

#installing necessary libraries
library(tidyverse)
library(rjags)
library(coda)

#Downloading datasets into dataframes
d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)

#Checking the size of my data
print(nrow(d1))
print(nrow(d2))

#Identifying # of duplicates in my data
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

#Combining the datasets
d4 = rbind(d1,d2)
print(nrow(d4)) # 382 students

#Removing duplicates from the data set 
dfinal = d4 %>% distinct(school,sex,age,address,famsize,Pstatus,Medu,Fedu,Mjob,Fjob,reason,nursery,internet, .keep_all = TRUE)

#Checking only unique values were kept
print(nrow(dfinal))

#Doublechecking content of my data
str(dfinal)

#Changing any failures greater than 0 to simply 1
dfinal$failures[dfinal$failures>0] <- 1
head(dfinal)


#Run Logit Model 
mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
        logit(p[i]) = int + b[1]*studytime[i] + b[2]*G3[i]
   
    }
    int ~ dnorm(0.0, 1.0/25.0)
    for (j in 1:2) {
        b[j] ~ ddexp(0.0, sqrt(2.0)) # has variance 1.0
    }
} "

set.seed(92)
data_jags = list(y=dfinal$failures, studytime = dfinal$studytime, G3 = dfinal$G3)

params = c("int", "b")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)

mod1_sim = coda.samples(model=mod1,
                        variable.names=params,
                        n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))

## calculate DIC
dic1 = dic.samples(mod1, n.iter=1e3)

#Results of Model Summarized
summary(mod1_sim)

par(mfrow=c(2,1))
densplot(mod1_csim[,1:2], xlim=c(-3.0, 3.0))

#Retrieve posterior coefficients
(pm_coef = colMeans(mod1_csim))

#Make Predictions on Data to measure classification accuracy
pm_dfinal = pm_coef["int"] + as.matrix(dfinal[,c(14,33)]) %*%pm_coef[1:2]

phat = 1.0 / (1.0 + exp(-pm_dfinal))
head(phat)

#Compare to Residuals
plot(phat, jitter(dfinal$failures))

#Include threshold
(tab0.5 = table(phat > 0.5, data_jags$y))

#Measure prediction accuracy
sum(diag(tab0.5)) / sum(tab0.5)

#Based on these results, it is clear that my model is good at predicting whether 
#a student has not failed a class, but it does not do well when predicting if a student
#has not a failed a class.
