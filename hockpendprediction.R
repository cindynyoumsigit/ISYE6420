#installing necessary libraries
library(tidyverse)
library(rjags)
library(coda)
library(bayestestR)
library(dplyr)
library(ggplot2)

#Downloading datasets into dataframes
dat=read.table("hockpend.dat",sep="",header=TRUE)

#check data was properly loaded
head(dat)
str(dat)

#Running model
mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*x1[i] + b[3]*x2[i] + b[4]*x3[i] 
        
        #running icpo
        r[i] <- y[i]-mu[i]
        f[i] <- sqrt(prec/6.2832)*exp(-0.5*prec*r[i]*r[i])
        icpo[i] <- 1/f[i]
    }
    
    
    for (i in 1:4) {
        b[i] ~ dnorm(0.0, 0.00001)
    }
    
    prec ~ dgamma(1,0.001)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
    
    # Mean Response & Prediction Response & Credible intervals
      pre1 <- 10
      pre2 <- 5
      pre3 <- 5
      mpredictresp <- b[1] + b[2]*pre1 + b[3]*pre2 + b[4]*pre3
      ypredictresp ~ dnorm(mpredictresp, prec)
      
      #Bayesian r-squared
      nminusp <- length(y) - 4
      sigma2 <- 1/prec #mse
      sse <- nminusp*sigma2
      for ( i in 1:length(y) ){ 
      cy[i] <- y[i] - mean(y[])}
      sst <- inprod(cy[],cy[])
      BR2 <- 1 - sse/sst
      BR2adj <- 1 - (length(y)-1)*sigma2/sst
      

} "

set.seed(73)
data_jags = list(y=dat$y, x1=dat$x1, x2=dat$x2, x3=dat$x3 )

params1 = c("b", "sig", "icpo", "mpredictresp", "ypredictresp",  "BR2")

inits1 = function() {
  inits = list("b"=rnorm(4,0.0,100.0), "prec"=rgamma(1,1.0,1.0) )
}

mod1 = jags.model(textConnection(mod1_string), data=data_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

#Parameter estimates
summary(mod1_sim)




