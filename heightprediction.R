
library("rjags")

#Creating dataframe with given data + mu1 - mu2 - mu3 + mu4
height <- c(13.2, 12.4, 12.8, 17.2, 13.0, 14.0, 14.2, 21.6, 15.0, 20.0,
            16.0, 12.6, 14.8, 13.0, 14.0, 23.6, 14.0, 17.0, 22.2, 24.4,
            7.8, 14.4, 20.0, 15.8, 17.0, 27.0, 19.6, 18.0, 20.2, 23.2,
            21.0, 14.8, 19.1, 15.8, 18.0, 26.0, 21.1, 22.0, 25.0, 18.2,
            10.4, 0.2, -2.9, 4.2, 0,-10.6, 1.7, 8.6, -2.4, -9.4)

#gnew = mu1 - mu2 - mu3 + mu4

conc <- as.factor(c("g150", "g150", "g150", "g150", "g150", "g150", "g150", "g150", "g150", "g150", 
          "g2100", "g2100", "g2100", "g2100", "g2100", "g2100", "g2100", "g2100", "g2100", "g2100",
          "g3200", "g3200", "g3200", "g3200", "g3200", "g3200", "g3200", "g3200", "g3200", "g3200",
          "g4400", "g4400", "g4400", "g4400", "g4400", "g4400", "g4400", "g4400", "g4400", "g4400",
          "gnew", "gnew", "gnew", "gnew", "gnew", "gnew", "gnew", "gnew", "gnew", "gnew"))

dat <- data.frame(height,conc)

#Checking data was correctly loaded and transforms correctly
head(dat)
str(dat)
as.numeric(dat$conc)


#Visualizing data
boxplot(height ~ conc, data=dat)


#Question 2 Part a , Running Model 

mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }
    
    for (j in 1:5) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt( 1.0 / prec )
     
    
} "

set.seed(82)
data_jags = list(y=dat$height, grp=as.numeric(dat$conc))

params = c("mu", "sig")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)

mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains

#Evaluating appropriateness of model by looking at the residuals
(pm_params = colMeans(mod_csim))
yhat = pm_params[1:5][data_jags$grp]
resid = data_jags$y - yhat
plot(resid)
plot(yhat, resid)

#Looking at results summary of simulation
summary(mod_sim)


