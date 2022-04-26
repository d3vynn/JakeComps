library(ggplot2)
library(stats4)
library(rjags)

WW <- read.csv(file="~/Documents/Github/Jake_Comps/WolvesYNP.csv")
WW["AVGpackSize"] <- WW$Wolves / WW$Packs

yrvspacksize <- ggplot(WW)+
                geom_point(aes(x=as.character(Year),
                               y=AVGpackSize))+
                theme_classic()

packvswolf <- ggplot(WW,aes(x=Packs,y=Wolves))+
              geom_point()+
              theme_classic()

NegLogLikeLNorm <- function(params, obs){
  mean <- params[1]
  sd   <- params[2]
  -sum(dlnorm(obs,
              meanlog = mean, 
              sdlog = sd, log = T))
}



m2 <- optim(par=p, NegLogLikeLNorm, obs=WW$AVGpackSize, hessian = T)


mean(WW$AVGpackSize)

NLL_lnorm_sd <- function(params, obs, sd){
  
  mean <- params[1]
  -sum(dlnorm(obs,
              meanlog = mean, 
              sdlog = sd, 
              log = T))
}

NLL_lnorm_mean <- function(params, obs, mean){
  sd <- exp(params[2])
  -sum(dlnorm(obs,
              meanlog = mean, 
              sdlog = sd, 
              log = T))
} 

sdVec    <- log(seq(1.1, 2.0, length=100))
sdProf   <- numeric(100)
meanVec  <- log(seq(6, 12, length=100))
meanProf <- numeric(100)

p <- c(log(8),log(1.9))

for (i in 1:100){
  sdProf[i] <- optim(par=p,
                     NLL_lnorm_sd,
                     obs=WW$AVGpackSize,
                     sd=sdVec[i])$value
  
  meanProf[i] <- optim(par=p,
                       NLL_lnorm_mean,
                       obs=WW$AVGpackSize,
                       mean=meanVec[i])$value
}

plot(sdVec, sdProf)

plot(meanVec, meanProf)

prof.lower <- meanProf[1:which.min(meanProf)]
prof.mvec  <- meanVec[1:which.min(meanProf)]
approx(prof.lower,prof.mvec, xout = -logLik(m2) - qchisq(.95,1)/2)

mean <- a + b*W

NegLogLikeLNorm <- function(params, data){
  a <- params[1]
  b <- params[2]
  mean <- a + b*data$Wolf
  sd   <- params[3]
  -sum(dlnorm(data$obs,
              meanlog = mean, 
              sdlog = sd, log = T))
}


Ds <- data.frame(obs=WW$AVGpackSize,
                 Wolf=WW$Wolves)

Pvals <- c(log(10),log(9),1.2)

optim(Pvals, NegLogLikeLNorm, data=Ds)


#### Do the same sort of likelihood tests and then figure out also how to do the ratio test.

NegLogLikeLNorm <- function(params, data){
  a     <- params[1]
  b     <- params[2]
  alpha <- params[3]
  mean  <- (a + b*data$Wolf)^alpha
  sd    <- params[4]
  -sum(dlnorm(data$obs,
              meanlog = mean, 
              sdlog = sd, log = T))
}

mean <- exp(mu + 0.5 * sigma^2)