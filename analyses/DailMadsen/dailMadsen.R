


library(unmarked)

# ----------------------- Exponential growth model -----------------------

## Simulate data

# Parameters
lam <- 5        # mean abundance in year 1
r <- 0.01       # intrinsic rate of increase
p <- 0.5        # detection probability

# Data dimensions
nSites <- 100
nReps <- 3
nYears <- 10
N <- matrix(NA, nSites, nYears)

# Generate latent abundance matrix (N)
set.seed(43489)
N[,1] <- rpois(nSites, lam)
for(t in 2:nYears) {
    N[,t] <- rpois(nSites, exp(r)*N[,t-1])
}

# Generate data
y <- array(NA, c(nSites, nYears, nReps))
for(t in 1:nYears) {
    for(k in 1:nReps) {
        y[,t,k] <- rbinom(nSites, N[,t], p)
    }
}



plot(1:nYears, colMeans(N), xlab="Year", ylab="Population size",
     type="o", ylim=c(0, 10))





## Fit the model in unmarked

library(unmarked)
y2 <- aperm(y, c(1,3,2))
ymat <- matrix(y2, nrow(y2))
umf.exp <- unmarkedFramePCO(y=ymat, numPrimary=nYears)
fm.exp <- pcountOpen(~1, ~1, ~1, ~1, data=umf.exp, dynamics="trend",
                     K=100, control=list(trace=TRUE))

mle <- as.numeric(coef(fm.exp))

print(c("lambda"=exp(mle[1]), "r"=mle[2], "p"=plogis(mle[3])),
      digits=2)







## Fit model in JAGS


# Write JAGS model (this creates a new file called "dm_exp.txt")
mod
model {
#lambda ~ dunif(0, 5)  # No problem in JAGS
lambda ~ dgamma(0.001, 0.001)
r ~ dunif(-5, 5)
p ~ dunif(0, 1)
for(i in 1:nSites) {
  N[i,1] ~ dpois(lambda)
  for(k in 1:nReps) {
     y[i,1,k] ~ dbin(p, N[i,1])
  }
  for(t in 2:nYears) {
    muN[i,t-1] <- exp(r)*N[i,t-1]
    N[i,t] ~ dpois(muN[i,t-1])
    for(k in 1:nReps) {
      y[i,t,k] ~ dbin(p, N[i,t])
    }
  }
}
}
", fill=TRUE)
sink()


# Load rjags
library(rjags)

# Format data and create function to initiate parameters
dat.exp <- list(nSites=nSites, nYears=nYears, nReps=nReps, y=y)
init.exp <- function() list(lambda=runif(1, 0, 5),
                        r=rnorm(1), p=runif(1), N=N+5)
pars.exp <- c("lambda", "r", "p")

# Compile the model.
# The adaptive phase tries to improve the efficiency of the samplers
jm.exp <- jags.model("dm_exp.txt", dat.exp, init.exp, n.chains=2,
                     n.adapt=500)

# Draw samples from the posterior
# In practice, you would need many more iterations
jc.exp <- coda.samples(jm.exp, pars.exp, n.iter=1000)

# View the Markov chains
plot(jc.exp)

# Summarize the posteriors
summary(jc.exp)


# MCMC diagnostics
gelman.diag(jc.exp)    # Should be ~ <1.1 (run chains longer if need be)
autocorr.plot(jc.exp)  # If samples are autocorrelated, you need more iters
crosscorr(jc.exp)      # Correlation among parameters





## Fit the model in WinBUGS

library(R2WinBUGS)
dm.bugs <- bugs(dat.exp, init.exp, pars.exp, "dm_exp.txt",
                n.chains=2, n.iter=1000, n.burnin=500, n.thin=1, DIC=FALSE,
#                bugs.directory="C:/WinBUGS14",
                debug=FALSE)

dm.bugsmc <- as.mcmc.list(dm.bugs)

summary(dm.bugsmc) # Yikes! Something is wrong with WinBUGS.



# Compate JAGS, WinBUGS, and unmarked
compare <- cbind(Actual=c(lam, p, r),
                 JAGS=summary(jc.exp)$stat[,"Mean"],
                 WinBUGS=summary(dm.bugsmc)$stat[,"Mean"],
                 unmarked=c(exp(mle[1]), plogis(mle[3]), mle[2]))

print(compare, digits=3)





# --------------------------- Ricker model -------------------------------
# No secondary samples this time (not recommended)

lam <- 3
r <- 0.5
K <- 50
p <- 0.8

nSites <- 100
nYears <- 20
y <- N <- matrix(NA, nSites, nYears)

set.seed(3340)
N[,1] <- rpois(nSites, lam)
for(t in 2:nYears) {
    N[,t] <- rpois(nSites, N[,t-1]*exp(r*(1-N[,t-1]/K)))
}
y[] <- rbinom(nSites*nYears, N, p)

# Plots
plot(1:nYears, colSums(N), xlab="year", ylab="Population size",
     type="o")

matplot(t(y[1:3,]), type="b", pch=1, lty=2, ylim=c(0, 100),
        xlab="Year", ylab="Population size")
matplot(t(N[1:3,]), type="o", pch=16, lty=1, add=TRUE)
legend(1, 100, c("Site 1 N", "Site 1 y", "Site 2 N", "Site 2 y",
                 "Site 3 N", "Site 3 y"), col=c(1,1,2,2,3,3),
                 lty=c(1,2,1,2,1,2), pch=rep(c(16,1), 3))


sink(file="dm_rick.txt")
cat("
model {
lambda ~ dunif(0, 5)
r ~ dnorm(0, 0.01)
K ~ dunif(10, 90)
p ~ dunif(0, 1)
for(i in 1:nSites) {
  N[i,1] ~ dpois(lambda)
  y[i,1] ~ dbin(p, N[i,1])
  for(t in 2:nYears) {
    muN[i,t-1] <- N[i,t-1]*exp(r*(1-N[i,t-1]/K))
    N[i,t] ~ dpois(muN[i,t-1])
    y[i,t] ~ dbin(p, N[i,t])
    }
  }
# Population size in each year
for(t in 1:nYears) {
  Ntot[t] <- sum(N[,t])
  }
}
", fill=TRUE)
sink()



library(rjags)

dat.rick <- list(nSites=nSites, nYears=nYears, y=y)
init.rick <- function() list(lambda=runif(1, 2, 4),
                             r=rnorm(1), p=runif(1, 0.7, 0.9),
                             K=runif(1, 30, 80),
                             N=N) # cheating to use true N here
pars.rick <- c("lambda", "r", "p", "K", "Ntot")

# Compile model
jm.rick <- jags.model("dm_rick.txt", dat.rick, init.rick,
                      n.chains=1, n.adapt=500)
# Draw posterior samples
ps.rick <- coda.samples(jm.rick, pars.rick, n.iter=1000)

plot(ps.rick, ask=TRUE) # did the chains converge?


# Take some more samples
ps.rick2 <- coda.samples(jm.rick, pars.rick, n.iter=5000)

# And maybe some more
# ps.rick3 <- coda.samples(jm.rick, pars.rick, n.iter=2000)


plot(ps.rick2, ask=TRUE)


# Plot posterior summaries of pop size in each year
ps.rick.mat <- as.matrix(ps.rick2)
ps.Ntot <- ps.rick.mat[,grep("Ntot", colnames(ps.rick.mat))]
head(ps.Ntot)

plot(1:nYears, colMeans(ps.Ntot), type="b", ylim=c(0, 5000),
     xlab="Year", ylab="Population size", pch=16, col="blue")
points(colSums(N))
arrows(1:nYears, apply(ps.Ntot, 2, quantile, prob=0.025),
       1:nYears, apply(ps.Ntot, 2, quantile, prob=0.975),
       angle=90, code=3, length=0.03)
legend(1, 5000, c("Actual", "Posterior mean and CI"), pch=c(1,16),
       col=c("black", "blue"))


# ---------------------------- Dail-Madsen Model -------------------------
# Simulate data under the so-called "constant" model
#    Basic birth-death process but birth rate isn't affected by
#    abundance in previous year. Not realistic, but it is easy to extend

lam <- 3
omega <- 0.5
gamma <- 2
p <- 0.8

nSites <- 100
nYears <- 20
y <- N <- matrix(NA, nSites, nYears)
S <- G <- matrix(NA, nSites, nYears-1)

N[,1] <- rpois(nSites, lam)
for(t in 2:nYears) {
    S[,t-1] <- rbinom(nSites, N[,t-1], omega)
    G[,t-1] <- rpois(nSites, gamma)
    N[,t] <- S[,t-1] + G[,t-1]
}
y[] <- rbinom(nSites*nYears, N, p)

plot(1:nYears, colSums(N), xlab="Year", ylab="Population size",
     type="o")

sink(file="dm_const.txt")
cat("
model {
#lambda ~ dunif(0, 5) # Fine for JAGS, but WinBUGS chokes
#gamma ~ dunif(0, 5)  # Fine for JAGS, but WinBUGS chokes
lambda ~ dgamma(0.001, 0.001)
gamma ~ dgamma(0.001, 0.001)
omega ~ dunif(0, 1)
p ~ dunif(0, 1)
for(i in 1:nSites) {
  N[i,1] ~ dpois(lambda)
  y[i,1] ~ dbin(p, N[i,1])
  for(t in 2:nYears) {
    S[i,t-1] ~ dbin(omega, N[i,t-1])
    G[i,t-1] ~ dpois(gamma)
    N[i,t] <- S[i,t-1] + G[i,t-1]
    y[i,t] ~ dbin(p, N[i,t])
    }
  }
}
", fill=TRUE)
sink()



library(rjags)


# Bundle data
dat.const <- list(nSites=nSites, nYears=nYears, y=y)

# Initial values
# Note, JAGS will throw an error if the initial values aren't in agreement
#       with the data. It helps to start N at large values
Ni <- N+2
Si <- S
Si[] <- 2
Gi <- Ni[,-1]-Si
Ni[,-1] <- NA
init.const <- function() list(lambda=runif(1, 2, 4),
                              gamma=runif(1, 1, 3),
                              N=Ni,
                              omega=runif(1, 0.4, 0.6),
                              S=Si, G=Gi,
                              p=runif(1, 0.5, 1))
pars.const <- c("lambda", "gamma", "omega", "p")

# Compile model
jm.const <- jags.model("dm_const.txt", dat.const, init.const,
                       n.chains=2, n.adapt=500)

# Posterior samples
ps.const <- coda.samples(jm.const, pars.const, n.iter=1000)

plot(ps.const)

summary(ps.const)






# Fit the model in unmarked

umf.const <- unmarkedFramePCO(y=y, numPrimary=nYears)
fm.const <- pcountOpen(~1, ~1, ~1, ~1, umf.const, dynamics="constant",
                       K=50, control=list(trace=TRUE))


# Compare JAGS and unmarked
mle.const <- as.numeric(coef(fm.const))

compare.const <- cbind(Actual=c(lambda=lam, gamma=gamma, omega=omega, p=p),
                       JAGS=colMeans(as.matrix(ps.const))[c(2,1,3,4)],
                       unmarked=c(exp(mle.const[1:2]),
                                  plogis(mle.const[3:4])))

print(compare.const, digits=3)





# --------------- Compute expected equilibrium distribution --------------


# This function returns probability of transitioning from N_t to N_t+1
# under the "constant" model of Dail and Madsen (Biometrics 2011)
tranprob <- function(Ninf, gamma, omega) {
    N <- 0:Ninf
    v <- length(N)
    P <- matrix(NA, v, v)
    rownames(P) <- paste("N0=", N, sep="")
    colnames(P) <- paste("N1=", N, sep="")
    for(j in N) {
        for(k in N) {
            s <- 0:min(j, k) # Possible number of survivors
            g <- k-s         # Possible number of recruits
            P[j+1,k+1] <- sum(dpois(g, gamma) * dbinom(s, j, omega))
        }
    }
    return(P) # matrix of 1-step transition probabilities
}



Ninf <- 30 # Maximum possible value of N
round(P <- tranprob(Ninf, 1, 0.4), 2)
rowSums(P)

# Project, the old fashioned way
T <- 50 # arbitrary, should be increased if equilibrium is not reached
Pt <- array(NA, c(Ninf+1, Ninf+1, T))
Pt[,,1] <- P
for(t in 2:T) {
    Pt[,,t] <- Pt[,,t-1] %*% P
}

# Make sure probs still equal 1
rowSums(Pt[,,T])

# Test is equilibrium has been reached with tolerance=1e-6
all(apply(Pt[,,T], 2, function(x) diff(range(x)) < 1e-6))

# Equilibrium state
round(Eq <- apply(Pt[,,T], 2, mean), 2)

sum(Eq) # Should be 1

plot(0:Ninf, Eq, type="h", xlab=expression(N[infinity]),
     ylab="Probability", lwd=4, lend="butt",
     main="Probability distribution at equilibrium")
abline(h=0, col=gray(0.8))



# Project the elegent and efficient way using eigendecomposition

eP <- eigen(t(P))
ev1 <- eP$vector[,1]
ev1 <- ev1/sum(ev1)

plot(0:Ninf, ev1, type="h", xlab=expression(N[infinity]),
     ylab="Probability", lwd=4, lend="butt",
     main="Probability distribution at equilibrium")









# -------------------------- Project using MCMC --------------------------



# Simulate data under model with density-dependent recruitment
# Predict abundance into the future by tacking on extra years with
#   missing values


lam <- 3
omega <- 0.5
gam0 <- 4    # Rescue effect. Recruitment rate independent of N(i,t-1)
gam1 <- -0.5 # Strength of density dependence. If positive, Allee effect
p <- 0.8

nSites <- 100
nReps <- 3
nYears <- 20
N <- matrix(NA, nSites, nYears)
S <- G <- matrix(NA, nSites, nYears-1)
y <- array(NA, c(nSites, nYears, nReps))

# Simulate true abundance data
N[,1] <- rpois(nSites, lam)
for(t in 2:nYears) {
    gamma <- exp(gam0 + gam1*N[,t-1])
    S[,t-1] <- rbinom(nSites, N[,t-1], omega)
    G[,t-1] <- rpois(nSites, gamma)
    N[,t] <- S[,t-1] + G[,t-1]
}

# Simulate observed counts
for(t in 1:nYears) {
    for(k in 1:nReps) {
        y[,t,k] <- rbinom(nSites, N[,t], p)
    }
}

plot(1:nYears, colMeans(N), xlab="Year", ylab="Average abundance",
     type="o")



sink(file="dm_dep.txt")
cat("
model {
lambda ~ dunif(0, 6)
gam0 ~ dnorm(0, 0.01)
gam1 ~ dnorm(0, 0.01)
omega ~ dunif(0, 1)
p ~ dunif(0, 1)
for(i in 1:nSites) {
  N[i,1] ~ dpois(lambda)
  for(k in 1:nReps) {
     y[i,1,k] ~ dbin(p, N[i,1])
  }
  for(t in 2:nYears) {
    S[i,t-1] ~ dbin(omega, N[i,t-1])
    gamma[i,t-1] <- exp(gam0 + gam1*N[i,t-1])
    G[i,t-1] ~ dpois(gamma[i,t-1])
    N[i,t] <- S[i,t-1] + G[i,t-1]
    for(k in 1:nReps) {
       y[i,t,k] ~ dbin(p, N[i,t])
       }
    }
  }
for(t in 1:nYears) {
  Ntot[t] <- sum(N[,t])
  }
}
", fill=TRUE)
sink()



library(rjags)


# Bundle data
# Add 10 extra years
extraYears <- 10
yFuture <- array(NA, c(nSites, nYears+extraYears, nReps))
yFuture[,1:nYears,] <- y
dat.dep <- list(nSites=nSites, nYears=nYears+extraYears, nReps=nReps,
                y=yFuture)

# Initial values
# Note, JAGS will throw an error if the initial values aren't in agreement
#       with the data. It helps to start N at large values
Ni <- N+5
Si <- S
Si[] <- 2
Gi <- Ni[,-1]-Si
Ni[,-1] <- NA
Ni <- cbind(Ni, matrix(NA, nSites, extraYears))
Gi <- cbind(Gi, matrix(NA, nSites, extraYears))
Si <- cbind(Si, matrix(NA, nSites, extraYears))
init.dep <- function() list(lambda=runif(1, 2, 4),
                              gam0=rnorm(1, 3, 0.1),
                              gam1=runif(1, -1, 0),
                              N=Ni,
                              omega=runif(1, 0.4, 0.6),
                              S=Si, G=Gi,
                              p=runif(1, 0.5, 1))
pars.dep <- c("lambda", "gam0", "gam1", "omega", "p", "Ntot")


# Compile model
jm.dep <- jags.model("dm_dep.txt", dat.dep, init.dep,
                     n.chains=2, n.adapt=500)

# Posterior samples
ps.dep <- coda.samples(jm.dep, pars.dep, n.iter=1000)

plot(ps.dep, ask=TRUE)

summary(ps.dep)





# Plot posterior summaries of pop size in each year
ps.dep.mat <- as.matrix(ps.dep)
ps.Ntot <- ps.dep.mat[,grep("Ntot", colnames(ps.dep.mat))]
head(ps.Ntot)

plot(1:(nYears+extraYears), colMeans(ps.Ntot), type="b", ylim=c(0, 2000),
     xlab="Year", ylab="Population size", pch=16, col="blue")
points(1:nYears, colSums(N))
arrows(1:(nYears+extraYears), apply(ps.Ntot, 2, quantile, prob=0.025),
       1:(nYears+extraYears), apply(ps.Ntot, 2, quantile, prob=0.975),
       angle=90, code=3, length=0.03)
polygon(c(20.5, 35, 35, 20.5, 20.5), c(-100, -100, 3000, 3000, -100),
        col=rgb(0,0,0,0.2), border=FALSE)
legend(5, 2000, c("Actual", "Posterior mean and CI"), pch=c(1,16),
       col=c("black", "blue"))














# ---------------------- Analysis of BBS data ----------------------------


# Bring in the data
source("gwwa.y.east1970.R")
str(gwwa.y.east1970)

source("data.east.1970.R")
str(data.east.1970)


# Format data
# Just analyze sites with > 0 detections
is.gr0 <- rowSums(gwwa.y.east1970, na.rm=TRUE) > 0
sites.with.dets <- rownames(gwwa.y.east1970)[is.gr0]
gwwa.sub <- gwwa.y.east1970[sites.with.dets,]
str(gwwa.sub)


# Standardize temperature data
tmax <- matrix(data.east.1970$tmax, nrow(gwwa.y.east1970), byrow=TRUE)
rownames(tmax) <- matrix(data.east.1970$route, nrow(gwwa.y.east1970),
                         byrow=TRUE)[,1]
tmax <- tmax-mean(tmax, na.rm=TRUE)
tmax <- tmax/sd(c(tmax), na.rm=TRUE)



# Dail-Madsen model with no explicit births and deaths
# Population change is a function of previous abundance and
# temperature and year
sink(file="dm_gwwa2.txt")
cat("
model {
lambda ~ dunif(0, 20)
gam0 ~ dnorm(0, 0.01)
gamTemp ~ dnorm(0, 0.01)
gamYear ~ dnorm(0, 0.01)
gam1 ~ dnorm(0, 0.01)
p0 ~ dunif(0, 1)
logitp0 <- log(p0/(1-p0))
pYear ~ dnorm(0, 0.01)
for(i in 1:nSites) {
  N[i,1] ~ dpois(lambda)
  logit(p[i,1]) <- logitp0 + pYear*year[1]
  y[i,1] ~ dbin(p[i,1], N[i,1])
  for(t in 2:nYears) {
    gamma[i,t-1] <- min(exp(gam0 + gamTemp*temp[i,t-1] +
                            gamYear*year[t-1] + gam1*N[i,t-1]), 20)
    N[i,t] ~ dpois(gamma[i,t-1])
    logit(p[i,t]) <- logitp0 + pYear*year[t]
    y[i,t] ~ dbin(p[i,t], N[i,t])
    }
  }
for(t in 1:nYears) {
  Ntot[t] <- sum(N[,t])
  }
}
", fill=TRUE)
sink()




dat.gwwa2 <- list(nSites=nrow(gwwa.sub),
                  nYears=ncol(gwwa.sub),
                  y=gwwa.sub,
                  temp = tmax[rownames(gwwa.sub),],
                  year=1970:2010)
dat.gwwa2$year <- (dat.gwwa2$year-mean(1970:2010)) / sd(1970:2010)





Ni <- gwwa.sub + 3
for(i in 1:nrow(Ni)) {
    Ni[i,is.na(Ni[i,])] <- max(Ni[i,], na.rm=TRUE)
}
init.gwwa2 <- function() list(lambda=runif(1, 2, 4),
                              gam0=rnorm(1),
                              gamTemp=rnorm(1),
                              gamYear=rnorm(1),
                              gam1=runif(1, -1, 0),
                              N=Ni,
                              p0=runif(1, 0.5, 1),
                              pYear=rnorm(1))
pars.gwwa2 <- c("lambda", "gam0", "gamTemp", "gamYear", "gam1",
               "p0", "pYear", "Ntot")


# Compile model
jm.gwwa2 <- jags.model("dm_gwwa2.txt", dat.gwwa2, init.gwwa2,
                       n.chains=1, n.adapt=100)

# Posterior samples
ps.gwwa2 <- coda.samples(jm.gwwa2, pars.gwwa2, n.iter=1000)

# View results
plot(ps.gwwa2, ask=TRUE)
summary(ps.gwwa2)






ps.gwwa.mat2 <- as.matrix(ps.gwwa2)
gwwaN2 <- ps.gwwa.mat2[,grep("Ntot", colnames(ps.gwwa.mat2))]
plot(colMeans(gwwaN2))
























# --------------------------- Figures for slides -------------------------



png("../figs/dmExp3sites.png", width=7, height=7, units="in", res=400)
matplot(t(y[1:3,,1]), type="n", pch=1, lty=2, ylim=c(0, 40),
        xlab="Year", ylab="Population size", cex.lab=1.3)
for(t in 1:3)
    matplot(t(y[1:3,,t]), type="b", pch=1, lty=2, add=TRUE,
            col=c("black", "blue", "purple"))
matplot(t(N[1:3,]), type="o", pch=16, lty=1, add=TRUE,
        col=c("black", "blue", "purple"))
legend(1, 40, c("Site 1 N", "Site 1 y", "Site 2 N", "Site 2 y",
                "Site 3 N", "Site 3 y"),
       col=rep(c("black", "blue", "purple"), each=2),
       lty=c(1,2,1,2,1,2), pch=rep(c(16,1), 3))
dev.off()




png("../figs/dmRicker3sitesNoY.png", width=8, height=3, units="in",
    res=400)
op <- par(mai=c(0.8, 0.9, 0.5, 0.1))
N <- matrix(NA, 3, 100)
# Generate latent abundance matrix (N)
set.seed(43489)
N[,1] <- rpois(3, 1.5)
for(t in 2:100) {
    N[,t] <- rpois(3, N[,t-1]*exp(.2*(1-N[,t-1]/50 + rnorm(1, 0, 0.5))))
}
matplot(t(N[1:3,]), type="b", pch=1, lty=1, ylim=c(0, 100),
        xlab="", ylab="Population size", cex.lab=1.3,
        col=c("black", "blue", "purple"), cex=0.5)
legend(1, 100, c("Site 1 N", "Site 2 N", "Site 3 N"),
       col=c("black", "blue", "purple"), lty=1, pch=1, pt.cex=0.5)
par(op)
dev.off()





png("../figs/dmRicker.png", width=7, height=7, units="in", res=400)
plot(1:nYears, colMeans(ps.Ntot), type="b", ylim=c(0, 5000),
     xlab="Year", ylab="Population size", pch=16, col="blue")
points(colSums(N))
arrows(1:nYears, apply(ps.Ntot, 2, quantile, prob=0.025),
       1:nYears, apply(ps.Ntot, 2, quantile, prob=0.975),
       angle=90, code=3, length=0.03, col="blue")
legend(1, 5000, c("Actual", "Posterior mean and CI"), pch=c(1,16),
       col=c("black", "blue"))
dev.off()





png("../figs/dmEq.png", width=7, height=7, units="in", res=400)
plot(0:Ninf, ev1, type="h", xlab=expression(N[infinity]),
     ylab="Probability", lwd=6, col="midnightblue", lend="butt",
     main="Probability distribution at equilibrium",
     cex.lab=1.3)
abline(h=0, col=gray(0.8))
dev.off()









png("../figs/mcmcFuture.png", width=7, height=7, units="in", res=400)
plot(1:(nYears+extraYears), colMeans(ps.Ntot), type="b", ylim=c(0, 2000),
     xlab="Year", ylab="Population size", pch=16, col="blue",
     cex.lab=1.3)
points(1:nYears, colSums(N))
arrows(1:(nYears+extraYears), apply(ps.Ntot, 2, quantile, prob=0.025),
       1:(nYears+extraYears), apply(ps.Ntot, 2, quantile, prob=0.975),
       angle=90, code=3, length=0.03)
polygon(c(20.5, 35, 35, 20.5, 20.5), c(-100, -100, 3000, 3000, -100),
        col=rgb(0,0,0,0.2), border=FALSE)
legend(5, 2000, c("Actual", "Posterior mean and CI"), pch=c(1,16),
       col=c("black", "blue"))
text(18, 1, "Past")
text(23, 1, "Future")
dev.off()









# Scratch

dd <- function(N0 = 10, gam0=1, gam1=-0.5, T=100) {
    N <- integer(T+1)
    N[1] <- N0
    for(t in 1:T) {
        cat("Nt=", N[t], "\n")
        N[t+1] <- rpois(1, exp(gam0 + gam1*N[t]))
    }
    N
}


plot(dd(gam0=0, gam1=0.22), type="b")


plot(dd(gam0=0, gam1=-0.9), type="b")

