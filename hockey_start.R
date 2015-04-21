## nhl hockey analysis

## the data is in gamlr.
## You need to first install this,
## via install.packages("gamlr")

library(gamlr) # loads Matrix as well
help(hockey) # describes the hockey data and shows an example regression

data(hockey) # load the data

# Q1
# Combine the covariates all together
x <- cBind(config,team,player) # cBind binds together two sparse matrices

# build 'y': home vs away, binary response
y <- goal$homegoal

nhlreg <- gamlr(x, y,
	free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
	family="binomial", standardize=FALSE)

## coefficients (grab only the players)
# AICc selection
Baicc <- coef(nhlreg)[colnames(player),]

# BIC selection
# We see that under BIC, the players have no effect only the configuration and the team season
Bbic <- coef(nhlreg, select=which.min(BIC(nhlreg)))

# Q3 add in the cross validation gammo lasso

cv.nhlreg <- cv.gamlr(x, y,
  free=1:(ncol(config)+ncol(team)),
  family="binomial", standardize=FALSE)

