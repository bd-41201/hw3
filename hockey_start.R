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

# Team coefficients
Baicc_team <- coef(nhlreg)[colnames(team),]
top_bottom_team <- Baicc_team[c(which.min(Baicc_team),which.max(Baicc_team))]
# top_bottom_team
# ~> NYI.20112012 TBL.20032004
# ~> -0.6835273    1.3035036
# exp(top_bottom_team)
# ~> NYI.20112012 TBL.20032004
# ~>   0.5048332    3.6821749
# 1/0.5048332
# ~> [1] 1.980852

# Intercept
intercept <- coef(nhlreg)[1]
# intercept
# ~> [1] 0.07913862
# exp(intercept)
# ~> [1] 1.082354

# BIC selection
# We see that under BIC, the players have no effect only the configuration and the team season
Bbic <- coef(nhlreg, select=which.min(BIC(nhlreg)))[colnames(player),]

# Q3 add in the cross validation gammo lasso

cv.nhlreg <- cv.gamlr(x, y,
  free=1:(ncol(config)+ncol(team)),
  family="binomial", standardize=FALSE)

ll <- log(nhlreg$lambda)

plot(nhlreg, col="grey")
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=2)
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=2)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=2)
abline(v=log(cv.nhlreg$lambda.min), col="blue", lty=2)
abline(v=log(cv.nhlreg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1,
  col=c("black","orange","green","blue","purple"),
  legend=c("AICc","AIC","BIC","CV.min","CV.1se"))
