## nhl hockey analysis

## the data is in gamlr.  
## You need to first install this, 
## via install.packages("gamlr")

library(gamlr) # loads Matrix as well
help(hockey) # describes the hockey data and shows an example regression

data(hockey) # load the data

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



## Conor's code --------------------------
plot(nhlreg)
cv.nhlreg <- cv.gamlr(x, y, free=1:(ncol(config)+ncol(team)), family="binomial", standardize=FALSE)
n <- nrow(goal)
plot(cv.nhlreg)

## log lambdas selected under various criteria
log(nhlreg$lambda[which.min(AICc(nhlreg))])
log(nhlreg$lambda[which.min(AIC(nhlreg))])
log(nhlreg$lambda[which.min(BIC(nhlreg))])
log(cv.nhlreg$lambda.min)
log(cv.nhlreg$lambda.1se)

## plot CV results and the various IC
ll <- log(nhlreg$lambda) ## the sequence of lambdas

par(mfrow=c(1,2))
plot(cv.nhlreg)
plot(ll, AIC(nhlreg)/n, 
	xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=3)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=3)
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=3)
points(ll, BIC(nhlreg)/n, pch=21, bg="green")
points(ll, AICc(nhlreg)/n, pch=21, bg="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))

## all metrics, together in a path plot.
plot(nhlreg, col="grey")
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=2)
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=2)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=2)
abline(v=log(cv.nhlreg$lambda.min), col="blue", lty=2)
abline(v=log(cv.nhlreg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, 
	col=c("black","orange","green","blue","purple"),
	legend=c("AICc","AIC","BIC","CV.min","CV.1se"))

## Q4

> x <- (player)
> nhlreg <- gamlr(x, y, family="binomial", standardize=FALSE)
> plot(nhlreg)
> cv.nhlreg <- cv.gamlr(x, y, family="binomial", standardize=FALSE)
> plot(cv.nhlreg)
> ll <- log(nhlreg$lambda)
> par(mfrow=c(1,2))
> plot(cv.nhlreg)
> plot(ll, AIC(nhlreg)/n, xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
> abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=3)
> abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=3)
> abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=3)
> points(ll, BIC(nhlreg)/n, pch=21, bg="green")
> points(ll, AICc(nhlreg)/n, pch=21, bg="black")
> legend("topleft", bty="n", fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))

## End Conor's Code ----------------------

