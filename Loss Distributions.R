# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\15 Loss Distributions")

# Calculate prob and generate random variates for some statistical distributions in R
# Do the same for distributions NOT in R
# Calculate basis statistics from the generated samples

# Distributions ->
# - Exponential
# - Gamma
# - Lognormal
# - Weibull

# Refer to table in notes on the parametrizations of the different distributions

# We will be required to calculate:
# -> f(x) density function
# -> F(x) distribution function or CDF
# -> Quantiles
# -> Simulate Random variates

# There are some funnies in table -> check Lognormal parameters, Weibull parameters, etc

# Example
# Calculate probability density -> f(30) from Exp(0.05)
dexp(30, 0.05)

# to find the log-likelihood ->
dexp(30, 0.05, log=TRUE)

# Why is this useful?

x = c(18.9, 16.2, 51.5, 65.2, 23.8, 4.2, 21.1, 0.5, 15.2, 3.3)
# likelihood l for each x ->
l = dexp(x, 0.05)

prod(l) # -> this is tiny...R will think its equal to zero

# So we work with log-likelihood...
lnL=log(prod(l))
lnL
# But be careful...the larger the sample, R will tackle the product first and think the resulting expression is zero
# rather ->

lnL=sum(dexp(x, 0.05, log=TRUE))
# The argument log = TRUE automatically takes the log of each likelihood without us having to write a separate line of code

# Calculate f(50) where X ~ Gamma(6,0.1)
dgamma(50, 6, 0.1)

# Calculate f(1000) where Y ~ logNormal(mu=5, sigma^2=4)
dlnorm(1000, 5, sqrt(4)) # -> remember the parametrization -> use sigma here, not sigma^2

# Y ~ Weibull(c, g)
c=0.00002
g=4

shape=g
scale=c^(-1/g)

set.seed(300)
y = rweibull(20, shape, scale)

lnL = sum(dweibull(y, shape, scale, log = TRUE))
lnL

# USing the CDF function
#  Y ~ lognormal(mu, sig^2)
# Calculate F(1000)

plnorm(1000, 5, sqrt(4))
plnorm(1000, 5, sqrt(4), lower.tail=FALSE)

# X ~ Gamma(6, 0.1)
# F(50)

pgamma(50, 6, 0.1)

# Remember 2*lambda*X ~ ChiSq(2*alpha)

# Calculate P(8 < Y < 12) where Y ~ Weibull(c, g)
# Using R...
shape=g
scale=c^(-1/g)
pweibull(12, shape, scale) - pweibull(8, shape, scale)

# Calculate P(Y > 0.3) where y ~ Weibull(c, g)
shape=g
scale=c^(-1/g)
pweibull(0.3, shape, scale, lower.tail=FALSE)


# Similarly we can use the quantile functions in R -> qweibull(prob, shape, scale)
# Calculate P(Y > y) = 0.3 where y ~ Weibull(c, g)
qweibull(0.7, shape, scale)

# Alternatively define a custom function that uses conventional parametrization ->
qweibull2=function(p, c, g){
	(-log(1-p)/c)^(1/g)
	}
	
qweibull2(0.7, c,g)

# Using Random Number generator functions in R

# Generating 10 random variates from lognormal(mu=5, sig=sqrt(4)) using seed -> 50
set.seed(50)
rlnorm(10, 5, sqrt(4))

# Question -> Claim amounts X ~ Gamma(6, lambda) -> lambda ~ Exp(10)
# Generate a random sample of 5 claims using seed -> 1
set.seed(1)
lambda = rexp(5, 10)
x=rgamma(5, 6, lambda)
# alternatively in one line
set.seed(1)
x=rgamma(5, 6, rexp(5,10))

# Question - RV -> Z=Y/X -> Y ~ Weibull(c, g) and X ~ Exp(lambda)
# Generate 3 Z variates using seed = 10

set.seed(10)

c=0.00002
g=4
l=0.1

shape=g
scale=c^(-1/g)

Z = rweibull(3, shape, scale) / rexp(3, l) 

set.seed(10)

rweibull2=function(n, c, g){
	(-log(1-runif(n))/c)^(1/g)
	}

Z = rweibull2(3, c, g) / rexp(3, l)


# Distributions not built in R
# R does not include functions for ->
# - 2-parameter Pareto
# - 3-parameter Pareto
# - Burr
# - Weibull ? perhaps the way its parametrized

rpareto = function(n, a, lambda){
	lambda * ((1 - runif(n))^(-1/a)-1)
	}

dpareto = function(x, a, lambda){
	a * lambda^(a)/((lambda + x)^(a+1))
	}

ppareto = function(q, a, lambda){
	1 - (lambda/(lambda + q))^a
	}

qpareto = function(p, a, lambda){
	lambda * ((1 - p)^(-1/a)-1)
	}

rburr = function(n, a, lambda, g){
	(lambda*((1 - runif(n))^(-1/a) - 1))^(1/g)
	}

dburr = function(x, a, lambda, g){
	a * g * lambda^(a)*x^(g - 1)/((lambda + x^g)^(a+1))
	}

pburr = function(q, a, lambda, g){
	1 - (lambda/(lambda + q^g))^a
	}

qburr = function(p, a, lambda, g){
	(lambda*((1 - p)^(-1/a) - 1))^(1/g)
	}


# Question - logic behine qpareto() above
# Remember determining a quantile requires a probability -> P(X<q) = p
# The Pareto distribution has a closed-form solution and is therefore invertible
# F(x) = 1 - ((lambda)/(lambda+x))^alpha

# Question - X ~ Burr(alpha, lambda, gamma) > plot CDF 0<x<100

alpha=10
lambda=500
gamma=0.5

x=seq(0,100,0.1)
F.x=pburr(x, alpha, lambda, gamma)

plot(x, F.x, xlab="X=x", ylab="Probability", main="CDf of X ~ Burr(alpha, lambda, gamma)", col="blue", type="l", lty=2)

# 3-parameter Pareto
# Remember there is no closed-form function for 3-parameter Pareto
# We can determine the density function -> d3pareto(alpha, lambda, gamme)

d3pareto = function(x, a, lambda, k){
	gamma(a + k) * lambda^a / (gamma(a) * gamma(k)) *x^(k-1) / (lambda + x)^(a + k)
	}

# To get a CDF for the 3-parameter Pareto, we will have to numerically integrate
# Because there are multiple arguments in the R fundtion, we speficy the values of the arguments in the integrate() function
# R will then know what to numerically integrate

# Determine P(X>10000) -> Becuase we coded this function, we can't user lower.tail=FALSE
# -> 1- P(X<10000)
# X ~ 3Pareto(alpha, lambda, k)
# REMEMBER TO put $value after the integrate -> you want the value of the integral

1-integrate(d3pareto, 0, 10000, a=2, lambda=200, k=7)$value


# Weibull Distribution when coded from scratch - USEFUL IF YOURE NOT USING R PARAMETRIZATION OF Weibull

rweibull2 = function(n, c, g){
	(-log(1 - runif(n))/c)^(1/g)
	}

dweibull2 = function(x, c, g){
	c * g * x^(g-1) * exp(-c * x^g)
	}

pweibull2 = function(q, c, g){
	1 - exp(-c * q^g)
	}

qweibull2 = function(p, c, g){
	(-log(1 - p)/c)^(1/g)
	}


#########################################################################################
# Question 1

rpareto = function(n, a, lambda){
	lambda * ((1 - runif(n))^(-1/a)-1)
	}

set.seed(80)
a=3
lambda=10000

x=rpareto(500, a, lambda)

skew.x = (1/length(x))*sum((x-mean(x))^3) # -> there is no skew functon in R...so rely on estimation of third non-central moment
skew.coef=skew.x/(var(x)^(3/2))


#########################################################################################
# Question 2

# Claims frequency X ~ beta(a, b)
# ?distributions helpful

a=0.02
b=0.4

# P(X>0.75)
pbeta(0.75, a, b, lower.tail=FALSE) # -> 0.02990434


#########################################################################################
# Question 3

d3pareto = function(x, a, lambda, k){
	gamma(a + k) * lambda^a / (gamma(a) * gamma(k)) *x^(k-1) / (lambda + x)^(a + k)
	}
	
# Calculate the median of X ~ Pareto(a=2, lambda=5, k=4)

xl=0
xu=1000
mid=0.5*(xl+xu)
eps=1e-9

a=3
lambda=5
k=4

prob = integrate(d3pareto, 0, mid, a=3, lambda=5, k=4)$value

while (abs(prob - 0.5 ) > eps){
	
	if (prob - 0.5 < 0 ){
		xl = mid
		mid = 0.5*(xl+xu)
		prob = integrate(d3pareto, 0, mid, a=3, lambda=5, k=4)$value
		}
	else {
		xu = mid
		mid = 0.5*(xl+xu)
		prob = integrate(d3pareto, 0, mid, a=3, lambda=5, k=4)$value
		}
	
	}
mid # -> 6.865009 using bisection method

# Reasonableness check -> 3 Pareto is positively skewed so we expect -> Median < E(X)
# Can solve it another way -> use nlm with objective function abs(F(m)-0.5)

Func = function(M){
	abs(integrate(d3pareto, 0, M, a=3, lambda=5, k=4)$value - 0.5)
	}

M.start = k*lambda/(a-1) # -> for a>1 -> this is the mean or expected value of  X ~ 3Pareto(a, lambda, k)

nlm(Func, M.start)$estimate # -> 6.865008


#########################################################################################
# Question 4

# X ~ lognormal(mu, sig^2) ->
mu=0
sig=sqrt(0.25)

# Mode is the turning point -> use -nlm i.e. find the maximum of the density function
obj=function(m){
	-dlnorm(m, 0, sqrt(0.25))
	}

m.start=exp(mu+0.5*(sig^0.5))

nlm(obj, m.start)$estimate
	
x=seq(0.77,0.8,0.005)
plot(x, dlnorm(x, 0, sqrt(0.25)), col="red", type="l", lty=2)

# R code in one line -> x[which(dlnorm(x, 0, 0.5) == max(dlnorm(x, 0, 0.5)))]
# -> where mu=0, sig=sqrt(0.25) -> sig=0.5


#########################################################################################

# Fitting a Distribution

# Summarize baic statistical features of a dataset -> first step in fitting a dsitribution
# How do you estimate parameters of a distribution -> MLE
# Q-Q plots -> assessing Goodness of Fit -> GoF

Losses = read.table("Losses.txt", header = TRUE)

# Useful overview of the data set -> summary() function

summary(Losses$Losses)

# -> from the summary, the median is less than the mean...so we can already suspect a postively skewed dataset

# Doing some of the calcs manually ->
min(Losses$Losses); max(Losses$Losses)

quantile(Losses$Losses, seq(0,0.5,0.1)) # -> useful

# Emperical Probability ->
# Prob of a loss being greater than 5000 ->
length(Losses$Losses[Losses$Losses > 5000])/length(Losses$Losses)

# Locating a specific observation within the dataset -> This gives the index in the vector under study
which(Losses$Losses == min(Losses$Losses)) # -> 837
Losses$Losses[which(Losses$Losses == min(Losses$Losses))] # -> 1.09 which is indeed the minimum of Losses$Losses


# Mean, Variance, and median
mean(Losses$Losses)
var(Losses$Losses)
quantile(Losses$Losses, 0.5) # -> or
median(Losses$Losses)

# Mode -> the mode is the value with the highest probability. We calculate it as the emperical probability associate to the 
# observation that occurs most frequently

# table(Losses$Losses) # -> Can't use this because...continuous values! need another way

Losses$b = Losses$Losses%/%1 # -> This groups the data into integer bands -> use the integer quotient %/% which is -> 87.456 = 87 + 0.456 i.e. integer part of lossess

# Try using aggregate() function
data = aggregate(Losses ~ b, data = Losses, length)
# now -> find the highest frequency -> look in column 2

f=max(data[,2]) # -> 8

# Locate the row in data that the frequency 8 occurs -> use which()

which(data[,2] == max(data[,2])) # -> row 114
data$b[114] # -> Loss = 120

# Therefore the mode is 120 -> corrsponds to highest frequency = 8 -> emperical prob = 8/1000 -> 0.8% of occuring
# or rather -> 120 <= mode < 121

# Maximum Likelihood Estimates
# Method -> First use functions in R and then use fitdistr() in MASS package

install.packages("MASS")
library(MASS)

# Use fitdistr(data, distribution, starting values) -> fits using MLE
# -> data is the vector of the data eg. claim values
# -> distribution -> anme of the dist eg. Gamma, Exponential, etc
# -> starting values are a list of starting values for the parameters -> probably use the list() function

# use the list() function -> list(name1 = value1, name2 = value2, name 3 = value 3, etc)
# If we're fitting, then you know GoF is coming..DoF!

# Note -> starting values are not required when formluae exists -> case for:
# - normal
# - logNormal
# - geometric
# - exponential
# - poisson

# Use lower argument to set a lower limi on the estimates when they have to be postive -> for instance the Gamma Distribution

# Example

set.seed(1066)
x = rlnorm(1000, 5, 2)

# We're now going to estimate MLE for mu and sigma

# ML estiamte for the lognormal distribution exists in closed form:
# mu = (1/n) * sum(log(xi))
# sigma^2 = (1/n) * sum((log(xi)-mu)^2)

# Use R to calc ->

mu = (1/length(x))*sum(log(x))
sigma=sqrt((1/length(x))*sum((log(x)-mu)^2))
# or 
sigma=sqrt((length(x)-1)/length(x)*var(log(x))) # -> R will calculate the sample variance -> we must convert it to the population Variance
# if we are using var() function

# We could instead use nlm ->
# Stat by defining the log likelihood -> lnL

lnL = sum(log(dlnorm(x, mu, sigma))) # or lnL = sum(dlnorm(x, mu, sigma, log=TRUE)) -> be careful with log=TRUE...if YOU log the likelihood, DONT INCLUDE log=TRUE

f=function(params){
	-sum(log(dlnorm(x, params[1], params[2]))) # -> or -sum(plnorm(x, params[1], params[2], log = TRUE))
	}

f1 <- function(data, params){
	-sum(log(dlnorm(data, params[1], params[2])))
	}

params.0=c(5, 5)
MLE = nlm(f1, params.0, data=x) # -> If we get a warnings() list, check GoF if we're worried

# A word of warning! This method is sensitive to choice of starting parameters
# Be mindful of the $code ande $iterations -> code = 4 means R stopped because it hit max number of iterations -> iterations = 100
# A good starting point for initial parameters -> Use Method of Moments!

# Now use the MASS package
# Use fitdistr() -> fitdistr(data, "distribution)

fitdistr(x, "lognormal")
# -> str() is useful again for extraction

# Its useful to use MoM to help with initial parameter estimation ->
# For instance, the lognormal dist -> E(X) = apprx estimate as xbar = exp(mu +0.5*sigma^2)
# and s^2 = (xbar)^2*(exp(sigma^2)-1)

# Question 

set.seed(1812)
x = rgamma(1000, rpois(1, 100), rexp(1,5)) # -> alpha ~ Poisson(100), beta ~ Exp(5)

# You believe that X ~ Gamma(alpha, beta)

alpha = 111; beta = 0.189
params.0=c(alpha, beta)

f1=function(data, params){
	-sum(log(dgamma(data, params[1], params[2])))
	}

MLE=nlm(f1, params.0, data=x)$estimate
alpha=MLE[1]; beta=MLE[2]
# Check for reasonableness -> try a different params.0 = c(u,v)

# Ty the same problem using MASS package -> fitdistr()

MLE=fitdistr(x, "gamma") # -> notice what parameters R gives us...shape and rate
# This is consistent in the interpretation of the parameters but dont be fooled with Weibull! VERY IMPORTANT!

# We can also use the lower argument to ensure the parameters searched for are > 0

MLE=fitdistr(x, "gamma", list(shape=111, rate=0.189), lower=0) # -> see the form...also, the list() here will be the intial starting parameters

# Question

Loss=Losses$Losses

a=5
l=1500

dpareto = function(x, a, lambda){
	a * lambda^(a)/((lambda + x)^(a+1))
	}

params.0=c(a,l)

f=function(data, params){
	-sum(log(dpareto(data, params[1], params[2])))
	}

MLE.nlm=nlm(f, params.0, data=Loss)$estimate

# ?fitdistr is useful on the distributions MASS can fit

# Analysing Goodness of Fit -> and DoF!
# Plotting the fitted density function
# ->

hist(Loss, freq=FALSE)
# Plot the fitted distrubution on same plot as histogram

x= seq(0,6500)
a=MLE.nlm[1]
l=MLE.nlm[2]

# Calcualte the pdf for Pareto(a, b)
y = dpareto(x, a, l)

# lines(x, y, col="red")
# or
curve(dpareto(x, a, l), add = TRUE, col="red")

legend("topright",
legend = c("histogram of losses", "PDF of fitted Pareto distn"),
col = c("black", "red"), lty = 1)


# Plotting Emperical Density 
# -> Better way to analyse GoF is to plot the fitted density on the same graph as the emperical density
# -> density() function calculates the emperical density function along a selected range


density(Loss, from = 0, to = 3000) # -> notice here the form: density(data, from=..., to=...)
# -> str() again is useful -> extract x and y values

lines(density(Loss, from = 0, to = 3000), col="blue")
	
# Update the limits becuase the density curve is being cut-off

hist(Losses, freq=FALSE, ylim = c(0, 0.002))
curve(dpareto(x, a, l), add = TRUE, col = "red")
lines(density(Losses, from = 0, to = 3000), col = "blue")

legend("topright", legend = c("histogram of losses", "PDF of fitted Pareto distn", "Empirical density"), col = c("black", "red", "blue"), lty = 1)

# Plot the emperical density on its own ->
plot(density(Losses, from = 0,to = 3000), xlab = "Losses", main ="Empirical density function versus Pa(3.416,1047)",col = "blue")

# Becaues we calculated the density from = 0 -> to = 3000, the density is over x -> 0:3000 interval
# But x runs from seq(0,6500) as defined -> so we need to shorten it to correspond with 0:3000 interval for the fitted density
x = seq(0,3000)
y=dpareto(x, a, l)
# lines(x,y, col="red")
# or 
curve(dpareto(x, a, l), add = TRUE, col="red")
legend("topright", legend = c("empirical density", "PDF of fitted Pareto distn"), col = c("blue", "red"), lty = 1)


# Q-Q Plots
# -> A Q-Q plot is a scatterplt plotting two sets of quantiles against one another
# -> It takes the sample data in ascending order and then plots each point in the data against quantiles calculated from theoretical distribution
# -> Result of plot is a visual tool to inspect how closely the dataset fits the chosen theoretical distribution

ozone = airquality$Ozone
summary(ozone)

ozone = ozone[!is.na(ozone)] # -> !is.na means to only include this entries that aren't NA
mean.ozone=mean(ozone)
sd.ozone=sd(ozone)

# Lets assume the data comes from a N(mean.ozone, sd.ozone^2) distribution
# To plot the Q-Q for this fitted distribution, first calculate the theoretical comparison Quantiles

n = length(ozone)
comparison.qs = qnorm(ppoints(n), mean.ozone, sd.ozone) # -> ppoints(n) splits the n points into equal intervals across 0:1 -> i.e. cumsum(1/n)
# the ppoints(n) splits n into 2 parts -> get 2n split across (0,1) because of the symmetrical nature of the distribution

# We've got the quantiles from theorectical distribution, and the now we need the quantiles from observed -> R will sort it automatically

qqplot(comparison.qs, ozone, xlab = "Quantiles from fitted normal distribution", ylab = "Sample quantiles", main = "Q-Q plot of ozone data", col="blue")
abline(0, 1, col="red")

# From inspection -> the plotted points don't fall closely onto the straight line -> fitted model seems to be a poor fit in tails of the distribution
# When comparing to the normal distribution, use qqnorm() function

qqnorm(ozone, xlab = "Quantiles from fitted normal distribution", ylab = "Sample quantiles", main = "Q-Q plot of ozone data")
qqline(ozone, col="red")
# Still not a great fit

# Fit a Gamma distribution
# Use Method of Moments to get initial parameters -> E(x)=alpha/beta , s^2 = alpha/(beta^2)
# -> alpha = xbar^2/(s^2)
# -> beta = xbar/(s^2)

shape = (mean.ozone/sd.ozone)^2 # -> alpha
rate = mean.ozone/(sd.ozone^2) # -> beta

params.0=c(shape, rate)
f=function(data, params){
	-sum(log(dgamma(data, params[1], params[2])))
	}
MLE=nlm(f, params.0, data=ozone)$estimate

comparison.qs = qgamma(ppoints(n), MLE[1], MLE[2])

qqplot(comparison.qs, ozone, xlab = "Quantiles from fitted Gamma distribution", ylab = "Sample quantiles", main = "Q-Q plot of ozone data", col="blue")
abline(0,1, col="red")
# Looks like a beter fit than the fitted normal -> Q-Q plots are a useful tool to visualize the GoF of a distribution given the dataset


# Question

# Use Loss dataset
# We fitted a Pareto(a, l) distribution with MLE.nlm fitted parameters ->

MLE.nlm

qpareto = function(p, a, lambda){
	lambda * ((1 - p)^(-1/a)-1)
	}

n=length(Loss)

comparison.qs=qpareto(ppoints(n), MLE.nlm[1], MLE.nlm[2])

qqplot(comparison.qs, Loss, xlab = "Quantiles from fitted Pareto distribution", ylab = "Sample quantiles", main = "Q-Q plot of Loss data", col="blue")
abline(0, 1, col="red")

# Pareto distribution seems a good fit for losses less than 2000 -> see Q-Q Plot
# -> The fit is less accurate in the upper tail i.e. for losses great than 2000
# -> We expect the fit to looks worse in the tail...particularly for a heay-tailed distribution...
# even if the fitted distribution is the true underlying distribution


#########################################################################################
# Question 5

claim.size = read.table("ClaimSize.csv", header = T)

alpha = c(1.7, 1.8, 1.9, 2)
beta = c(15, 16, 17, 18, 19, 20)*0.0001

l.a=length(alpha)
l.b=length(beta)

result = numeric(0)

# Assume X ~ Gamma(alpha, beta)

for (i in 1:l.a){
	for (j in 1:l.b){
		logL = log(dgamma(claim.size$ClaimSize, alpha[i], beta[j]))
		sumL = sum(logL)
		result = rbind(result, c(alpha[i], beta[j], sumL))
		}
	}

result # -> We show the log likelihood -> we want the pair of parameters that give the highest negative value of the log likelihood

a = result[result[, 3] == max(result[,3])][1]
b = result[result[, 3] == max(result[,3])][2]

par(mfrow=c(2,1))

x=seq(0,3000)
hist(claim.size$ClaimSize, breaks=20, xlim=c(0,5000) ,freq=FALSE, ylim=c(0,0.001), ylab="density")
curve(dgamma(x, a, b), add=TRUE, col="red", ylim=c(0,15e-04))

n=length(claim.size$ClaimSize)
comparison.qs=qgamma(ppoints(n), a, b)
qqplot(comparison.qs, claim.size$ClaimSize, xlab = "Quantiles from fitted Gamma distribution", ylab = "Sample quantiles", main = "Q-Q plot of Claim Size data", col="blue")
abline(0, 1, col="red")

par(mfrow=c(1,1))


#########################################################################################
# Question 6

# Claims ~ Exp(lambda)

claims=read.table("exp.txt")$x
n=length(claims)

l.start=0.002
# We know the value of lambda = n/sum(x)

f=function(data, l){
	-sum(log(dexp(data, l)))
	}

MLE=nlm(f, l.start, data=claims)$estimate
l=MLE

# Check -> n/sum(claims) ->
n/sum(claims) # -> close enough

# Emperical density versus fittede density ->
# Fitted density:
x=seq(0:6000)
plot(x, dexp(x, l), col="red", type="l", lwd=2, lty=2)

# Emperical density
lines(density(claims, from = 0, to = 6000), col="blue", lwd=2)

max(density(claims, from = 0, to = 6000))
# Looks like a close fit but for lowe calim abounts, there doesn't look like it suits an expoenential Distribution

#########################################################################################
# Question 7

motor.claims=read.table("MotorClaims.txt", header= T)$MotorClaims
n=length(motor.claims)
lhat=0.003

# Believe claim follow exponential distribution -> calculated lhat=0.003

comparison.qs=qexp(ppoints(n), lhat)
qqplot(comparison.qs, motor.claims, xlab = "Quantiles from fitted Exponential distribution", ylab = "Sample quantiles", main = "Q-Q plot of Motor Claims data", col="blue")
abline(0,1, col="red")

# Poor fit -> data doesn't like near the refence line. We recommend to fit alternative distribution

hist(motor.claims, freq=FALSE, ylim=c(0,0.005))
# Try fitting lognormal ->

xbar=mean(motor.claims)
s2 = var(motor.claims)

mu = (1/n)*sum(log(motor.claims))
sigma = sqrt((1/n)*sum((log(motor.claims)-mu)^2))

params.0 = c(mu, sigma)

f=function(data, params){
	-sum(log(dlnorm(data, params[1], params[2])))
	}
	
MLE=nlm(f, params.0, data=motor.claims)$estimate
mu=MLE[1]; sigma=MLE[2]

x=seq(0,870)
curve(dlnorm(x, mu, sigma), add=TRUE, col="red")


#########################################################################################

# Loss Distribution functions

# Calculate probabilities and generate random variates from statistical distributions
# Do the same for customisable distribution functions 
# Calc basic metrics of a distribution -> mean, median, mode, etc

# Weibull has shape and scale parametrization -> shape = g and scale = c^(-1/g)
# Normal distribution in R is parametrised N(mu, sig) -> not sig^2

# Example
dexp(30, 0.05)

# To get the log likelihood -> use log = TRUE
dexp(30, 0.05, log = TRUE)


# Example with sample
X = c(18.9, 16.2, 51.5, 65.2, 23.8, 4.2, 21.1, 0.5, 15.2, 3.3)
# -> vector with likelihoods ->
l = dexp(X, 0.05) # -> vector

L = prod(l)
# -> and the log(L) ->
LnL = log(L) # -> -40.95232

# another way to get lnL ->
LnL = sum(log(dexp(X, 0.05)) # -> -40.95232

n.LnL = function(data, params){
	-sum(log(dexp(data, params)))
	}

params0 = 0.02
mle = nlm(n.LnL, params0, data = X)$estimate

# Other examples ->
x = 50; dgamma(x, 6, 0.1)
x = 1000; dlnorm(x, 5, sqrt(4))

# Y ~ Weibull(c, g) -> generate sample of 20 variates from Weibull(c, g)
c = 0.00002
g = 4

set.seed(300)
shape = g
scale = c^(-1/g)

y = rweibull(20, shape, scale)
log.like = sum(log(dweibull(y, shape, scale)))
log.like # -> -56.0779

# Y ~ Weibull(c, g) -> Calc P(8 < Y < 12)
pweibull(12, shape, scale) - pweibull(8, shape, scale) # -> 0.2608205

# Quartile function -> X ~ exp(0.02)
# Want P(X <= M) = 0.5
qexp(0.5, 0.02) # -> 34.65736

# IQR for X ~ gamma(6, 0.1)
qgamma(0.75, 6, 0.1) - qgamma(0.25, 6, 0.1) # -> 32.03492

# Y ~ Weibull(c, g) -> calculate P(Y > y) = 0.3
qweibull(0.3, shape, scale, lower=FALSE) # -> 15.66378

# Random Number Generation ->
set.seed(50)
rlnorm(10, 5, 2) # -> 10 variates geneated randomly from X ~ logN(5, 2^2)

# Example
# Claim amounts on given policy X ~ gamma(6, lambda)
# -> lambda differs between policies and is believed to follow Exp(10)
# Generate random sample of five claims using seed = 1

set.seed(1)

lambda = rexp(5, 10)
X = rgamma(5, 6, lambda) # -> 32.85776  56.72204 505.65996 495.95443 110.23761

# Example
# Z = Y / X where:
# -> Y ~ Weibull(c, g)
# -> X ~ Exp(0.1)

set.seed(10)
Z = rweibull(3, shape, scale) / rexp(3, 0.1) # -> 3.5138341 0.6385402 0.9119199

# R packages do not include functions for:
# -> two - parameter Pareto distribution
# -> three - parameter Pareto distribution
# -> Burr distribution
# -> Weibull standard -> we use the shape, scale version as long as you know how to use it both way - REFER TO SURVIVAL MODELS! DID A LOT OF WORK WITH WEIBULL THERE

# -> GOMPERTZ! REMEMBER shape, rate -> rate =B*(C^(dt)) eg. dt = a tor T + a

# Two - Parameter Pareto Functions: R doesnt have pareto functions...weird that it doesn't

rpareto = function(n, a, lambda){
	lambda * ((1 - runif(n))^(-1/a)-1)
	}
	
dpareto = function(x, a, lambda){
	a * lambda^(a)/((lambda + x)^(a+1))
	}
	
ppareto = function(q, a, lambda){
	1 - (lambda/(lambda + q))^a
	}
	
qpareto = function(p, a, lambda){
	lambda * ((1 - p)^(-1/a)-1)
	}
	
rburr = function(n, a, lambda, g){
	(lambda*((1 - runif(n))^(-1/a) - 1))^(1/g)
	}

# Burr Functions:

dburr = function(x, a, lambda, g){
	a * g * lambda^(a)*x^(g - 1)/((lambda + x^g)^(a+1))
	}
	
pburr = function(q, a, lambda, g){
	1 - (lambda/(lambda + q^g))^a
	}
	
qburr = function(p, a, lambda, g){
	(lambda*((1 - p)^(-1/a) - 1))^(1/g)
	}
	
# Plot pburr() for 0 <= x <= 100
# X ~ Burr(a, lambda, g)
curve(pburr(x, 10, 500, 0.5), 0, 100 )

# Three - Parameter Pareto functions

d3pareto = function(x, a, lambda, k){
	gamma(a + k) * lambda^a / (gamma(a) * gamma(k)) * x^(k-1) / (lambda + x)^(a + k)
	}
	
# There is NO CDF FOR THREE PARAMETER PARETO! ... only a desnity function
# We can use numerical integration to solve for CDF requirement

# P(X <= 10) - F(10) where X ~ 3Pareto(a, l, g)
integrate(d3pareto, 0, 10, a = 2, lambda = 4, k =5)$value # -> 0.451555 

# Claims for a class of insurance follow 3-parameter Pareto -> X ~ 3Pareto(2, 200, 7)
# P(X > x) i.e. numerical integration as a function for CDF

# CDF -> P(X <= x):
p3pareto = function(x, a, lambda, k){
	integrate(d3pareto, 0, x, a = a, lambda = lambda, k = k)$value
	}

# P(X > 10000) -> 1 - P(X <= 10000)
1 - p3pareto(10000, 2, 200, 7) # -> 0.009951169

# For reference -> alternative to WEibull in R:

rweibull2 = function(n, c, g){
	(-log(1 - runif(n))/c)^(1/g)
	}

dweibull2 = function(x, c, g){
	c * g * x^(g-1) * exp(-c * x^g)
	}

pweibull2 = function(q, c, g){
	1 - exp(-c * q^g)
	}

qweibull2 = function(p, c, g){
	(-log(1 - p)/c)^(1/g)
	}


#########################################################################################
# Question 1

set.seed(80)

# USe custom Pareto function becuase R doesn't have a built in one
a = 3
l = 10000

x = rpareto(500, a, l)

# Skewness -> third centrol momement -> E((x - mu)^3)
skew = (1/length(x)) * sum((x - mean(x))^3)
skew # -> 4.193369e+12

# Coefficient of skewness ->
skew / (var(x)^(3/2)) # -> 6.903348


#########################################################################################
# Question 2

# Claim frequency follows a beta distribution -> N ~ beta(a, b)
a = 0.02
b = 0.4

# P(N > 0.75)
pbeta(0.75, a, b, lower=FALSE) # -> 0.02990434


#########################################################################################
# Question 3

# X ~ 3Pareto(a, l, k)
a = 3
lambda = 5
k = 4

# R doesn't built in function for the CDF
# Also there is no analytical formula CDF -> need to numerically evaluate
# Calculate the median of X

p3pareto = function(x, a, lambda, k){
	integrate(d3pareto, 0, x, a = a, lambda = lambda, k = k)$value
	}

obj = function(x, target){
	abs(p3pareto(x, a, lambda, k) - target)
	}

x0 = k * lambda / (a - 1)

nlm(obj, x0, target = 0.5)$estimate # -> 6.865008


xl=0
xu=1000
mid=0.5*(xl+xu)
eps=1e-9

a=3
lambda=5
k=4

prob = integrate(d3pareto, 0, mid, a=3, lambda=5, k=4)$value

while (abs(prob - 0.5 ) > eps){
	
	if (prob - 0.5 < 0 ){
		xl = mid
		mid = 0.5*(xl+xu)
		prob = integrate(d3pareto, 0, mid, a=3, lambda=5, k=4)$value
		}
	else {
		xu = mid
		mid = 0.5*(xl+xu)
		prob = integrate(d3pareto, 0, mid, a=3, lambda=5, k=4)$value
		}
	
	}
mid # -> 6.865009 using bisection method


#########################################################################################
# Question 4

# X ~ LogN(0, 0.25) -> remember sig = sqrt(0.25) in R parametrization
# mode -> the max of the pdf

obj = function(x, mu, sig){
	-dlnorm(x, mu, sig)
	}

mu = 0
sig = sqrt(0.25)

x0 = exp(mu + 0.5*sig^2)

nlm(obj, x0, mu, sig)$estimate # -> 0.7788003

x = seq(0, 2, by = 0.001)
x[which.max(dlnorm(x, 0, sqrt(0.25)))] # -> 0.7891086

plot(x, dlnorm(x, mu, sig), main = "Density function for LogN(0,0.25)", col = "blue")


#########################################################################################

# Loss Distributions Fitting a Loss distribution

# Estimate the parameters of a distribution using MLE
# Discuss Q-Q plots -> assess Goodness of Fit

Losses = read.table("Losses.txt", header = TRUE)
head(Losses)

summary(Losses$Losses)

quantile(Losses$Losses, seq(0, 1, by = 0.1))

# Emperical probability
large.Losses = Losses$Losses[Losses$Losses > 5000]
length(large.Losses) # -> 2
# P(Lossess > 5000) ->
length(large.Losses) / length(Losses$Losses) # -> 0.002

# Locating specific observations within the data set
# Example: find the minimum observation
which.min(Losses$Losses) #- > give the index in the array -> 837
Losses[which.min(Losses$Losses), ]

# Key metris
# Example - find the 50% quantile
quantile(Losses$Losses, 0.5) # -> 230.95 (50%)

# Mode -> value with highest probability -> so calculate the observation that occurs most frequently
# First start by converting continuoes data into an integer ->
Losses$b = (Losses$Losses %/% 1)
head(Losses)

# now get the frequency ->
data = aggregate(Losses ~ b, data = Losses, FUN = length)
head(data)

which.max(data$Losses) # -> gives the index in data$Losses where the most loss is: 114
data[which.max(data$Losses), ]
mode = data[which.max(data$Losses), ][1] # -> 120
# So the mode lies between 120 <= mode <= 121

# Maximum Likelihood Estimates
# There are two ways to fit a distribution wuing MLE:
# -> using nlm()
# -> using fitdistr() from MASS Package

# We can numerically estimate the ML estimtaes by:
# -> Write a function to calculdate the ngative log likelihood of the observed sample
# -> Specify a set of initial paramter values to use as an input into the function
# -> R will flex the parameters to minimise the negative log-Likelihood

# MASS Package
install.packages("MASS")
library(MASS)

# To use fitdistr() ->
# fitdistr(<data>, <distribution>, <starting values>)
# Where <data> -> oberved sample, <distribution> the Distribution to fit, and <starting values> -> list starting values for the parameters
# list() in R -> list(<name1> = <values1>, <name2> = <values2>, ...)

# R knows if an analytical formula exists for the ML estiamte and as such we don't have to specify starting values
# Distributions with know analytical MLE solutions -> normal, lognormal, geometric, exponential, and Poisson

# When R has to numerically peform the MLE, it needs starting values

# We can use the 'lower' argument to set a lower limit on the estimtes when they have to be positive -> see this for the gamma Distribution

# Example ->

set.seed(1066)
x = rlnorm(1000, 5, 2)
# here mu = 5, sig = 2

head(x)

# We can use fitdistr() to find mu and sig -> hopefully close to the values mu = 5 and sig = 2

mu0 = mean(log(x))
sig0 = sqrt(var(log(x)) * length(x) / (length(x) -1 ))

params0 = c(mu0, sig0)

neg.loglik = function(data, params){
	-sum(log(dlnorm(data, params[1], params[2])))
	}
	
params = nlm(neg.loglik, params0, data = x)
params$estimate # -> 4.988398 2.017083
# From the random sample generatd x -> mu = 5, sig = 2

# If we generated 10000 -> 
set.seed(1066)
x = rlnorm(1000, 5, 2)
# here mu = 5, sig = 2
params = nlm(neg.loglik, params0, data = x)
params$estimate # -> 4.999820 2.000643

# Always check the interations and code of the minimization -> starting values can cuase the minimization scheme to drift into a direction that eventualy wont find the negative minimum

# We can use the MASS fitdistr() function without need to speficy initial values -> R will use the analytical solution to fit the distribution to the sample ->
fitdistr(x, "lognormal")
# meanlog -> 4.999822586  with std error 0.002000643 
# sdlog -> 2.000642925 with std error 0.001414668

# mu0 = mean(log(x)) -> R uses this to calculated meanlog
# sig0 = sqrt(var(log(x)) * length(x) / (length(x) -1 )) -> R uses this to calculate the sdlog

# We can also use the method of moments -> for two paramters, we will need two equations that invole the parameters
# In the log-normal case ->

# xbar = exp(mu + 0.5*sig^2)
# s^2 (of sample) = exp(2*mu + sig^2) * (exp(sig^2) - 1)

# The estimation using MoM -> use as starting values in MLE procedure ->

mu0 = log(mean(x)) - 0.5*log(1 + var(x)/mean(x)^2)
sigma0 = (log(1 + var(x)/mean(x)^2))^0.5

params0 = c(mu0, sig0)

params = nlm(neg.loglik, params0, data = x)
params$estimate # -> 4.988397 2.017083

set.seed(1812)
x = rgamma(1000, rpois(1, 100), rexp(1, 5))
# Claims X ~ gamma(A ~ Poisson(100), B ~ Exp(5))

a0 = 111
b0 = 0.189

params0 = c(111, 0.189)

obj = function(data, params){
	-sum(log(dgamma(data, params[1], params[2])))
	}
	
params = nlm(obj, data = x, params0)
params$estimate

# For A ~ Poisson(100) -> E(A) = 100
# For B ~ Exp(5) -> E(B) = 0.2

# MASS Package -> Remember there is no analytical solution for the MLE in the case of the gamma distribution -> need to provide starting values
params = fitdistr(x, "gamma", list(shape = a0, rate = b0)) # -> check in R dgamma() -> you;ll see shape -> alpha and rate -> beta
# shape -> 1.117783e+02
# rate -> 1.903857e-01 


# Example using Losses -> X ~ Pareto(alpha, lambda)
x = Losses$Losses

# Use MoM to get initial estimates of a0 and l0 ->
# E(X) ~ mean(x)
# Var(X) ~ var(x)

a0 = 5
l0 = 1500

params0 = c(a0, l0)

# Also R doesn't have function for d3pareto ->
dpareto = function(x, a, lambda) {
	a*(lambda^a)/((lambda + x)^(a + 1))
	}

obj = function(data, params){
	-sum(log(dpareto(data, params[1], params[2])))
	}

params = nlm(obj, data = x, params0)
params$estimate # -> 3.416292 1047.018690

# Package MASS can fitdistr() to the following distributions

# "beta" "lognormal"
# "cauchy" "logistic"
# "chi-squared" "negative binomial"
# "exponential" "normal"
# "gamma" "Poisson"
# "geometric" "t"
# "log-normal" "weibull"

# Analysing Goodness of fit

# Plotting the fitted density function ->
hist(Losses$Losses, freq = FALSE)
# Now we can plot the fitted distribution on the the histogram

x = seq(0, 6500)
a = params$estimate[1]
lambda = params$estimate[2]

y = dpareto(x, a, lambda)

hist(Losses$Losses, freq = FALSE)
lines(x, y, col = "red")
legend("topright", legend = c("pdf of X ~ Pareto(a, lambda)"), col = "red", lwd = 1)
# or
hist(Losses$Losses, freq = FALSE)
curve(dpareto(x, a, lambda), add = TRUE, col = "red")
legend("topright", legend = c("pdf of X ~ Pareto(a, lambda)"), col = "red", lwd = 1)

# Plotting the Emperical Density ->
density(Losses$Losses, from = 0, to = 6500) # Output in R to summarise the emperical density of Losses$Losses

# use lines() to plot the density after the hist() plot
hist(Losses$Losses, freq = FALSE)
lines(density(Losses$Losses, from = 0, to = 6500), col = "blue", lty = 2) 
legend("topright", legend = c("Emperical Density of Losses$Losses"), col = "blue", lwd = 1)

# Add the theoretical desnity to the plot ->
hist(Losses$Losses, freq = FALSE)
lines(x, y, col = "red")
lines(density(Losses$Losses, from = 0, to = 6500), col = "blue", lty = 2) 
legend("topright", legend = c("Emperical Density of Losses$Losses", "pdf of X ~ Pareto(a, lambda)"), col = c("blue", "red"), lwd = 1)

# When the density lines are cut-off -> adjus tthe ylim in the first plot -> in this case hist()
hist(Losses$Losses, freq = FALSE, ylim = c(0, 0.0025))
lines(x, y, col = "red")
lines(density(Losses$Losses, from = 0, to = 6500), col = "blue", lty = 2) 
legend("topright", legend = c("Emperical Density of Losses$Losses", "pdf of X ~ Pareto(a, lambda)"), col = c("blue", "red"), lwd = 1)

# We can plot the emperical density on its own ->
plot(density(Losses$Losses, from = 0, to = 3000), xlab = "Losses", main = "Empirical density function versus Pa(3.416,1047)", col = "blue")

# Good idea to store density() into a variable -> q = density(Losses$Losses) 
# use str(q) to get info out especially on the x and y values -> also you will understand the length(q$x)) and why you can't just plot theorectical pdf onto the density plot
# -> the length of the x values differ unless you make them both work -> adjust the seq(start, end, adjust by...)

# Q-Q plots
# A Q-Q plot is a scatter plot that plots two sets of quantiles against one another
# It takes the sample data, sort in ascending order and plot each point again the quantiles calculated from theoretical distribution
# USe the Q-Q plot as a visual tool for inspecting how closely the dataset fits the chosen theoretical distribution

# Example
# Using built in dataset airquality in R

head(airquality)

ozone = airquality$Ozone
summary(ozone)

# There are 37 NA's -> missing values so we need to exclude these ->
ozone = ozone[!is.na(ozone)]
summary(ozone)

mean.ozone = mean(ozone) # -> 42.12931
sd.ozone = sd(ozone) # -> 32.98788

# Fortunately, fitting the parameters are easy with normal distribution ->
# mu = mean()
# sig = sd() -> be careful here if the sample size is too small -> must adjust to make s^2 an unbiaded estimator of variance

# Let's make the assumption that the sample data is Normally distributed -> N(42.12931, 32.98788^2)
# To plot the Q-Q plot, for this fitted distribution we first need to calculate the theoretical quantiles using the ppoints()

# Note: the fitted distribution will be the theoretical 
# We use ppoints() on the sample -> to get the quantiles for ppoints()

n = length(ozone)
comparison.qs = qnorm(ppoints(n), mean.ozone, sd.ozone)
# -> this takes the ppoints(n) as probability and returns the quantile according to the fitted Distribution
# ppoints() will split the lenght(ozone) into equal intervals across (0,1) range

qqplot(comparison.qs, ozone, xlab = "Quantiles from fitted normal distribution", ylab = "Sample quantiles", main = "Q-Q plot of ozone data")
abline(0, 1, col = "red")

# What see is that the points don't fall closely onto the straight line -> model fitted seems to be poor in the tails
# When comparing to the normal distribtion -> use qqnorm() -> does the comparison qs all inside the plot -> no need for ppoints(n) ->

qqnorm(ozone, xlab = "Quantiles from fitted normal distribution", ylab = "Sample quantiles", main = "Q-Q plot of ozone data")
qqline(ozone, col ="red") # use this instead of abline(0, 1)


# Ok, we've seen how fitting a wrong distribtion can be confirmed visually using Q-Q plot
# Fit a Gamma distribtion

params = fitdistr(ozone, "gamma")
shape = params$estimate[1]
rate = params$estimate[2]

comparison.qs = qgamma(ppoints(n), shape, rate)
qqplot(comparison.qs, ozone, xlab = "Quantiles from fitted Gamma distribution", ylab = "Sample quantiles", main = "Q-Q plot of ozone data")
abline(0, 1, col ="red")
# see a better fit using a gamma distribution

# Can we also do a ChiSq GoF test?

# Example - Create a Q-Q plot for the Lossess data against a Pareto distribution
# Pareto distribution -> have to specify the initial parameter values

# E(X) = lambda / (a - 1)
# Var(X) = ...

a0 = 3.416
l0 = 1047

params0 = c(a0, l0)

obj = function(data, params){
	-sum(log(dpareto(data, params[1], params[2])))
	}

params = nlm(obj, data = Losses$Losses, params0)
shape = params$estimate[1]
rate = params$estimate[2]

qpareto = function(p, a, lambda){
	lambda * ((1 - p)^(-1/a)-1)
	}

comaprison.qs = qpareto(ppoints(length(Losses$Losses)), shape, rate)

qqplot(comaprison.qs, Losses$Losses, xlab = "Quantiles from fitted Pareto distribution", ylab = "Losses$Losses Sample quantiles", main = "Q-Q plot of Losses$Losses data")
abline(0, 1, col = "blue")
# Pareto appears to be a good fit for losses < 2000. The fit is less accurate in the tail i.e. for losses > 2000
# May expec the fit to look worse in the upper tail -> particularly for a heavy tail distrubution even if the fitted dsitribution is teh true underlying distribution

#########################################################################################
# Question 5

claims = read.csv("ClaimSize.csv")
head(claims)

claims = claims$ClaimSize

# Claims ~ Gamma(a, b)

# set.seed(123)
# a0 = sample(c(1.7, 1.8, 1.9, 2), size = 1, replace = TRUE)
# b0 = sample(c(0.0015, 0.0016, 0.0017, 0.0018, 0.0019, 0.0020), size = 1, replace = TRUE)

MLE.tbl = expand.grid(a = c(1.7, 1.8, 1.9, 2), b = c(0.0015, 0.0016, 0.0017, 0.0018, 0.0019, 0.0020))

for (j in 1:nrow(MLE.tbl)){
	
params0 = c(MLE.tbl$a[j], MLE.tbl$b[j])

obj = function(data, params){
	-sum(log(dgamma(data, params[1], params[2])))
	}

result = nlm(obj, data = claims, params0)

MLE.tbl$ML[j] = result$minimum
MLE.tbl$a.mle[j] = result$estimate[1]
MLE.tbl$b.mle[j] = result$estimate[2]

	}
	
MLE.tbl

a = MLE.tbl[which.max(MLE.tbl$ML), ]$a.mle
b = MLE.tbl[which.max(MLE.tbl$ML), ]$b.mle

comparison.qs = qgamma(ppoints(length(claims)), a, b)

par(mfrow = c(2, 1))

hist(claims, freq = FALSE, ylim = c(0, 0.001))
lines(density(claims, from = 0, to = 5000), xlab = "Claim Amounts", ylab = "Density", main = "Emperical Fitted Density of Claims Size", col ="red", lty = 2)
lines(seq(0, 5000), dgamma(seq(0, 5000), a, b), col = "blue")
legend("topright", legend = c("Experical", "Fitted"), col = c("red", "blue"), lwd = 2)

qqplot(comparison.qs, claims, main="")
abline(0, 1, col = "red")

par(mfrow = c(1, 1))

# Fit looks reasonable -> can comment on the bars under and over the fitted density and positive skewness


#########################################################################################
# Question 6

claims = read.table("exp.txt", header = T)$x
head(claims)

l0 = 0.002

# Expected that l0 = 1 / mean(claims)

params0 = l0

obj = function(data, params){
	-sum(log(dexp(data, params)))
	}
	
result = nlm(obj, data = claims, params0)
l = result$estimate # -> 0.001490295
1 / mean(claims) # -> 0.001490312

hist(claims, freq = FALSE)

comparison.qs = qexp(ppoints(length(claims)), l)

par(mfrow = c(2, 1))

plot(density(claims, from = 0, to = 6000), col = "red", lty = 2, xlab = "Claim Amounts", ylab = "Density", main = "Plot of Emperical and Fitted Density", ylim = c(0, 0.0015))
lines(seq(0, 6000), dexp(seq(0, 6000), l), col = "blue")
legend("topright", legend = c("Emperical", "Fitted"), col = c("red", "blue"), lwd = 2)

qqplot(comparison.qs, claims, main ="")
abline(0, 1, col = "dark green")

par(mfrow = c(1, 1))

# Fit seems ok until the claim sizes get very large -> Q-Q plot showing a big deviation in tails of the claim size suggesting the exponential distribtion is not suitable at predicting large claim amounts

#########################################################################################
# Question 7

claims = read.table("MotorClaims.txt", header = T)$MotorClaims
head(claims)

hist(claims, freq = FALSE)

l = 0.003

comparison.qs = qexp(ppoints(length(claims)), l)

par(mfrow = c(2, 1))

plot(density(claims, from = 0, to = 900), col = "red", lty = 2, xlab = "Claim Amounts", ylab = "Density", main = "Plot of Emperical and Fitted Density", ylim = c(0, 0.005))
lines(seq(0, 900), dexp(seq(0, 900), l), col = "blue")
legend("topright", legend = c("Emperical", "Fitted"), col = c("red", "blue"), lwd = 2)

qqplot(comparison.qs, claims, main ="")
abline(0, 1, col = "dark green")

par(mfrow = c(1, 1))

# Completely wrong fit -> emperical density suggestiv of a log normal or heavier tail-ed postive skewness distribution
# Complete overestimation of small claims, and dramatic underestimation of larger Claims
# The data doesn’t lie anywhere near the reference line. Overall, we would recommend the student tries to fit an alternative distribution