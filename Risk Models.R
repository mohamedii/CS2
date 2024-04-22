# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\19 & 20 Risk Models")

# Collective Risk Models
# -> Simulate differnt types of compound distrbutions
# -> Use results of simulations to estimate proabilities of the aggregate claims distrbutions
# => Fit statitical distributions to set of aggregate claims data -> analyse GoF
# -> Apply reinsurance to compound claim simulation

# Method 1
# S ~ Compound Poisson (lambda) -> N ~ Poisson(lambda)
# Want to simulate 1000 observations of S 

# Procedure ->
# 1. Simulate N number of random poisson numbers from Poisson(lambda)
# 2. Generate for each n generated N, generate n claims from X distribtion
# 3. Aggregate -> sum(N generated X)

set.seed(123)
sims = 10000 # -> This will be the number of S we will Simulate
# -> for each sim, we need a random N from Poisson(lamda) -> number of claims amounts to generate, and then sum over

n = rpois(sims, 1000) # -> lamdba here is  = 1000

S = rep(NA, sims) # -> setup vector S of length sims = 10000

# X ~ Gamma(750, 0.25) -> we will genearate n of these...then take the sum over all of them for each n in sims
# -> for instance, the first simulation -> the number of X we generate comes from n[1]

# x = rgamma(n[1], 750, 0.25)
# S[1] = sum(x) # This is the value of the first sim
# -> repeat this for each n[i] in sims

for (j in 1:sims){
	x = rgamma(n[j], 750, 0.25)
	S[j] = sum(x)
	}

head(S)

mean(S)
var(S)


# Question -> S ~ Compound Poisson(lambda = 100) and X ~ Exp(beta = 0.002)
# -> simulated 1000 variate from compound dsitribution and state 98th percentile of S

set.seed(8143)

sims = 1000
n = rpois(sims, 100)

S = rep(0, sims)

for (j in 1:sims){
	x = rexp(n[j], 0.002)
	S[j] = sum(x)
	}

quantile(S, 0.98)


# Using Simulated data
# Use simualted data to estimate insurer's claims experience

# Suppose youre given S ~ CP(800), X ~ Gamma(900, 2)
# Simulate 10,000 S

set.seed(123)

sims = 10000
n = rpois(sims, 800)
S = rep(0, sims)

for (j in 1:sims){
	x = rgamma(n[j], 900, 2)
	S[j] = sum(x)
	}

# Estimate the probability that insurer's total claim payment is anyear exceeds 350,000 -> P(S > 350,000)
length(S[S > 350000]) / length (S)

# Question 
# Estimate the mean, variance and median of insurer's aggregate claim payment for the above ->

mean(S)
var(S)
median(S) # or
quantile(S, 0.5)

# Question 
# Estimate the coefficient of skewness of insurer's aggregate claim payment for the above ->

skew = (1/length(S))*sum((S-mean(S))^3)
coeff = skew/(var(S)^1.5)

# Fitting a distributio to the Compound Possion Claims Data
# Fit distribution to aggregate claims data using MLE

# Example -> fit a normal distribution to a compound Poisson distribution with parameter lambda = 1000
# X ~ Gamma(750, 0.25)

# -> So we first get S...which we know how to do
# -> Using S, maybe do a histogranm or density plot to see if perhaps a normal can Fit S
# -> Analyse GoF after fitting -> perhaps using interval breaks, looking at oberseved and expected -> 
# -> Determine ChiSq GoF TS and DoF (m - 1) ... for completeness

# Let go

set.seed(123)
sims = 10000
n = rpois(sims, 1000)
S = rep(0, sims)

for (j in 1:sims){
	x = rgamma(n[j], 750, 0.25)
	S[j] = sum(x)
	}

hist(S, freq = FALSE)
lines(density(S, from = 0 , to = max(S)), col="red")
# Looks like a normal distribution will be a good fit to S

# Now determine what mu and sig are for the Normal distribution fit ->

mu = mean(S)
#sigma = sqrt(var(S))
# or convert the sample sd to population sd -> sqrt((length(S) - 1)/length(S) * var(S))
sigma = sqrt((length(S) - 1)/length(S) * var(S))

params.0 = c(mu, sigma)

obj=function(data, params){
	-sum(log(dnorm(data, params[1], params[2])))
	}

MLE = nlm(obj, params.0, data = S)$estimate
mu = MLE[1]; sigma = MLE[2]
# -> mu = 2997651
# -> sigma = 93715 -> got a better estimate here because we changed from sample sd -> population sd
# Unsurprizing ... we know mathmetically, MLE of the mean is the sample mean, and the MLE for the variance is the population variance! Be able to spot this quickly

# Altenrative is to use the MASS package -> fitdistr
library(MASS)
fitdistr(S, "normal")

# mean -> 2997650.5645
# sigma -> 93715.0248 
# Consistent with above

# Analysing Goodness of Fit
x = seq(range(S)[1], range(S)[2], 1000)
hist(S, main = "Simulated aggregate claims data", xlab = "aggregate claims", ylab = "density", prob = TRUE)
lines(density(S), col = "blue")
curve(dnorm(x, mu, sigma), add = TRUE, col="red")

legend("topright", legend = c("histogram of sample", "empirical density", "fitted normal distn"), col = c("black", "blue", "red"), lty = 1)

comparison.qs = qnorm(ppoints(length(S)), mu, sigma)
qqplot(comparison.qs, S, main = "Q-Q plot comparing quantiles of simulated compound distribution values against normal quantiles")
abline(0,1, col="red")

# Reasonable approximation but there appears to be less accurate in the tails -> some tail action in lower and upper quantiles


# The Compound Binomial distribution

# Method -> S ~ follows a compound binomial with parameters (n, p) -> Number of claims N ~ Bin(n, p) for some claim size X
# To simulate a set of observations from S, we adapt the code from above -> assume X ~ N(1000, sqrt(32^2)) -> generate 2000 observations from S
# N ~ Bin(500, 0.7)

set.seed(123)
sims = 2000
S = rep(0, sims)

n = rbinom(sims, 500, 0.7)

for (j in 1:sims){
	x = rnorm(n[j], 1000, 32)
	S[j] = sum(x)
	}

summary(S)


# Question -> T ~ CBin(80, 0.3), X ~ two-parameter Pareto(4, 5)
# -> simulate 10,000 observation from T, display tail of T


set.seed(123)
sims = 10000
T =rep(0, sims)
n = rbinom(sims, 80, 0.3)

rpareto = function(n, a, lambda){
	lambda * ((1 - runif(n))^(-1/a)-1)
	}


for (j in 1:sims){
	x = rpareto(n[j], 4, 5)
	T[j] = sum(x)
	}

tail(T, 5)

# Using Simulated data ->

# Question 
# Estimate q such that P(T > q) = 0.05
# -> P(T < q) = 0.95

quantile(T, 0.95)

# Fitting a Compound Binomial distribution to claims data
# -> Fit a gamma distribution to dataset T 

# First, let look at T
hist(T, freq=FALSE)
lines(density(T), col="red")

# Two approaches, either can solve for MLE -> alpha, lambda
# -> or ust fitdistr()
fitdistr(T, "gamma")
# shape (or alpha) -> 9.273535030 
# rate (or beta) -> 0.231711576 
# str() is useful...

# Using the MLE approach ->

alpha = (mean(T)^2)/var(T)
beta = mean(T)/var(T)

params.0 = c(alpha, beta)

obj=function(data, params){
	-sum(log(dgamma(data, params[1], params[2])))
	}

nlm(obj, params.0, data = T)
MLE = nlm(obj, params.0, data = T)$estimate

alpha = MLE[1]; beta = MLE[2]
alpha; beta

# Analysing Goodness of Fit -> use hist() and qqplot()

par(mfrow=c(2,1))
comaparison.qs = qgamma(ppoints(length(T)), alpha, beta)
qqplot(comaparison.qs, T, main = "Q-Q plot comparing quantiles of simulated compound distribution values against gmma quantiles")
abline(0,1, col="red")

x=seq(range(T)[1], range(T)[2], 5)
hist(T, freq=FALSE)
lines(density(T), col = "blue")
curve(dgamma(x, alpha, beta), add = TRUE, col="red")

par(mfrow=c(1,1))

# Reasonable fit but some tail action in the right side -> the quantiles of the simulated data are higher than quantiles of the fitted distribution
# -> The simulated data is more positively skewed than the fitted Gamma

# We can invetigate the tail further by looking at the ration of density plots -> fitted versus emperical density ->

x = seq(80, max(T))

plot(x, dgamma(x, alpha, beta), type="l", col="red")
lines(density(T, from = 80, to = max(T)), col="blue")

# Emperical desnity is indeed more postively skewed than the fitted distribution
# However across the entire claim range, the two denstiies are fairly consistent with each other...its only in the right tail


# The Compound Negative Binomial
# -> S ~ CNegBin(k, p) -> type II negative Binomial

set.seed(123)
sims = 1000
S = rep(0, sims)

n = rnbinom(sims, 700, 0.2)

for (j in 1:sims){
	x = rlnorm(n[j], 4, sqrt(9))
	S[j] = sum(x)
	}

summary(S)

# Question -> W ~ CNegBin(5, 0.8)
# X ~ Burr(0.3, 10, 1.2)...generate 10 observations from W

dburr = function(x, a, lambda, g){
	a * g * lambda^(a)*x^(g - 1)/((lambda + x^g)^(a+1))
	}

pburr = function(q, a, lambda, g){
	1 - (lambda/(lambda + q^g))^a
	}

qburr = function(p, a, lambda, g){
	(lambda*((1 - p)^(-1/a) - 1))^(1/g)
	}

rburr = function(n, a, lambda, g){
	(lambda*((1 - runif(n))^(-1/a) - 1))^(1/g)
	}

set.seed(123)
sims = 10
W = rep(0, sims)

a = 0.3
l = 10
g = 1.2

n = rnbinom(sims, 5, 0.8)

for (j in 1:sims){
	x = rburr(n[j], a, l, g)
	W[j] = sum(x)
	}

# Determine P(W > 2000)
length(W[W > 2000]) / length(W)
# -> obtained zero probability...likely to be inaccurate because sample is so small

# Remember that X ~ Burr -> the claim size has no upper limite -> P(W > c) is greater than zero or rather
# will never be zero exactly. For extremely large values of c, we are likely to underestimate the probability even if use a much larger 
# sample than we have

# Fitting a Distribition to the CNegBin claim data
# Fit a normal distribution to CNegBin(20, 0.1) where X ~ lognormal(mu = 3, sigma = sqrt(0.64))
# Seed -> 19 for sims = 100

set.seed(19)
sims = 100
W = rep(0, sims)

n = rnbinom(sims, 20, 0.1)

mu = 3
sigma = sqrt(0.64)

for (j in 1:sims){
	x = rlnorm(n[j], mu, sigma)
	W[j] = sum(x)
	}

params.0 = c(mean(W), sqrt(var(W)*(length(W) - 1)/length(W))) # -> using the MLE estimates becuase they're unbiased estimates -> essentially we have the parameters already
# but it doesn't hurt to do the exercise

obj=function(data, params){
	-sum(log(dnorm(data, params[1], params[2])))
	}

nlm(obj, params.0, data = W)
MLE = nlm(obj, params.0, data = W)$estimate

mu.W = MLE[1]; sigma.W = MLE[2]
mu.W; sigma.W
# Check this with params.0 -> should be the same

# Analysing Goodness of Fit -> hist(), density(), qqplot()...

x = seq(range(W)[1], range(W)[2])
par(mfrow=c(2,1))

hist(W, freq=FALSE)
lines(density(W), col = "blue")
curve(dnorm(x, mu.W, sigma.W), add = TRUE, col="red")

comparisons.qs = qnorm(ppoints(length(W)), mu.W, sigma.W)
qqplot(comparisons.qs, W, main = "Q-Q plot comparing quantiles of simulated compound distribution values against normal quantiles")
abline(0,1, col="red")

par(mfrow=c(1,1))

# There does appear to be some tail depdencies but overall a good fit
# -> Analyse the tail as the simulated W tends to be understimated at high and lower values

# Reinsurance -> XoL
# Recall that x is the gross claim amounts -> x = y + z
# -> Retention level M

# y = pmin(x, M)
# z = pmax(0, x - M)

# If we think of x as the Aggregate claims, S ->
# Sy -> Aggregate claims net of reinsurance
# Sz -> Reinsurers payments on claim

# Using Example from above ->

set.seed(123)
sims = 10000
S = rep(0, sims)

n = rpois(sims, 1000)

for (j in 1:sims){
	x = rgamma(n[j], 750, 0.25)
	S[j] = sum(x)
	}

# This is how we would simulate Sy, Sz - >

M = 2500 # -> on the individual level of claim X
set.seed(123)
sims = 10000
Sy = rep(0, sims)
Sz = rep(0, sims)

n = rpois(sims, 1000)

for (j in 1:sims){
	x = rgamma(n[j], 750, 0.25)
	Sy[j] = sum(pmin(x, M))
	}

for (j in 1:sims){
	x = rgamma(n[j], 750, 0.25)
	Sz[j] = sum(pmax(0, x-M))
	}

mean(Sy); mean(Sz)

# Aggreagte Xol Reinurance
# Sy = min(S, M)
# Sz = max(0, S-M)

# -> Sy = pmin(S, M)
# -> Sz = pmax(0, S - M)

M = 3000000
# Using Example from above ->

set.seed(123)
sims = 10000
S = rep(0, sims)

n = rpois(sims, 1000)

for (j in 1:sims){
	x = rgamma(n[j], 750, 0.25)
	S[j] = sum(x)
	}

Sy = pmin(S, M)
Sz = pmax(0, S - M)

# Adapt the code for Porportional Reinsurance  -> Can do it an individual claim level or aggregate claim level

# Retention level - alpha = a

set.seed(123)
sims = 1000
S = rep(NA, sims)

n = rnbinom(sims, size = 700, prob = 0.2)

for (j in 1:sims){
	x = rlnorm(n[j], meanlog = 4, sdlog = 3)	
	S[j] = sum(x)
	}
	
a = 0.9

Sy = a * S
Sz = (1-a) * S

sd(Sy)
length(Sz[Sz > 1500000]) / length(Sz)

#########################################################################################
# Question 1

# S~ CNegBin(10, 0.1) and X ~ lognormal(10, sqrt(5)

set.seed(16)
sims = 10000
S = rep(0, sims)

mu = 10
sigma = sqrt(5)
k = 10
p = 0.1

n = rnbinom(sims, k, p)

for (j in 1:sims){
	x = rlnorm(n[j], mu, sigma)
	S[j] = sum(x)
	}

head(S, 5)

mean(S); var(S)

# E(N)*E(X)
theo.mean = k*(1-p)/p*exp(mu+0.5*sigma^2)

# Var(N)*E(X)^2 + Var(X)*E(N)
theo.var = (k*(1-p)/(p^2))*(exp(mu+0.5*sigma^2)^2) + (exp(mu+0.5*sigma^2)^2) * (exp(sigma^2) -1)*k*(1-p)/p

theo.mean; theo.var

set.seed(16)
M = 2000000
Sy = rep(0, sims)

mu = 10
sigma = sqrt(5)
k = 10
p = 0.1

n = rnbinom(sims, k, p)

for (j in 1:sims){
	x = rlnorm(n[j], mu, sigma)
	y = pmin(x, M)
	Sy[j]=sum(y)
	}

head(Sy, 5)

#########################################################################################
# Question 2

# S ~ CP(500) and X ~ Exp(0.001)
l = 500
b = 0.001

set.seed(1975)
sims = 1000
S = rep(0, sims)

n = rpois(sims, l)

for (j in 1:sims){
	x = rexp(n[j], b)
	S[j] = sum(x)
	}

tail(S, 5)

# S ~ approx. N(mu, sigma^2)
# MLE can be calculated using the sample mean and sample variance ->

mu_hat = mean(S)
sigma_hat = sqrt((length(S) -1)/length(S)*var(S))

mu_hat; sigma_hat^2

# qqplot() will be useful ->
comparisons.qs = qnorm(ppoints(length(S)), mu_hat, sigma_hat)
qqplot(comparisons.qs, S, main="QQ Plot of Simulated versus fitted normal distribution")
abline(0,1, col="red")

#########################################################################################
# Question 3

# Company wishes to model claim numbers as a Poisson process
# -> data file:

claims = read.csv("claims.csv")
claims$claim.date = as.Date(claims$claim.date, format("%d/%m/%Y"))
head(claims)

claims$wait = c("NA", diff(claims$claim.date))
head(claims)

# Waiting times T ~ Exp(lambda)
wait = as.numeric(diff(claims$claim.date))
wbar = mean(wait)
l_hat = (1/wbar)

hist(wait, freq=FALSE)
lines(density(wait, from = min(wait), to = max(wait)), main = "Density plot of waiting times", xlab = "Waiting time", col = "blue")
lines(seq(range(wait)[1], range(wait)[2]), dexp(seq(range(wait)[1], range(wait)[2]), l_hat), type = "l", lty=2, col = "red")

intervals = c(0, 10, 20, 30, 40, 50, 60, Inf)

O = hist(wait, breaks = intervals, plot=FALSE)$counts
length(O)

cdf = pexp(intervals, l_hat)
E = length(wait) * diff(cdf)
length(E)

DoF = length(E) -1

TS = sum((O-E)^2/E)
p.val = pchisq(TS, DoF, lower.tail = FALSE)
# -> p.val = 0.5397816 so fail to reject null -> observed waiting times are consistent with distribution

# Model the total future claim amount as CP -> amounts claimed are IID -> X ~ Gamma(a, b)

mean(claims$claim.cost)
sd(claims$claim.cost) # -> adjust this to population variance

b = mean(claims$claim.cost)/((length(claims$claim.cost)-1)/length(claims$claim.cost)*var(claims$claim.cost))
a = b * mean(claims$claim.cost)

a/b; sqrt(a/(b^2))

# l_hat was estimated waiting time in days per claim
# l_hat -> expected number of claims per day
# l = l_hat * 365 -> expected number of claims per year

l = l_hat*365

# mean(S) = E(N)*E(X) -> lambda * m1
S.mean = l * a/b
# var(S) = E(X)^2*Var(N) + E(N)*Var(X) -> lambda * m2
S.var = (a/b)^2*l + l*(a/(b^2))

S.mean; sqrt(S.var)

#########################################################################################

# Individual Risk Model
# -> Simulate aggregate claims arising under the individual risk model
# -> Use results of simulations to etiamte probabilities  and moments of aggregate distribution
# -> Calculate the insurer's and reinsurer's share of aggregate claim payments under simple type of reinsurance arrangements

# Simulating Claims using the Individual Risk model
# Recall the aggregate claim amount RV S can be modeled using the IRM if - 
# -> The number of risks n is fixed
# -> Risk are independent and not neccesarily identically distributed
# -> The number of claims on each risk ie either 0 or 1

# For the example, lets assume a company sells three types of insurance -> pet, home, and car
# Assume the insurer will pay a maximum of one claim per policy -> not realsitic but let assume

# From table, you will see policy number, claim amount, and policy type
# -> some policies claim, some dont
# -> assumption is that a policyholder can claim once only

# Insurer has sold 100 policies
# By inspection, the claim amounts on for each policy type are very diffeerent ->
# Don't appear to come from an identical distrbution
# Similarly, look at the claim frequency

# The insurers aggregate claim payment for these policies would be the sum of the claim amounts 
# Therefore to simulate 1000 observations of S1, S2, ..., S1000 from the aggreagte claim dsitribution S
# -> We need to observe for each S -> 100 policies in aggregate

# Example
# Company A sold 1000 independent life policies
# -> 250 sold to somkers, 750 sold to non-smokers
# If a claim on a policy is made, the size of the claim:
#		-> X ~ Gamma(200, 0.8) for smokers
#		-> Y ~ Gamme(300, 0.4) for non-smokers
# The probability of death occuring during the term of the policy is:
#		-> qs = 0.002 for smokers
#		-> qn = 0.001 for non-smokers

# Generate 500 aggregate claim amounts from this portfolio of policies

set.seed(54321)

a.s = 200; l.s = 0.8; q.s = 0.002
a.n = 300; l.n = 0.4; q.n = 0.001

# To begin simulate a single observation of S -> expand it further once we get the idea
# Imagine that the first 230 policies are smokers, remaining 750 are for non-smokers ->
# -> for each of these policies, generate a random claim amount (assuming that a claim is paid)

x = c(rgamma(250, a.s, l.s), rgamma(750, a.n, l.n))

# Now remember, a claim will happen if the policy holder dies...its a one off event hence why the probabilies 
# of death were given. We can extend this later to thinking about other claims on reinsurance

death = c(rbinom(250, 1, q.s), rbinom(750, 1, q.n))

# Now we can calculate the simulated claim amounts -> Remember that the first 250 in x are for smokers and
# -> the first 250 in death are for smokers

sim = x*death

S = sum(sim)

# Putting it all together for 500 observations of S

set.seed(54321)

a.s = 200; l.s = 0.8; q.s = 0.002
a.n = 300; l.n = 0.4; q.n = 0.001

sims = 500
S.sim = rep(0, sims)

for (j in 1:sims){
	x = c(rgamma(250, a.s, l.s), rgamma(750, a.n, l.n))
	death = c(rbinom(250, 1, q.s), rbinom(750, 1, q.n))
	sim = x*death
	S.sim[j] = sum(sim)
	}

quantile(S.sim, 0.9)
# -> 1672.376


# Question 
# Company B has sold 100 life policies, gives rise to max one claim per year
# -> Type I - Claim amounts ~ exp(0.005), prob of claim 0.001
# -> Type II - Claim amounts ~ exp(0.006), prob of claim 0.002
# -> Type III - Claim amounts ~ exp(0.007), prob of claim 0.003

n.I = 30
n.II = 50
n.III = 20
# sum of these -> 100

# Simulate 5000 observations from aggregate claims size distribition
# Estimate probability it will have to payout more thatn 100,000 a year on these policies -> 100 (in '000s)

set.seed(54321)

l.I = 0.005; q.I = 0.001
l.II = 0.006; q.II = 0.002
l.III = 0.007; q.III = 0.003

sims = 5000
S.sim = rep(0, sims)

for (j in 1:sims){
	x = c(rexp(n.I, l.I), rexp(n.II, l.II), rexp(n.III, l.III))
	death = c(rbinom(n.I, 1, q.I), rbinom(n.II, 1, q.II), rbinom(n.III, 1, q.III))
	sim = x*death
	S.sim[j] = sum(sim)
	}

length(S.sim[S.sim > 100]) / length(S.sim)


# individual Risk Model with Resinsurance
# When reinsurance applies, the number of policies and the number of claims by the IRM will be unaffected
# However the size of each claim should be adjusted to reflect the amount ceded to the reinsurer

# XoL reinsurance
# Extending from the smoker / non-smoker example...company A has bought individual XoL with rention level M = 200 

M = 200

# y = pmin(x, M) # -> x will be changed to y in the algorithm

set.seed(54321)

a.s = 200; l.s = 0.8; q.s = 0.002
a.n = 300; l.n = 0.4; q.n = 0.001

sims = 500
S.sim = rep(0, sims)

for (j in 1:sims){
	x = c(rgamma(250, a.s, l.s), rgamma(750, a.n, l.n))
	y = pmin(x, M) # -> here we made the change
	death = c(rbinom(250, 1, q.s), rbinom(750, 1, q.n))
 	sim = y*death # -> dont forget to use y... NOT x
	S.sim[j] = sum(sim)
	}

quantile(S.sim, 0.9)
# -> 600...significantly lower than 1672


# Propoertional insurance
# Simlilar adjustments makde to allow for effects of proportional reinsurance
# y = alpha * x
# z = (1-alpha) * x

# Question
# Estimate the mean of the REinsurer's aggregate claims distribution  when proportional insurance in force
# -> alpha = 0.8
# Again we can modify what we have...and base it off REinsureres claim payments

alpha=0.8

set.seed(54321)

a.s = 200; l.s = 0.8; q.s = 0.002
a.n = 300; l.n = 0.4; q.n = 0.001

sims = 500
S.sim = rep(0, sims)

for (j in 1:sims){
	x = c(rgamma(250, a.s, l.s), rgamma(750, a.n, l.n))
	z = (1-alpha) * x # -> here we made the change
	death = c(rbinom(250, 1, q.s), rbinom(750, 1, q.n))
 	sim = z*death # -> dont forget to use y... NOT x
	S.sim[j] = sum(sim)
	}

quantile(S.sim, 0.9) # -> 334.4751
mean(S.sim) # -> 139.8874


#########################################################################################
# Question 4

probability.of.claim = read.table("IRM.txt", header = T)[, 2]
exponential.parameter = read.table("IRM.txt", header = T)[, 3]

prob.claim = probability.of.claim
lambda = exponential.parameter

mean.claim = 1 / lambda

E.S = sum(mean.claim * prob.claim)

set.seed(5)
sims = 10000
n = 500
S = rep(0, sims)

for (j in 1:sims){
	x = rexp(n, lambda)
	death = rbinom(n, 1, prob.claim)
	sim = x*death
	S[j] = sum(sim)
	}

# use mean(S) as unbiased estimator of pop mean

length(S[S <= E.S*1.1 & S >= E.S*0.9]) / length(S)


length(S[S == 0]) / length(S)
prod(1-prob.claim)
# Small discrepancy between the simulation values  S =0 and P(N=0) -> Probability of no claim
# The simulation appears to be represenative sample from the aggregate claims distribution...

# hist(S) # -> shape consisnte with a normal distribution
# Probability point mass at S=0 -> this is quite big -> 0.07284173
# Remember that S is a continuous distribution for S > 0 so S does take a discrete value, we expect it to be very low
# S has a point mass at zero
# The high probability is due to the low probability of claims -> therefore relatively high  probability of insurer
# having to pay nothing at all i.e. non of the policies have a claim

# hist(S.sim, breaks = 500, freq = FALSE, xlim = c(0, 60000), ylim = c(0, 0.0002), main = "Simulated PDF of S")
# Plot will show that S is very positively skewed -> expected since underlying claims distributions are exponential
# Although the CLT will reduce this with more policies added


#########################################################################################
# Question 5

set.seed(1812)
q = runif(200, 0, 0.01) # Probality of a claim
mu = runif(200, 5, 13)
sigma = runif(200, 0.5, 0.8)
policies = cbind(q, mu, sigma)

# Aggregate claims under certain portfolio of insurance are expected to follow IRM
# Data frame: policies contain risk characteristics
# Individual claim amounts follow lognormal distribution -> mu and sigma parametrization for ith policy

# There are 200 policies in the portfolio - see the table length
# XoL in place

M = 60000
n = 200

set.seed(1854)
sims = 5000

# Simulate the REinsurers mean aggregate claim amount using 5000 observations

Sz = rep(0,sims)

for (j in 1:sims){
	claim = rbinom(n, 1, policies[,1]) # -> remember this is an indicator variable...claim or not
	x = rlnorm(n, policies[,2], policies[,3])
	z = pmax(0, x - M)
	sim = z*claim
	Sz[j] = sum(sim)
	}

mean(Sz)


set.seed(1854)
sims = 5000
ASz = rep(0,sims)
AM = 300000


for (j in 1:sims){
	claim = rbinom(n, 1, policies[,1]) # -> remember this is an indicator variable...claim or not
	x = rlnorm(n, policies[,2], policies[,3])
	sim = x*claim
	ASz[j] = pmax(0, sum(sim) - AM)
	}

mean(ASz)


#########################################################################################

# Paramater Uncertainty
# Simulate claims from CP with parameter Uncertainty
# Simulate Aggregate claims from heterogenous portoflio with parameter uncertainty
# Simulate Aggreagte claims from homogenous portfolio with parameter uncertainty
# Use results of simulations to estimate moments on individual policy claims distribution and aggregate claims distribition

# Simulating claims from Heterogenous Portfolio
# Method ->
# Consider a portfolio of 100 policies, where each policy has a separate CP distribution -> hence heterogenous
# Analyse two cases of uncertainty in the Poisson parameter

# The RV S = S1 + S2 + ... + S100 represents aggregate claim amount from a portfolio of 100 independent policies
# Each Si has a CP distribution with unknown parameter lambda_i

# So...each Si = Xi,1 + Xi,2 + ... + Xi,Ni where Ni ~ Poisson(lambda_i)

# Assum further that X iid ~ Gamma(750, 0.25)

# Example 1 from Core Reading
# Assume lambda_i is equally like to be 0.1 or 0.3 for each policy

# Simulating a randomly chosen policy ->
# The aggregate claims from a randomly chosen policy has a mixture distribution as the CP has an unknown parameter
# Simulate 10,000 values from thie mixture distribution and use it to estimate its moments

#First simulate a value of the Poisson parameter and use this to simulate the aggregate claim...repeat 10000 simulations

sims = 10000
S = rep(0, sims)

set.seed(123)
lambda = sample(x = c(0.1, 0.3), replace=TRUE, size=sims, prob = c(0.5, 0.5))

# We now have 10,000 simulated values of lambda which we use as the Poisson parameters to generate 10,000 aggregate claim amounts from repsective CP
# distibutions -> first we generate 10,000 obervations from relavant Poisson distributions to represent the number of claims ->

N = rpois(sims, lambda)

for (i in 1:sims){
	S[i] = sum(rgamma(N[i], 750, 0.25))
	}

mean(S)
sd(S)

# Why do all this above? This is for an individual policy! 

# Now we consider the aggregate portfolio -> Rememmber than each policy follows a CP -> Si
# The Sum of Si -> Total S is also a CP

# Simulating the Aggregate Portfolio
# To generate simulations for the whole portfolio, we need to simulate a Poisson parameter for each of the 100 policies ->
# W then need to cimulate the aggregate claim amount for each individual policy from the relavant CP distribution
# We must stor the total claims for each policy across all simulations -> we will later sum over the policies to get
# the aggregate claim amount on the entire portfolio for each simulation

sims = 10000
policies = 100

results = matrix(0, nrow = sims, ncol = policies)

set.seed(123)

for (j in 1:policies){
	
	lambda = sample(c(0.1, 0.3), replace = TRUE, size = sims, prob = c(0.5, 0.5)) # -> sample the values of lambda
	N = rpois(sims, lambda) # -> generate the random claim numbers
	
	for (i in 1:sims){
		x = rgamma(N[i], 750, 0.25)
		results[i,j] = sum(x)
		}	
		
	}


Total = rowSums(results)
mean(Total)
sd(Total)

# You did it differently to the Core Reading ... You focused on the simulation of individual policies, then on portfolio
# Try the Core Reading approach

sims = 10000
policies = 100

S = rep(0, policies)
results = matrix(0, nrow = sims, ncol = policies)

set.seed(123)

for (i in 1:sims){
	
	lambda = sample(c(0.1, 0.3), replace = TRUE, size = policies, prob = c(0.5, 0.5))
	N = rpois(policies, lambda)
	
	for (j in 1:policies){
		S[j] = sum(rgamma(N[j], 750, 0.25))
		}
	
	results[i,] = S
	}

Total = rowSums(results)
mean(Total)
sd(Total)

# Example 2 -> Core Reading
# Assume that lambda_i ~ Gamma(0.1, 1)

# Simulating the number of claims from a randmly chosen policy
# The number of claims from a randomly chosen policy has a mixture distribution as it depends
# on unknown Poisson parameter lambda_i

# We can simulate 10,000 values from this misture dsitrbution and use it to estimate its moments
# -> We first simulate a value of the Poisson parameter and use this to simulate the number of claims
# for a policy with this parameter -> repeeat process for a total of 10,000 simulations

sims =10000
set.seed(123)
lambda = rgamma(sims, 0.1, 1) # as lambda ~ gamma(0.1, 1))

N = rpois(sims, lambda)
mean(N)
var(N)

# Simulating a randomly chosen policy

sims = 10000
S = rep(0, sims)

set.seed(123)
lambda = rgamma(sims, 0.1, 1)
N = rpois(sims, lambda)

for (j in 1:sims){
	S[j] = sum(rgamma(N[j], 750, 0.25))
	}

mean(S)
sd(S)

# Simulating Aggregate portfolio claims

sims = 10000
policies = 100

results = matrix(0, nrow = sims, ncol = policies)

set.seed(123)

for (j in 1:policies){
	
	lambda = rgamma(sims, 0.1, 1)
	N = rpois(sims, lambda) # -> generate the random claim numbers
	
	for (i in 1:sims){
		x = rgamma(N[i], 750, 0.25)
		results[i,j] = sum(x)
		}	
		
	}


Total = rowSums(results)
mean(Total)
sd(Total)

# This is your way...still giving a larger variance because you generate more uncertainty in the loop level i.e.
# -> you geneate sims number of lambda, rather than policies number of lambda -> do it the Core Reading way...

sims = 10000
policies = 100

S = rep(0, policies)
results = matrix(0, nrow = sims, ncol = policies)

set.seed(123)

for (i in 1:sims){

	lambda = rgamma(policies, 0.1, 1)
	N = rpois(policies, lambda)
	
	for (j in 1:policies){
		x = rgamma(N[j], 750, 0.25)
		S[j] = sum(x)
		}
	
	results[i,] = S
	}
	
Total = rowSums(results)
mean(Total)
sd(Total)

# Simulating claims from a homogenous portfolio
# Method -> Still working with a portoflio where each policy has a CP distribution
# The difference now is they all have the SAME CP distribution
# -> However, the Poisson parameter is still unknown

# But because all the policies have the same CP distribution, the lambda even though unknown is the same for all 100 policies
# As before, Claim sizes X ~ Gamma(750, 0.25)

# Example 3 -> Core Reading
# For this example we assume that lambda is equally likey to be 0.1 or 0.3
# But once we know laambda, we can fix it for the 100 policies

# Simluating a randomly chosen policy

sims = 10000
S = rep(0, sims)

set.seed(123)

lambda = sample(c(0.1,0.3), replace = TRUE, size = 1, prob = c(0.5,0.5)) # shouldn't we use size = 1 because it must be the same lambda for all policies?
N = rpois(sims, lambda)

for (j in 1:sims){
	x = rgamma(N[j], 750, 0.1)
	S[j] = sum(x)
	}

# Simulating Aggregate claims for the portfolio -> Only simulate a singale value of the Poisson parameter for all 100 policies
# i.e. lambda = sample(c(0.1,0.3), replace = TRUE, size = 1, prob = c(0.5,0.5)) 

sims = 10000
policies = 100
S = rep(0, policies)
results = matrix(0, nrow = sims, ncol = policies)

set.seed(123)

for (i in 1:sims){
	

	lambda = sample(c(0.1, 0.3), replace = TRUE, size = 1, prob = c(0.5, 0.5))
	N = rpois(policies, lambda)

	for (j in 1:policies){
		x = rgamma(N[j], 750, 0.25)
		S[j] = sum(x)
		}
	
	results[i,] = S
	}
	
Total = rowSums(results)
mean(Total)
sd(Total)

# higher ovverall variation for the homogenous portfolio -> Think about it...if lambda were sampleed equally likely amongst 0.1 and 0.3
# -> This means policies with different parameters balance each other out somewhat reducing overall variation
# -> If the parameter is either fixed at 0.1 or 0.3 -> likely to get extreme results than compared to case in Example 1
# -> Extreme results like very low or very high aggregate claim amounts -> Lead to higher overall variation for the homogenous portfolio


#########################################################################################
# Question 6

# 70 independent policies, each having a CBin with (unknown p, n = 50 trials)
# success p ~ Beta(8,5)

sims = 5000
set.seed(42)

p = rbeta(5000, 8, 5)
head(p, 10)

N = rbinom(5000, 50, p)

mean(N)
sd(N)

# X ~ Exp(0.01)

sims = 5000
S = rep(0, sims)

set.seed(42)

for (j in 1:sims){	
	x = rexp(N[j], 0.01)
	S[j] = sum(x)
	}

mean(S)
sd(S)

sims = 5000
policies = 70

S = rep(0, policies)
results = matrix(0, nrow = sims, ncol = policies)

set.seed(42)

for (i in 1:sims){
	
	p = rbeta(policies, 8, 5)
	N = rbinom(policies, 50, p)
	
	for (j in 1:policies){
		x = rexp(N[j], 0.01)
		S[j] = sum(x)
		}
	
	results[i, ] = S
	}

Total = rowSums(results)
mean(Total)
sd(Total)

#########################################################################################

# Collective Risk Model

# Simulate the different types of compound distrbutions -> Poisson, Binomial, Negative Binomial
# Simulations to estimate probabilities, moments and aggregate claims distrbutions
# Fit statistical distribution to set of aggregate claims data -> Analyse Goodness of Fit
# Apply simple type of reinsurance to compound claims simulation

# Compound Poisson Distribution
# S ~ CPoisson(lambda)
# -> To simulate 1000 observatiosn from S -> first generate 1000 Poisson observations from Poisson(lambda)
# -> You'll get 1000 N values 
# Then generate 1000 aggreagte of X values using the N values -> sum[i = 1 to N](Xi)

# Example
# N ~ Poisson(1000)
# X ~ Gamma(750, 0.25) independent of N

set.seed(123)

sims = 10000
lambda = 1000
a = 750
b = 0.25

n = rpois(sims, lambda)
S = numeric(sims)

for (j in 1:sims){
	x = rgamma(n[j], a, b)
	S[j] = sum(x)
	}

head(S) # -> 2944545 3112333 2841017 3010643 3154315 3044631
mean(S) # -> 2997651
var(S) # -> 8783384216

skew = (1 / sims) * sum((S - mean(S))^3) # -> 2.186291e+13
skew.coeff =skew / (var(S)^(3/2) # -> 0.02655921

hist(S) # -> confirm the skewness -> distribution resembles a normal distribution

# Question ->
# Aggregate claims S ~ Compound distribution:
# N ~ Poisson(lamda = 100)
# X ~ Exponential(beta = 0.002)

set.seed(8143)
# Simutlate 1000 random variates from the compound distribution - state the value of 98th simulated value of S
sims = 1000

lambda = 100
beta = 0.002

N = rpois(sims, lambda)
S = numeric(sims)

for (j in 1:sims){
	x = rexp(N[j], beta)
	S[j] = sum(x)
	}

S[98] # -> 37309.99

# Using Simulated Data 
# Use simulated claims data to estimate insurer's claim experience -> numerical determination

# Insurer believes that annaul aggregate claims follows a CP(lamda = 800) and X ~ gamma(a = 900, b = 2)

set.seed(123)
sims = 10000

lambda = 800
a = 900
b = 2

N = rpois(sims, lambda)
S = numeric(sims)

for (j in 1:sims){
	x = rgamma(N[j], a, b)
	S[j] = sum(x)
	}

# P(S > 350000) ->
length(S[S > 350000]) / length(S) # -> 0.781

mean(S) # -> 359707.1
var(S) # -> 157649328
median(S) # -> 359681
# or ...
quantile(S, 0.5) # -> 50% 359681 

skew = (1 / sims) * sum((S - mean(S))^3) # -> 61945768159
skew.coeff = skew / (var(S)^(3/2)) # -> 0.03129489

# Fit a distrbution to Compound Poisson claims data -> 
# hist(S) suggest normality but careful -> skewness is imporant especally when predicting extreme claims using distrbution

# We can fit a distrbution to aggregate claims data using MLE ->

# Example
# Attempt to fit a normal distrbution to CP(lambda = 1000) and claim size distribution  X ~ gamma(a = 750, b = 0.25)

set.seed(123)
sims = 10000

lambda = 1000
a = 750
b = 0.25

N = rpois(sims, lambda)
S = numeric(sims)

for (j in 1:sims){
	x = rgamma(N[j], a, b)
	S[j] = sum(x)
	}

mu0 = mean(S)
sig0 = sqrt((length(S) - 1) / (length(S)) * var(S)) # -> Remember we want the n-denominator for sig0 -> Var(S) is the sample variance -> convert to population variance


params0 = c(mu0, sig0)

obj = function(data, params){
	-sum(log(dnorm(data, params[1], params[2])))
	}

result = nlm(obj, data = S, params0)
mu = result$estimate[1] # -> 2997651
sig = result$estimate[2] # -> 93715.02

# S approx. ~ N(mu, sig^2)

# We can also use the MASS package ->
library(MASS)

fitdistr(S, "normal") # -> don't have to specify starting values becuase R will do it for distribitions with analytical solutions ->
# -> close enough to numerical MLE

# Analysing Goodness of Fit -> A few ways
hist(S, main = "Simulated aggregate claims data", xlab = "aggregate claims", ylab = "density", prob = TRUE)
lines(density(S), col = "blue")
curve(dnorm(x, mu, sig), add = TRUE, col = "red")
legend("topright", legend = c("histogram of sample", "empirical density", "fitted normal distn"), col = c("black", "blue", "red"), lwd = 2)

# Can also do a QQ-Plot - >
comparison.qs = qnorm(ppoints(length(S)), mu, sig)
qqplot(comparison.qs, S, xlab = "quantiles from fitted distribution", ylab = "quantiles from emperical", main = "QQ Plot of Aggregate Claims distribution")
abline(0, 1, col = "red")
# See some deviation in the tails of the distrbution -> on account of the skewness in sample S

# Compound Binomial distribution
# S follow a CBin(n, p) and X ~ individual claim distribution

# Example
# S ~ Cbin -> N ~ Bin(500, 0.7) and X ~ N(1000, 32^2) -> genearate sample of 2000 obervations from S

set.seed(123)
sims = 2000

n = 500
p = 0.7
mu = 1000
sig = 32

N = rbinom(sims, n, p)
S = numeric(sims)

for (j in 1:sims){
	x = rnorm(N[j], mu, sig)
	S[j] = sum(x)
	}

summary(S) # -> output matches notes

# Question 
# A random variable T ~ CBin where N ~ Bin(80, 0.3) and X ~ Pareto(a = 4, l = 5)
# Generate 10000 size sample from T and provide the last 5 values in T

set.seed(123)
sims = 10000

n = 80
p = 0.3
a = 4
l = 5

N = rbinom(sims, n, p)
T = numeric(sims)

rpareto = function(m, a, lambda){
	lambda * ((1 - runif(m))^(-1/a)-1)
	}

for (j in 1:sims){
	x = rpareto(N[j], a = 4, lambda = 5)
	T[j] = sum(x)
	}

tail(T, 5) # -> 65.56948 52.48138 44.43718 28.66464 50.33910

# Using Simulated Data ->
# As before, we use dhte simulated claims data to esitmate an insurer's claim experience

# Question ->
# Insuruer believes T ~ CBin with N ~ Bin(80, 0.3) and X ~ two-paramter Pareto(a = 4, lambda = 5)
# Estiamte q such that P(T > q) = 0.05

set.seed(123)
sims = 10000

n = 80
p = 0.3
a = 4
l = 5

N = rbinom(sims, n, p)
T = numeric(sims)

for (j in 1:sims){
	x = rpareto(N[j], a = 4, lambda = 5)
	T[j] = sum(x)
	}

# Need to determin q such that P(T > q) = 0.05
quantile(T, 0.95) # -> q = 64.24356

# Fitting a distribution to Compound Binomial claims data
# We could fit a gamma distrbution to T given the shape of the histogram ->

# We've got the sample T -> fit a gamma(a, b) to T

a0 = 10
b0 = 0.1 # careful here -> b = 10 won't work as in notes

params0 = c(a0, b0) # -> initial estimates

obj = function(data, params){
	-sum(log(dgamma(data, params[1], params[2])))
	}

result = nlm(obj, data = T, params0)
a = result$estimate[1] # -> 9.2730404
b = result$estimate[2] # -> 0.2316987

# Can also try fitdistr()
fitdistr(T, "gamma")
# shape -> 9.273535030 
# rate -> 0.231711576 

# Analysing Goodness of Fit fo Compound Binomial
# Create a QQ-plot of the ML gamma distribution -> yo've solved already for fitted parameters >

comparison.qs = qgamma(ppoints(length(T)), a, b)
qqplot(comparison.qs, T, main = "Q-Q plot comparing quantiles of simulated compound distribution values against gamma quantiles", ylab = "quantiles of sample", xlab = "quantiles of fitted gamma distribution")
abline(0, 1, col = "red")
# Reasonable fit except for the righ hand tail -> there are quantiles of the fitted distribution is lower than the sample quantiles (fitted quantiles is the red line)
# Thre is also one outlier very far away from the ML distribtion -> see a point high above the red line which is the fitted quantiles
# This is a good example -> the simualted quantiles above the fitted indicates POSITIVE SKEWNESS THAN THE FITTED GAMMA DISTRIBUTION

# Dig deeper into the tail -> plot the right hand tail of the fitted density agains the right hand tail of the emperical ->

plot(density(T), xlim = c(90, 350), ylim = c(0, 0.001), main = "Simulated data versus fitted distribution", xlab = "Aggregate claim size", col = "blue")

agg = seq(from = 80, to = 350)
y = dgamma(agg, a, b)

lines(agg, y, col = "red")
legend("topright", legend = c("empirical density", "fitted gamma density"), col = c("blue", "red"), lwd = 2)
# Confirm our thinking of positive skewness -> emperical lies above fitted density in the righ hand tail

# But overall the fit is good ->
plot(density(T), main = "Simulated data versus fitted distribution" , xlab = "Aggregate claim size", col = "blue")

agg = seq(from = 0, to = 350)
y = dgamma(agg, a, b)

lines(agg, y, col = "red")
legend("topright", legend = c("empirical density", "fitted gamma density"),col = c("blue", "red"), lwd = 2)


# Compound Negativa Binomial distribution
# N ~ NegBin(k, p) -> type II negative Binomial -> refer to Formula and Tables for IFoA Exams

# Example
# The random variable S follows a Compound Negative Binomial distribution N ~ NBin(700, 0.2) and X ~ LogN(4, sqrt(9))
# Generate 1000 sample size of S

set.seed(123)
sims = 1000

k = 700 # -> refer to tables for exams 
p = 0.2
mu = 4
sig = sqrt(9)

# TO check the distributions in R -> ?distributions

N = rnbinom(sims, k, p)
S = numeric(sims)

for (j in 1:sims){
	x = rlnorm(N[j], mu, sig)
	S[j] = sum(x)
	}

hist(S) # -> looks like we could fit a log-Normal distribution to S

summary(S) # -> Matches the notes

# Question ->
# The random variable W follows a CNegBin distribution where N ~ NegBin(5, 0.8) and X ~ Burr(a = 0.3, l = 10, g = 1.2)
# -> Generate a sample of 10 observations from W

# Need rburr(), and qburr()

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

set.seed(123)
sims = 10

k = 5
p = 0.8
a = 0.3
l = 10
g = 1.2

N = rnbinom(sims, k, p)
W = numeric(sims)

for (j in 1:sims){
	x = rburr(N[j], a, l, g)
	W[j] = sum(x)
	}

W # -> 558.14071  387.40722   23.26043   39.80472   50.99664   48.72566 1546.04093  620.37950  0.00000   63.42809
summary(W)

# Using Simulated Data
# Again as before we can use the simulated claims data to estimate insrurer's claims experience

# Question 
# P(W > 2000) 
length(W[W > 2000]) / length(W) # -> 0 .. sample is very small -> summary(W)  show the max at 1546.04093 -> there are no elements in W > 1546.04093


# Fitting a distribution to Compound Negative Binomial Claims data ->
# Question ->
# Fit a normal distribution to a CNegBin where N ~ NegBin(20, 0.1) and X ~ LogNormal(mu = 3, sig = sqrt(0.64))
# Use seed value of 19 and sims = 100

set.seed(19)
nsims = 100

k = 20
p = 0.1
mu = 3
sig = sqrt(0.64)

N = rnbinom(sims, k, p)
S = numeric(sims)

for (j in 1:sims){
	x = rlnorm(N[j], mu, sig)
	S[j] = sum(x)
	}

# Now we estimate the paramaters of a normal distibution for S ->

mu0 = mean(S)
sig0 = sqrt((length(S) - 1)/(length(S))*var(S)) # -> BE CAREFUL! You must square root the var to get sig

params0 = c(mu0, sig0) # -> we can stop here because these are the analytical ML estimates for S ... but practice is good -> confirm that mu0 = mu.S and sig.0 = sig.S

obj = function(data, params){
	-sum(log(dnorm(data, params[1], params[2])))
	}

result = nlm(obj, data = S, params0)
mu.S = result$estimate[1] # -> 4976.372
sig.S = result$estimate[2] # -> 1110.1761
params.0 # -> 4976.372 1110.176

# Analysing Goodness of Fit
# Create a QQ-plot comparing quantiles of the sample to quantiles of the fitted distribution (abline) -> Goodness of Fit

comparison.qs = qnorm(ppoints(length(S)), mu.S, sig.S)

qqplot(comparison.qs, S, main = "Q-Q plot comparing quantiles of simulated compound distribution values against fitted normal quantiles", xlab = "Fitted distn quantiles", ylab = "Sample quantiles")
abline(0, 1, col = "red")
# Look reasonable but there is some deviation away from the line of perfect fit in the tail quantiles

# Plot the fitted density with emperical density ->
plot(density(S), main = "Simulated data versus fitted distribution", xlab = "Aggregate claim size", col = "blue")

agg = seq(from = 2000, to = 8500)
lines(agg, dnorm(agg, mu.S, sig.S), col = "red")

legend("topright", legend = c("empirical density", "fitted normal density"), col = c("blue", "red"), lwd = 2)
# We can see differences more clearly here than the QQ-plot -> fit is ok but not perfect


# Reinsurance - XoL
# If x is a vector os individual claim amounts and XoL reinsurance with retention level M ->
# y = pmin(x, M)

# and for the reinsurer ->
# z = pmax(0, x - M)

# To simulate the insurer's aggregate claim amount S(I) after reinsurance -> adapt the code

# Example 
# S = X1 + X2 + ...XN -> aggreagte claim amount from a portfolio of insurance policies
# -> N ~ Poisson(1000)
# Claim sizes are independent to N and Xi ~ Gamma(750, 0.25)

# First simulate S
set.seed(123)
sims = 10000

lambda = 1000
a = 750
b = 0.25

N = rpois(sims, lambda)
S = numeric(sims)

for (j in 1:sims){
	x = rgamma(N[j], a, b)
	S[j] = sum(x)
	}

# Now simulate the Insurer's aggregate claims after Reinsurance ->
M = 2500

# Repeat above with some changes -> Calculate the Aggregate claims for the insurere net of reinsurance, the the aggregate claim amount for the Reinsurer

set.seed(123)
sims = 10000

lambda = 1000
a = 750
b = 0.25

N = rpois(sims, lambda)
S = numeric(sims)
SI = numeric(sims)
SR = numeric(sims)

for (j in 1:sims){
	
	x = rgamma(N[j], a, b)
	y = pmin(x, M)
	z = pmax(0, x - M)
	
	S[j] = sum(x)
	SI[j] = sum(y)
	SR[j] = sum(z)
	
	}

mean(S) # ->
mean(SI) + mean(SR) # ->

# HAVE TO BE CAREFUL -> WHEN CONSIDERING A PORTFOLIO OF POLICIES -> INDIVIDUAL POLICIES CAN HAVE XoL -> APPLY M = 2500
# IF WE CONSIDER THE AGGREGATE Xol REINSURANCE ON AT PORTOFOLIO LEVEL -> MUCH BIGGER RETENTION LEVEL M!

# SI = min(S, M) -> pmin(SI, M)
# SR = max(0 S - M) -> pmax(0, S -M)

# Think of renaming these variable so you don't confuse youreself ->

# Question ->
# Simulate the Reinsurer's aggregate claim amount SR -> M 3000000

set.seed(123)
sims = 10000

lambda = 1000
a = 750
b = 0.25

M = 3000000

N = rpois(sims, lambda)
S = numeric(sims)
S.R = numeric(sims) # This at portfolio level and NOT POLICY LEVEL

for (j in 1:sims){
	x = rgamma(N[j], a, b)
		S[j] = sum(x)
	}

S.R = pmax(0, S - M)
head(S.R, 20)

# IMPORTANT TO SEE AT WHAT LEVEL THE REINSURANCE IS APPLIED -> IN THE CASE WHERE ITS AT PORTFOLIO LEVEL -> SIMPLIFIES EASILY

# Proportional Reinsurance ->
# Under individual claim amounts where proportional reinsruance is in place with retention percentage alpha ->
# y = alpha * x
# z = (1 - alpha) * x

# Adapt the same code at POLICY LEVEL:

set.seed(123)
sims = 1000

k = 700 # -> refer to tables for exams 
p = 0.2
mu = 4
sig = sqrt(9)
alpha = 0.9 # For the proportional retention %

# TO check the distributions in R -> ?distributions

N = rnbinom(sims, k, p)
S = numeric(sims)
S.I = numeric(sims)

for (j in 1:sims){
	x = rlnorm(N[j], mu, sig)
	y = alpha * x
	S[j] = sum(x)
	S.I[j] = sum(y)
	}

sd(S.I) # -> 8700210

S.R = S - S.I
# P(S.R > 1500000) ->
length(S.R[S.R > 1500000]) / length(S.R) # -> 0.221

# -> at the Portfolio level ->
# S.I = alpha * S
# S.R = (1 - alpha) * S

set.seed(123)
sims = 10000

lambda = 1000
a = 750
b = 0.25
alpha = 0.9 # For the proportional retention %

N = rpois(sims, lambda)
S = numeric(sims)
S.R = numeric(sims) # This at portfolio level and NOT POLICY LEVEL

for (j in 1:sims){
	x = rgamma(N[j], a, b)
		S[j] = sum(x)
	}

S.R = (1 - alpha) * S
head(S.R, 20)

#########################################################################################
# Question 1

# S ~ CNegBin -> N ~ NegBin(k = 10, p = 0.1) and X ~ LogNormal(mu = 10, sig = sqrt(5)
# Simulate 10000 random variates from the compound distribution and display the first 5 values

set.seed(16)
sims = 10000

k = 10
p = 0.1
mu = 10
sig = sqrt(5)

N = rnbinom(sims, k, p)
S = numeric(sims)

for (j in 1:sims){
	x = rlnorm(N[j], mu, sig)
	S[j] = sum(x)
	}

head(S, 5) # -> 17710443 15350841 11165450  9013810 39121408

# E(S) = E(N) * E(X)
EN = k*(1 - p) / p
EX = exp(mu + 0.5*sig^2)
ES = EN * EX

ES; mean(S) 
# 24150356 ; 23900459

# Var(S) = Var(N) * E(X)^2 + Var(X) * E(N)
VN = k*(1 - p) / (p^2)
VX = exp(2*mu + sig^2)*(exp(sig^2) - 1)
VS = (VN * (EX^2)) + (VX * EN)

VS; var(S)*(length(S) - 1) / length(S)
# 1.020107e+15 ; 7.76259e+14

# Simulate Insurer's aggregate claims net of reinsurance S.I 
# -> the question is providing individual XoL retention limit of M = 2000000 ?
# Ok...lets try

# KEY WORD -> INDIVIDUAL XoL RETENTION LIMIT

set.seed(16)
sims = 10000

k = 10
p = 0.1
mu = 10
sig = sqrt(5)

M = 2000000

N = rnbinom(sims, k, p)
SI = numeric(sims)

for (j in 1:sims){
	x = rlnorm(N[j], mu, sig)
	y = pmin(x, M)
	SI[j] = sum(y)
	}

head(SI, 5) # -> 13026210 15149555 10314330  6505138 24884351

#########################################################################################
# Question 2

# S ~ CP where N ~ Poisson(lambda = 500) amd X ~ Exp(beta = 0.001)

set.seed(1975)
sims = 1000

lambda = 500
beta = 0.001

N = rpois(sims, lambda)
S = numeric(sims)

for (j in 1:sims){
	x = rexp(N[j], beta)
	S[j] = sum(x)
	}

tail(S, 5) # -> 519744.4 481606.8 557499.7 476133.6 507867.3

mu0 = mean(S)
sig0 = sqrt((length(S) - 1) / length(S) * var(S))
# mu0 and sig0 are the ML estimates for mu and sig
# We will do it anyway using nlm for practice ->

params0 = c(mu0, sig0)

obj = function(data, params){
	-sum(log(dnorm(data, params[1], params[2])))
	}

result = nlm(obj, data = S, params0)
mu = result$estimate[1] # -> 499635.51 
sig = result$estimate[2] # -> 32001.13

plot(density(S), main = "Simulated data versus fitted distribution", xlab = "Aggregate claim size", col = "blue")
agg = seq(from = 375000, to = 625000)
lines(agg, dnorm(agg, mu, sig), col = "red")
legend("topright", legend = c("empirical density", "fitted normal density"), col = c("blue", "red"), lwd = 2)
# Resonably ok fit but there the left tails seems to be underestimated by the fitted distribtion
# Fit is adequate

# Could consider QQ-Plot
comparison.qs = qnorm(ppoints(length(S)), mu, sig)
qqplot(comparison.qs, S, main = "")
abline(0, 1, col = "red")
# Apart from left tail, fit is Ok


#########################################################################################
# Question 3

# Insurance company investigatig modelling insurance claim numbers as a Poisson Process

claims = read.csv("claims.csv", header = TRUE, sep = ",")
claims$claim.date = as.Date(claims$claim.date, format("%d/%m/%Y"))
head(claims)

claims$wait = c(NA, diff(as.numeric(claims$claim.date), 1, 1))
head(claims) # -> 

# Distribution of the waiting time -> Exp(lambda)

w.bar = mean(claims$wait[2:nrow(claims)])
lambda.hat = 1 / w.bar # -> 0.06100943
# Using Method of Moments -> which is also the ML estimate for lambda

hist(claims$wait[-1], xlab = "Waiting Time", ylab = "Density", main = "Density Histogram of Waiting Times to next Claim payment", freq = FALSE)
agg = seq(from = 0, to = max(claims$wait[-1]))
lines(agg, dexp(agg, lambda.hat), col = "red", lty = 2)
legend("topright", legend = c("Histogram Density", "Fitted Density"), col = c("blue", "red"), lty = 2)

# Want to test how well the distribution fits observed waiting times -> Table on page 5 of Chapter Questions
# The number of observed times in each of these bind can be calculated using ->

hist(claims$wait, breaks = c(seq(0, 60, 10),Inf), plot = FALSE)$counts # -> easy to understand ->
# -> breaks are the interval like we did for graduation -> here it start at 0 to 60 in gaps of 10
# plot = FALSE ... so dont plot the histogram
# $counts -> gives the frequencies

observed = hist(claims$wait, breaks = c(seq(0, 60, 10),Inf), plot = FALSE)$counts

sum(observed) # -> 220 ... so its exlcuding the NA waiting time

breaks = c(seq(0, 60, 10),Inf) # -> length(breaks) -> 8
expected = diff(pexp(breaks, lambda.hat)) * length(claims$wait[-1])

# This s ChiSq Gof -> DoF = 7 - 1 -> lose degree of freedom for makig the total match
# Count the number of groups less 1 degree of freedom -> 7 - 1 =6
dof = 6

TS = sum((observed - expected)^2 / expected)
p.value = pchisq(TS, dof, lower=FALSE) # -> 0.5397816
# Fail to reject the null that the distribution of the waiting times is Exponential(lambda = 0.06100943

claims.mean = mean(claims$claim.cost) # -> 7259.018
claims.sd = sd(claims$claim.cost) # -> 3426.188

# If X ~ gamma(a, b) ->
# E(X) = a / b
# Var(X) = a / (b^2) -> E(X) / b

# b = claims.mean / (claims.sd^2 * ((length(claims$claim.cost) - 1) / length(claims$claim.cost))) # -> 0.0006211908 CAREFUL! b = E(X) / Var(X) -> you kept it as a std deviation, and not variance!
b = claims.mean / (claims.sd^2) # -> 0.00061838
# a = claims.mean * b # -> 4.509235
a = claims.mean * b # -> 4.488831

# Assuming 365 days in a year -> lambda * t -> 0.06100943 * 365
L = 0.06100943 * 365

# Need to calculated the E(S) and Var(S)
# Because the ggregate claim process S ~ CP

# E(S) = E(Nt) * E(X)
# Var(S) = E(Nt) * E(X^2)

ES = L * a / b # -> 161647
VS = L * (a / (b^2) + (a / b)^2) # -> 1434802708
# sd.S = sqrt(VS) # -> 37863.17 -> because you adjusted to population variance -> unless we keep the sample variance with n-1 denominator as unbiased estimator of population variance?
sd.S = sqrt(VS) # -> 37878.79 -> keeping sample variance with n-1 denominator as unbiased estimator of population variance


#########################################################################################

# Individual Risk Model ->
# Simulate aggregate claims arising under Individual Risk Model
# Use results of simulations to estimate probabilities and moments of aggregate claims distributions
# Calculate the insurer and reinsurer share of aggregate claim payments under simple types of reinsurance arrangements

# Simulating Claims Using teh IRM ->
# Method:
# -> Recall that under IRM, the aggregate claim amount RV S can be modelled using IRM if:
# * number of risks are fixed
# * risk are independent
# * number of claims on each risk is either 0 or 1 -? {0,1}

# Also don't require identically distributed claim amounts for the IRM model to apply

# Refer to example in Notes -> pg 3

# Example 
# Company A has sold 1000 independent life insurance policies
# 250 were to ppl who smoke, 750 to non-smokers
# If a claim is paid, the size and distribution depends on the smoker status:
# -> for smokers, X ~ Gamma(200, 0.8)
# -> for non-smokers, X ~ Gamma(300, 0.4)

# probability of death for a smoker during term of the policy -> qs = 0.002
# probability of death for a non-smoker during term of the policy -> qns = 0.001

# Generate a sample of 500 aggreagte claim amounts from thiw portfolio policy ->
set.seed(54321)

# Specify parameters ->
a.s = 200
l.s = 0.8
q.s = 0.002

a.ns = 300
l.ns = 0.4
q.ns = 0.001

# First show how to simlulate a single observation of S -> expand idea to larger simulation
# 250 smokes, 750 non-smokers -> genenerate claim value from their distributions ->

x = c(rgamma(250, a.s, l.s), 
	  rgamma(750, a.ns, l.ns))

death = c(rbinom(250, 1, q.s),
		  rbinom(750, 1, q.ns)) # it doesn't matter if a claim amount was simulated and the policyholder doesn't die -> the policy will simply not pay because the life is still alive
		  

sims = 500
S.sim = numeric(sims)

for (j in 1:sims){

	x = c(rgamma(250, a.s, l.s), 
	      rgamma(750, a.ns, l.ns))

	death = c(rbinom(250, 1, q.s),
			  rbinom(750, 1, q.ns))
	
	sim = x * death
	
	S.sim[j] = sum(sim)
	
	}

quantile(S.sim, 0.9) # -> 1672.376 90%

# Question
# Company B sold 100 life insurance policies which gives risk to a maximum of one claim per year -> makes sense ... just renew policy every year
# There are three types of policy ->

# Type I:   claims follow Exponential(0.005) and probability of a claim is 0.001
# Type II:  claims follow Exponential(0.006) and probability of a claim is 0.002
# Type III: claims follow Exponential(0.007) and probability of a claim is 0.003

# 30 Type I, 50 Type II, and 20 Type III policies -> All policies are independent

# Simualte 5000 observations from the insurer's aggregate claims size distribution estimate the probability that if will have to payout more then 100000 for these policies

set.seed(54321)

l.I = 0.005
l.II = 0.006
l.III = 0.007

q.I = 0.001
q.II = 0.002
q.III = 0.003

sims = 5000
S.sim = numeric(sims)

for (j in 1:sims){
	
	x = c(rexp(30, l.I),
		  rexp(50, l.II),
		  rexp(20, l.III))

	death = c(rbinom(30, 1, q.I),
			  rbinom(50, 1, q.II),
			  rbinom(20, 1, q.III))
	
	sim = x * death
	S.sim[j] = sum(sim)
	}

# P(S.sim > 100) ->
length(S.sim[S.sim > 100]) / length(S.sim) # -> 0.1056


# Individual Risk Model with Reinsurance ->
# When reinsurance applies, the number of policies and number of claims assumed by the IRM will be unaffected
# -> number of risk are fixed, and each risk will have at most one claim on it
# -> The SIZE of each claim should be adjusted to relect the amounts ceded to the reinsurer 

# Excess of Loss XoL Reinsurance
# Consider the example above with the smokers and non-smoker -> insurance company has bought INDIVIDUAL XoL reinsurance with M = 200
# -> each policy is capped on claim payment by insurer to M = 200
# -> y = pmin(x, M)

# So ... Generate a sample of 500 aggreagte claim amounts from thiw portfolio policy ->
set.seed(54321)

# Specify parameters ->
a.s = 200
l.s = 0.8
q.s = 0.002

a.ns = 300
l.ns = 0.4
q.ns = 0.001

M = 200

# First show how to simlulate a single observation of S -> expand idea to larger simulation
# 250 smokes, 750 non-smokers -> genenerate claim value from their distributions ->
 
sims = 500
S.sim = numeric(sims)

for (j in 1:sims){

	x = c(rgamma(250, a.s, l.s), 
	      rgamma(750, a.ns, l.ns))

	y = pmin(x, M)
	
	death = c(rbinom(250, 1, q.s),
			  rbinom(750, 1, q.ns))
	
	sim = y * death
	
	S.sim[j] = sum(sim)
	}

quantile(S.sim, 0.9) # -> 600 90%

# This is lower than not having the reinsurance but this is expected -> you're capping the claim payments

# Proportional reinsurance ->
# Similar adjustment can be made for proportional reinsurance agreements ->
# Y = alpha * X
# Z = (1 - alpha) * X

# Question 
# Estimate the mean of the reinsurer's aggregate claims distribution under proportional insurance agreement with retention alpha = 80%

set.seed(54321)

# Specify parameters ->
a.s = 200
l.s = 0.8
q.s = 0.002

a.ns = 300
l.ns = 0.4
q.ns = 0.001

alpha = 0.8

# First show how to simlulate a single observation of S -> expand idea to larger simulation
# 250 smokes, 750 non-smokers -> genenerate claim value from their distributions ->
 
sims = 500
S.sim = numeric(sims)

for (j in 1:sims){

	x = c(rgamma(250, a.s, l.s), 
	      rgamma(750, a.ns, l.ns))

	z = (1 - alpha) * x
	
	death = c(rbinom(250, 1, q.s),
			  rbinom(750, 1, q.ns))
	
	sim = z * death
	
	S.sim[j] = sum(sim)
	}

mean(S.sim) # -> 139.8874


#########################################################################################
# Question 4

prob.claim = read.table("IRM.txt", header = TRUE)[, 2]
exp.param = read.table("IRM.txt", header = TRUE)[, 3]

# Insurer has sold 500 life policies -> each policy gives rise to at most one claim
# If claim occurson risk i -> benefit ~ Exponential(lambda i) for i = 1, 2, ..., 500
# Policies are independent and claim sizes and claim numbers are not neccesarily identically distributed

sum(prob.claim * (1 / exp.param)) # -> 25047.8
mu.S = sum(prob.claim * (1 / exp.param))

set.seed(5)
sims = 10000

S.sim = numeric(sims)

for (j in 1:sims){
	
	x = rexp(500, exp.param)
	death = rbinom(500, 1, prob.claim)
	
	sim = x * death
	S.sim[j] = sum(sim)
	}

# P(0.9 * mu.S < S < 1.1 * mu.S)
(length(S.sim[S.sim <= 1.1 * mu.S]) - length(S.sim[S.sim <= 0.9 * mu.S])) / length(S.sim) # -> 0.0756

lower = 0.9 * mu.S
upper = 1.1 * mu.S
length(S.sim[(S.sim <= upper) & (S.sim >= lower)]) / length(S.sim) # -> 0.0756

# If the Aggregate claim of portfolio is equal to zero -> it means that there are no claims
# P(no claims) -> 
prod(1 - prob.claim) # -> 0.07284173

# P(S.sim = 0) ->
length(S.sim[S.sim == 0]) / length(S.sim) # -> 0.0724

# Quite similar and differ mainly because of sampling error in S.sim

# Point mass at S = 0 -> The probability of no claims is quite high in this case and does have a pull on the distribution
# We can see this if we break the histogram into caller intervals -> see the the positive skewness effect the point mass will have on S

hist(S.sim, breaks = 500, freq = FALSE, xlim = c(0, 60000), ylim = c(0, 0.0002), main = "Simulated PDF of S")
# S is very postively skewed -> although CLT does reduce this effect

#########################################################################################
# Question 5

set.seed(1812)
q = runif(200, 0, 0.01)
mu = runif(200, 5, 13)
sigma = runif(200, 0.5, 0.8)
policies = cbind(q, mu, sigma)

# Aggregate claims under portfolio of insurance business are expected to follow IRM
# ith row of policies contains the risk characteristics
# Individual claims follow logNormal(mu, sig)

# There are 200 policies 
# XoL arrangement in place -> M = 60000 on any one claim

set.seed(1854)
sims = 5000

M = 60000

SR.sim = numeric(sims)

for (j in 1:sims){

	death = rbinom(200, 1, policies[, 1])
	
	x = rlnorm(200, policies[, 2], policies[, 3])
	z = pmax(0, x - M)
		
	sim = z * death # ORDER MATTERS! First death, then z
	SR.sim[j] = sum(sim)
	}
	
mean(SR.sim) # -> 51425.54

# Assume now an AGGREGATE XoL -> M = 300000
set.seed(1854)
sims = 5000

M = 300000

S.sim = numeric(sims)

for (j in 1:sims){

	death = rbinom(200, 1, policies[, 1])
	
	x = rlnorm(200, policies[, 2], policies[, 3])
		
	sim = x * death # ORDER MATTERS! First death, then x
	S.sim[j] = sum(sim)
	}
	
S.R.sim = pmax(0, S.sim - M)
mean(S.R.sim) # -> 20635.58


#########################################################################################

# Paramater Uncertainty
# -> Simulate claims from a CP with parameter Uncertainty
# -> Simulate aggregate claims arising from a heterogenous portfolio with parameter Uncertainty
# -> Simulate aggregate claims arising from a homogenous portfolio with parameter Uncertainty
# -> Use results of simulation to estimate moments of individuals policy claims distribution and the aggregate claims Distribution

# Method
# Consider a portfolio of 100 policies -> each have a separate CP hence the PORTFOLIO IS HETEROGENOUS

# The random variable S = S1 + S2 + ... + S100
# Each Si has a CP distribution with unknown parameter lambda i ->
# That is Si = X1i + X2i + ... + X(Ni, i)
# -> Ni ~ Poisson(lambda i)
# -> Xi ~ Gamma(750, 0.25)

# Core Reading Example 1 - 
# Assume lambda i if equally likely to be 0.1 or 0.3
# Aggregate claims from a randomly chosen policy has a mixture distribution -> CP depends on an unknown Paramater

# We can simulate 10000 values from this mixture distribution and use it to estimate moments
# First simulate a value for lamdba i and use to simulate the aggregate claim amount using this paramter -> that is for Si -> get lambda i, generate N -> generate X -> Si

sims = 10000
S = numeric(sims)

set.seed(123)
lambda = sample(c(0.1, 0.3), replace = TRUE, size = sims, prob = c(0.5, 0.5))
# generate sims number of lambda -> 
# Instead of using a fied lambda to generate N as before, each value in N will use have been sampled from its own lambda i -> for i in sims
N = rpois(sims, lambda)

for (j in 1:sims){
	x = rgamma(N[j], 750, 0.25)
	sim = sum(x)
	S[j] = sim
	}

mean(S) # -> 606.8383
sd(S) # -> 1373.769

# So what we've done essentially is focus on a single policy in the portfolio -> that policy can have multiple claims
# Now we consider the aggregate policy -> that is 100 policy portoflio with each policy having a CP distribution
# We need to generate simulations for the whole portfolio -> simulate a Poisson parameter for each of the 100 POLICIES
# -> Then need to simulate the aggregate claim amount for each individual policy from the relavant CP distribution
# -> Store the total claims for each policy across all the simulations
# -> Then sum up across 100 policies to get the total S

sims = 10000

policies = 100
S = numeric(policies) # see how this is not numeric(sims) -> we are going to repeat the below for sim later -> look at Results matrix below

Results = matrix(nrow = sims, ncol = policies)

# Now consider one simulation -> for each simulation we first simulate a Poisson parameter for each policy:
lambda = sample(c(0.1, 0.3), replace = TRUE, size = policies, prob = c(0.5, 0.5))
# Then simulate the number of claims from each policy using the lambdas generated
N = rpois(policies, lambda)
# There are 100 policies for each simulation in sims -> each policy has its own lambda for each i in sims

for (j in 1:policies){
	x = rgamma(N[j], 750, 0.25)
	sim = sum(x)
	S[j] = sim
	}
	
# Store S in Results[i, ] = S

# Ok now do this for i in sims ->
sims = 10000
policies = 100

Results = matrix(nrow = sims, ncol = policies)

set.seed(123) # Dont forget about this guy...

for (i in 1:sims){

	S = numeric(policies) # see how this is not numeric(sims) -> we are going to repeat the below for sim later -> look at Results matrix below
	lambda = sample(c(0.1, 0.3), replace = TRUE, size = policies, prob = c(0.5, 0.5))
	N = rpois(policies, lambda)
	
	for (j in 1:policies){
	x = rgamma(N[j], 750, 0.25)
	sim = sum(x)
	S[j] = sim
	}
	
	Results[i, ] = S

	}
	
# There are sims = 10000 rows in Results, and policies = 100 columns
# Each row sum is a simulation of the PORTFOLIO of POLICIES
# Total simulations of S.agg = rowSums(Results)

S.agg = rowSums(Results)
mean(S.agg) # -> 60120.26
sd(S.agg) # -> 13563.84


# Core Reading Example 2 - 
# Assume lambda i  follows a Gamma(0.1, 1) distribution -> lambda i for i in sims -> therefore 10000 lambda i, each for Si 

# First simulate the number of claims from a single policy -> follow the logic above
# The number of claims from a randomly chosen policy has a mixture distribution as it depends on unknown Poisson parameter lambda i
# We can simulate 10000 values from this mixture distribution and use it to estimate its Moments
# First simulate a value of the Poisson parameter and use this to simulate the number of claims for a policy with this paramter -> do this i in sims = 10000

sims = 10000
set.seed(123)

lambda = rgamma(sims, 0.1, 1)
N = rpois(sims, lambda)
mean(N) # -> roughly 0.1 (alpha * beta) but from above -> 0.0979
sd(N) # -> 0.4560004

S = numeric(sims)

for (i in 1:sims){
	x = rgamma(N[i], 750, 0.25)
	sim = sum(x)
	S[i] = sim
	}

mean(S) # -> 293.6952
sd(S) # -> 1369

# Now for the policies in the Portfolio
sims = 10000
policies = 100

Results = matrix(nrow = sims, ncol = policies)

set.seed(123) # Dont forget about this guy...)

for (i in 1:sims){

	lambda = rgamma(policies, 0.1, 1)
	N = rpois(policies, lambda)
	
	S = numeric(policies)
	
	for (j in 1:policies){
		x = rgamma(N[j], 750, 0.25)
		sim = sum(x)
		S[j] = sim
		}
	
	Results[i, ] = S
	
	}
	
S.agg = rowSums(Results)
mean(S.agg) # -> 30149.24
sd(S.agg) # -> 13372.75


# Simulating claims from a HOMOGENOUS PORTFOLIO ->
# Method - we again consider 100 policy portfolio
# Each of 100 policies will now have the same CP distribution -> that is the parameter lambda i = lambda for i in sims for the 100 policies
# There will still be sims number of lambdas -> just that for each i in sims, the lambda will be the same for the 100 policies -> hence homogenous Portfolio
# -> The Poisson paramater underling this distribution is still unknown 


# The RV S = S1 + S2+ ... + S100 represents the aggregate claim amount from a portfolio of 100 independent insurance policies
# Each Si have the same CP distribution lambda -> MAKES THE PORFOLIO HOMOGENOUS
# The parameter is the same across all policies -> however it is still unknown

# Si = X(i, 1) + X(i, 2) + ... +X(i, Ni) where:
# -> Number of claims Ni ~ Poisson(lambda) lambda fixed for i across 100 policies
# -> Xi are independent and identically distributed -> Xi ~ gamma(750, 0.25) for all policies independent of Ni

# Core Reading Example 3 - 
# We assume that lambda is equally likelt to be 0.1 or 0.3 -> still an unknown parameter

# The homogenous case is similar to example 1 above -> just keep lambda constant across the policies in each i of sims

# Ok now do this for i in sims ->
sims = 10000
policies = 100

Results = matrix(nrow = sims, ncol = policies)

set.seed(123) # Dont forget about this guy...

for (i in 1:sims){

	S = numeric(policies) # see how this is not numeric(sims) -> we are going to repeat the below for sim later -> look at Results matrix below
	lambda = sample(c(0.1, 0.3), replace = TRUE, size = 1, prob = c(0.5, 0.5)) # here we dont need 100 different lambda for each policy -> we keep the distribuion of Ni the same for 100 policies
	# -> hence the homogeneity of the portfolio
	N = rpois(policies, lambda) # the distribution of Ni is the same for all 100 policies! 
	
	for (j in 1:policies){
	x = rgamma(N[j], 750, 0.25)
	sim = sum(x)
	S[j] = sim
	}
	
	Results[i, ] = S

	}
	
# There are sims = 10000 rows in Results, and policies = 100 columns
# Each row sum is a simulation of the PORTFOLIO of POLICIES
# Total simulations of S.agg = rowSums(Results)

S.agg = rowSums(Results)
mean(S.agg) # -> 59949.78
sd(S.agg) # -> 32936.86

# We can compare all the moments for the difference examples covered ->
# Example 1 and Example 3 -> because lambda is equally likely to be 0.1 or 0.3 in example 1 -> smaller values of lambda offset the larger value which leads to reduced variance compared to Example 3
# Example 3 -> all policies are either 0.1 or 0.3 -> this means taht we more likely to get more extreme results comapred to Example 1 i.e. very low or very high aggregate claim amounts for the portfolio
# -> This is what leads to higher overall variation for the homogenous portfolio


#########################################################################################
# Question 6

# Portfolio has 70 independent policies each with CBin with unknown probability of success pi and n = 50
# The success probability pi is assumed to follow a Beta(8, 5) distrubution independently for each policy

set.seed(42)
sims = 5000

p = rbeta(sims, 8, 5)
head(p, 10) # -> 0.6985561 0.5581849 0.5133324 0.5514826 0.6315002 0.3524886 0.6297709 0.2550679 0.6249380 0.3920331

N = rbinom(sims, 50, p)

mean(N) # -> 30.825
sd(N) # -> 7.360734

# Each underlying claim X ~ Exponential(0.01) independently of other claim amounts and number of claims

S = numeric(sims)

for (i in 1:sims){
	x = rexp(N[i], 0.01)
	sim = sum(x)
	S[i] = sim
	}

mean(S) # -> 3075.395
sd(S) # -> 928.3324

######################

sims = 5000
policies = 70

Results = matrix(nrow = sims, ncol = policies)

set.seed(42)

for (i in 1:sims){

	p = rbeta(policies, 8, 5)
	N = rbinom(policies, 50, p)
	
	S = numeric(policies)
	
	for (j in 1:policies){
		x = rexp(N[j], 0.01)
		sim = sum(x)
		S[j] = sim
		}
	
	Results[i, ] = S
	
	}
	
S.agg = rowSums(Results)
mean(S.agg) # -> 215410.1
sd(S.agg) # -> 7583.685