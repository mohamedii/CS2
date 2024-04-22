# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\18 Reinsurance")

# Reinsurance ->
# Carry out simple operations to calc impact of Reinsurance
# Obtain reinsurer's conditional claims distribution i presence of XoL
# Allow for effects in inflation on Reinsurance
# Fit a distributionto claims data when Reinsurance present

set.seed(2030)
x = round(rlnorm(5000, 7, 1), 2) # -> we will use this dataset through out the chapter

# XoL Reinsurance ->
# Y = min(X, M)
# Z = max(0, X-M)

# Reinsurers Conditional Claim Amounts -> W = X-M -> Z | Z>0
# Easy to implement in R -> think on how we can run queries on the vectors ->

# w = z[z>0]

# Example

M=1000

y = pmin(x,M)
z = pmax(0, x-M)

x[20]; y[20]; z[20] # -> x = y + z

# Calculate the proporation of cliams that involve the reinsurer
length(z[z>0])/length(x) # -> 0.534
# or
length(x[x>M])/length(x)

# Question 
# Claims ~ Pareto(2, 800)
a = 2
l = 800

# XoL -> M=1500

M = 1500

rpareto = function(n, a, l){
	l * ((1 - runif(n))^(-1/a)-1)
	}

set.seed(5)
X = rpareto(10000, a, l)
# P(claim involves reinsurer) ->
length(X[X>M])/length(X)
# -> 0.1231 -> Be careful though...we used the sample to determine this. We should theoretically do it from at the distriubtion level

# Analysing the effect of XoL

# Variance of the insurer's claim amounts before reinsurance ->
var(x)

# Variance of insurer's claim amounts after reinsurance
var(y)

# clearly with reinsurance, the variance of net claims is lower than claims without reinsurance
# If the reduction is quite high...consider then the rention level might be too low and the reinsurer is active in more claims
# The calculated probability that the reinsurer is involved is 0.534 ... in more than half the claims

# Calculate the reduction in the insurer's mean claim amount ->

1 - mean(y)/mean(x) # -> 56.18% reduction

# Question

# Median claim amount of net of insurnace ->
# X ~ lognormal(7, 1)
# M = 1000
# Median of the claim if there is no reinsurance
x.m=qlnorm(0.5,7,1) #-> 1096.633
# x.m > M so under reinsurance -> min(x.m, M) -> M
# From the data vector y=min(X,M)
y.m = quantile(y, 0.5) # -> 1000: 50%


# Estimate the probability that insurer pays < 500
# P(X<500)
length(x[x<500])/length(x) # -> 0.2184

# Reinsurers mean claim amount on which it is involved
mean(z[z>0]) # -> 1880.778

# Total amount paid by reinsurer on 3 largest claims
sum(sort(z, decreasing=TRUE)[1:3]) # -> 95276.49

# Designing a Reinsurance Arrangement
# Insruance companies deisgn reinsurance cover that:
# -> mazimisese their profits
# -> minimisese the volatility of their claim payments

# Example -> Calculate the minimum premium the reinsurer should charge for the reinsurance arrangement
# -> to avoid making a loss

# To do this, write a function to calc the absolute value of the reinsurers profit for a given level of premium ->
z.profit = function(premium){
	z = pmax(0, x-1000)
	total.reinsurance.payments = sum(z)
	abs(premium - total.reinsurance.payments)
	}

premium.0 = 2000 # initial guess -> we want to solve for this...which from the equations above must equal the sum of claims -> breakeven
# -> thats what this function is going to do
nlm(z.profit, premium.0) # -> which indeed it does ->sum(z) = 5021677

# Insurer has charged a premium of 2,000,000 -> what must the retention limit be such that reinsurer doesn't make a loss?

z.profit = function(M){
	z = pmax(0, x-M)
	total.reinsurance.payments = sum(z)
	abs(premium - total.reinsurance.payments)
	}

premium = 2000000
M.0=2000
nlm(z.profit, M.0) # -> M = 3054.289


# Proportional Reinsurance
# Use of a retention proportion alpha

# Y = alpha * X
# Z = (1-alpha) * X

set.seed(2030)
x = round(rlnorm(5000, 7, 1), 2) # -> we will use this dataset through out the chapter

a = 0.8
y=a*x
z=(1-a)*x

mean(y); var(y)
mean(z); var(z)

# Inflation of Claim Amounts
# Recall that claim amounts X inflate by a factor of k each year -> the claim amount RV will be kX
# Formulae for calc claim payments when reinsurance is in force -> adjusted 

# For XoL ->
# y = min(k*x, M)
# z = max(0, k*x - M)

# For Proportional resinsurance ->
# y = a*k*x
# z = (1-a)*k*x

# Example 1

set.seed(2030)
x = round(rlnorm(5000, 7, 1), 2) # -> we will use this dataset through out the chapter

k=1.03
a=0.85

# For XoL ->
# y = min(k*x, M)
# z = max(0, k*x - M)

# For Proportional resinsurance ->
y = a*k*x
z = (1-a)*k*x

mean(z)

y1=var(a*x)
y2=var(a*k*x)
# What is the increase in variance? -> whould be k^2 -> 1.03^2 -1 -> 6.09%

# Question -> Esitimate 90th percentile of reinsurer's claim distribution in 10 years time
# k -> 1.03^10

k=1.03
k=k^10
quantile((1-a)*k*x, 0.9) # -> 786.9652: 90%

# Example 2

# Claim sizes X ~ Exp(0.001)
# Insurer has introduced a policy excess of 100 next year
# Annual claims are expected to to increase with inflation of 5%

j=0.05

set.seed(2357)
x = rexp(10000, 0.001)
# Inflate by 5% to determine next year's claim distribution
x.infl = x*(1+j)

# Policy Excess = 100 -> E = 100
E=100
y=pmax(0, x.infl-E)
mean(y)

mean(y[y>0])
1-length(y[y==0])/length(x.infl)

# Estimation
# When XoL reinsurance is in place, the insurer and reinsurer's data my be incomplete
# -> insurers claims are capped at specific rention level M
# -> the reinsurer may not be notified of claims below retention level
# -> We therefore call this censored data or truncated data

# We will use R to derive MLE or a claim size distribution  when reinsurance is in place
# We adapt what we know on MLE estimation in R to account for incomplete data

# Remember how the Likelihood function will be constructed when censoring is involved - 

# L = [P(X < M]^n1 * Prod(i=1, n2){f_X(zi + M)}
# We use f_X(zi + M) essentially as the conditional distribution because reinsurer will pay what he knows...n2 claims and n1 claims are censored
# zi is the amount paid by the reinsurer

# log Likelihood then -> lnL = n1*log(P(X<M)) + sum(i=1, n2){log(f_X(zi + M))}
# We will derive the MLE on the negative log-Likelihood -> nlm

set.seed(1955)
zclaims = pmax(0, rgamma(10000, floor(runif(1, 10, 15)), round(runif(1, 2.2, 2.3), 2)) - 7)
# here we assume alpha ~ uni(10,15) and beta ~ uni(2.2, 2.3)
# is the rentention level on the policy 7?...yip it is

# Remember, there are zclaims = 0 in the vector but the reinsurer doesn't know that...he experiences claims > 0

n = length(zclaims)
n1 = length(zclaims[zclaims==0])
n2 = n - n1
M=7

# P(X < M) -> no claims paid by reinsurer
p=length(zclaims[zclaims==0])/length(zclaims)
# Can't use this...remember there are parameters...this is using the sample on one part...must be consistent for both parts -> censored and without

params.0 = c(floor(runif(1, 10, 15)), round(runif(1, 2.2, 2.3), 2))


f=function(data, params){
	-(n1 * log(pgamma(M, params[1], params[2])) + sum(log(dgamma(data[data>0] + M, params[1], params[2]))))
	}

MLE = nlm(f, params.0, data = zclaims)
# alpha -> 10.488022
# beta -> 2.136183

# Question
# For the insurer now -> consult the example in the notes but essentially ->
# Claims payments look like this -> {x1, x2, M, x4, M, x5, x6, M, M, x9, M, ...}

set.seed(10249)
yclaims = pmin(rexp(10000, rgamma(1, 3, runif(1, 580, 620))), 1000)

M=1000

# You believe that X ~ Exp(lambda)...lambda unknown -> want to extimate it using MLE
params.0 = rgamma(1, 3, runif(1, 580, 620)) # -> this is an intial starting point...because we know the mean(y) < mean(x) with this reinsurance agreement

n = length(yclaims)
n1 = length(yclaims[yclaims == M]) # -> these are the number of claims that we claim from the reinsurer for the balance -> X > M 
n2 = n - n1

f=function(data, params){
	-(n1*log(pexp(M, params[1], lower.tail=FALSE)) + sum(log(dexp(data[data != M], params[1]))))
	}

MLE = nlm(f, params.0, data = yclaims)
l = MLE$estimate

x=seq(100, 1500, 10)

plot(x, dexp(pmin(x, M), l), type="l", col="red", lty=2)
lines(density(yclaims, from = 100, to = 1500), col="blue")

# The two lines are quite close across the rest of the range (ie where the underlying claim sizes are
# the same as the insurer’s claim amounts), so the fitted distribution appears to be a good fit

#########################################################################################
# Question 1

# X ~ Gamma(a, l) -> has XoL reinsurance
a = 5
b = 0.04
M =200

# Calculate the proportion of claims in which reinsurer is involved
# -> P(X>M)
pgamma(M, a, b, lower.tail=FALSE) # -> 0.0996324


#########################################################################################
# Question 2

set.seed(1); x = sort(rpois(365, rgamma(365, 20, 0.01)))

# Number of claims recored in x
# one in a hundred claims rejected as fraudulent -> rate = 1/100 constant of many years
# Expect the farud rate to increase by 4% next year -> no change though to number of claims next year -> x stays the same

r = 0.01
r = r*1.04

p=1-r
w = p*x

n=length(x)

skew.x = (1/n)*sum((w - mean(w))^3)
skew.x # -> 48615614
# You can take the third root of this -> skew.x^(1/3) -> 364.9712
coeff.skew.x = skew.x/(var(w)^(3/2))
coeff.skew.x # -> 0.5347658


#########################################################################################
# Question 3

set.seed(16550)

# X ~ Exp(0.0005)
# Purchase XoL with rention M
l = 0.0005
M = 2500

x = rexp(10000, l)
y = pmin(x, M)
mean(y)

y=function(data, M){
	
	z=c(rep(0, length(data)))
	
	for (j in 1:length(data)){
			if (data[j] <= M){
				z[j]=0 }
			else if (data[j] > M & data[j] <= (M+1000)){
				z[j] = data[j] - M }
			else {
				z[j]=1000 }
		}
	
	data - z
	}

f=function(data, M){
	var(y(data, M))
	}

M.0 = 2500
M.est = nlm(f, M.0, data = x)$estimate
M.est # -> 1625.883


#########################################################################################
# Question 4

# A reinsurer has entered into XoL -> M = 45000

pmts = read.table("Reinsurance payments.txt", header=FALSE)$V1

# X ~ lognormal(mu, sigma)

M = 45000
n = length(pmts)
n1 = length(pmts[pmts>0]) # where the reinsurer pays
n2 = n - n1 # where the insurer pays i.e. no claim made for reinsurer to pay -> iow censored -> X < M

params.0 = c(mean(log(pmts[pmts>0])), sd(log(pmts[pmts>0])))

obj=function(data, params){
	-(n2*log(plnorm(M, params[1], params[2])) + sum(log(dlnorm(data[data>0]+M, params[1], params[2]))))
	} # Dont forget to log the first term...thats why convergence was poor

MLE = nlm(obj, params.0, data = pmts)
mu = MLE$estimate[1]; sigma = MLE$estimate[2]
mu; sigma

x = seq(0,87000, 1000)
hist(pmts + M, freq = FALSE, breaks = c(20000, 45000, seq(46000, 87000, 1000)), xlim = c(0, 87000), ylim = c(0, 0.00005), ylab = "Density", main = "Truncated data versus logN(10.475,0.265)", col = "grey")
lines(x, dlnorm(x, mu, sigma), type="l", lty=2, col="red")
# X ~ lognormal(mu, sigma) distribution appears to be a fairly reasonable fit for the data

claim.max=max(pmts)
# which(pmts == max) cant use this becuase of how you imported the table
plnorm(claim.max + M, mu, sigma, lower.tail=FALSE) # -> 0.00046499


#########################################################################################

# Reinsurance 
# -> Carry out calcs on impact of Reinsurance
# -> Obtain the conditional claim distribution in presense of XoL
# -> Allow policy excess for insurer and reinsurer
# -> fit a distribution to claims data when reinsurance is present

set.seed(2030)
x = round(rlnorm(5000, 7, 1), 2)

# Insurers claim amounts ->
# Y = {X for X < M, M for X >= M}
# In other words -> Y = min(X, M)

# y = pmin(x, M)

# Reinsurers claim amounts ->
# Z = {0 for X < M, X - M for X >= M}
# In other words -> Z = max(0, X - M)

# z = pmax(0, x - M)

# The Reinsurers conditional claim amounts ->
# W = Z | Z > 0
# -> W = X - M | X > M

# w = z[z > 0] -> length(w) < length(z) -> obviously by claims that not paid because they're zero -> the conditional distribution of W:
# -> F_W(w) = (F_X(w + M) - F_X(M) ) / (1 - F_X(M))

# Example
# X is a vector gross claims -> Insurer has arrange XoL with retention M = 1000
# X was generated at the beginning -> x
M = 1000

y = pmin(x, M)
z = pmax(0, x - M)

x[20] # -> 2912.93
y[20] # -> 1000
z[20] # -> 1912.93

# Proportion of claims that involved the reinsurer -> P(X > M)
# Use emperical probability to estimate this

length(z[z > 0]) / length(z) # -> 0.534
# or ->
length(x[x > M]) / length(x) # -> 0.534

# Example
# Claims from insurance policies follow Pareto(a = 2, l = 800). XoL arranged with retention limit = 1500

a = 2
l = 800
M = 1500

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


set.seed(5)

x = rpareto(10000, a, l)

# P(X > M) ->
length(x[x > M]) / length(x) # -> 0.1231

# Analysing the effect of XoL -> consider the mean and variance of claims paid by insurer and reinsurer
var(x) # -> 5087855
var(y) # -> 83783.14
# The variance has reduced because of XoL -> variability in the data passed onto the reinsurer

# Calculate the % reduction in the insurer's mean claim as a result of reinsurance ->
1 - (mean(y) / mean(x)) # -> 0.5618093

# Question
median(y) # -> 1000

length(x[x < 500]) / length(x) # -> 0.2184

mean(z[z > 0]) # -> 1880.778

sum(head(sort(z, decreasing = TRUE), 3)) # -> 95276.49

# Designing a Reinsurance Contract - >
# -> Maximise profit
# -> Minimize volatility of claim payments

# Example
# Calculate the minimum premium the reinsurer should have charged for the reinsurance arrangement -> to avoid making a loss on the contract ->

z.profit = function(premium){
	z = pmax(0, x - 1000)
	total.reinsurance.payments = sum(z)
	profit = premium - total.reinsurance.payments
	abs(profit) # done purpose for the nlm -> show the minimum premium must equal the total payout -> sum(z)
	}

premium0 = 20000

result = nlm(z.profit, premium0)
result$estimate # -> 5021677
sum(z) # -> 5021677

# Question
# Reinsurer has charged a premium of 2mil for the XoL -> what is the minimum retention limit such that reinsurer does not make a loss

z.profit = function(M){
	z = pmax(0, x - M)
	total.reinsurance.payments = sum(z)
	profit = 2e6 - total.reinsurance.payments
	abs(profit)
	}
	
M0 = 500

result = nlm(z.profit, M0)
result$estimate # -> 3054.289
sum(pmax(0, x - result$estimate)) # -> 2e6

# Proportional Reinsurance ->
# Y = a * X
# Z = (1 - a) * X

# Example
# X ~ logN(mu = 7, sig = 1)
# Proportional reinsurance in force with a = 0.8 

# E(Z) = (1 - a) * E(X)
# Var(Y) = (1 - a)^2 * Var(X)

EX = exp(7 + 0.5*1)
VX = exp(2*7 + 1)*(exp(1) - 1)

EZ = (1 - 0.8) * EX # -> 361.6085
VZ = (1 - 0.8)^2 * VX # -> 224683.7


# Inflation of Claim Amounts ->
# If claim amounts X inflate by a factor k -> the amount RV will be k*X

# XoL with Inflation ->
# y = pmin(k*X, M)
# z = pmax(0, k*X - M)

# Proportional reinsurance with Inflation -> 
# y = a * k*x
# z = (1 - a) * k*X

# READ EXAMPLE ON pg 12 - 14 -> straight forward 

# Estimation
# When XoL reinsurance is in place -> insurers and reinsureres data may be incomplete
# -> Insurers claims are capped at M retention level -> censoring of claims > M
# -> Reinsurer may not be notified of claims below retention level -> truncation

# We can use R to derive the MLE for a claim distribution when reinsurance is in place ->
# BE CAREFUL WITH THE LOG AND SUM -> BE VERY VERY CAREFUL
# Formula is too detailed -> but you understand how to setup the likelihood function -> pg 15
# EXAMPLE IN THE NOTES ARE FOR THE MLE ESTIMATION FROM THE PERSPECTIVE OF THE REINSURER!!

# L = (P(X < M))^n1 * product[i = 1, n2](f_X(zi + M))
# logL = n1*log(P(X < M)) + sum[i = 1, nz](log(f_X(zi + M)))

# Remember there will be:
# n1 -> the number of claims that don't involve the reinsurer
# n2 -> the number of claims when the reinsurer is involved
# n1 + n2 = total number of claims
# zi -> amount paid by the reinsurer on the ith claim which it is involved i.e. claim > M

# Example 

set.seed(1955)
zclaims <- pmax(0, rgamma(10000, floor(runif(1, 10, 15)), round(runif(1, 2.2, 2.3), 2)) - 7)
# There are some zeros in there -> will have to calculate the number of times reinsurer paid and not

n2 = length(zclaims[zclaims > 0]) # -> 933
n1 = length(zclaims) - n2 # -> 9067

# Use retention level of M = 7
M = 7

# Construct the log likelihood function PROPERLY to use in nlm ->
# We are assuming X ~ Gamma(a, l) ->

params0 = c(10, 10)
# i.e. a0 = 10 and l0 = 10

obj = function(data, params){
	# claim not paid -> P(X < M)
		const1 = -n1*log(pgamma(M, params[1], params[2]))
		
	# claims are paid i.e. X > M
		const2 = -sum(log(dgamma(data[data > 0] + M, params[1], params[2]))) # IMPORTANT! MAKE SURE YOU USE z > 0
		
	const1 + const2	
	
	}

result = nlm(obj, data = zclaims, params0)
a = result$estimate[1] # -> 10.48802
l = result$estimate[2] # -> 2.136183

# Example
set.seed(10249)
yclaims = pmin(rexp(10000, rgamma(1, 3, runif(1, 580, 620))), 1000)

# X is assumed to follow Exp(lambda)
# Insurer pays claims up to and including the retention level M

M = 1000

# Estimate the distribution parameter lambda using MLE

# Note:
# n1 -> number of claims paid < M
# n2 -> number of claim paid = M

n1 = length(yclaims[yclaims < M]) # -> 9357
n2 = length(yclaims) - n1 # -> 643

lambda0 = 1 / mean(yclaims)
params0 = lambda0 # -> just as starting value for lambda

obj = function(data, params){
	# Claims paid = M (reinsurance claim made) i.e. P(X > M)
	const1 = -n2*log(pexp(M, params, lower=FALSE))
	
	# Claims paid by insurer that are < M
	const2 = -sum(log(dexp(data[data != M], params)))
	
	const1 + const2
	
	}

result = nlm(obj, data = yclaims, params0)
lambda = result$estimate # -> 0.00272175

# There is an analytical solution to this problem when X ~ Exp(lambda) ->
# Refer to notes but simply ->
# lambda = n1 / (M * n2 + sum[i = 1, n1](yi [yi < M])

lambda.exact = n1 / (M * n2 + sum(yclaims[yclaims < M]))
lambda.exact # -> 0.002721486

# Plot the fitted density against the emperical density ->
# We will expect the emperical density to be truncated at M = 1000 i.e. at the tail of the density there is a PROBABILITY MASS

# y = c(seq(0, M), M)

# plot(density(yclaims, from = 0, to = 1000), xlab = "claim size", ylab = "probability density", main = "plot of emperical and fitted density for Insurers claim payments", lty = 2, col = "blue")
# lines(y, c(dexp(y[-length(y)], lambda), pexp(M, lambda, lower=FALSE)), col = "red")
# legend("topright", legend = c("emperical", "fitted"), col = c("blue", "red"), lwd = 2)

y = c(seq(100, M))

plot(density(yclaims, from = 100, to = 1000), xlab = "claim size", ylab = "probability density", main = "plot of emperical and fitted density for Insurers claim payments", lty = 2, col = "blue")
lines(y, dexp(y, lambda), col = "red")
legend("topright", legend = c("emperical", "fitted"), col = c("blue", "red"), lwd = 2)

# The probability mass at X = M -> need to include that somehow
# The emperical density show the probability mass - blue line at X = M
# The fitted distribution doesn't accout for the mass at M -> the lines diverge right-hand tail of the distribution since the data represents claim
# amounts after reinsurance whereas the fitted distribution models the underlying claim size before reinsurance
# Overall the fitted distribution appears to be a good fit on visual inspection

# Look at Q-Q pot even though you know the result ->
comparison.qs = qexp(ppoints(length(yclaims)), lambda)
qqplot(comparison.qs, yclaims)
abline(0, 1, col = "red")


#########################################################################################
# Question 1

# X ~ Gamma(a, l) -> XoL with M = 200
# Proportion of claims when reinsurer is involed -> P(X > M)

a = 5
l = 0.04
M = 200
pgamma(M, a, l, lower=FALSE) # -> 0.0996324


#########################################################################################
# Question 2

set.seed(1)
x = sort(rpois(365, rgamma(365, 20, 0.01)))
# 365 claims over last 365 days

# On average 1 in 100 claims are rejected on suspicion of fraud
# Insurer expects this rate to increase to 4% whilst the claim amount stay the same -> expect 96% of claims to be legitmate

claims = x * (1 - 1.04 * 0.01) # becuas 1% was the constant, it changes by 4% now in next year -> not that it changes to -> CHANGES BY NOT CHANGES TO!
skew = (1 / length(claims)) * sum((claims - mean(claims))^3)
skew # -> 48615614
skew^(1/3) # -> 364.9712^3 per day 


#########################################################################################
# Question 3

set.seed(16550)

# X ~ Exp(lambda)
lambda = 0.0005

# XoL with retention M such that Z = pmax(0, X - M)
M = 2500

x = rexp(10000, lambda)
y = pmin(x, M)
mean(y) # -> 1416.969

z = function(x, M){
	
	z = numeric(length(x))
	
	for (j in 1:length(x)){
	
		if (x[j] <= M){
			z[j] = 0
			}
		else if ((M < x[j]) & (x[j] <= (M + 1000))){
			z[j] = x[j] - M
			}
		else if (x[j] > (M + 1000)){
			z[j] = 1000
			}
	}
	z	
	}

obj = function(data, params){
	zclaim = z(data, params)
	yclaim = data - zclaim
	var(yclaim)
	}

param0 = 2500

result = nlm(obj, data = x, param0)
M = result$estimate # -> 1625.883


#########################################################################################
# Question 4

z = read.table("Reinsurance payments.txt", header = FALSE)$V1

# X ~ logNormal(mu, sig)
# determine mu and sig using MLE 
# Note: truncation

M = 45000

n1 = length(z[z > 0])
n2 = length(z) - n1

mu0 = mean(log(z[z > 0]))
sig0 = sd(log(z[z > 0]))

params0 = c(mu0, sig0)

obj = function(data, params){
	
	const1 = -n2 * log(plnorm(M, params[1], params[2]))
	const2 = -sum(log(dlnorm(data[data > 0] + M, params[1], params[2]))) # IMPORTANT! -> ALWAYS WRITE OUT THESE LOG-LIKELIHOOD FUNCTIONS! -> YOU MISSED z + M
	
	const1 + const2
	
	}

result = nlm(obj, data = z, params0)
mu = result$estimate[1] # -> 10.4747560 
sig = result$estimate[2] # -> 0.2651853

x.r = seq(range(z)[1], range(z)[2] + M, 100) # We have to do this for X -> not Z

hist(z + M, freq = FALSE, breaks = c(20000, 45000, seq(46000, 87000, 1000)), xlim = c(0, 87000), ylim = c(0, 0.00005), ylab = "Density", main = "Truncated data versus logN(10.475,0.265)", col = "grey")
# Since we don’t know the exact size of some of the claims (ie all those below 45,000), we’ll stipulate the size of the lowest band in the histogram to be between, say, 20,000 and 45,000
# We’ll space the remaining bands at regular intervals up to 86,000

lines(x.r, dlnorm(x.r, mu, sig), xlab = "claim amount", ylab = "density", main = "Plot of Fitted and Emperical Density", col = "red", type = "l")

# Removed the point mass at z = 0 -> Reinsurer woult not know of these claims
# Fit looks reasonably good

z[which.max(z)] # -> 40200
# Largest claim paid by reinsurer

# P(X > 40200 + M) -> P(X > 85200) -> This is the ultimate probability of the reinsurer paying the largest claim -? EVERYTHING MUST BE BASED OFF PROBABILITY RELATED TO X
plnorm(z[which.max(z)] + M, mu, sig, lower=FALSE) # -> 0.00046499