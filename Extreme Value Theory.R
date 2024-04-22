# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\16 Extreme Value Theory")

# Extreme Value Theory
# -> Find block maxima for a set of data
# -> Fit GEV to block maximum using MLE
# -> Find threshold exceedances
# -> Fit GEV distribution to threshold exceedances using MLE

data = read.table("EVTdata.txt", header=TRUE) # -> Always check how R imports

# Block maxima
# Want to find the Block maxima for each month
# -> Data isn't in chronological order...but doesn't matter
# -> Use the aggregate function

maxima = aggregate(Claim ~ Year:Month, data, max) # -> aggregate() is a useful function
head(maxima)

# Grouping into bespoke intervals
# If we wanted to group data into 5y blocks, we first find earliest year ->

min(data[ ,2]) # -> simple function to find the earliest Year
# -> 1990

# Now sub-divide the data into required block -> 1990 to 1994; 1995 to 1999; etc
YearIndex = (data[,2] - 1990)%/%5 + 1 
# -> (Year  -1990) mod 5 + 1
# %/% is the integer quotient -> rounds down to next integer...so the code %/%5 divides by 5 and then rounds down

# Question -> Calculate the relavant block for a claim that occured in 1994?
# (1994 - 1990)/5 -> 0.8 ... so R rounds down to zero -> add one to it -> +1 -> therefore the block for the claim -> Block 1

# We can append this a new column in the data frame ->

data = cbind(data, YearIndex)
head(data)

# Now Grouping by Year
# We look at Block Maxima based on yearly data -> Use Year in aggregate() function and omit the other factors -> omit Month and YearIndex

maxima = aggregate(Claim ~ Year, data, max)
head(maxima)

# Fitting a GEV ->
# Fit an EVT to these yearly maxima by fiding the MLE parameters
# MAy seem difficult but we know the procedures

# Initialize parameters -> alpha, beta, gamma -> dont know yet what they should be but set them to something ->
alpha = 10
beta = 10
gamma = 10

p=c(alpha, beta, gamma)

# Write a function to return the PDF of the GEV when gamma <> 0

dGEV=function(x, a, b, g){
	(1/b)*(1+g*(x-a)/b)^(-(1+1/g))*exp(-((1+g*(x-a)/b)^(-1/g)))
	}
	
# Calculate the negative log Likelihood function:

f=function(data, params){
	-sum(log(dGEV(data, params[1], params[2], params[3])))
	}

f(maxima[,2],p)

MLE=nlm(f, p, data = maxima[,2])$estimate
# alpha -> 12636
# beta -> 8148
# gamma -> -0.04 ... which does not equal zero

# Using the Fitted distribution
# Use the fitte distribution to calculate probabilities - >

# For instance, calculate P(BMax of claim m > 30000) -> 1- P(M < 30000)
# We know the Distribution function for a GEV -> H(m)
# Therefore P(M > 30000) -> 1 - P(M < 30000) - > 1 - H(30000)

H=function(m, a, b, g){
	exp(-(1+g*(m-a)/b)^(-1/g))
	}

1-H(30000, MLE[1], MLE[2], MLE[3])


# Goodness of Fit
# Analyse Gof -> 
# - Hisotgram
# - Emperical density plots
# - Q-Q plots

# Noe the the block maxima data set -> know how this was determined!
# Histogram will be done on maxima[,2] -> the bock maxima numbers -> maxima is a two colums vector, year, and maximum block claim
a=MLE[1]; b=MLE[2]; g=MLE[3]
x=seq(0,50000,1000)

hist(maxima[,2], freq=FALSE, main="histogram", xlab="maxima")
curve(dGEV(x, a, b, g), add=TRUE, col="red")

# Emperical Densities
# Plot emperical density on maxima[,2] -> 
plot(density(maxima[,2]), ylim=c(0,0.000045),xlab="maxima", main="Empirical density function versus fitted distribution",col="blue")

# Superimpose the PDF of the GEV onto the density plot ->
x=seq(from = -10000,to = 50000)
lines(dGEV(x, a,b,g), col="red")

# Q-Q plots
# Analyse the Q-Q plots of teh data agains the fitted distribution ->

# Require the quantile function of the GEV ->
# We know H(m) -> H(m, a,b,g) -> Is H(m, a,b,g) invertible?


# For g <>0 meaning that gamma parameter does not equal zero
# ->
qGEV = function(p, a, b, g){
	b/g * ((-log(p))^(-g) - 1) + a
	}

rGEV = function(n, a, b, g){
	b/g * ((-log(runif(n)))^(-g) - 1) + a
	}

pGEV <- function(q, a, b, g){
	exp(-(1 + g * (q - a)/b)^(-1/g))
	}

n=length(maxima[,2])

comparison.qs=qGEV(ppoints(n), a, b, g)
qqplot(comparison.qs, maxima[,2], xlab = "Quantiles from fitted distribution", ylab = "Sample quantiles", main = "QQ plot of data and fitted GEV distribution")
abline(0, 1, col="red")

# Shows a fairly close Fit

	
# Generalized Pareto distribution
# Threshold Exceedances

# For a given threshold u, the threshold exceedance for a RV X ->  [X-u | X>u]
# Hence we calculate threshold exceedance in R -> ONLY INCLUDE CLAIMS THAT ARE GREATER THAN THE THRESHOLD
# -> X > u ... 

u = 10000
x = data[,3] # -> coloumn 3 are the claim amounts
# Now we get claims that are strictly greater than the threshold u ->
x=x[x>u] # -> so this is like conditioning first...given X>u
# We can calculate X-u ->
te= x - u # => Because x is already conditioned on the info that X > u

te

# In one line ->
x = data[,3]
te = x[x>u]-u; te

# Fitting a GPD distribution
# Question - Asked to fit GPD to threshold exceedances of teh calims, for threshold u = 10000
# Fine the MLE of the parameters of this distribution using -> beta = gamma = 10 as initial values

# PDF of a GPD -> 

dGPD=function(x, b, g){
	(1/b)*((1+x/(g*b))^(-(g+1)))
	}

params.0=c(10,10)

f=function(data, params){
		-sum(log(dGPD(data, params[1], params[2])))
	}
	
MLE=nlm(f, params.0, data=te)$estimate # -> DONT FORGET, FIT GPD ONTHRESHOLD EXCEEDANCES! -> te and not x
b = MLE[1]; g = MLE[2]
# -> b = 8802; g = 10959

# Question - Plots and Fits

hist(te, freq=FALSE, main="histogram", xlab="threshold exceedances")
x=seq(0,32000,2000)
curve(dGPD(x, b, g), add=TRUE, col="red")

plot(x, dGPD(x, b, g), xlab="te", ylab="density", main="Theoretical and Emperical PDF", col="red", type="l", lty=2, lwd=2)
lines(density(te), col="blue", lwd=2)

qGPD = function(p, g, b){
	g * b * ((1 - p)^(-1/g) - 1)
	}

rGPD = function(n, b, g){
	g * b * ((1 - runif(n))^(-1/g) - 1)
	}
	
pGPD = function(q, b, g){
	1 - (1 + q / (b*g))^(-g)
	}

n=length(te)

comparison.qs = qGPD(ppoints(n), b, g)
qqplot(comparison.qs, te, xlab = "Quantiles from fitted distribution", ylab = "Sample quantiles", main = "QQ plot of te and fitted GPD distribution")
abline(0,1,col="red", lty=2)

# The fit is ok for smaller threshold exceedances but at the te's increase beyound 1800 or so, the fit becomes poor
# Bear in mind, it looks ok given that the sample size is quite small

#########################################################################################
# Question 1

claims = read.table("table.txt", head=TRUE)

maxima = aggregate(Claim ~ Year:Month, claims, max) # -> aggregate() is a useful function
head(maxima)

required.year = maxima[maxima$Year == 2016,]
required.year[required.year$Month ==  "Jul",]$Claim
required.year[required.year$Month ==  "Aug",]$Claim
required.year[required.year$Month ==  "Sep",]$Claim

months = c("Jan","Feb","Mar","Apr","May","Jun","Jul", "Aug","Sep","Oct","Nov","Dec")
claims$month.no = match(claims$Month, months) # -> assigns the month index i.e. Jan =1, Feb =2, etc

QuarterIndex = (claims[,4] - 1)%/%3 + 1

claims = cbind(claims, QuarterIndex)
head(claims)

maxima = aggregate(Claim ~ Year:QuarterIndex, claims, max) # -> Aggregate is a nice fucntion -> Here we found the max of the aggregate of Claim according Year:Quarter index
head(maxima)

# Maximum claim that occured in the third quarter of 2016 ->
req=maxima[maxima$Year == 2016,]
req[req$QuarterIndex == 3,]$Claim


#########################################################################################
# Question 2

aggClaims = read.table("aggClaims.txt", header=TRUE)
head(aggClaims)

u = 3000000

te=aggClaims[aggClaims > u]-u


#########################################################################################
# Question 3

claim = read.table("exp.txt")$x
n=length(claim)
# X ~ Exp(lambda)

# Estiamte lambda -> MLE
# lambda -> l

l =  n / sum(claim) # -> lambda^

# By the memoryless property of the exponential distribution -> Distribtion of the TE is Exp(lambda)

#########################################################################################

# Measure of Tail Weight
# -> Limiting density ratios
# -> Hazard rates
# -> Mean Residual Life

# Limiting Density Ratios
# First create two vectors of d1 and d2, for RV X1 and X2
# -> Make sure each vectors are the same length
# -> Divide d1/d2

# The result is a ratio vector -> if there are very large at the end of the vector
# -> indicates X1 has a larger tail than X2
# If the result is very small at the end of the vector
# -> indicates X2 has a larger tail than X1

# Example
# X1 ~ Beta(2, 0.5)
# X2 ~ Beta(3,0.8)

x=seq(0.001, 0.999, 0.001) # -> remember that Beta distribution has support [0,1]

d1 = dbeta(x, 2, 0.5)
d2 = dbeta(x, 3, 0.8)

dr=d1/d2
plot(x, dr, type="l", lty=2, col="red", xlim=c(0.95,1), ylim=c(0.5,3))

tail(dr,20) # -> increasing...so the tail of X1 is heavier than tail of X2


# Hazard rates
# Remember the hazard rate of a distribution -> h(x) = f(d)/(1-F(x)) -> pdf/(1-CDF)

# In general, an increasing hazard coreespond to a lighter tail and a decreasing hazard to a heavy tail
# HOWEVER, when comparing tail weight of two specific distributions -> consider also how the hazard rate decrease or increase! VERY IMPORTANT
# R offer the hazard rates so no need to code them up...but you can for non R distributions -> if you use R functions, access to lower.tail=FALSE
# i.e. Pareto, Weibull (if you want o choose parametrization), etc -> if you code these up, can't use lower.tail=FALSE

# Remember also the existence of moments -> dtermine whether a light or heavy tail distribution

# Question 
# X ~ lognormal(mu=4, sig=sqrt(4))

mu = 10
sig = sqrt(4)

x=seq(0,1000,10)
h.x=dlnorm(x, mu, sig)/plnorm(x, mu, sig, lower.tail=FALSE)

plot(x,h.x, type="l", lty=2, lwd=2, col="blue")
# From the plot, we can see its relatively hard to survive until about x=400 -> increasing hazard
# Thereafter, the hazard rate decreases corresponding to a heavy tail -> sirvival becomes easier after x=400

# Example
# X ~ Burr(a, l, g)
# Calculate the survival rate for x = 3

dburr=function(x, a, l, g){
	a * g * l^(a)*x^(g - 1)/((l + x^g)^(a+1))
	}

pburr=function(x, a, l, g){
	1 - (l/(l + l^g))^a
	}
	
a = 0.5
l = 10
g = 2
x=3

h = dburr(x, a, l, g)/(1-pburr(x, a, l, g))


# Mean Residual Life
# The method ->
# e(x) = integral(x, Inf){1-F(y)dy} / (1-F(x)) -> "sum" the survival from x -> Inf and divide by survival to x
# -> Feels like a conditonal problem -> What is survival after age x given that you survived to age x

# R can split this integral numerically

# Think of decreasing MRL -> increasing hazard -> lighter Tail
# -> Increasing MRL -> decreasing hazard -> heavier Tail
# HOWEVER when comparing the tail weight of MRL, it is imporant to conider how the MRL increase of decreases! VERY IMPORTANT

# Example
# Calculate RML for X ~ Gamma(50, 1.5) @ x=40

x = 40
a = 50
l = 1.5

# for 1 - F(y) -> Survival function at y

S=function(y){
	1 - pgamma(y, shape = a, rate = l)
	}

e.40 = integrate(S, 40, Inf)$value / S(40)

# Unhappy about using integrate function -> so we can't integrate ( integrate( ...))) or I dont know

# Calculate MRL @ x = 10 for X ~ Weibull(c=0.001, g=2)

x=10
c = 0.001
g = 2

shape = g
scale = c^(-1/g)

S=function(y){
	pweibull(y, shape, scale, lower.tail=FALSE)
	}

e.10 = integrate(S, 10, Inf)$value/S(10)

#########################################################################################
# Question 4

# Hazard Rate for Weibull(0.4, 5)

c=0.4
g=5

shape = g
scale = c^(-1/g)

h=function(x, shape, scale){
	dweibull(x, shape, scale)/(1-pweibull(x, shape, scale))
	}

x=seq(0,10,0.1)
plot(x, h(x, shape, scale), type="l", lty=2, lwd=2, col="red")
# Weibull has an increasing hazard rate function -> lower survival probability -> light Tail


#########################################################################################
# Question 5

# Plot density ratios of X ~ Pareto(2,4) and Y ~ Pareto(7,12)

x=seq(0,100,1)

dpareto=function(x, a, l){
	a*l^(a)/((l + x)^(a+1))
	}

dx=dpareto(x, 2, 4); dy=dpareto(x,7, 12)

ratio = dx/dy

tail(ratio,10)
# The Pareto(2,4) has a heavier Pareto(7,12)

#########################################################################################

# GEV and GPD Distributions 
# -> Find block maxima for a set of data
# -> Fit a GEV to the block maxima using MLE
# -> Find threshold exceednaces fora a set of data
# -> fit a GPD to threshold exceedances using MLE

data = read.table("EVTdata.txt", header = TRUE)
head(data)

# To get the block maxima for each month -> WE need data in chronological order...aggreagte function will do taht automatically ->
maxima = aggregate(Claim ~ Year:Month, data = data, FUN = max)

# Grouping data into bespoke interavls -> want to group data into 5y-blocks
# -> we doule first earliest year:

min(data[, 2])
# -> 1990

# subdivide the data into required 5y blocks starting from the minimum ->
YearIndex = (data[, 2] - 1990)%/% 5 + 1 # -> %/% is the integer quotient ... like a mod -> so the function divides by 5 and then downs down ->

# Calculate a block for a claim that occured in 1994 ->
# (1994 - 1990) / 5 -> 0.8
# Rounding this down -> 0.8 -> 0
# -> adding +1 -> 1 and therefore the block for a claim that occured in 1994 is block 1

data = cbind(data, YearIndex)
head(data)
# We've just assigned the year to a block where teh blocks are 5y periods

# Now we're in a position to get the block maxima ->
maxima = aggregate(Claim ~ YearIndex, data = data, FUN = max)

# Grouping by year -> we will use the block maximum based on yearly data ->
maxima = aggregate(Claim ~ Year, data = data, FUN = max)
# What that means -> find the max claim for each year -> each year may have different number of claims -> take the max of them for each Year

# Fitting a GEV ->
# We can fit a GEV to there yearly maxima -> find the maximum MLE of the parameters

# First setup a vector of parameters -> doesn't matter wha the values are this stage
alpha = 10
beta = 10
gamma = 10

# parameters can be stored in p
p = c(alpha, beta, gamma)

# Recall the density and cumulative functions of a GEV -> we have to write custom functions to be able to use them

dGEV = function(x, a, b, g){
	1/b * (1 + g*(x - a)/b)^-(1 + 1/g) *
		exp(-((1 + g*(x - a)/b)^(-1/g)))
	}
	
# x is a maxima
 
fMLE = function(data, params){
	f = dGEV(data, params[1], params[2], params[3])
	lnf = log(f)
	sum(-lnf)
	}

# Here we have calculated the pdf for every block maximum -> called it 'f'
# -> then logged it and taken the negative sum -> now we have the objective function for the nlm

# Remember maxima -> the second column of the maxima table are the maximum claims!
# -> to evalue the fMLE using maxima claims ->

fMLE(maxima[, 2], p) # -> 381.0497

# Calculate the MLE for alpha, beta, gamma -> Remember so far we don't know the underlying distribution of the claim amounts
MLE = nlm(fMLE, data = maxima[, 2], p)
MLE$estimate # -> 12636.1034909  8147.7038865    -0.0407898

alpha = MLE$estimate[1] # -> 12636.1034909
beta = MLE$estimate[2] # -> 8147.7038865
gamma = MLE$estimate[3] # -> -0.0407898

# Comments on the ML estimate -> gamma < 0 or rather close to zero ... could be Gumbel GEV if the underlying claim distribution has an Exponential distribution

# Using the fitted distribution ->
# Use the fitted sitrubtuion to calculate probabilities just as we would for any distribution

# Example -> estimate the probability 'p' that the maximum claim 'm' is any given year is > 30000 
# -> P(maxima > 30000) -> 1 - P(maxima <= 30000)
# -> 1 - H(m) -> Remember H(m) = P(maxima <=m ) like any CDF

# Remember that H(x) is the GEV CDF -> H(x) = P(maxima <= x)

m = 30000
p = 1-exp(-(1+gamma*(m-alpha)/beta)^(-1/gamma)) # -> 0.1019961

# P(maxima > 30000) = 10.2%

# Goodness of Fit ->
# Make use of histograms, emperical density plots and Q-Q Plot
# Use the methods in context of GEVs

x = seq(0, 45000)

hist(maxima[,2], freq = FALSE, main="Histogram, Fitted GEV density, and Emperical Density", xlab="maxima")
lines(x, dGEV(x, alpha, beta, gamma), col = "red")

# Include the emperical density ->
hist(maxima[,2], freq = FALSE, main="Histogram, Fitted GEV density, and Emperical Density", xlab="maxima")
lines(x, dGEV(x, alpha, beta, gamma), col = "red")
lines(density(maxima[,2], from = 0, to = 45000), col = "blue", lty = 2)
legend("topright", legend = c("Fitted GEV", "Emperical"), col = c("red", "blue"), lwd = 2)

# Q-Q Plot
# We need a quantile function for a GEV ->

qGEV = function(p, a, b, g) {
	b/g * ((-log(p))^(-g) - 1) + a
	}
	
rGEV = function(n, a, b, g) {
	b/g * ((-log(runif(n)))^(-g) - 1) + a
	}

pGEV = function(q, a, b, g) {
	exp(-(1 + g * (q - a)/b)^(-1/g))
	}

comparisons.qs = qGEV(ppoints(nrow(maxima)), alpha, beta, gamma)
qqplot(comparisons.qs, maxima[,2], xlab = "Quantiles from Fitted Distribution", ylab = "Yearly Maxima", main = "Q-Q plot")
abline(0, 1, col = "red")
# shows a fairly close fit

# Generalised Pareto Distribution - >
# Gven threshold 'u' -> threshold exceedance for a RV is X - u | X > u

u = 10000
# We will use data -> from previous Year:Month -- Claim
x = data[, 3] # -> get the info

x = x[x > u] # -> given condition

te = x - u # -> find the threshold exceedances
te

# Fit a GPD to threshold exceedance for claims above u = 10000
# That is use MLE to find beta and gamma parameters

# Know that GPD CDF has form: G(x) = (1 / beta) * (1 + x / (gamma * beta))^(-(gamma + 1)) for gamma != 0

beta0 = 10
gamma0 = 10

params0 = c(beta0, gamma0) # given starting values

# Require the pdf: g(x) function -> R doesn't have it so we need to write one
# NOTE: the input into the GPD is the Threshold Exceedance! Don't confuse with x -> values x - u | x > u are for the new support for the GPD

dGPD = function(te, b, g){
	1/b * ((1 + te/(b*g))^(-g - 1))
	}

fMLE = function(data, params) {
	f = dGPD(data, params[1], params[2])
	lnf = log(f)
	sum(-lnf)
	}

results = nlm(fMLE, data = te, params0)
beta = results$estimate[1] # -> 8801.553
gamma = results$estimate[2] # -> 10959.4

# Plot the fitted pdf against a histogram of the threshold exceedances -> also include emperical density and Q-Q plot
# Need G(te) -> i.e. the CDF for the threshold exceedances
# Also need the quantile function of the GPD -> all functions below for use and reference

dGPD = function(te, b, g){
	1/b * ((1 + te/(b*g))^(-g - 1))
	}
	
# We can use this form of the GPD because gamma != 0 REMEMBER THAT
pGPD = function(te, b, g){
	(1 / b) * (1 + x / (g * b))^(-(g + 1))
	}

qGPD <- function(p, g, b){
g * b * ((1 - p)^(-1/g) - 1)
}

rGPD = function(n, b, g) {
	g * b * ((1 - runif(n))^(-1/g) - 1)
	}
	

r = seq(min(te), max(te)) # -> use this to get range consistent
comparison.qs = qGPD(ppoints(length(te)), beta, gamma)

par(mfrow = c(2, 1))

hist(te, fre = FALSE, xlab="threshold exceedances", main="Histogram versus fitted GPD distribution")
lines(r, dGPD(r, beta, gamma), col = "red")
lines(density(te, from = 0, to = 35000), col = "blue", lty = 2)
legend("topright", legend = c("fitted density", "emperical density"), col = c("red", "blue"), lwd = 2)

qqplot(comparison.qs, te, xlab = "quantile from fitted distribution", ylab = "quantile from sample te", main = "Q-Q plot of sample versus fitted")
abline(0, 1, col = "dark green")

par(mfrow = c(1, 1))

# Quantiles appear broadly similar across the range even though there is variation around the comparison line
# Even if the fitted dsitrubtion is the underlying distribution of the te -> the sample size is small so we will expect to see variation like this
# Note: we've not given function for the GPD when gamma = 0 -> you must be able to work that out quickly if the questions asks for it

#########################################################################################
# Question 1

claims = read.table("table.txt", header = TRUE)

# We will aggreagate according to Year:month
maxima = aggregate(Claim ~ Year:Month, data = claims, FUN = max)

maxima[maxima$Year == 2016 & maxima$Month == "Jul", ][3] # -> 31075
maxima[maxima$Year == 2016 & maxima$Month == "Aug", ][3] # -> 24275
maxima[maxima$Year == 2016 & maxima$Month == "Sep", ][3] # -> 25365

months = c("Jan","Feb","Mar","Apr","May","Jun","Jul", "Aug","Sep","Oct","Nov","Dec")
claims$month.no = match(claims$Month, months)

claims$QuarterIndex = (claims$month.no - 1)%/%3 + 1
maxima.q = aggregate(Claim ~ Year:QuarterIndex, data = claims, FUN = max)
maxima.q[maxima.q$Year == 2016 & maxima.q$QuarterIndex == 3, ][3] # -> 31075


#########################################################################################
# Question 2

aggClaims = read.table("aggClaims.txt")[-1]

u = 3000000
te = aggClaims[aggClaims > u] - u


#########################################################################################
# Question 3

# You are such an idiot -> Expontial distribution is memoryless -> hence whataver threshold you choose -> the resulting distribution is still exponential
claims = read.table("exp.txt", header = TRUE)$x
l_hat = 1 / mean(claims) # -> GPD follows Exponential Distribtion -> te ~ Exp(l_hat = 0.001490312)


#########################################################################################

# Extreme Value Theory - Measures of Tail Weight
# -> Limiting Density ratios
# -> Hazard rates
# -> Mean Residual Life

# Limiting density ratio -> ratio of two probability Densities
# Look at form of the ratio:
# -> Power decay or exponential decay?
# -> exponential decay is stronger than power -> depends on how x tends to the Extreme

# Example 
# X1 ~ beta(2, 0.5)
# X2 ~ beta(3, 0.8)
# Which distribution has a heavier tail?

x = seq(0.001, 0.999, by = 0.001)

d1 = dbeta(x, 2, 0.5)
d2 = dbeta(x, 3, 0.8)

r = d1 / d2

tail(r) # -> see its increasing -> d1 /d2 is increasing meaning d1 is increasing more than d2 -> X1 ~ beta(2, 0.5) has a heavier Tail
plot(x, r)

# Hazard Rates
# h(x) = f(x) / (1 - F(x))
# Increasing hazard as lighter wailt and decreasing hazard as heavier tail
# BE CAREFUL -> When comparing the tail weight of two specific distributions, important to consider how the hazard rate increases of decreases

# Example 1
mu = 10
sig = 2

x = seq(1, 1000)

H  = dlnorm(x,meanlog=10,sdlog=2)/(1-plnorm(x,meanlog=10,sdlog=2)) # -> hazard rate for log normal distribution using definition h(x) = f(x) / (1 - F(x))
plot(x,H,type="l",main="Hazard rate for LogN(10,4)")

# -> can see the hazard rate does decrease at the extreme -> so it has a relatively heavier tail

# Example 2

dburr = function(x, a, lambda, g){
	a * g * lambda^(a)*x^(g - 1)/((lambda + x^g)^(a+1))
	}
	
pburr = function(q, a, lambda, g){
	1 - (lambda/(lambda + q^g))^a
	}
	
# Calcualte the hazard rate for the Burr distribution using the definition -> h(x) = f(x) / (1 - F(x))

x = 3
alpha = 0.5 
lambda = 10
gamma = 2

h = dburr(x,alpha,lambda,gamma)/ (1-pburr(x,alpha,lambda,gamma)) # -> h(3) = 0.1578947

# Mean Residual Life
# Method: 
# -> e(x) = integral[x, Inf][1 - F(y)]dy / (1- F(x))
# -> analogous to the future expected lifetime of age x

# Decreasing MRL -> lighter tail
# Increasing MRL -> heavier tail
# BE CAREFUL -> When comparing the tail weight of two specific distributions, important to consider how the MRL increases or decreases

# Example 1
# X ~ Gamma(50, 1.5) for x = 40

x = 40
alpha = 50
lambda = 1.5

# Rather than use 1 - F(x) -> write a function for S(x) -> this is important for R to numerically integrate

# Gamma distribution doesn't have a CDF function...hmmm -> have to be careful here because we will be integrating and integral -> have to apply sapply() somewhere
S = function(x){
	1 - integrate(dgamma, 0, x, shape = alpha, rate = lambda)$value
	}

# Or we can use the pgamma() built-in R function ->
S = function(x){
	pgamma(x, shape = alpha, rate = lambda, lower=FALSE)
	}

# evaluate e(40) -> do it in two parts -> numerator and denominator

# Numerator -> integral[x, Inf](S(y) dy)
integrate(S, x, Inf)$value # -> 0.2129954

# Denominator -> S(x)
S(40) # -> 0.08440668

e40 = integrate(S, x, Inf)$value / S(x) # evalauted at x = 40
e40 # -> 2.523442 -> MRL for age x = 40 -> 2.523442

# Example 2
# Caculate the MRL at x = 10 for X ~ Weibull(0.001, 2)

x = 10
c = 0.001
g = 2

shape = g
scale = c^(-1/g)

# need to evaluate e(10) -> BE VERY CAREFUL! For the integrate over a distribution -> use lower.tail
e10 = integrate(pweibull, 10, Inf, shape = shape, scale = scale, lower.tail=FALSE)$value / pweibull(10, shape, scale, lower=FALSE)
e10 # -> 20.27825

#########################################################################################
# Question 4

# X ~ Weibull(c, g)

c = 0.4
g = 5

shape = g
scale = c^(-1/g)

x = seq(0, 10)
h = dweibull(x, shape, scale) / pweibull(x, shape, scale, lower=FALSE)

plot(x, h, xlab = "x", ylab = "hazard rate value", main = "Plot of hazard rate for X ~ Weibull(c, g)", type = "l", col = "blue")
# Hazard rate increase with x -> Weibull has a light tail in the extremes
# if g < 1 -> the opposite occurs -> hazard decreasing and WEibull has a heavier tail -> depends on g or shape parameter


#########################################################################################
# Question 5

# Calculate density ratio ->
# X1 ~ Pareto(2, 4)
# X2 ~ Pareto(7, 12)

x = seq(0, 1000)

dpareto = function(x, a, lambda){
	a * lambda^(a)/((lambda + x)^(a+1))
	}

d1 = dpareto(x, 2, 4)
d2 = dpareto(x, 7, 12)

r = d1 / d2

plot(x, r, xlab = "x", ylab = "ratio of densities f(x1) / f(x2)", main = "Plot of density ratio for X1 and X2", col = "blue", type = "l")
# the density ratio is increasing with x more for X1 than for X2
tail(r) # -> can see it also in the tail(r)
# as x tends to extreme, there are larger desnity values of X1 over X2 ->Pareto(2, 4) has a heavier tail than Pareto(7, 12)