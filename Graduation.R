# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\10 & 11 Graduation")

# Calculating crude mortality rates
# Calculating graudduated mortality rates
# Plots of crude, graudated rates

# Graduation Tests
# Calc third differences -> test for smoothness
# Calc standardized deviations -> Zx
# Tests:
# -> Chi Sq GoF
# -> ISD test ~ normality, and overall GoF..differnt to sum{(O-E)^2/E}
# -> Signs test
# -> Cumulative deviations test
# -> Grouping of signs test
# -> Serial correlations test

Grad=read.csv("Graduation.csv")

# Age -> age x nearest birthday -> (x-0.5,x+0.5)
# ETR -> Central Exposed to Risk for age x nearest birthday
# Deaths -> number of deaths recorded for age x nearest birthday

Grad$CRUDE=Grad$DEATHS/Grad$ETR
head(Grad)

# Calculating Graduated rates
# Using Gompertz law -> cmu(x)=B*(C^x) -> cmu(x) read mu(circle: graduated)
# -> take logs -> log(cmu(x))=log(B) + x*log(C)
# We do this to fit a linear model at the log level -> IMPORTANT

# Need to fit the parameters -> remember you are graduating crude rates -> requie smoothness, etc
# Parameters required are B,C
# Use linear regression here on log(mu^crude{x}) -> lm( log(mu^crude(x)) ~ Age, data=Grad)

gomp.fit = lm(log(Grad$CRUDE) ~ AGE, data=Grad)
summary(gomp.fit)

gomp.fit$coefficients

params=exp(gomp.fit$coefficients)

B=params[1]
C=params[2]

Grad$GRAD=B*(C^Grad$AGE)

#plot(Grad$AGE, Grad$CRUDE, col="blue", type="l")
#lines(Grad$AGE, Grad$GRAD, col="red", type="l", lty=2)

plot(Grad$AGE, Grad$CRUDE, main = "Crude and graduated rates against age", xlab = "age", ylab = "mortality rate")
lines(Grad$AGE, Grad$GRAD, col = "blue")

legend("topleft", legend = c("Crude rates", "Graduated rates"),
col = c("black", "blue"), lty = c(NA, 1), pch = c(1, NA))

# There will be slightly omre variation around the line for th eolder ages, from age 45 onwards
# The graduation appears to UNDERESTIMATE mortality at OLDER ages -> Find the graduated rates lie below the crude rates

# Smoothness test
# Need to peform third differences -> D^3{mu^circle(x)}

# Use the diff() function -> diff(z, lag =, difference = )
# lag -> lag of each difference -> default is = 1
# differences -> number of times the vestor is differenced

# For the smoothness test, D^3{mu^circle(x)}, we want a lag=1, differences=3
# The results we expect to see, if the graduated rates are smooth, is small, stable differences, and progress fairly regularly

third.diff = diff(Grad$GRAD,lag=1, differences=3)
 # The shape of third.diff is 3 elements less than nrow(Grad) -> because of the differencing
 # To add third differences to Grad ->
 
third.diff=as.numeric(c(third.diff, "","",""))
Grad$THIRD_DIFF = third.diff
tail(Grad)

# As we've fitted an exponential relationship btw age and mortality using Gompertz Law,
# we expect to see a mooth increase in the third differences across the entire age range...as is observed here

# Standardized Deviations

# Zx = (observed deaths - expected deaths)/sqrt(expected deaths)
# Observed deaths -> dx -> Grad$DEATHS
# Expected deaths -> ETR * Graduated mortlity rates

Grad$EXPECTED = Grad$ETR*Grad$GRAD

Grad$ZX=(Grad$DEATHS-Grad$EXPECTED)/sqrt(Grad$EXPECTED)


# ChiSq GoF test
# Sum the squared std deviations ~ Chisq (DoF=nrow(Grad)-2) distribution
# Note here...the expected number of deaths should be greater than 5 for each group (here there are m groups, m=nrow(Grad))
# We require that the expected deaths to be > 5 for the ChiSq GoF test to be reliable
# Taking the less conservative approach of only combining categories if any
# are less than 1 or more than 20% of values are less than 5, we can continue with the values as they are -> VERY IMPORTANT

# The 2 DoF that we subtract is for the two parameters B,C that we fitted

DoF=nrow(Grad)-length(params)

obs.test.stat = sum(Grad$ZX^2)
obs.test.stat

qchisq(0.95, DoF)
pchisq(obs.test.stat, DoF, lower.tail=FALSE) # -> Reject the null hypothesis, evidence to conclude that the graduated rates are not the true underlying mortality rates

# Individual Standardized Deviations ChiSq test
# Under the null, the graduated rates are the true underlying rate of mortality -> the standardized deviations
# shoud approx form a sample from the std normal distribution ~ N(0,1)

# We can investigate by dividing the real number line into intervals and comapre the observed number of standardized deviations with 
# the expected number in each interval

# -> (-Inf,-3) (-3,-2) (-2,-1) (-1,0) (0,1) (1,2) (2,3) (3,Inf)

m=nrow(Grad)

intervals = c(-Inf,-3,-2,-1,0,1,2,3,Inf)
CDF.probs = pnorm(intervals) # default is N(0,1) for pnorm
expected.ZX = m*diff(CDF.probs)

expected.ZX
# -> 0.0688448  1.0914119  6.9311612 17.4085820 17.4085820  6.9311612  1.0914119  0.0688448

# Ideally, we want the expected values to bea at least 5 (as rule of thumb) to ensure that the ChiSq approximation is good.
# So we need to create intervals that meet this criteria
# Looking at the expected.ZX values, we could combine (-Inf -> -1) (-1,0) (0,1) (1 -> Inf) ... this will add up the expected values in the sub-intervals

intervals=c(-Inf,-1,0,1,Inf)
CDF.probs = pnorm(intervals) # default is N(0,1) for pnorm
expected.ZX = m*diff(CDF.probs)

expected.ZX
# -> 8.091418 17.408582 17.408582  8.091418

# Just to make sure we keep track, the DoF used for the ChiSq test on the ISD GoF will be 3 -> we made 4 groups on the new inteval because of expected number >5 criterion less 1)
# Therefore 3 DoF = 4 groups - 1

# Now we need to calculate the Observed number of standardized deviations in each interval
# Do this using the hist() function
# We can skip the plot in the function by setting plot=FALSE
# We just want to extract the frequencies

observed.ZX=hist(Grad$ZX, breaks=intervals, plot=FALSE)$counts # -> $counts will extract the frequencies for the intervals -> intervals=c(-Inf,-1,0,1,Inf)
observed.Zx
# -> 9  4 21 17

# There other inspection tests we can do to compare the observed to expected numbers -> remember the ISDs are meant to follow N(0,1) under the null
# Check to see how many ISD sit in the +- 1 SD, proportion wise
# Symmetry of the ISDs...

# Now we can peform the ChiSq test
obs.test.stat.ZX=sum(((observed.ZX-expected.ZX)^2)/expected.ZX)
obs.test.stat.ZX

DoF.ZX= (length(intervals)-1)-1
qchisq(0.95, DoF.ZX)
pchisq(obs.test.stat.ZX, DoF.ZX, lower.tail=FALSE) # -> Reject the null hypothesis, evidence to conclude that the graduated rates are not the true underlying mortality rates

# There are additional ways of reviewing the individual standardized deviations
# We can check:
# -> Whether there are outliers
# -> Symmetry around 0
# -> The number of deviations outside the interval (-2/3,2/3)
# -> The number of positive and negative deivations
# -> For groupings of deviations of the same Signs  

# To consider outliers and symmetry of the ISDs, plot a histogram
hist(Grad$ZX, main = "Histogram of individual standardised deviations", xlab = "zx") # -> won't resemble a normal distribution...suggesting this is not a good Graduation

# Outliers
# We know that for a sample from the standard normal distribution we expect around 1% to have
# an absolute value larger than about 2.5 (more accurately 2.56). We also expect around 5% to be
# larger than about 2 (more accurately 1.96) in magnitude -> Remember both side of the distribution -> 5% -> 2.5% -> z = 1.96

# We have to be concerned about ISDs greater than 2.5 in magnitude -> tail value...expect the frequcny of such to low according to N(0,1)

length(Grad$ZX[abs(Grad$ZX)>2.5]) # -> will give us the number of ISDs that have value greater than 2.5
# result -> 4

# According to N(0,1), this should be 1%*m -> m=51 * 0.01 -> 0.51 ISDs...but here there are 4

length(Grad$ZX[abs(Grad$ZX)>1.96]) # -> will give us the number of ISDs that have value greater than 1.96
# result -> 10

# According to N(0,1), this should be 5%*m -> m=51 * 0.05 -> 2.55 ISDs...but here there are 10

# So clearly there is aproblem with the adherence of the data to the Graduation

# Symmetry
# Look for the number and maginuted of positve and negative ISDs
# Under N(0,1), there should be roughly the same number of positive and negative deviations of similar magnitude (obviosuly negative and positive, but focus on magnitude)
# In this case, there is no Symmetry. Just look at the histogram

# The Interval (-2/3,2/3)
# Roughly 50% of the probability of the ISDs lie in the interval (-2/3,2/3)
# -> {pnorm(2/3)-pnorm(-2/3)
# Use the Binomial test -> See notes

# Count the number of ISDs outside interval (-2/3,2/3)
length(Grad$ZX[abs(Grad$ZX) > 2/3])
# result -> 33
# now 33/51 = 0.6471...64.71% of the ISDs lie outside (-2/3,2/3) -> thats more than 50%...so again not consistent with N(0,1)
# See notes on the Binomial test you can do as well...here we just taking a crude look, but statistically...there could be evidence to not reject the null



# Signs test
# Under the null, the graduated rates are the true underlying rates of mortality. That is the ISDs sampled from the graduated data
# will follow a standard normal N(0,1) distribution
# So the probability that any one ISD is positive is 0.5 -> pi=0.5

# The number of Positives, P, is a RV representing the positive deviations. Under the null -> #P ~ Bin(m=nrow(Grad), pi=0.5)
# E(P)= m*0.5 -> Compare this to the number of P obervsed. If P>E(P)...then calc the probality P(#P>P observed). 
# Do the opposite if P observed < E(P) -> calc P(#P < P observed)
# You are calculating p-values...upon which you will know to reject/fail to rejct the null
# m -> number of age groups

# VERY IMPORTANT...This Binomial test is two-sided so when calculating the p-value, you need to muliply it by 2

# Calculating the observed number of postive groups -> 
P=length(Grad$ZX[Grad$ZX>0]) # -> 38
# so 38 out of 51 ISDs are positive...compare to the interval (-2/3,2/3) analysis

P.expected = 0.5*nrow(Grad)
P.expected # -> 25.5 -> 25.5 < 38 so calc the p-value: P(#P > P observed)

2*pbinom(P-1, nrow(Grad), 0.5, lower.tail=FALSE) # -> IMPORTANT! BINOMIAL IS A DISCRETE DISTRIBUTION -> use 37 [38 - 1] when using lower.tail=FALSE 
# compare this to alpha = 5%
# -> 0.0006210435 < 0.05 -> reject the null that the ISDs ~ N(0,1) -> the graduated rates are not true underlying rates of mortality

# Alternatively, use the Binomial test in R
binom.test(P, nrow(Grad), p=0.5)



# Cumulative Deviations test
# This is a two-sided test to compare total observed deaths to total expected deaths
# either over the entire agre range or subsets of the age range

# The overserved value of the test statistic
# -> [sum{dx} - sum{mu^cirle(x)*cEx}] / sqrt[sum{mu^cirle(x)*cEx}]

# -> (total observed deaths - total expected deaths)/ sqrt(total expected deaths)
# Under the null, the graduated rates are the tru underlying rates of mortality
# The Cumulative Deviations (under assumption of IID) and CLT follow ~ N(0,1)

# observed test statistic ->
obs.test.stat.CD = (sum(Grad$DEATHS)-sum(Grad$EXPECTED))/sqrt(sum(Grad$EXPECTED))
# -> 2.859958

qnorm(0.975)
# -> 1.959964
 
2*pnorm(obs.test.stat.CD, lower.tail=FALSE) # -> don't forget to multiply 2 VERY IMPORTANT BECAUSE ITS TWO-SIDED i.e. TWO-TAIL TEST
# -> 0.004236965

# Based on the results, reject the null at 5% significance level -> evidence that graduated rates are not true underlying rates of mortality


# Grouping of Signs test
# Test looks at the grouping, clumping or runs of deviations of the same sign
# Under the null, the ISDs form a sample of independent values from N(0,1)
# This test compares the obseved clumping of sign with what would be expected id the positive and negative
# deviations were arranged in random order

# This test is usually to test for OVERGRADUATION -> If there is overgraduation, the ISDs won't swap from positive to negative very often
# There will be few groups of deviations of the same sign
# This is a one-sided test for whether there are too few groups

# We conider the number of groups of positive deviations of the ISDs ... choosing the groups of negative deviations can lead to different conclusions

# -> G is a RV representing the number of groups of positive deviations
# -> n1 the number of positive deviations
# -> n2 the number of negative deviations

n1=length(Grad$ZX[Grad$ZX > 0])
n2 = m-n1

# To calculate the observed value of G, construct a vector containing the sign of each ZX i.e. sign of each standardized deviations
signs=sign(Grad$ZX)

table(signs) # -> to determine n1, n2 if you get stuck with the formula n1=length(Grad$ZX[Grad$ZX > 0])

# Now...we need to determine the number of group of 1's there are in the signs vector
# Use run-length encoding -> sum(rle(vector)$values ==1 )

result=rle(signs)
G=sum(result$values == 1) # -> G=9 groups of positive deviations

# Refer to notes on the distribution for G -> P(G=t) -> ...


# P(G<=9) -> this calculate a p-value...FROM THE NOTES
prob=0

for (t in 1:G){
	prob = prob + choose(n1 - 1, t - 1) * choose(n2 + 1, t)/ choose(n1 + n2, n1)
	}

prob # -> 0.2470046

# Remember this is one-side -> 0.2470046 > 0.05 so we fail to reject the null
# No significant evidence of clumping of deviations of the same sign
# Reasonable to conclude that the graduated rates may not differ from the true rates
# There are alternatives -> refer to the notes

# Serial Correlations test
# The null is as before, graduated rates are the true underlying rates
# As with grouping of signs test, the serial correlations test looks for clumping of the standardized deviations

# Refer to the formula for the correlation function at differnt lags

# We will test at lag 1 but the test can be performed at higher lags
# Remember that r*sqrt(m) ~ N(0,1)
# We will calc r1 -> the serial autocorrelation at lag 1

# Under the null, the value r1*sqrt(m) is a sample from the N(0,1) distribution

# Use the acf() function -> estimate the serial correlation at lag 1 -> This will work if m is sufficently large
# -> If m is small...then use the cor() function
# -> For now, we are using the acf() so rj = [sum{i=1, m-j}((zi-zbar)(z(i+j)-zbar))]/[si,{i=1,m}((zi-zbar)^2)] for lag j

corr.lag1 = acf(Grad$ZX, lag.max=1, plot=FALSE)
str(corr.lag1)

r1=corr.lag1$acf[2]

obs.test.stat.SA.lag1=r1*sqrt(m)
obs.test.stat.SA.lag1
# -> under null, r1*sqrt(m) ~ N(0,1)

qnorm(0.975)

pnorm(obs.test.stat.SA.lag1, lower.tail=FALSE) # -> 0.1247366
# -> fail to reject null

# There are other alternatives -> review the notes for details on alternatives. Use the acf() rather for quicker result

#########################################################################################
# Question 1

GradQ1=read.csv("Graduation_Q1.csv")
head(GradQ1)

GradQ1$EXPECTED = GradQ1$GRAD*GradQ1$ETR
head(GradQ1)

GradQ1$CRUDE = GradQ1$DEATHS/GradQ1$ETR

plot(GradQ1$AGE, GradQ1$CRUDE, main = "Crude and graduated rates against age", xlab = "age", ylab = "mortality rate")
lines(GradQ1$AGE, GradQ1$GRAD, col = "blue")

legend("topleft", legend = c("Crude rates", "Graduated rates"),
col = c("black", "blue"), lty = c(NA, 1), pch = c(1, NA))

third_diff=diff(GradQ1$GRAD, differences=3)
# third differences appear small in magnitude and progress fairly regularly
# Suggests that the graduation is smooth

# Given that paramatric formula was used to graduate -> 3 parameters estimated
GradQ1$ZX=(GradQ1$DEATHS-GradQ1$EXPECTED)/sqrt(GradQ1$EXPECTED)

# ChiSq GoF -> DoF = nrow(GradQ1)-3
ts.ZXsq=sum(GradQ1$ZX^2)
ts.ZXsq

DoF=nrow(GradQ1)-3
qchisq(0.95, DoF)
pchisq(ts.ZXsq, DoF, lower.tail=FALSE) # -> reject null

# Signs test
pi=0.5
m=nrow(GradQ1)

P=length(GradQ1$ZX[GradQ1$ZX > 0])
Ex.P=m*pi

# multiply p-value by 2 -> two-tail test
p.value = pbinom(P, m, p=0.5, lower.tail=FALSE)+dbinom(P,m,p=0.5)*0.5
2*p.value # -> fail to reject null

# ISD test...considering interval (-2/3,2/3) with p-value
intervals=c(-Inf,-3,-2,-1,-0,1,2,3,Inf)
w=length(intervals)-1
expected.ZX=diff(pnorm(intervals))*m
observed.ZX= hist(GradQ1$ZX, breaks=intervals, plot=FALSE)$counts
# Inspect...then decide what to do next

# use two groups for the expected deaths criteria > 5
intervals=c(-Inf,0,Inf)
w=length(intervals)-1
expected.ZX=diff(pnorm(intervals))*m
observed.ZX= hist(GradQ1$ZX, breaks=intervals, plot=FALSE)$counts

# This test will result in failure to reject null but trivially -> p-value=1
# Not enough data 

# Considering the interval (-2/3,2/3)
# Should expect 50% of the probability to lie in this interval if null ~ N(0,1)
length(GradQ1$ZX[abs(GradQ1$ZX) > 2/3])
# result -> 5 which is 5/10 -> 50% so again, we fail to reject the null assumption
# result again not conclusive...data set is too small

# Cumulative Deviations test
# The [sum(dx)-sum(expected)]/sqrt[sum(expected)] ~ N(0,1) under the null

ts.CD=(sum(GradQ1$DEATHS)-sum(GradQ1$EXPECTED))/sqrt(sum(GradQ1$EXPECTED))
2*pnorm(ts.CD, lower.tail=FALSE) # -> two-tail test
# p-value -> 0.02193619 so we reject the null at 5% level


# Serial correlations test at lag 1 and lag 2
# Under null, r1*sqrt(m) ~ N(0,1) and r2sqrt(m) ~ N(0,1)

corr.lag1=acf(GradQ1$ZX, lag.max=1, plot=FALSE)
r1=corr.lag1$acf[2]
pnorm(r1*sqrt(m), lower.tail=FALSE)
# -> 0.1944891 therefore fail to reject null

corr.lag2=acf(GradQ1$ZX, lag.max=2, plot=FALSE)
r2=corr.lag2$acf[3]
pnorm(r2*sqrt(m), lower.tail=FALSE)
# -> 0.40354841 therefore fail to reject null


# Grouping of Sign test

# Number of postive devations -> n1
n1=length(GradQ1$ZX[GradQ1$ZX > 0])
n2=m-n1

# Vector of signs -> +1 is positive ISD, -1 is negative ISD
signs=sign(GradQ1$ZX)

# Now...we need to determine the number of group of 1's there are in the signs vector
# Use run-length encoding -> sum(rle(vector)$values ==1 )

result=rle(signs)
G=sum(result$values == 1) # -> G=2 groups of positive deviations

# Using the table in Formula and Table 2002
# The critical value for n1=5, n2=5 -> 1 @ 5% significan level
# Conclusion -> because G=5 > k=1, we G is outside rejection region -> fail to reject null

# Alternatively calculate the p-value
# P(G<=5) -> this calculate a p-value...FROM THE NOTES
prob=0

for (t in 1:G){
	prob = prob + choose(n1 - 1, t - 1) * choose(n2 + 1, t)/ choose(n1 + n2, n1)
	}

prob # -> 0.2619048
# Conclusion -> fail to reject the null

# This is a small data set so the conclusions from the tests were nt definitive
# Overall, the GoF test rejcted the null, supported by the CD test
# The other tests such as signs, interval (-2/3,2/3) were not definitive given the small data set
# The serial correlations, and groupoing of signs which test for clumping of deviations were also not definitive with the 
# small sample

# The plot however show the magnitude of the deviations are biased toward underestimating mortility btw ages 17 and 20
# Overall adherence to the data is quite poor
# Plot does suggest smoothness of the graduated rates

#########################################################################################
# Question 2

splines=read.csv("splines.csv")

knots=c(0,5,15,20,30,40)

phi=function(x,xi){
	(pmax(x-xi,0))^3
	}

mu=function(x, knots){
	0.0008-0.00013*x + 7.481*10^(-7)*phi(x,knots[1]) - 9.566*10^(-7)*phi(x,knots[2]) - 8.928*10^(-7)*phi(x,knots[3]) + 
		1.7593*10^(-6)*phi(x,knots[4]) - 9.309*10^(-7)*phi(x,knots[5]) + 2.729*10^(-7)*phi(x,knots[6])
		}

mu.grad=mu(splines$AGE, knots)

splines$GRAD=mu(splines$AGE, knots)


plot(splines$AGE, splines$MU, main = "Crude and Spline-Graduated rates against age", xlab = "age", ylab = "mortality rate")
lines(splines$AGE, splines$GRAD, col = "blue")

legend("topleft", legend = c("Crude rates", "Spline-Graduated rates"),
col = c("black", "blue"), lty = c(NA, 1), pch = c(1, NA))

third.diff=diff(splines$GRAD, differences=3)
third.diff=as.numeric(c(third.diff,0,0,0))

splines$THIRD_DIFF=third.diff # -> regular progression, suggests graduated rates are smooth

splines$DEATHS=splines$MU*splines$ETR
splines$EXPECTED=splines$GRAD*splines$ETR

splines$ZX=(splines$DEATHS-splines$EXPECTED)/sqrt(splines$EXPECTED)

# Chi Sq GoF
# DoF -> nrow(splines) - 8: 8 = 2 parameters estimated (a0,a1) and 6 knot parameters (b1,b2,b3,b4,b5,b6)

DoF=nrow(splines)-8
ts.ChiSq=sum(splines$ZX^2)

qchisq(0.95, DoF)
pchisq(ts.ChiSq, DoF, lower.tail=FALSE) # -> 0.9561536 therefore fail to reject null

# Signs test
# #P ~ Bin(m,p=0.5)
# m = nrow(splines)

m=nrow(splines)
P=length(splines$ZX[splines$ZX > 0])
EP=m*0.5 # -> P < EP -> P(#P<P)
p.value=2*pbinom(P, m, p=0.5)
p.value # -> 0.8746293 therefore fail to reject null

# Grouping of Sign test
# p.value = P(G<g) -> G = number of positve groups of deviations

n1=P
n2=m-n1

signs=sign(splines$ZX)
# Now...we need to determine the number of group of 1's there are in the signs vector
# Use run-length encoding -> sum(rle(vector)$values ==1 )

result=rle(signs)
G=sum(result$values == 1) # -> G=8 groups of positive deviations

# Using the table in Formula and Table 2002
# The critical value for n1=19, n2=21 -> 7 @ 5% significan level
# Conclusion -> because G=8 > k=7, we G is outside rejection region -> fail to reject null

# Alternatively calculate the p-value
# P(G<=8) -> this calculate a p-value...FROM THE NOTES
prob=0

for (t in 1:G){
	prob = prob + choose(n1 - 1, t - 1) * choose(n2 + 1, t)/ choose(n1 + n2, n1)
	}

prob # -> 0.1071615
# Conclusion -> fail to reject the null

# Alternatively, the Hypergeometric distribution ->
phyper(G, n2 + 1, n1 - 1, n1)
# This will calculate a p-value...READ NOTES

#########################################################################################

# Mortality Rates:
# -> Calculating mortality rates
# -> Calculating graduated mortality rates
# -> Plotting crude and graduated mortality rates

# Graduation Tests:
# -> Calculating thirs differences and performing a smoothness test
# -> Calculating standardized deviations zx
# -> Test: ChiSq Gof, Std Dev test, signs test, cumulative deviations test, grouping of signs test, serial correlations test

Grad = read.csv("Graduation.csv")
head(Grad)

Grad$CRUDE = Grad$DEATHS / Grad$ETR

# Calculating graudate mortality rates
# mu^o = B*C^x -> Gompertz law
# -> log(mu(x)) = log(B) + x*log(C)
# -> linear: log force of moreliaty linearly related to Age

# Perform a linear regression of the logged mortality against age to obtain estimates of log(B) and log(C) ->
gomp.fit = lm(log(CRUDE) ~ AGE, data = Grad)
coeffs = gomp.fit$coefficients

# To obtain B and C -> exp(coeffs)
exp(coeffs)

# Propose the parametric formula for the graudated rates based on Gompertz Law ->
# mu^o(x) = 1.55789x10^-5 * 1.1121^x

B = exp(coeffs)[1]
C = exp(coeffs)[2]

Grad$GRAD = B*C^Grad$AGE
head(Grad)

# Plotting Crude and Graduated rates
# Compare graudated rates with crude rates using scatter plot with curve overlay ->
plot(Grad$AGE, Grad$CRUDE, main = "Crude and Graduated Rates against age", xlb = "age", ylab = "Mortality Rate")
lines(Grad$AGE, Grad$GRAD, type = "l", col = "blue")
legend("topleft", legend = c("Crude", "Grad"), col = c("black", "blue"), lty = c(NA,1), pch = c(1, NA))

# The graduation captures the general trend of mortality observed across the ages
# There is slightly more variation around the line for older ages -> from ages 45 and onwards
# The graduation appears to underestimate mortality at older ages
# Investigate adherence -> Graduation Tests

# Smoothness test
# Smoothness test designed to investigate whether a graduation progresses smoothly from age to Age
# -> Carry out third differences of the graduated rates -> D^3(mu^o(x))
# -> diff(x, lag = , differences = )
# Here lag is the lag of each difference -> in this case 1
# -> differences will be how many times the vector is differenced -> default is 1

third.diff = diff(Grad$GRAD, 1, 3) # -> set lag = 1, differences = 3
# Looks quite smooth from age to age

Grad$THIRD_DIFF = c(third.diff, NA, NA, NA)
# Third differences appear small in magnitude compared to the graudated rates themselves and progress fairly regularly
# -> Graduated rates progress smoothly from age to age -> what we did expect already based on the plot

# Standardised Deviations
# Dx ~ Poisson(mu^o(x) * Ex)
# -> E(Dx) = mu^o(x) * Ex
# -> Var(Dx) = mu^o(x) * Ex
# -> Dx ~ N(mu^o(x) * Ex, mu^o(x) * Ex)

# zx = (Observed deaths - Expected deaths) / sqrt(Expected deaths)

Grad$EXPECTED = Grad$GRAD * Grad$ETR
Grad$ZX = (Grad$DEATHS - Grad$EXPECTED) / sqrt(Grad$EXPECTED)

# Chi Square Goodness of Fit Test
# The null hypothesis is that the graudated rates are the true underlying rates
# Calculate the TS -> sum(zx^2) -> i.e. sum of square standardized deviations

# We need to ensure the expteced deaths are large enought to use this test
# Look at the expected deaths -> make sure the figures are reasonably larger than 5 for most of the data
# Combine categories to make the expected deaths >= 5 -> remember that this will reduce the DoF -> PAY ATTENTION

TS = sum(Grad$ZX^2)
# The Degrees of Freedom depends on how many paramters were estimated -> in this case it was 2 -> B^ and C^
# The number of standardized devations is equal to 51 -> DoF = 51 - 2 -> 49
dof = 49
p.value = pchisq(TS, dof, lower=FALSE)
p.value # -> 1.003558e-07 and we therefore reject the null at 5% level -> Reasonable to conclude that the graduated rates are not the true underlying mortality rates

# Individual Standardized Deviations Test
# Under the null hypothesis, the graduated rates are the tru underlying mortality rates
# -> the standardized deviations should approximately for a sample from the standard normal distribution

# Divide the real number line into intervals and compare the observed number of standardised deviations with expected numer in each interval
intervals = c(-Inf, -3, -2, -1, 0, 1, 2, 3, Inf)
# We can already see that there is GoF test coming and we need to ensure expected values are >=5 for the intervals

# To get the expected values for each interval -> Number in our sample = N -> N * probability over the interval
CDF.interval = pnorm(intervals) # -> remember pnorm() by default is N(0,1)
diff.CDF.interval = diff(CDF.interval)
Expected = nrow(Grad) * diff.CDF.interval
Expected

# Alrady can see that we have to collapse the intervals to get higher expected values

intervals = c(-Inf, -1, 0, 1, Inf)
# Check the interval size -> 5 elements and therfore 4 groups -> this will be important for the Degrees of Freedom
CDF.interval = pnorm(intervals) # -> remember pnorm() by default is N(0,1)
diff.CDF.interval = diff(CDF.interval)
Expected = nrow(Grad) * diff.CDF.interval
Expected # -> can work with This

# We can check how the observed standardized deviations lie in the intervals if chose -> VERY IMPORTANT TO GET THIS RIGHT
Observed = hist(Grad$ZX, breaks = intervals, plot=FALSE)$counts
Observed

# Calculate the Gof TS ->
TS = sum((Observed - Expected)^2 / Expected) # -> ~ ChiSq(DoF = 4 - 1 = 3)
p.value = pchisq(TS, 3, lower=FALSE)
p.value # -> 0.0001063447 and so we have sufficient evidence to reject the null hypothesis at 5% level -> evidence that ISDs do not forma sample from a standard normal distribution -> resonable to conclude that
# graduated rates are NOT the true underlying mortality rates

# Other ISD tests -> Checks that can be done on either inspection or otherwise
# Check whether there are Outliers
# Symmetry around 0
# The number of devations outside the inteval (-2/3, 2/3) -> 50% of ISDs should lie inside the interval
# The number of positive and negative devations -> Symmetry
# The groupings of deviations of the same sign

# Outliers -> do a histogram
hist(Grad$ZX, main = "Histogram of ISDs", xlab = "zx")
# -> Does not appear to resemble normal distribution
# We know that from a sample from a standard normal distribution, around 1% to have value larger than 2.5 (2.56) -> Also expect 5% to be larger than absolute value of 1.96
# When performing a graduation, we're not likely to have 100 ages or data points from 100 ages -> We should generally be concerned about any ISD larger than 2.5 in absolute value

# To find any data points > 2.5 in magnitude -> 
length(Grad$ZX[abs(Grad$ZX) > 2.5]) # -> 4
# 4 / 51 -> 7.8% > 1% so already we know the ISDs are not following a std normal distribution

# To find any data points > 1.96 in magnitude -> 
length(Grad$ZX[abs(Grad$ZX) > 1.96]) # -> 10
# 10 / 51 -> 19.67% > 5%

# Symmetry
# Roughly equal number of postive and negative deviations
# We can use the signs test to get a more definitive answer

# Interval (-2/3, 2/3)
# Roughly 50% of a std normal distribution lies in the interval (-2/3, 2/3)
length(Grad$ZX[abs(Grad$ZX ) > 2/3]) # -> 33
binom.test(length(Grad$ZX[abs(Grad$ZX ) > 2/3]), nrow(Grad), p = 0.5, alt = "greater")
p.value = binom.test(length(Grad$ZX[abs(Grad$ZX ) > 2/3]), nrow(Grad), p = 0.5, alt = "greater")$p.value
p.value # -> 0.02443695 so we can reject the null and conclude there is evidence of too many large deviations -> reasonable to conclude that graudated rates are not true underling mortality rates

# Signs test
# Under null hypothesis, the graduated rates are the true underlying mortality rates
# The ISDs for a sample from a std normal distribution
# Therefor the probaility of any ISD being positive is 0.5

# Let P be a RV representing the number of positive deviations -> under the null
# P ~ Bin(nrow(Grad), 0.5)
# Calculating the number of postive ISD ->
P = length(Grad$ZX[Grad$ZX > 0])
P # -> 38

# Expected number of postive ISD -> length(Grad$ZX) * 0.5
Exp.P = length(Grad$ZX) * 0.5
Exp.P # -> 25.5

# P > Exp.P -> WE CALCULATE 2* P(P > 38) p-value because its a two-tail test!
p.value = 2 * pbinom(P - 1, length(Grad$ZX), 0.5, lower=FALSE) # BE VERY CAREFUL WITH LOWER=FALSE for the complimentary probability -> P(P >= 38) -> 1 - P(P <= 37) hence why you subtracted 1
p.value # -> 0.0001980395 so we reject the null -> and conclude that the graudated rates are not the true underlying mortality rates

# Cumulative Deviations Test
# This is a TWO-SIDED test that compared the total observed deaths to the total expected deaths either over the entire age range or subsets of teh age range
# (sum(obs deaths) - sum(exp detahs)) / sqrt(sum(exp deaths)) ~ N(0,1) under the null
# Derivation of the null -> Core Reading notes

obs.deaths.total = sum(Grad$DEATHS)
exp.deaths.total = sum(Grad$EXPECTED)

TS = (obs.deaths.total - exp.deaths.total) / sqrt(sum(exp.deaths.total))
p.value = 2 * pnorm(TS, lower=FALSE)
p.value # -> 0.004236965 so can reject the null and conclude as before

# Grouping of Signs test
# This test looks at groups, clumps or runs of deviations of the same sign
# Under the null, the graudated rates are the true rates of underlying mortality -> the standard ISDs for a sample of indepdendent values from a standard normal distribution
# This test comapes the oberved clumping of signs with what would be expected if the postive and negative deviations were arranged in a random order ->

# Remember this test is a test for overgraudation -> bias
# If there is overgraduation -> the ISDs wont swap back from positive to negative very often
# There wil be few groups of deviations of the same sign 
# The Grouping of Signs test -> one-sided looking whther ther are too few groups

# -> G is an RV represetning the number of groups of positive deviations
# -> n1 is the number of positive deviations
# -> n2 is the number of negative deviations

# Calculating the observed value n1, n2 and G in R ->
n1 = length(Grad$ZX[Grad$ZX >= 0]) # -> 38
n2 = length(Grad$ZX) - n1 # -> 13

# To calculate the observed value of G, 
signs = sign(Grad$ZX)
table(signs) # -> table of summarising signs of Grad$ZX

# Now we need to determine the numer of groups of 1's in the signs vector
# -> What this means:
# -> The postive signs occur throughout between negative signs -> think of + signs placed between - signs
# -> There will be groups of + signs in between
# We need to determine the number of group of 1's there are in the signs vector
# Use run-length encoding -> sum(rle(signs)$values == 1)

results = rle(signs)$values
G = sum(results == 1) # -> G = 9
# You can compare G to CV k in the Formula and Tables for n1 and n2
# If G is inside the critical region i.e. G <= k -> reject the null
# If G > k -> fail to reject the null
# Still a good idea to calculate a p-value
# To calculate a p-value -> P(G <= 9) -> this is a p-value...FROM THE NOTES -> Remember also that this is a one-side test!

# Refer to notes on the distribution for G -> P(G=t) -> ...

prob=0
for (t in 1:G){
	prob = prob + choose(n1 - 1, t - 1) * choose(n2 + 1, t)/ choose(n1 + n2, n1)
	}

prob # -> 0.2470046 > 0.05 so we fail to reject the null -> no significant evidence of clumping of deviations of the same sign -> reasonable to conclude that graudated rates may not differ from the 
# true rates of Mortality

# Alternatively -> if the number of groups n1 and n2 are large enough -> we can use the normal approxiamtion with the continuity correction -> CHECK CORE READING notes

# G ~ N(mu, sigma^2) -> if n1 and n2 are large enough -> DONT FORGET THE CONTINUITY CORRECTION

mu = n1 * (n2 + 1) / (n1 + n2)
sigma = sqrt((n1 * n2)^2 / ((n1 + n2)^3))

TS = (G + 0.5 - mu) / sigma # -> apply the continuity correction
p.value = pnorm(TS) # -> the p-value is in the bottow tail -> THIS IS ONLY TIME WE DO THIS
p.value # -> 0.2461435 ... similar to the hypergeomtric result above


# Serial Correlations Test
# The null hypothesis is graudated rates are tru underlying rates -> we use the serial correlations test much like the grouping of signs test -> clumping
# As with the grouping of signs test, the SC test is looking for clumping of ISDs
# rj -> autocorrelation at lag j

# Refer to course notes for the formulas

# We perform the test at lag 1 -> but it can also be done at higher lags

# We calculate the serial correlation using cor() functions
# -> We calculate the correlation between the vector of ISDs and and a one-lag vector of the same quantiles

r1 = cor(Grad$ZX[-length(Grad$ZX)], Grad$ZX[-1])
# -> [-length(Grad$ZX)[ removes the last ISD -> vector 1:n-1
# -> [-1] removes the first ISD -> vector 2:n

# Under the null -> rj ~ N(0, sqrt(m))
# -> m = length(Grad$ZX)
# -> rj*sqrt(m) ~ N(0,1)

m = length(Grad$ZX)
TS = r1 * sqrt(m)
p.value = pnorm(TS, lower=FALSE)
# This is one-sided test -> DONT FORGET...you DONT NEED to multiply the p.value by 2
p.value # -> 0.124571 so we fail to reject the null -> same result with grouping of signs test -> find that serial correlations test and grouping of signs test will be correlated 

# We can also rely on the TS function acf() -> sample autocorrelation function
acf(Grad$ZX, lag.max = 1, plot = FALSE)

r1.acf = acf(Grad$ZX, lag.max = 1, plot = FALSE)$acf[2] # -> when you run this, you'll understand -> you are extracting the correlation values through $acf on acf() -> [2] is rho^1
# Compare this value with r1
r1.acf # -> 0.1612605
r1 # -> 0.1613733

# As before, r1.acf * sqrt(m) ~ N(0, 1) and we can do Hypothesis Testing 
# -> TS = r1.acf * sqrt(m)
# p.value = pnorm(TS, lower=FALSE) 
# Make conclusion from there

#########################################################################################
# Question 1

UK.M = read.csv("Graduation_Q1.csv")
head(UK.M)

UK.M$CRUDE = UK.M$DEATHS / UK.M$ETR
plot(UK.M$AGE, UK.M$CRUDE, xlab = "Age", ylab = "Force of Morality", main = "Plot of Crude and Graduated Mortality Rates for UK.M", col = "blue")
lines(UK.M$AGE, UK.M$GRAD, type = "l", col = "red")
legend("topleft", legend = c("Crude", "Grad"), col = c("blue", "red"), lty = c(NA,1), pch = c(1, NA))

diff(UK.M$GRAD, 1, 3) # -> smoothness looks ok ... progression from age to age looks regular ... differences appear small in magnitude

# Told that graudation was done using a paramatric model with 3 parameters estimated
# Need to prepare the data ->

UK.M$EXPECTED = UK.M$GRAD * UK.M$ETR
UK.M$ZX = (UK.M$DEATHS - UK.M$EXPECTED) / sqrt(UK.M$EXPECTED)

# ChiSq GoF test
m = length(UK.M$ZX)
TS = sum(UK.M$ZX^2) # ~ ChiSq(DoF = m - 3)
p.value = pchisq(TS, m - 3, lower=FALSE)
p.value # -> 0.0001304895 reject null -> sufficent evidence that grad rates are not true underlying mortality rates

# Signs test
# P ~ Bin(m, 0.5) 
P = length(UK.M$ZX[UK.M$ZX >= 0]) # -> 5
EP = m * 0.5 # -> 5
# p.value = 2 * Prob(P >= 5) ->
p.value = 2 * (pbinom(P, m, 0.5, lower=FALSE) + 0.5*dbinom(P, m, 0.5)) # -> BE VERY CAREFUL! KNOW THE SHAPE OF THE DISTRIBUTION AND HOW TO SPLIT AT THE MIDDLE
p.value # -> 1 fail to reject null -> insufficent evidence that the graduated rates are the true underlying rates of Mortality -> also suggestive that there isn't bias in Graduation

# ISD in interval (-2/3, 2/3) -> peform binomial test
C = length(UK.M$ZX[abs(UK.M$ZX) > 2/3]) # -> 5
# have about 50% of graduated rates lie in interval (-2/3, 2/3) -> small sample ... difficult to suggest that ISD follow N(0,1)
binom.test(C, m, p = 0.5, alt = "greater")
p.value = binom.test(C, m, p = 0.5, alt = "greater")$p.value
p.value # -> 0.6230469 so we fail to reject the null -> insufficient evidence of too many large deviations -> reasonable to conclude that graudated rates are true underling mortality rates

# Cumulative Deviations Test on entire agre range
# under null -> TS ~ N(0,1) # -> TWO - SIDED TEST
TS = (sum(UK.M$DEATHS) - sum(UK.M$EXPECTED))/ sqrt(sum(UK.M$EXPECTED))
p.value = 2 * pnorm(TS, lower=FALSE)
p.value # -> 0.02193619 reject the null hypothesis -> evidence of bias -> OVERGRADUATION

# Serial Correlation test -> lags 1 and 2
# Under null, rho1*sqrt(m) ~ N(0,1) and rho2*sqrt(m) ~ N(0,1)
rho1 = acf(UK.M$ZX, lag.max = 1, plot = FALSE)$acf[2] # -> 0.2724214
rho2 = acf(UK.M$ZX, lag.max = 2, plot = FALSE)$acf[3] # -> 0.07721429

# rho1
TS = rho1*sqrt(m)
p.value = pnorm(TS, lower=FALSE)
p.value # -> 0.1944891 fail to reject null -> evidence of clumping of signs at lag 1 of ISDs -> sample is too small to make definite conclusions

# rho2
TS = rho2*sqrt(m)
p.value = pnorm(TS, lower=FALSE)
p.value # -> 0.4035484 fail to reject null -> evidence of clumping of signs at lag 2 of ISDs -> sample is too small to make definite conclusions

# Grouping of Signs
n1 = length(UK.M$ZX[UK.M$ZX >= 0]) # -> 5
n2 = m - n1 # -> 5

signs = sign(UK.M$ZX)
table(signs) # -> as a check
results = rle(signs)$values
G = sum(results == 1) # -> 2
k = 1 # -> from table and formuals when n1 = 5, n2 = 5

# G > k so we fail to reject null -> G is outside critical region
 
# Alternatively ->
pvalue = 0
for (t in 1:G){
	pvalue = pvalue + choose(n1 - 1, t - 1) * choose(n2 + 1, t)/ choose(n1 + n2, n1)
	}
pvalue # -> 0.2619048 fail to reject null -> insufficent evidence of clumping of signs -> sample is too small to make definite conclusions

# Sample is too small to make definite Conclusions
# Smoothness test is ok ... progression between ages regular -> need to assess adherence
# ChiSq GoF  and Cumulate Deviations test reject the null hypothesis
# Other tets fail to reject but the sample size is too small
# On inspection of the plot, can see overgraduation present -> large deviations of crude rate and graduated rates at ages 18 - 20

#########################################################################################
# Question 2

splines = read.csv("splines.csv")

# knots  at ages (0, 5, 15, 20, 30, 40}

knots = c(0, 5, 15, 20, 30, 40)

phi = function(x, xi){
	pmax(x - xi, 0)^3 # -> just stick to pmax always...R is a pain
	}
	
mu = function(x){
		0.0008 - 0.00013*x + 7.481e-7*phi(x, knots[1]) - 9.566e-7*phi(x, knots[2]) - 8.928e-7*phi(x, knots[3]) + 1.7593e-6*phi(x, knots[4]) - 9.309e-7*phi(x, knots[5]) + 2.729e-7*phi(x, knots[6])
	}

splines$GRAD = mu(splines$AGE)

plot(splines$AGE, splines$MU, xlab = "Age", ylab = "mu(x)", main = "Plot of Crude and Graduated Rates mu(x) for Ages x", col ="blue")
lines(splines$AGE, splines$GRAD, col = "red", type = "l")
legend("topleft", legend = c("Crude", "Grad"), col = c("blue", "red"), lty = c(NA,1), pch = c(1, NA))

diff(splines$GRAD, 1, 3)
# -> Graduation progresses regularly from age to age and small in magnitude

splines$DEATHS = splines$MU*splines$ETR
splines$EXPECTED = splines$GRAD*splines$ETR

splines$ZX = (splines$DEATHS - splines$EXPECTED) / sqrt(splines$EXPECTED)

# ChiSq GoF test
DoF = length(splines$ZX) - length(knots) - 2
TS = sum(splines$ZX^2)
p.value = pchisq(TS, DoF, lower=FALSE)
p.value # -> 0.9561536 fail to reject null -> graudation adheres to the observed moratality

# Signs Test
P = length(splines$ZX[splines$ZX >= 0]) # -> 19
EP = length(splines$ZX)*0.5 # -> 20
p.value = 2 * pbinom(P, length(splines$ZX), 0.5)
p.value # -> 0.8746293 failt to reject null

# Grouping of Signs Test
n1 = length(splines$ZX[splines$ZX >= 0]) # -> 19
n2 = length(splines$ZX) - n1 # -> 21

signs = sign(splines$ZX)
results = rle(signs)$values
G = sum(results == 1) # -> 8

p.value = 0
for (t in 1:G){
	p.value = p.value + choose(n1 - 1, t - 1) * choose(n2 + 1, t)/ choose(n1 + n2, n1)
	}
p.value # -> 0.1071615 fail to reject null
