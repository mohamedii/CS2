# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\6 Survival Models")

# Calculating:
# mux, tPx, tQx, mx, eCx, ex

# flexsurv package
# Calculating densities, probabilities, quantiles of Gompertz distribution
# Simulating from Gompertz
# Calculating the hazard, cumulative hazard function of Gompertz distribution

# Note. Dont confuse Gompertz law with the Gompertz distribution

#Start ->

# Makeham's law: mux = A + BC^x

A=0.00005
B=0.000001
C=1.15

mu=function(x){
	A+B*(C^x) }

mu(50)

mu2=function(A,B,C,x){
	A+B*(C^x) }

mu2(A,B,C,50)

# Calculating tPx from Makeham's Law
# Makeham's law provide the force of mortality of the form:
# poly(1)+exp{poly(2)}

# mux = A+BC^x -> force of mortality according to Makeham
# Therefore tPx=(s^t)*(g^((C^x)*(C^t-1))
# g=exp(-B/Log C)
# s=exp(-A)

tPx=function(x,t){
	s=exp(-A)
	g=exp(-B/log(C))
	(s^t)*(g^((C^x)*(C^t-1))) }

tPx(40,20)
tPx(40,5:10)

# Calculating tPx with integration
# Remember that tPx=exp(-int(x,x+t){mu(x)ds}
# -> tPx=exp(-int(0,t){mu(x+s)ds}
# In R, we can use the function:
# integrate(function, lower limit, upper limit, ...)

# To calcutate int(40,60){mu(s)ds}

integrate(mu, 40,60)
integrate(mu, 40,60)$value

integrate(mu2, 40,60, A=.000005, B=0.0000001, C=1.15)$value

exp(-integrate(mu,40,60)$value)

tPx_int=function(x,t){
	exp(-integrate(mu,x,x+t)$value) }

tPx_int(40,20)

tPx_int(40, 5:10)
# Can't evaluate this...so use sapply() function
# sapply(vector to iterate over, custom function {})

tPx_int_sapply=function(x,t){
	sapply(t, function(t){exp(-integrate(mu,x,x+t)$value)})
	}

# Calculating tQx...simply 1-tPX
# you can use the tPx_int_sapply

# Calculating mx -> central rate of mortality
# expecte mx to between mu(x) and mu(x+1)
# -> mu(x) < mx < mu(x+1)

# Calculate mx requires a few extra steps
# Remember mx=1Qx/(int(0,1){tPx dx}
# So we need to x,t, and range(0,1) for the integral

# 1Q50 = Q50
Q50=1-tPx(50,1)
m50=Q50/(integrate(tPx,0,1,x=50)$value)

# We can use tPx_int_sapply if we don't have a nice formula
# for tPx

# 1Q50 = Q50
Q50=1-tPx(50,1)
m50=Q50/(integrate(tPx_int_sapply, 0,1,x=50)$value)

# Note tPx function is under Makeham's force of mortality - see above
# What happens if we use general tPx_int? i.e. where we actally integrate
# over the Makeham's force of mortality rather than relying on the formula?

# here we will use tPx_int=exp(-(integrate(mu,x,x+t)$value)

# 1Q50 = Q50
Q50=1-tPx(50,1)
m50=Q50/(integrate(tPx_int,0,1,x=50)$value)

# See that there is a problem!
# Better to use the sapply() version of the tPx_int function

Q50=1-tPx(50,1)
m50=Q50/(integrate(tPx_int_sapply,0,1,x=50)$value)

# Calcualte eCx -> complete expected lifetime
# eCx=integral(0,inf){tPx dx}

eCx=integrate(tPx, 0, Inf, x=40) 
# Above calculates the complete future lifetime opf x=40

# We can also use the tPx_int_sapply function but caution here
# we can't use Inf in the limit of integration when using sapply()
# function...try it

eCx=integrate(tPx_int_sapply, 0, Inf, x=40)

# Get around this if we know where the true function has a limit point
# i.e. where we know with sufficiently large age x ->
# mu(z) -> Inf
# try higher ages of x to see if the eCx value stabilizes / converges

eCx=integrate(tPx_int_sapply,0,60,x=40)$value
eCx=integrate(tPx_int_sapply,0,100,x=40)$value
eCx=integrate(tPx_int_sapply,0,500,x=40)$value

# Waht about ex...the kurate expected future lifetime?
# ex=sum(t=1,t=Inf){tPx}

ex=sum(tPx(40, 1:1000))

# can see the result: eCx -> ex + 0.5

# Plotting a function with one input
# -> plot(x values, y values, type="l")
# also create a vector seq() that has structure seq(start, end, increment)

xvalues=seq(0,100,0.1)
plot(xvalues, mu(xvalues), type="l", lwd=3, xlab="Age", ylab="Force of mortality according to Makeham's Law", 
	main="Plot of mu versus age")

# Also can plot a function by using structure plot(function, start, end)
plot(mu,0,100,lwd=3, xlab="Age", ylab="Force of mortality", main="Plot of mu versus age")

# Alternative is to use curve() -> curve(function(x), state, end)
curve(mu(x), 0, 100,lwd=3, xlab="Age", ylab="Force of mortality", main="Plot of my versus age")

# Plotting function with multiple inputs
# Can't use this aymore if function has more that one input: plot(function, start, end)
# So...we either need to use plot(xvalues, yvalues) or curve(function(x), start, end)

tvalues=0:100
plot(tvalues, tPx(40,tvalues),type="l", lwd=2, xlab="Age", ylab="Probability of Survival tPx", main="Plot of tP40")

curve(tPx(x,x=40),0,100,lwd=2, xlab="Age", ylab="Survival Probability", main="Plot of tP40")
# careful here...the functional form of tPx(x,t) but here we say (x, x=40). Here we are telling R age=40, and the other
# argument t, the one that must vary is denoted by x.

# Survival Distribution Calculations
# Survival Time

# If survival times follow an exponential distribution, Tx~exp(mu)
plot(seq(0,100,0.1), dexp(seq(0,100,0.1),0.05), type="l", lwd=2, xlab="t", ylab="density", main="pdf of exp(mu=0.05)")
plot(seq(0,100,0.1), pexp(seq(0,100,0.1),0.05), type="l", lwd=2, xlab="t", ylab="probability", main="cdf of exp(mu=0.05)")
plot(seq(0,100,0.1), 1-pexp(seq(0,100,0.1),0.05), type="l", lwd=2, xlab="t", ylab="probability", main="cdf of exp(mu=0.05)")

#Simulating Survival Times

set.seed(123)
sims=rexp(1000,0.05)
mean(sims)

hist(sims, xlab="Survival Times", main="Histogram of Sample Survival Times")

# If survival times follow Weibull(alpha, beta) -> Tx~Weibull(alpha, beta)
# shape -> beta
# scale -> alpha^(-1/beta)
# remember this because R parametrises the Weibull distribution using these version of parameters

# In R, dweibull) pweibull(), qweibull() with parametrization shape=beta, scale = alpha^(-1/beta)

alpha=0.00025
beta=2.3

shape=beta
scale=alpha^(-1/beta)

# You must use these parametrizxation in order to use Weibull in R

dweibull(0.5, shape, scale)

plot(seq(0,100,0.1), dweibull(seq(0,100,0.1), shape, scale), type="l",
	xlab="t",
	ylab="density",
	main="pdf of Weibull(t, shape=beta, scale=alpha^(-1/beta)")

# Simulating random numbers from Weibull(alpha, beta)
set.seed(123)
sims=rweibull(1000, shape, scale)
head(sims)

# alternative fucntions for Weibull distribution using parametrizations alpha, beta (conventional)

rweibull2=function(n, c, g){
	(-log(1-runif(n))/c)^(1/g)
	}

dweibull2=function(x,c,g){
	c*g*x^(g-1)*exp(-c*x^g)
	}

pweibull2=function(x,c,g){
	1-exp(-c*x^g)
	}

qweibull2=function(p,c,g){
	-(log(1-p)/c)^(1/g)
	}

set.seed(123)
sims=rweibull2(1000,alpha,beta)
head(sims)
hist(sims)

# Calculating expected lifetime calculation under exponential or WEibull models
# first define tPx functions for exponential, Weibull

tPx_exp=function(t,mu){
	exp(-mu*t)
	}

tPx_weibull=function(t,c,g){
	exp(-c*t^g)
	}

# alternatively, you can use pexp() and pweilbull2() functions - remeber the parametrization!

# Remember for expected fucture lifetime calculations, we integrate over tPx
# Tx~exp(0.05) and Tx~Weibull(c,g)

integrate(tPx_exp, 0, Inf, mu=0.05)$value
integrate(tPx_weibull, 0, Inf, c=alpha, g=beta)$value

# Alternatively, we can integrate the pdfs since the survival distribution
# is the complement of the mortality distribution -> S(t;X)=1-F(t;X) -> P(Tx>t)=1-P(Tx<t)

integrate(pexp, 0, Inf, rate=0.05, lower.tail=FALSE)$value # use the lower=FALSE
integrate(pweibull, 0, Inf, scale=scale, shape=shape, lower.tail=FALSE)$value 
# an only use the lowr.tail=FALSE with
# functions in-R functions. Custom functions aren't defined with lower.tail=FALSE setting

# Remember what the expected future lifetime is -> E[Tx]
# so if we know the distribution and if the moment exists, use the expectation...to kae things easier

# For instance, the first non-central moment, or expectation of Tx~Weibull(alpha, beta)
# -> Gamma(1+1/gamma*(c^(-1/gamma))

# For instance, the first non-central moment, or expectation of Tx~exponential(mu)
# -> 1/mu

# to get the curtate future expected lifetime, sum(1,Inf){tPx}
# can't use Inf as a limit in the summation...so rather make it a large number

ex=sum(tPx_exp(1:2000,0.05))
exWei=sum(tPx_weibull(1:2000,alpha, beta))

# Again, we can use the P(Tx>t)=1-P(Tx<t) for tPx functions

ex=sum(1-pexp(1:2000,0.05))
exWei=sum(1-pweibull(1:2000, scale=scale, shape=shape))

# or

ex=sum(pexp(1:2000,0.05, lower.tail=FALSE))
exWei=sum(pweibull(1:2000, scale=scale, shape=shape, lower.tail=FALSE))

# Write out functions to see where you can calculate from first principles rather than relying on formula
# Consider the exponential distibution where mu=constant -> use the infinite series geometric sum to calculate
# the summation -> sum(t=1,Inf){tPx} -> sum(t=1,Inf){exp(-mu*t)} -> exp(-mu)*{sum(t=0,t=Inf){exp(-mu*t}
# -> exp(-mu)/(1-exp(-mu)) -> if |exp(-mu)| < 1


# R code - flexsurv package
# We use the Gompertz distribution that follows from Gompertz law -> mu(x)=B*C^x
# Gompertz law is Makeham's law when A=0 -> mu(x)=A+B*C^x

# install.packages("flexsurv")
# library(flexsurv)

# Load the flexsurv package to get access to Gompertz distribution
# dgompertz(), pgompertz(), qgompertz(), rgompertz()

# Gompertz law -> mu(x)=B*C^c
# rate=B, shape=log(C)

B=0.000003
C=1.2

rate=B
shape=log(C)

# Gompertz law is appropriate for motality with middle and older ages
# Form: mu(x) = B*C^(x-30) for x>=30
# Notice this form...because we will have to make some adjustments to work with it

# B, C are given above. So T30 follows Gompertz distribution with parameters shape=log(C), rate=B
# The distribution of T(30+a) will follow Gompertz distribution with:
# shape=log(C)
# rate=B*(C^a)

# You can see this from the formula...write it out on paper

# Whenever you've working with form: Form: mu(x) = B*C^(x-30) for x>=30, and asked to calc T(30+a)
# for instance, adjust the rate=B*(C^a) THIS IS IMPORTANT

# dgompertz(t, shape, rate)
dgompertz(0.5, shape, rate)

plot(seq(0,100,0.1), dgompertz(seq(0,100,0.1), shape, rate), type="l",
	xlab="time after age 30", ylab="desnity", main="pdf of Gompertz(shape, rate)")


# CDF for Gompertz survival times
# mu(x)=B*(C^(x-30))
# tPx=g^(C^(x-30)*(C^(t)-1)) where g=exp(-B/log(C))
# tQx=1-tPx

# pgomertz(t, shape, rate)
# Calc P(T30<50)...which is 20 Q 30
pgompertz(50,shape, rate)

# Check 1-tPx
tPxGom=function(t,x, B, C){
	g=exp(-B/log(C))
	g^(C^(x-30)*(C^(t)-1))
	}
1-tPxGom(50,30,B,C)

# To calculate the probabilities for different ages, adjust the parameters
# Calculating probabilties for someone aged 30+a, need to adjust the rate parameter -> rate=B*(C^a)
# Now calc P(T40<50)

rate=B*(C^10)
shape=log(C)

pgompertz(50,shape, rate)

# Check 1-tPx
tPxGom2=function(t,x, B, C){
	g=exp(-B/log(C))
	g^(C^(x-40)*(C^(t)-1))
	}
1-tPxGom2(10,40,rate,C) #...can't adjust age shift becuase Gompertx consructed at x=30, where B,C were determined
# meaning that B,C are relative to x=30

# Try to use a x+a approach -> t=10+a, adjust rate -> B*(C^a)
1-tPxGom(50,30,B*(C^10),C)

# Always REMEMBER...P(Tx<t) is the mortality tQx. The SURVIVAL prob is P(Tx>t)

# Calculate P(20 < T35 < 30) -> 35=30+5 there a=5
rate=B*(C^5)
shape=log(C)
pgompertz(30,shape,rate)-pgompertz(20,shape,rate)

# Determine the upper quartile of lifetime distribtuion of life currently aged 45
# -> T45 -> T(30+15) -> rate=B*(C^15), shape=log(C)

rate=B*(C^15)
shape=log(C)

qgompertz(0.75,shape, rate) 
#P(T45<47.2073)=0.75

# Simulating Survival Times 
# Use the rgompertz function to sample from the distribution
# rgompertz(n, shape, rate)


# Simulating 1000 future lifetime values for lives currently aged 32 -> 30+2 therefore a=2
set.seed(123)
sims=rgompertz(1000, log(C),B*(C^2))

head(sims)

# Hazard Rate
# Continuing from mu(x)=B*(C^(x-30))
# The Hazard rate for Gompertz: H = hgompertz(x-30, shape, rate)
# So for instance, mu(50) based on B,C determined from age x=30 -> hgompertz(50-30=20, shape, rate)

rate=B
shape=log(C)
hgompertz(20,shape, rate)

# The Hazard rate for Gomperts -> mu(x)
# Therefore mu(50)-> mu(30+20) based on B,C determined on x=30
# mu(x)=B*(C^(x-30))

B*(C^(20))

plot(30:100, hgompertz(30:100, shape, rate), type="l", lwd=2, xlab="age", ylab="Force of Mortality", main="Graph of mu(x) against age")

# Integrated Hazard
# Integrating hazard function -> H(t, shape, rate)
# -> int(30, 30+t){mu(x)dx} -> int(30,30+t){B*C^(x-30) dx}
# Think about tPx=exp(-int(x,x+t){mu(s)ds} -> tPx=exp(-H(x,t))

# So for instance, P(T30<50) -> probability of life ending in 50 years
# -> 1-50P30 -> 1- exp(-int(30,30+50){mu(s)ds})

1-exp(-Hgompertz(50,shape,rate))

# and if life is aged 40 -> P(T40<50)
1-exp(-Hgompertz(50,shape, rate*C^10))

# Expected Lifetimes for Gompertz -> use Makeham's law with A=0

tPxGom=function(x,t, B,C){
	g=exp(-B/log(C))
	g^(C^(x-30)*(C^t-1))
	}

# we can also use pgompertz() -> P(T30>t) -> 1-pgompertz(t,shape,rate)

# Remeber expected future lifetimes -> eC40
eCx=integrate(tPxGom, 0,Inf,B,C,x=40)$value

integrate(pgompertz, 0, Inf, shape=log(C), rate=B*C^10, lower.tail=FALSE)$value
# remember with flexsurv gompertz...no need to specify x, just update parameters to show age is different
# -> shape=log(C), rate=B*(C^a)) -> x->x+a

# For the curtate expected future lifetime

ex=sum(pgompertz(1:100, shape=log(C), rate=B*(C^10), lower.tail=FALSE))
ex=sum(tPxGom(1:100, x=40, B,C))

#########################################################################################
# Question 1

# T40 RV denoting expected future life time of lives currentl aged 40
# T40~Weibull(0.00001,3.5)

c=0.00001
g=3.5

shape=g
scale=c^(-1/g)


set.seed(100)
sims=rweibull(10000,shape,scale)
head(sims)

# Estimate P(T50>10) -> think of it as P(T40>20 | T40>10)
# P(T40>20 and T40>10) / P(T40>10)
# Alternatively -> 20P40 -> 10P40 x 10P50 -> 10P50 = 20P40/10P40

P=length(sims[sims>20])/length(sims[sims>10])

# Under this model, the hazard rate at age 40+t -> mu(40+t)=alpha*beta*t^(beta-1)
# alpha=0.00001, beta=3.5

alpha=0.00001
beta=3.5

mu=function(x,t, alpha, beta){
	alpha*beta*((x-40+t)^(beta-1))
	} # mu(x+t)= alpha*beta*(x-40+t)^(beta-1)

# Calculate P(T50>10) -> age 50 -> age60
mu(50,0,alpha,beta)
exp(-integrate(mu,0,10,x=50,alpha=0.00001, beta=3.5)$value)

# The answers are very similar, even though R evaluates, the integral numerically.
# We should expect the answers to be the same given the hazard rate is directly from the Weibull
# Also, using the same scale and shape parameters. based of age 40
# T40 was given as Weibull, we used a hazard rate for age 40+t -> mu(x+t)= alpha*beta*(x-40+t)^(beta-1)
# The answer is slightly different due to sampling error. Perhaps take a larger sample...could get it closer

#########################################################################################
# Question 2

# Mortality assumed to follow Gompertz for ages 25 and over -> x>=25

# mu(x)=B*C^(x-25)

B=0.00000729
C=1.128

# Two ways to do the plot...use a custom function for the force of mortality following gompertz law or use
# flexsurv package

mu=function(x,t,B,C){
	B*C^(x-25+t)
	}

plot(seq(25,100,1), mu(x=25, seq(25,100,1),B,C), type="l", xlab="age 25 to 100", ylab="force of mortality", main="Gompertx FoM from ages 25 -> 100")

# library(flexsurv)

shape=log(C)
rate=B

plot(seq(25,100,1), hgompertz(seq(25,100,1),shape=shape, rate=rate), type="l", xlab="age 25 to 100", ylab="force of mortality", main="Gompertx FoM from ages 25 -> 100")

# PDF of T45 -> T(25+20) -> a=20
# adjust parameters because B,C are determined at age 25
shape=log(C)
rate=B*C^20

plot(0:100, dgompertz(0:100,shape=shape, rate=rate), type="l", xlab="future lifetimes for age 45", ylab="density", main="Gompertx PDF for age 45")

# estiamte of eC50
# tPx -> x=50 -> 25+25 -> a=25
eC50=integrate(pgompertz, 0, Inf, shape=log(C), rate=B*(C^25), lower.tail=FALSE)$value
eC50

# We can also use a custom function tPx for Gomperts -> tPx=g^(C^(x-25)*(C^t-1)), g=exp(-B/log(C))
# For reference :tPxGom=function(x,t, B,C){
#	g=exp(-B/log(C))
#	g^(C^(x-30)*(C^t-1))
#	}

# T50 -> T(25+25) -> a=25
set.seed(50)
sims=rgompertz(100000, shape=log(C), rate=B*C^25)

# estimate of eC50 -> remember that its the expectation E[Tx]=integral(0, Inf){t*pdf(t)dt)
# -> the expected value of first non-central moment
# use the sample mean as an estimator of the expected value
# therefore mean(sims)

mean(sims)

# From the sample, we estimte eC50 = 50.94788
# Using the integral of the survival function, eC50 = 50.91776


#########################################################################################
# Question 3

life.table = data.frame(duration = 0:9, lives = c(100000,72000,51000,36000,24000,15000,10000,6000,2500,0))

# Construct a new column for the survival probability
life.table=cbind(life.table, 0)
colnames(life.table)[3]="Survival Probability"

for (row in rownames(life.table)){
	life.table[row, "Survival Probability"]=life.table[row,"lives"]/life.table[1,"lives"]
	}

# Calculate the number of complete years taht a new grad will complete
# i.e. calculate (becuase of the word complete) the curtate expected future lifetime -> e0
# e0 = sum(1,Inf){tP0} -> sate with 1P0, 2P0, etc. make sur you DONT invluce 0P0...becuase that is just = 1. You're after the future lifetime...not present included

e0=sum(life.table[2:nrow(life.table),"Survival Probability"])
e0

# Require e1 -> ex = 1Px(1+e(x+1))
# 1Px -> 1P0
oneP0=life.table[2, "Survival Probability"]
e1=(e0-oneP0 )/oneP0
e1

life.table=cbind(life.table, 0)
colnames(life.table)[4]="Survival Probability after 1y"
for (j in 2:nrow(life.table)){
	life.table[j,4]=life.table[j,"lives"]/life.table[2,"lives"]
	}

e1=sum(as.double(life.table[3:nrow(life.table), "Survival Probability after 1y"]))
e1


# What this means -> for the graudates that have worked there for a year, and survived, the expected number of additional years
# that they will work for the company is 2.01


#########################################################################################
# Question 4

# Values for mu(x) in AM92 mortality table for age range 17 <= x <= 119

a0=0.00005887
a1=-0.0004988
b0=-4.363378
b1=5.544956
b2=-0.620345

mu=function(x){
	t=(x-70)/50
	a0+a1*t+exp(b0+b1*t+b2*(2*t^2-1))
	# look at this form: poly(t1)+exp(polyt2)) -> Gompertz-Makeham...nice
	}

mu(80) # checked againts mu(80) in the table

plot(17:119, log(mu(17:119)), type="l", xlab="Ages 17 to 119", ylab="Force of Mortality", main="mu(x) for ages 17 to 119")
plot(mu,17,119,log="y", lwd = 3, xlab="Age (x)", ylab="mu(x)", main="Mortality rates for AM92 (log scale)")

# Given that qx can be estimated using 1-exp(-mu(x+0.5))

qx=function(x){
	1-exp(-mu(x+0.5))
	}
	
mortality.table=data.frame(Age=seq(20,110,10))
mortality.table=cbind(mortality.table, 0)
colnames(mortality.table)=c("Age x", "Approx value of qx (AM92)")

for (j in 1:nrow(mortality.table)){
	mortality.table[j,2]=round(qx(mortality.table[j,1]), 6)
	}

# Accuracy is pretty good but diminishes as age increases. This is expected because there will be larger variation
# in the value of mu(x) over the year of age at older ages. The the approximation of using the values in the middle
# of the age range will not as accurate here with older ages. VERY IMPORTANT!

# Calculate approximate value for curtate expectation of life e25

tPx=function(x,t){
	
	if (t == 1){
		p=1-qx(x) }
	else {
		p=1-qx(x)
			for (n in 1:(t-1)){
				p=p*(1-qx(x+n))
				}	
		 }
	p	 
	}	 

t=1:1000
e25=sum(sapply(t,tPx, x=25))
e25


#########################################################################################

# -> packages: flexsurv -> calc densities, prbabilitlies, and quantiles of Gompertz -> Simulate from Gompertz -> Calc the hazard rate and cumulative hazard for Gompertz distribution

# Calculate:
# -> mu_x
# -> tPx
# -> tQx
# -> m_x
# -> e^o_x
# -> e_x

# Calculate mu_x -> Makeham's law
# mu_x = A + BC^x

A = 0.00005
B = 0.000001
C = 1.15

mu =function(x){
	A + B*C^x
	}
mu(50)

mu2 = function(x, A, B, C){
	A + B*C^x
	}
mu2(50, A, B, C)

# Calcualte tPx exactly ->
# tPx = s^t * g^(c^x*(c^t-1))
# - > Makeham's law: mu_x = A + B*C^x

# g = exp(-B / log(C))
# s = exp(-A)

tPx = function(x, t){
	g = exp(-B/log(C))
	s = exp(-A)
	
	s^t * g^(C^x * (C^t - 1))
	}

tPx(40, 5:10)	# -> straight forward so far

# Calculating tPx with integration
# -> Numerically integrate using integrat()
# -> integrate(<function>, <lower limit>, <upper limit>)

integrate(mu, 40, 60) # -> integral(40,60) [mu(s) ds] -> always has error becuase its numerically evaluated
# str(W = integrate(mu, 40, 60)) -> $value to get the integral value

# We can also perform integration on mu2 -> mu2(x, A = ... , B = ... , C = ...) -> 
integrate(mu2, 40, 60, A = 0.00005, B = 0.000001, C = 1.15)
integrate(mu2, 40, 60, A = 0.00005, B = 0.000001, C = 1.15)$value

# We can calculate tPx = exp(-integral(x, x+t)[mu(w) dw] ->
exp(-integrate(mu, 40, 60)$value)

# We can setup this function alot better ->
tPx_int = function(x, t){
	exp(-integrate(mu, x, x+t)$value)
	}
	
tPx_int(40, 20)

# But if we try to input a vector of t -> 5:10 say...problem
tPx_int(40, 5:10) # -> Error in integrate(mu, x, x + t) : length(upper) == 1 is not TRUE

# The error is because of how the integrate() function works -> not allowed to input multiplte values in the upper argument i.e. the upper limit of the integral

# We can use the sapply() function to get R to repeate the integration for each value of t that we input -> sapply() is used to rpeat the same type of calculation ->
# -> sapply(<vector to iterate over>, <custom function>)

# If we cant to iterate over the inputted values of t -> 5:10 -> we do it in the following way:

tPx_int_sapply = function(x, t){
	sapply(t, function(t){exp(-integrate(mu, x, x+t)$value)})
	}

tPx_int_sapply(40, 5:10)
# we can use tPx_int_sapply to calculate just one value of t ->
tPx_int_sapply(40, 20) # -> can label this fucntion as general tPx for exams

# Calculating tQx
# -> tQx = 1 - tPx

1 - tPx(40, 20)
1 - tPx_int_sapply(40, 20)

# Calculating m_x 
# m_x = 1Qx / integral(0, 1)[tPx dt]

# We will use integrate() function -> integrate(tPx, 0, 1, x = 50) -> integrate(mu, 0, 1, <x = 50, other inputs>)
# example -> m50

# 1Q50 ->
q50 = 1 - tPx(50, 1)

# integral(0,10)[tPx dt] ->
integrate(tPx, 0, 1, x = 50)$value

m50 = q50 / integrate(tPx, 0, 1, x = 50)$value

# Careful here in general -> when trying to evaluate q50 / integrate(tPx_int, 0, 1, x=50) -> Error in integrate(mu, x, x + t) : length(upper) == 1 is not TRUE
# Because R is trying to numerically evaluate an integral of a function that uses the integrate() function
# Try to analytically solve for the integral and get tPx in functional form without the integral -> then using the integrate function on tPx -> won't get error

# Calculating e^o_x using integration
# e^o_x = integral(0, Inf)[tPx dt]

ec40 = integrate(tPx, 0, Inf, x = 40)$value
# We can use Inf in the upper limit in this form -> R knows what to do
# However using tPx_int_sapply() -> R can intepret Inf in the sapply() function
# Alternatively just use a large upper limit -> eg. 100

ec40 = integrate(tPx_int_sapply, 0, 100, x = 40)$value

# Calculating e_x using summation
# e_x = sum(t=1, Inf)[tPx]

sum(tPx(40, 1:Inf)) # -> Get this error: Error in 1:Inf : result would be too long a vector
# Inf is too large a number -> use something reasonable

e40 = sum(tPx(40, 1:100))
# -> 40.18883
# We can expect this number to be roughly 0.5 lower than ec40 -> 40.68881 ... looks good

# We can use sapply() function here ->
e40 = sum(tPx_int_sapply(40, 1:100))
# -> 40.18883

## Plotting functions with one and two inputs
# Plot with one input -> plot(<x values>, <y values>, type = "l")

# Example
xvalues = seq(0, 100, 0.1)
plot(xvalues, mu(xvalues), type = "l", lwd = 3, xlab = "Age", ylab = "Force of Mortality", main = "Plot of mu veruss age")

# Another way: plot(<function>, <start>, <end>)
plot(mu, 0, 100, type = "l", lwd = 3, xlab = "Age", ylab = "Force of Mortality", main = "Plot of mu veruss age")

# Another way: curve(<function>(x), <start>, <end>)
curve(mu(x), 0, 100, type = "l", lwd = 3, xlab = "Age", ylab = "Force of Mortality", main = "Plot of mu veruss age")

# Plot with multiple inputs -> CANT USE plot(<function>, <start>, <end>) -> doesn't allow to specify all inputs
# Therefore we need to either use:
# -> plot(xvalues, yvalues) or
# -> curve(function(x, fixed arguments), start, end)

xvalues = 0:100
plot(xvalues, tPx(40, xvalues), type = "l", lwd = 3, xlab = "t", ylab = "Force of Mortality", main = "Plot of mu veruss age")

curve(tPx(x, x=40), 0, 100,  type = "l", lwd = 3, xlab = "t", ylab = "Force of Mortality", main = "Plot of mu veruss age") # -> here x=40 is fixed, R know is hard coded to keep the varying variable labelled x ->
# -> you tried to change the x to t because that makes sense but R is hard coded to keep the variable as x -> don't confuse this x with age z = 40

## Survival Distribution Calculations
# Future lifetimes times follow an exponential distribution -> Tx ~ Exp(mu)

plot(seq(0, 100, 0.1), dexp(seq(0, 100, 0.1), 0.05), type = "l", lwd = 3, xlab = "t", ylab = "pdf of Tx", main = "PDF of Tx ~ Exp(0.05)")

# P(Tx <= 25) -> 25Qx -> 1 - exp(-25*0.05)
1 - exp(-25*0.05)
pexp(25, 0.05)

# P(Tx > 20) -> exp(-20*0.05)
exp(-20*0.05)
pexp(20, 0.05, lower=FALSE)

# P(20 < Tx < 30) -> F(30) - F(20)
pexp(30, 0.05) - pexp(20, 0.05)

# Simulating survival times
# Simulate 1000 sample lifetimes from Exp(0.05) ->
set.seed(123)
sims = rexp(1000, 0.05)
head(sims)
mean(sims) # -> should be close to 1 / 0.05 -> 20

hist(sims, xlab = "survival times", main = "Histogram of sample survival times")

# Weibull Distribution
# Tx ~ Weibull(a, b)
# R uses the following form in the built in functions: Tx ~ Weibull(shape, scale)
# shape -> b
# scale -> a^(-1/b)

a = 0.00025
b = 2.3

shape = b
scale = a^(-1/b)

shape; scale

# Weibull in R -> weibull(<shape>, <scale>)

dweibull(0.5, shape, scale)

plot(seq(0, 100, 0.1), dweibull(seq(0, 100, 0.1), shape, scale), type = "l", xlab = "t", ylab = "f(t) density", main = "PDF of T ~ Weibull(shape, scale) in R")

# Cumulative distribution function of Survival times ->
# If Tx ~ Weibull(a, b) -> in R -> Weibull(shape, scale)
# P(Tx > t) = exp(-a*t^b)
# P(Tx <= t) = 1- exp(-a*t^-b)

shape = b
scale = a^(-1/b)

pweibull(25, shape, scale) # -> P(Tx <= 25)
pweibull(25, shape, scale, lower=FALSE) # -> P(Tx > 25)

# VERY IMPORTANT WHEN USING WEIBULL -> IF YOU ARE USING R PARAMETRIZATION -> Shape and Scale

# Simulating Survival times from Weibulll distribution
set.seed(123)
sims = rweibull(1000, shape, scale)

hist(sims, xlab = "Survival Times", main = "Histogram of Tx ~ Wiebull(shape, scale)")
curve(dweibull(x, shape, scale), 0, 90, type = "l", col = "blue")

# Alternative Functions for Weibull Distribution -> Using the convention T ~ Weibull(c, g)

rweibull2 = function(n, c, g){
	(-log(1 - runif(n))/c)^(1/g)
	}

dweibull2 = function(x, c, g){
	c * g * x^(g - 1)*exp(-c * x^g)
	}

pweibull2 = function(q, c, g){
	1 - exp(-c * q^g)
	}

qweibull2 = function(p, c, g){
	(-log(1 - p)/c)^(1/g)
	}

dweibull2(0.5, a, b)

# P(Tx < 25) ->
pweibull2(25, a, b)

# Simulating Survival times from T ~ Weibull(a, b)
set.seed(123)
sims = rweibull2(1000, a, b)
hist(sims, xlab = "Survival Times", main = "Histogram of T ~ Weibull(a, b) where F(t) = 1 - exp(-a*t^-b)")

# Expected Lifetime Calculations ->
# e^o_x or eox
# We want to calculate the expected future lifetimes under exponential and Weibull Models

tPx_exp = function(t, lambda){
	exp(-lambda*t)
	}

#tPx_weibull = function(x, t, a, b){
#		exp(-a*((x+t)^b - x^b))
#		}

tPx_weibull = function(t, c, g){
		exp(-c*(t)^g )
		}

integrate(tPx_exp, lambda = 0.05, 0, Inf)$value
integrate(tPx_weibull, c = a, g = b, 0, Inf)$value

integrate(pexp, 0, Inf, rate = 0.05, lower.tail=FALSE)$value # -> Note: the functional form here ahs lower.tail = FALSE and not lower = FALSE
integrate(pweibull, 0, Inf, shape = b, scale = a^(-1/b), lower.tail=FALSE)$value # -> used the R Weibull funcion to do this -> try using the customized weibull -> pweibull2 but no lower.tail = FALSE can be used

# R wont integrate a functional form (1 - tPx) -> it must be a function tPx only
# Also, lower.tail argument only avaialble for R functions / distributions

# Also, expected future lifetime -> E(Tx) -> whcih we know already for the distributions:
# -> Exp(0.05) -> E(Tx) = 1/ 0.05 -> 20
# -> Weibull(c, g) -> E(Tx) = Gamma(1 + 1/g) * c^(-1/g)

# Expected Lifetime Calculations ->
# ex
# We want to calculate the kurtate expected future lifetimes under exponential and Weibull Models

# Tx ~ Exp(0.05)
sum(tPx_exp(1:1000, 0.05))
sum(1-pexp(1:1000, 0.05))
sum(pexp(1:1000, 0.05, lower.tail=FALSE))

# Tx ~ Weibull(c, g)
sum(tPx_weibull(1:1000, a, b))
sum(1-pweibull(1:1000, shape = b, scale = a^(-1/b)))
sum(pweibull(1:1000, shape = b, scale = a^(-1/b), lower.tail=FALSE))

# As before, we expect the difference between the expected future lifetimes and jurtaet expected lifetimes to be 0.5 -> e^o_x = e_x +0.5

# R Code - flexsurv package
# We use the flexsurv package to load the Gompertz distribtuion

# Gompertz's law -> mu_x = B*C^x
# Makehame's law -> mu_x = A + B*C^x

install.packages("flexsurv")
library(flexsurv)

# Once the libary is loaded -> flexsurv -> we have access to dgompertz(), pgompertz(), rgompertz(), qgompertz()
# R parametrizes like Weibull -> shape and Scale
# shape = log(C)
# scale = B

# Careful not to miz up with Makeham

B = 0.000003
C = 1.2

shape = log(C)
rate = B

# Assuming mortality follows Gompertz's law -> generally only appropriate for middle and older ages
# As an example -> we assum mortality for lives agred 30 and older -> mu_x = B*C^(x-30)

# T30 ~ Gompertz(shape = log(C), rate = B)
# T(30 + a) ~ Gompertz(shape = log(C), rate = B*C^a) -> makes sense

# Probability Density Function and Surival Times
# dgompertz(<shape>, <rate>) -> remember this functional form for GOMPERTZ -> shape and rate

dgompertz(0.5, shape, rate)

curve(dgompertz(x, shape, rate), 0, 100, type = "l", col = "blue", xlab = "survival times", ylab = "Gompertz probability density", main = "PDF of Tx ~ Gompertz(shape = log(C), rate = B)")
# from now on we will use T30 ~ Gompertz(shape, rate) as the example distribution -> x - 30 for x > 30
# Note that T(30 + a) ~ Gompertz(shape = log(C), rate = B*C^a))

# Cumulative Density Function of Survival Times
# If mu_x = B*C^x 
# -> tPx = g^(C^(x-30) * (C^t - 1)) where:
# -> g = exp(-B / log(C))

# pgompertz(shape, rate) -> CDF of Gompertz - P(Tx<=t)

# P(T30 <= 50) ->
pgompertz(50, shape, rate)

# If we use the tPx = g^(C^(x-30) * (C^t - 1)) definition -> P(T30 <= 50)
g = exp(-B / log(C))
1 - g^(C^(30 - 30) * (C^50 - 1)) # -> x - 30 -> 30-30 = 0 -> t = 50

# Now let calculate probabilities for a different age -> x = 40
# T40 = T (30 + 10) ~ Gompertz(shape = log(C), rate = B*C^10)

shape2 = log(C)
rate2 = B*C^10

pgompertz(50, shape2, rate2)
g = exp(-B*C^10 / log(C))
1 - g^(C^(40 - 40) * (C^50 - 1)) # -> x - 40 -> 40-40 = 0 -> t = 50

# P(T30 > 20)
shape = log(C)
rate = B
pgompertz(20, shape, rate, lower=FALSE)
1 - pgompertz(20, shape, rate)

# P(20 < T35 < 30)
# T35 = T(30 + 5) ~ Gompertz(shape = log(C), rate = B*C^5

shape3 = log(C)
rate3 = B*C^5

# P(T35 < 30)
P30 = pgompertz(30, shape3, rate3)
g = exp(-B*C^5 / log(C))
1 - g^(C^(35 - 35) * (C^30 - 1)) # -> 35 - 35 -> 0 -> t = 30

# P(T35 < 20)
P20 = pgompertz(20, shape3, rate3)

P30 - P20
# -> 0.008103911

# Quantiles -> Always remember for flexsurve and Gompertz -> shape, rate and T(x + a) ~ Gompertz(shape = log(C), rate = B*C^a))

# Simulating Survival Times
# Simulate 1000 future lifetime values for lives currently agre 32 -> a = 2
shape4 = log(C)
rate4 = B*C^2

set.seed(123)
sims = rgompertz(1000, shape4, rate4)

# Hazard Rate -> flexsurv has the Gompertz hazard rate built in 
# mu_x = B*C^x
# In our example -> hgompertz(<x - 30>, <shape>, <rate>)

hgompertz(20, shape, rate) # -> m_50 -> 30 + 20
# Checking the formula -> mu_x = B*C^(x-20)
mu50 = B*C^(20)

plot(seq(30:100), hgompertz(seq(30:100), shape, rate), type = "l", col = "green", xlab = "age", ylab = "Force of Mortality", main = "Graph of mu_x versus age")

# Integrated Hazard -> Hgompertz(t, shape, rate)
# Returns the integrated hazard rate -> Integral of the hazard

# integral(30, 30 + t)[mu_x dx]
# -> integral(30, 30 +t)[B*C^(x-30) dx]

# We can use the integrated hazard in CDF and Survival probability calculations ->
# P(T30 <= 50)
1 - exp(-Hgompertz(50, shape, rate)

# P(T40 <= 50)
rate5 = B*C^10
1 - exp(-Hgompertz(50, shape, rate5))

# Expected Lifetimes -> Gompertz

# Gompertz tPx function -> required to calculated intergral(0, Inf)[tPx dt]

tPx = function(x, t){
	g = exp(-B / log(C))
	g^(C^(x-30)*(C^t - 1))
	}

# we can also use pgompertz -> 1 - pgompertz(t, shape, rate)

# Calculate e^o_40 - ec40
ec40 = integrate(tPx, x = 40, 0, Inf)$value

# Alternatively ->
integrate(pgompertz, shape = log(C), rate = B*C^10, 0, Inf, lower.tail=FALSE)$value # -> you see how x not in these built in functions -> careful!

# curtate future lifetime ->
sum(tPx(40, 1:1000))
sum(pgompertz(1:1000, shape = log(C), rate = B*C^10, lower.tail=FALSE))
# -> As expected, curtate is 0.5 less the ec40 -> e^o_x = e_x + 0.5

#########################################################################################
# Question 1

# T40 ~ Weibull(c, g)

c = 0.00001
g = 3.5

set.seed(100)

shape = g
scale = c^(-1/g)

sims = rweibull(10000, shape, scale)
head(sims)

# Estiamte P(T50 > 10)
p = length(sims[sims > 20]) / length(sims[sims > 10])
p # -> 0.7277898

mu = function(x){
	c * g * (x-40)^(g -1)
	}

p2 = exp(-integrate(mu, 50, 60)$value)
p2 # -> 0.7216983

# The answers differ due to sampling error in the simulation -> if we use generate a larger simulated sample, the error will decrease and get close to the analystical value
# Also bear in mind the numerical integral in p2 has error too -> integrating mu over (50, 60) has error  < 3.6e-15 -> which we can accept as fairly accurate

#########################################################################################
# Question 2

library(flexsurv)

# Mortality of group of lives assumed to follow Gompertz Law for ages 25 and over -> x>=25
# mu_x = B*C^(x-25)

B = 0.00000729
C = 1.128

shape = log(C)
rate = B

plot(seq(0:100), hgompertz(seq(0:125), shape, rate), type = "l", col = "blue", xlab = "survival time", ylab = "Force of Mortality", main = "Hazard rate function of T25 ~ Gompertz(shape, rate)")
			#xlim = c(25, 100), ylim = c(0, 0.07))


mu_g = function(x, t){
	B*C^(x-25+t)
	}

plot(seq(0:100), mu_g(25, seq(0:100)), type = "l", col = "blue", xlab = "survival time", ylab = "Force of Mortality", main = "Hazard rate function of T25 ~ Gompertz(shape, rate)")

# T45 -> T(25 + 20)
rate2 = B*C^20

plot(seq(0:100), dgompertz(seq(0:100), shape, rate2), type = "l", col = "blue", xlab = "survival time", ylab = "Probablity Ddensity", main = "PDF of T45 ~ Gompertz(shape, rate2)")

# Calculate e^o_50 -> eC50
# ec50 = integral(0, Inf)[tP50 dt]

tPx = function(x, t){
	g = exp(-B / log(C))
	g^(C^(x - 25)*(C^t - 1))
	}

ec50 = integrate(tPx, x = 50, 0, Inf)$value

shape = log(C)
rate3 = B*C^25

set.seed(50)
sims = rgompertz(100000, shape, rate3)
head(sims)

ec50_num_g = integrate(pgompertz, shape = log(C), rate = B*C^25, 0, Inf, lower.tail = FALSE)


tPx_G = function(t){
	pgompertz(t, shape, rate3, lower=FALSE)
	}

ec50_num = sum(tPx(50, 1:1000))
ec50_num_G = sum(tPx_G(1:1000)) # -> this is the curtate future lifetime

# Estimate ec50 as the mean of the sample sims ->
mean(sims) # -> Remember E(Tx) -> integral(0, Inf) [t* pdf(t) df]
# what is in sims is not the same thing as the support of t -> sims can have duplicates -> sims are simulated numbers from the support T ~ Gompertz(shape, rate)

#########################################################################################
# Question 3

life.table = data.frame(duration = 0:9, lives = c(100000,72000,51000,36000,24000,15000,10000,6000,2500,0))

for (j in 1:(length(life.table$duration) - 1)){
	life.table$SurvProb[j+1] = life.table$lives[j+1] / life.table$lives[j]
	}
life.table$SurvProb[1] = 1

for (j in 1:length(life.table$duration)){
	life.table$SurvProb[j+1] = life.table$lives[j+1] / life.table$lives[1]
	}
life.table$SurvProb[1] = 1

# Expected number of complete years -> e0
e0 = sum(life.table$lives[-1] / life.table$lives[1])
# -> 2.165

# 45 of the graduates that started working at the company on 1 Sep last year are still at the company exactly 1y later
# Calculate expected number of complete additional that the grads will complete with the company

life.table2 = data.frame(duration = 0:8, lives = c(72000,51000,36000,24000,15000,10000,6000,2500,0))
e0_2 = sum(life.table2$lives[-1] / life.table2$lives[1])
# -> 2.006944

e0 / (life.table$live[2] / life.table$live[1]) - 1 # -> ex = px * (1+ e(x+1))
# -> 2.006944

#########################################################################################
# Question 4

# Value of mu_x from AM92 for ages in range 17 <= x <= 119 are calculated using ->

a0 = 0.00005887
a1 = -0.0004988
b0 = -4.363378
b1 = 5.544956
b2 = -0.620345

# for t = (x - 70) / 50

mu = function(x){
	t = (x - 70) / 50
	a0 + a1*t + exp(b0 + b1*t + b2*(2*t^2-1))
	}
	
mu(80) # -> 0.06827114

plot(seq(17:119), mu(seq(17:119)), log = "y", type = "l", col = "blue", xlab = "age x from 17 -> 119", ylab = "Force of Mortality", main = "Plot of log-scale mu(x) for ages 17 to 119")
# To plot on log scale -> include log = "y" in plot

# Given that 1qx = 1 - exp(-mu(x + 0.5)) -> approximation

AM92 = data.frame(age = seq(20, 110, 10), qx = NA)
AM92$qx = round(1- exp(-mu(AM92$age + 0.5)), 6)
# Refer to AM92 table in Formula and Tables pages 78 and 79
# There are accurate values for q20, q60, q110
# The approximations are very good -> accurate to 4 decimal places however accuracy diminished as the age increases
# -> This is expected because there will be a larger variation in the values of mu(x) over the range of age at older ages ->
# The approximation of using the value for middle age range will not be as accurate here

# Calculate e25

tPx = function(x, t){
	
	M = 1
	for (j in 1:t){
		M = M * exp(-mu(x + (j-1) + 0.5))
		}
	M
	}

e25 = sum(sapply(1:1000, tPx, x = 25))
# -> 53.61316
# -> AM92 e25 = 53.609
# Small error in approximation -> sufficient enough
