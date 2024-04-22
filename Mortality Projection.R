# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\12 Mortality Projection")

#########################################################################################
# Question 1

# Fitting a model ->
# - Fit LC using approximate formulae for parameter estimates
# - Fit LC model using SVD
# - Fit LC using gnm package

# Projecting Mortality ->
# - Project mortality using LC
# - Project mortality using expecation approach

rates = read.table("mort_rates.txt", header = TRUE, row.names = 1)

# LC model is to be fitted to the data -> log(m(x,t)) - ax + bx*kt + err(x,t)

log.rates=log(rates)

# ax = (1/n) * sum(all t for each x){log(m(x,t))}

ax=numeric(ncol(log.rates))

for (j in 1:ncol(log.rates)){
	ax[j]=(1/nrow(log.rates))*sum(log.rates[,j])
	}

temp=matrix(0,nrow(log.rates), ncol(log.rates), dimnames=list(rownames(rates), colnames(rates)))
for (x in 1:length(ax)){
	temp[,x]=log.rates[,x]-ax[x]
	}

log.centred=data.frame(temp)

# to Calculate kx -> sum across all x for each log.centred -> sum(all x for each t){log.centred}
# -> should have nrow(log.rates) values for kt

kt=numeric(nrow(log.rates))

for (i in 1:nrow(log.rates)){
	kt[i]=sum(log.centred[i,])
	}


bx_=function(log.centred, kt){
	
	temp=numeric(ncol(log.centred))
	
	for (j in 1:ncol(log.centred)){
		temp[j]=sum(kt*log.centred[,j])/sum(kt^2)
		}
	temp 
	}
	
bx=bx_(log.centred, kt)

sum(kt) # expect this = 0
sum(bx) # expect this = 1

# We need to perform a regression for each age X -> lm(log.centred[ ,x] ~ kt)
# Need to remove the coefficient by lm(y ~ x - 1)

lm.60=lm(log.centred[,"X60"] ~ kt - 1)
lm.61=lm(log.centred[,2] ~ kt - 1)
lm.62=lm(log.centred[,3] ~ kt - 1)
lm.63=lm(log.centred[,4] ~ kt - 1)
lm.64=lm(log.centred[,5] ~ kt - 1)
lm.65=lm(log.centred[,6] ~ kt - 1)

# lm.age$coefficients

# Here is a quicker way of doing it
output = sapply(1:6, function(i) {lm(log.centred[, i] ~ kt - 1)})
output # -> look at the coefficients

# The output here is for bx, the coefficents from the regression are determined via OLS
# Unsrpsiing that the regression coefficients determined match the bx solved -> the equations to solve for bx, kt come from OLS minimization

kt_=function(log.centred, bx){
	temp=numeric(nrow(log.centred))
	
	for (i in 1:nrow(log.centred)){
		temp[i]=sum(bx*log.centred[i,])/sum(bx^2)
		}
	temp 
	}
	
# The constraint for the estimate using kt -> bx using the equations will be different to using bx from the regression -> kt
# This is because of the errors -> using kt = sum (centred logs for all x) assumes the sum of the errors are zero
# But when using bx to determine the kt, the regresson coefficients for bx are constraint 1 but the sum of the error terms are embedded in kt

#########################################################################################
# Question 2

# The estimates of bx, kt that satisfy the OLS equations can be determined using SVD
# We perform the SVD on the matrix of log-centred values -> log.centred

# Let M = log.centred

M=log.centred

# SVD identifies 3 matrices -> U, D, and values]
# M = U D V'
# V' is the tranpose of V
# Its imporant taht you get the dimensions right

mort.svd=svd(M) # -> SVD the centred.log matrix
str(mort.svd)

M
mort.svd$u %*% diag(mort.svd$d) %*% t(mort.svd$v)

# kt from the svd will be the first column of U
kt.svd=mort.svd$u[ ,1]

# and bx will be the first singular value, d1 fom d -> esesntially the first row of D -> [d1 0 0 0 0] -> d1 the first singular value
# Remember that D is a diagonal matrix, so the singluar values lie on the diagonal
# bs will be the first singular value d1 mulitplied by the first column of V -> bx = d1*V[ 1]
bx.svd=mort.svd$d[1]*mort.svd$v[,1]

# Check if bx = bx_(M, kt.svd)
bx_(M, kt.svd)
bx.svd

# Check if kt = kt_(M, bx.svd)
kt_(M, bx.svd)
kt.svd

# Therefore equations are satified

sum(bx.svd) # -> doesn't sum to one...but we can force it -> rescale to 1
sum(kt.svd)

kt.svd=kt.svd*sum(bx.svd)
sum(kt.svd) # -> We must first scale the kt.svd with old sum(bx.svd)...otherwise the equations wont balance

bx.svd=bx.svd/sum(bx.svd) # -> we can now rescale to 1 but remember...first adjust kt.svd with bx.svd before update the bx.svd
sum(bx.svd)

# Check if kt = kt_(M, bx.svd)
kt_(M, bx.svd)
kt.svd

# Check if bx = bx_(M, kt.svd)
bx_(M, kt.svd)
bx.svd

#########################################################################################
# Question 3

# We will now use the gnm package to perform MLE to determine bx, kt

install.packages("gnm")
library(gnm)

mort.df=read.table("mort_rates_df.txt", header = TRUE)

# Add a column to mort.df that has the centred log values -> log(m(x,t)) - ax
mort.df$lxt.centred=log(mort.df$mxt)-rep(ax, each=5)

# Run code to fit non-linear model -> log(m(x,t) - ax = bx*kt +err(x,t)
set.seed(1)
gnm.LC = gnm(lxt.centred ~ Mult(as.factor(age), as.factor(year)) - 1, data = mort.df)

bx.gnm = gnm.LC$coefficients[1:6]
kt.gnm = gnm.LC$coefficients[7:11]

sum(bx.gnm) # -> You'll see this doesn't sum to 1
sum(kt.gnm) # -> This is ok but we will need to rescale to get sum(bx) - >1

kt.gnm=kt.gnm*sum(bx.gnm)
bx.gnm=bx.gnm/sum(bx.gnm)

sum(bx.gnm) # -> 1
sum(kt.gnm) # -> very very close to 0

# Check if kt = kt_(M, bx.svd)
kt_(M, bx.gnm)
kt.gnm

# Check if bx = bx_(M, kt.svd)
bx_(M, kt.gnm)
bx.gnm

#########################################################################################
# Question 4
# Use estimates of bx, kt from Question 2

bx
kt

times=c(rownames(rates))
mort.LC60=exp(ax[1]+bx[1]*kt)
plot(times,rates$X60, xlab="Year", ylab="Mortality Rate", main="Mortality Rate for Age 60", type="l", col="blue")
lines(times, mort.LC60, type="l", lty=2, col="red")

mu=(1/(length(kt)-1))*sum(diff(kt)) # -> Be areful with mu calculation. Remember that we are averaging the increments i.e. first difference
# So the number to average over is the number of increments and NOT length(kt)

k2014=kt[length(kt)]
l=seq(1,16,1)

k.forecast=k2014+l*mu

k=c(kt, k.forecast)
intervalf=2014+seq(1,16,1)
interval=c(rownames(rates), intervalf)

plot(2010:2030, c(kt, k.forecast), type = "b", col = "red", lwd = 2, main = "Fitted and projected values of kt", xlab = "Year", ylab = "kt")
lines(2010:2014, kt, col = "black", lwd = 2, type = "b")
legend("topright", legend = c("estimated kt", "projected kt"), col = c("black", "red"), lwd = 2)

mort.LC60f=exp(ax[1]+bx[1]*k.forecast)
mort.LC60=exp(ax[1]+bx[1]*kt)

plot(2010:2030, c(mort.LC60, mort.LC60f), type = "l", col = "blue", lwd = 2, main = "Fitted and projected values of Mortality using LC", xlab = "Year", ylab = "m(60,t)")
lines(2010:2014, mort.LC60, col = "red", lwd = 2, type = "l")
lines(2010:2014, rates$X60, col="black", type="l", lty=2)
legend("topright", legend = c("fitted m(60,t)", "projected m(60,2014+t)", "Observed m(60,t)"), col = c("blue", "red", "black"), lwd = 2)

#########################################################################################
# Question 5

ELT=read.csv("ELT.csv")

# q(x,t) = q(x,0)*R(x,t) -> q(x,0) is today's mortality table!!
# R(x,t) = alpha(x) +(1-alpha(x))*(1-f(n,x))^(t/n)
# n=20

alpha=function(x){
	if (x<=60){
		0.13 }
	else if (x>=110){
		1	}
	else {
		m=(1-0.13)/(110-60)
		m*(x-60)+0.13 }
	}

f=function(x){
	if (x<=60) {
		0.55 }
	else if (x>=110){
		0.29 }
	else {
		m=(0.29-0.55)/(110-60)
		m*(x-60)+0.55 }
	}

# alpha(x) -> ultimate reduction in morality -> reduction factor that applies to current mortality in the distance future
# f(n,x) -> proportional reduction measured [1-R(x,n))/[1-alpha(x)] -> controls the rate at which
# the projected rates at age x will approach their longer term value -> f(n,x) clsoe to 1  means the rates will converge quickly
# to the long-run or ultimte reduction factor. If f(n,x) is close to zero, convergence will be slow.

R=function(x,t){
	alpha(x)+(1-alpha(x))*(1-f(x))^(t/20)
	}

# Base year for ELT15: 1991
# Base year for ELT17: 2011 -> We will compare projected ELT15, t=20 againts ELT17 actual

# For ELT15, t=20 and t=30 -> 1991:2011 and 1991:2021
# -> q(x,t) = q(x,0)*R(x,t) ... We need to calculate this

ELT.M=ELT[ELT$SEX=="M", ]
ELT.F=ELT[ELT$SEX=="F", ]

# For projection year 2021, t=20 -> 2011-1991
ages=seq(0,100,10)

ReducFac=sapply(ages,R,t=20)
# So now we are going to project todays mortality table to 2011 i.e. t=20
ELT.M$PROJ_2011_ELT15=ELT.M$ELT15*ReducFac
ELT.F$PROJ_2011_ELT15=ELT.F$ELT15*ReducFac

ReducFac=sapply(ages,R,t=30) # -> Projecting now to 2021...calculating the reduction factors
# So now we are going to project todays mortality table to 2021 i.e. t=30
ELT.M$PROJ_2021_ELT15=ELT.M$ELT15*ReducFac
ELT.F$PROJ_2021_ELT15=ELT.F$ELT15*ReducFac

# -> We will compare projected ELT15, t=20 againts ELT17 actual

# For Males
plot(ages, log(ELT.M$ELT17), type="l", xlab="Age", ylab="", las=1, main="Mortality of ELT17 Males", col="blue")
lines(ages, log(ELT.M$PROJ_2011_ELT15), col="red", type="l", lty=2)
legend(10, 0.1, c("ELT17", "Projected"),lty = c(1, 2), col = c("blue", "red"))

# For Females
plot(ages, log(ELT.F$ELT17), type="l", xlab="Age", ylab="", las=1, main="Mortality of ELT17 Males", col="blue")
lines(ages, log(ELT.F$PROJ_2011_ELT15), col="red", type="l", lty=2)
legend(10, 0.1, c("ELT17", "Projected"),lty = c(1, 2), col = c("blue", "red"))

# The pattern is similar for male and female rates
# At the yougest and oldest ages, the projected rates are very close to the actual 2011 ELT17
# However, for both males and females, the middle age range (below 70 years of age) are noticeably lower than the actual rates
# and the for ages greater than 70, the projected rates are higher than actual
# Some underestimation at ages < 70, and overestiamteion at ages > 70


#########################################################################################

# Fitting a model
# -> Fit LC model using parameter estimates
# -> Fit LC model using Singular Value Decompositio SVD
# -> Fit LC using gnm package

# Projecting Mortality
# -> Project Mortality using LC model
# -> Project Mortality using Expectation Approach

#########################################################################################
# Question 1

rates = read.table("mort_rates.txt", header = TRUE, row.names = 1)

log.m = log(rates)
ax = colSums(log.m) / nrow(log.m)

log.centred = as.data.frame(log.m - matrix(rep(ax, 5), nrow = 5, byrow = TRUE))

# Under conditions -> sum[t](kt) = 0 and sum[x](bx) = 1
# -> kt = sum[x](log(m(x,t)) - ax)

kt = rowSums(log.centred)

b_x = function(lc, k){
	t(k)%*%as.matrix(lc) / as.numeric(t(k)%*%k)
		
	}

bx = b_x(log.centred, kt)

sum(kt) # -> 0 as expected
sum(bx) # -> 1 as expected

# We need to perform a regression for each age X -> lm(log.centred[ , x] ~ kt - )
# Need to remove the intercept by lm(y ~ x - 1)

lm.60 = lm(log.centred[, "X60"] ~ kt -1)
lm.61 = lm(log.centred[, 2] ~ kt -1)
lm.62 = lm(log.centred[, 3] ~ kt -1)
lm.63 = lm(log.centred[, 4] ~ kt -1)
lm.64 = lm(log.centred[, 5] ~ kt -1)
lm.65 = lm(log.centred[, 6] ~ kt -1)

# Compare the ceofficients from the linear models to bx above ->
B = c(lm.60$coefficients,
		 lm.61$coefficients,
		 lm.62$coefficients,
		 lm.63$coefficients,
		 lm.64$coefficients,
		 lm.65$coefficients)
		 
bx
# -> Beta cofficients from regression and bx are identical

k_t = function(lc, b){
	as.matrix(lc)%*%t(b) / as.numeric(b%*%t(b))
	
	}

kt.solved = k_t(log.centred, bx)
sum(kt.solved) # -> equal to 0 as expected

# kt and kt.solved are not equal 

#########################################################################################
# Question 2

# The estimates of bx and kt that satisfy equations (I) and (II) in the notes can be found exactly by using SVD on log centred values
# -> The sum(x)[...] and sum(t)[...] equations -> solve for kt and bx

# Let M = log(m(x,t)) - ax
# -> M = U * D * transpose(V)
# Perform SVD in R using svd()

M = log.centred
dim(M) # -> 5x6 matrix

# Expect then that
# -> U: 5x5
# -> D: 5x5 diagonal
# -> tanspose(V): 5x6

mort.svd = svd(M)
mort.svd
# To extract matrices from mort.svd -> str(mort.svd)

# To recover M
mort.svd$u %*% diag(mort.svd$d) %*% t(mort.svd$v) # -> does equal M

u = mort.svd$u[, 1] # -> first column of U
v = mort.svd$v[, 1] # -> first column of V

kt.svd = u
bx.svd = mort.svd$d[1] * v

# Check these inputs in the functions for k_t and b_x from above ->
k_t(M, t(bx.svd)) # -> equal to kt.svd
b_x(M, kt.svd) # -> equal to bx.svd
# we see that kt.svd and bx.svd satisfy equations (I) and (II)

# Checking the constraints ->
sum(bx.svd) # -> 0.1407814 ... doesn't equal 1
sum(kt.svd) # -> 0

# We can force the estimates of bx to equal 1 -> divide each element by the sum
kt.svd = kt.svd * sum(bx.svd)
bx.svd = bx.svd / sum(bx.svd) 
# -> now I understand -> you don't want to change bx*kt -> hence you multiply and divide by sum(bx.svd)

# Check again
sum(bx.svd) # -> 1
sum(kt.svd) # -> 0

# Check again if these estimate satisfy equations(I) and (II)
k_t(M, t(bx.svd))
kt.svd

b_x(M, kt.svd)
bx.svd
# updated kt.svd and bx.svd satify equations (I) and (II)

#########################################################################################
# Question 3

install.packages("gnm")
library(gnm)

mort.df = read.table("mort_rates_df.txt", header = TRUE)
mort.df

log.m = log(mort.df$mxt)

mort.df$lxt.centred = log.m - rep(ax, each = 5) # -> need th each = 5 to repeat

# Fit a non-linear model for the form:
# -> log(m(x,t)) - ax = bx*kt + err(x,t)

set.seed(1)
gnm.LC = gnm(lxt.centred ~ Mult(as.factor(age), as.factor(year)) - 1, data = mort.df) # -> easy to intepret this ... no intercept 
gnm.LC$coefficients
# The fitted coefficents are estimates of bx and kt -> you can see in the output
# These coefficients don't satisfy the constraints: sum(x)bx = 1 and sum(t)kt = 0

# The parameter that ends as.factor(age) -> bx 
# The parameter that ends as.factor(year) -> kt
# Refer to mort.df

bx.gnm = gnm.LC$coefficients[1:6] # -> there are 6 ages
kt.gnm = gnm.LC$coefficients[7:11] # -> there are 5 years

sum(bx.gnm) # -> 0.3213309 does not = 1
sum(kt.gnm) # -> 4.482525e-15 this is ok 

# To get the constaints to their target values -> you don't want to change bx*kt -> hence you multiply and divide by sum(bx.svd)
kt.gnm = kt.gnm * sum(bx.gnm) # -> do this first because if we do it second, you're updating bx.gnm and the original bx*kt will change
bx.gnm = bx.gnm / sum(bx.gnm)

sum(kt.gnm) # -> 1.43982e-15
sum(bx.gnm) # -> 1

# Check if estimates satisfy equations (I) and (II)
k_t(M, t(bx.gnm))
kt.gnm

b_x(M, kt.gnm)
bx.gnm
# updated kt.gnm and bx.gnm satify equations (I) and (II)

# Assume sum[x](err(x,t) = 0 which will not neccessarily be the case
# The SVD should returnt he exact approach and the gnm attempts to find the exact solution to the error minimization problem numerically

#########################################################################################
# Question 4

# Use bx.svd and kt.svd
# Plot m(60, t) and m^(60,t)

t = rownames(rates)
m60 = rates[ , "X60"]
m60.LC = exp(ax[1] + bx.svd[1]*kt)

plot(t, m60, xlab = "Year", ylab = "Mortality", main = "Estimated and LC fitted Mortality for Age 60", col = "blue")
lines(t, m60.LC, col = "red", type = "l")
legend("topright", legend = c("Estimate", "LC"), col = c("blue", "red"), lty = c(NA,1), pch = c(1, NA))

# Random Walk model used to forecast kt for future years ->
dk = mean(diff(kt.svd))

kt.fc = function(k0, dk, l){
	k0 + l*dk
	}

kt.forecast = kt.fc(kt.svd[length(kt.svd)], dk, 1:16) 

t = 2010:2030
plot(t, c(kt.svd, kt.forecast), type = "b", col = "blue", xlab = "year", ylab ="kt", main = "Plot of kt from 2010-2014 and forcasedted kt from 2015-2030")
lines(t[5:21], c(kt.svd[length(kt.svd)], kt.forecast), type = "b", col = "red")
legend("topright", legend = c("kt 2010-2014", "kt fc 2015-2030"), col = c("blue", "red"), lwd = 2)

proj.mort60 = exp(ax[1] + bx.svd[1]*kt.forecast)

plot(t, c(m60, proj.mort60) , xlab = "Year", ylab = "Mortality", main = "Estimated, LC fitted, and Projected Mortality for Age 60", col = "blue", type = "l")
lines(t[1:5], m60.LC, col = "red", type = "l")
lines(t[5:21], c(m60[length(m60)] ,proj.mort60), col = "green", type = "b")
legend("topright", legend = c("Estimate", "LC", "Projected"), col = c("blue", "red", "green"), lwd = 2)

#########################################################################################
# Question 5

ELT = read.csv("ELT.csv")
ELT

# In 1991 it was expected that mortality rates would evolve according to the projection formula
# -> q(x,t) = q(x,0) * R(x,t)
# -> R(x,t) = alpha(x) +(1 - alpha(x))*(1 - f(n,x))^(t/n)
# -> n = 20

n = 20

alpha = function(x){
	if (x <= 60){
		alpha = 0.13
		}
	if (x == 110){
		alpha = 1
		}
	if ((60 < x) & (x < 110)){
		alpha = (1 - 0.13) / (110 - 60) * (x - 60) + 0.13
		}
	alpha
	}

f = function(x){
	if (x <= 60){
		f = 0.55
		}
	if (x == 110){
		f = 0.29
		}
	if ((60 < x) & (x < 110)){
		f = (0.29 - 0.55) / (110 - 60) * (x - 60) + 0.55
		}
	f
	}

# alpha(s) -> ultimate level of mortality reduction
# -> as t tends to Inf, R(x,t) -> alpha(x)
# -> R(x,Inf) = alpha(x) -> reduction factor that is projected to apply to the current mortality rates for age x in the distant future

# f(n,x) controls the rate at which the projected rates at age x will approach their long term valie
# -> if f(n,x) is close to 1, the rates will coverge very quickly
# -> if f(n,x) is close to 0, the rates will converge very slowly

R = function(x, t){
	alpha(x) + (1 - alpha(x))*(1 - f(x))^(t/n)
	}

# Use q(x,t) = q(x,0) * R(x,t)

ELT$ELT15.Proj2011 = ELT$ELT15 * sapply(ELT$AGE, R, t = 20)
ELT$ELT15.Proj2021 = ELT$ELT15 * sapply(ELT$AGE, R, t = 30)

ELT.M = ELT[ELT$SEX == "M", ]
ELT.F = ELT[ELT$SEX == "F", ]

par(mfrow=c(2,1))

plot(ELT.M$AGE, ELT.M$ELT17, type = "l", col = "blue", xlab = "Age", ylab = "Mortality Rate", log = "y", main = "Plot of Male ELT17 and ELT15 Projected to 2011")
lines(ELT.M$AGE, ELT.M$ELT15.Proj2011, type = "b", col = "red")
legend("topleft", legend = c("Male ELT17", "Male ELT15 Projected 2011"), col = c("blue", "red"), lwd = 1)

plot(ELT.F$AGE, ELT.F$ELT17, type = "l", col = "blue", xlab = "Age", ylab = "Mortality Rate", log = "y", main = "Plot of Female ELT17 and ELT15 Projected to 2011")
lines(ELT.F$AGE, ELT.F$ELT15.Proj2011, type = "b", col = "red")
legend("topleft", legend = c("Female ELT17", "Female ELT15 Projected 2011"), col = c("blue", "red"), lwd = 1)

par(mfrow=c(1,1))

# Pattern is similar for the male and female rates
# -> At the youngest and oldest ages, the projected rates are close to the actual rates in 2011 (ELT17)
# -> However for both males and females, the middle age range below 70, the projected ratesa are noticeably lower than the actual rates
# -> Above 70, the projected rates are slighlty higher