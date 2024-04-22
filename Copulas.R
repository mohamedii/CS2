# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\17 Copulas")

# Exercise 1
# Clayton Coupula

clayton=function(u, v, alpha){
	(u^(-alpha) + V^(-alpha) - 1)^(-1/alpha)
	}

n=1000

set.seed(1729)
u=runif(n, 0, 1)
w=runif(n, 0, 1)

alpha = 2

v=((w^(-alpha/(alpha+1)) - 1)*u^(-alpha) + 1)^(-1/alpha)

clay.sims=data.frame(U=u, V=v)
head(clay.sims, 5)

par(mfrow=c(2,1))
plot(clay.sims$U, clay.sims$V)
plot(u,w)
par(mfrow=c(1,1))

# There is clear signs of lower tail dependence with the Clayton Copula -> The reference plot below shows u, w are fairly independent

# Given the theoretical value of Kendall's rank correlation coefficient = 0.5 and theoretical value of Spearman's rank correlation = 0.6829

kendalls.tau = cor(clay.sims$U, clay.sims$V, method="kendall")
spearmans.rho = cor(clay.sims$U, clay.sims$V, method="spearman")

kendalls.tau; spearmans.rho

# As expected, all 3 correlation coefficients are positive -> look at the slope of the scatterplot -> the expectation is a positive relationship
# Kendalls tau and Spearmans rho are close to theoretical values -> good
# Variation is due to the sample size

# Exercise 2

# X ~ N(100, 400) - sig=sqrt(400)
# Y ~ Gamma(3, 0.1)

# The joint distribution of X and Y initially modelled as independent
n=1000

set.seed(1337)
x = rnorm(n, 100, sqrt(400))
y = rgamma(n, 3, 0.1)

head(x); head(y)

plot(x,y)

xy.sim = data.frame(X.sim=qnorm(clay.sims$U, 100, sqrt(400)), Y.sim=qgamma(clay.sims$V, 3, 0.1))
head(xy.sim)

x.sim=xy.sim$X.sim; y.sim=xy.sim$Y.sim

par(mfrow=c(2,1))
plot(x,y)
plot(x.sim, y.sim)
par(mfrow=c(1,1))

# The indepedence scatterplot hardly shows an association
# -> The use of the Clayton Coula has introduce lower tail dependence
# -> There is now a broad association between the variables
# -> However, from the plot, we see the association weaken (look at top right hand corner) -> inidication of no upper-tail dependence
# -> Even though there is lower tail dependence, it isn't as extreme as the first Clayton graph we showed -> This is due to the marginal distributions of the RVs
# -> Be careful -> even though we used Clayton with alpha = 2, it doesn't mean that we've pulled the the joins distribution to towards the lower left ->
# -> rather more likely to experience low values of both X and Y -> When X is low, likely that Y is low (and vice versa)



# List all objects in the current workspace
ls()

# Clear all variables
rm(list = ls())

# Verify that all variables have been cleared
ls()


#########################################################################################

# Exercise 1
# Bivariate Clayton Copula ->
# C(u, v) = (u^-a + v^-a - 1)^(-1/a)


clay.pair = function(n, a){

	sim = data.frame(u = numeric(n), v = numeric(n))

	sim$u = runif(n, 0, 1)
	w = runif(n, 0, 1)

	sim$v = ((w^(-a/(a+1)) - 1) * sim$u^(-a) + 1 )^(-1/a)

	sim
	}
	
# (u,v) -> simulated pair from Clayton copula with parameter a > 0


set.seed(1729)
clay.sims = clay.pair(1000, a = 2)

head(clay.sims, 5)

plot(clay.sims$u, clay.sims$v, xlab = "u", ylab = "v", main = "Scatterplot of u and v where U ~ U(0,1) and V generated using Bivariate Clayton Copula")
# Should expect lower tail dependence with Clayton Copula -> observed
# Gumbel -> Upper tail dependence
# Frank -> No tail dependence

a = 2
k.tau = 0.5 # theoretical value
spear.rank = 0.6829 # theoretical value

# from the simulated values ->
clay.sims.k.tau = cor(clay.sims, method = "kendall")
clay.sims.spear.rank = cor(clay.sims, method = "spearman")
clay.sims.pearson = cor(clay.sims, method = "pearson")

clay.sims.k.tau # -> 0.4876917
clay.sims.spear.rank # -> 0.6690138
clay.sims.pearson # -> 0.667249

# Values are close to the theoretical -> may differ because of sampling error and that generating independent random numbers on a pc is not truly independent
# All values are positive and less than one -> shows a positive relationship between u and v
# Also check the sample size -> sampling error from a small sample size

# X ~ N(100, 400) and Y ~ Gamma(3, 0.1)
# marginal RVs 

# Initially will model the joint distribution as being independent

set.seed(1337)

x = rnorm(1000, 100, sqrt(400))
y = rgamma(1000, 3, 0.1)

sim.xy = data.frame(x, y)
head(sim.xy, 5)
plot(sim.xy$x, sim.xy$y, xlab = "x", ylab = "y", main = "Scatterplot of x and y where X ~ N(100, 400) and Y ~ Gamma(3, 0.1)")
# Bunching of (x,y) pairs -> not truly independent but close enough


# Model joint distribution of X, Y using Clayton Bivariate Copula with a = 2
# Use clay.sims (u, v) values to generate variates for X and Y ->

clay.sim.xy = data.frame(x = qnorm(clay.sims$u, 100, sqrt(400)), y = qgamma(clay.sims$v, 3, 0.1))
head(clay.sim.xy, 5)

plot(clay.sim.xy$x, clay.sim.xy$y, xlab = "x", ylab = "y", main = "Scatterplot of x and y where X and Y are generated from clay.sims (u, v) Copula")
# Identify the lower tail dependence in X, Y 
# Obvious positive relationship between X and Y but for larger values of x any y the association fades a bit -> No upper tail dependence!


par(mfrow = c(3, 1))

plot(clay.sims$u, clay.sims$v, xlab = "u", ylab = "v", main = "Scatterplot of u and v where U ~ U(0,1) and V generated using Bivariate Clayton Copula")
plot(sim.xy$x, sim.xy$y, xlab = "x", ylab = "y", main = "Scatterplot of x and y where X ~ N(100, 400) and Y ~ Gamma(3, 0.1)")
plot(clay.sim.xy$x, clay.sim.xy$y, xlab = "x", ylab = "y", main = "Scatterplot of x and y where X and Y are generated from clay.sims (u, v) Copula")

par(mfrow = c(1, 1))

# List all objects in the current workspace
ls()

# Clear all variables
rm(list = ls())

# Verify that all variables have been cleared
ls()
