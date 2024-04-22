# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\4 & 5 Markov Jump Processes")

# Generator Matrix
A=matrix(c(-1,0.4,0.6,0.5,-1.5,1,0.8,1.2,-2), nrow=3, byrow=TRUE)
states=c("State 1", "State 2", "State 3")
rownames(A)=states
colnames(A)=states

# Check if rows sum to zero
rowSums(A)==0

# For small h:
# ->	p(ij)(h)=mu(ij)*h + O(h)
# ->	p(ii)(h)=1 + mu(ii)*h + O(h)
# Therefore P(ij)(h) = Id + h*A, Id is the Identity Matrix (dim(I)=dim(A))

Ph=function(h, A){
	I=diag(nrow(A))
	Y=I+h*A 
	dimnames(Y)=dimnames(A)
	Y }

# Appromiate P(t) by P(h)^(t/h)
# Can't raise matrix to a power so we assume:
# t/n will be an integer


Pn=function(n, h, A){
	m=n/h	
	B=Ph(h,A)
	M=diag(nrow(A))	
	for (j in 1:m){
		M=M%*%B }
	dimnames(M)=dimnames(A)
	M }

Pt=function(t, h, A){
	Pn(t, h, A) }

# Alternative to writing the code above, use Markov Chain package
# -> install.packages("markovchain")
# -> library(markovchain)

Qt=function(t, h, A){
	mc=new("markovchain", transitionMatrix=Ph(h,A), states=states, name="Ph.mc")
	mc^(t/h) }

# Examples
h=1/12
Ph(h,A)
Pn(h,h,A)
Pn(10,h,A)
Pt(20,h,A)
Qt(20,h,A)

# Simulating time homogenous MJPs
# There are two components to a sample path of MPJ: the order of states visited,
# and the time spent in each state. We simulate each of these components which 
# together make up a sample path from MJP

# Approximate method: Simulated homo MJP by using small time increment h
# -> use P(h) as the transition matrix for a MC with jumps taking place
# every h time units. For instance:

h=0.01
mc1=new("markovchain", transitionMatrix=Ph(h,A), states=states)
# remember we've created an MC. When we simulate, transitions/jumps will take
# place every h time units

set.seed(25)
sims = rmarkovchain(1000, mc1, include.t0=TRUE)
head(sims)

# result: ->  "State 3" "State 3" "State 3" "State 3" "State 3" "State 2"
# so process started in State 3, and stayed there for 5/100 time units
# i.e 5h time units before jumping to State 2


# Exact method: the order of staes visited can be simulated by constructing
# a MC with transition probabilities:
# p(ij) = mu(ij) / lambda(i) for i<>j
# p(ii) = 0
# Now use generatorToTransitionMatrix function from markovchain package
# -> this is a fucntion to conver a generator matrix into a transition matrix

P=generatorToTransitionMatrix(A) # Does the conversion for you i.e.
# -> p(ij)=mu(ij)/lambda(i)
# -> mu(ij)/|lambda(i)| i.e. take mu(ij) and divide by abs(lambda(i))

# Use this transition matrix to construct a MC as before
mc2=new("markovchain", transitionMatrix=P)

# Now we can generate a sample from this MC using rmarkovchain() as before

set.seed(100)
sims2=rmarkovchain(1000, mc2,include.t0=TRUE)
head(sims2)

# result: ->  "State 2" "State 3" "State 2" "State 3" "State 2" "State 3"

# We've simulated the first compenent above. Now to simulate the time spent
# in each of the states. The distribution of the waiting times in each state
# is exponential with parameter equal to the sum of transition rates leaving 
# state i i.e. lambda(i)
# These will be the absolute value of the diagonal entries in the 
# generator matrix -> lambda(i)=abs(diag(A)(i))

# In R, execute the following to the lambda(i)'s

(ljs = abs(diag(A)))

ljs["State 2"]
ljs[2]
# same thing ...

# use the ability to use a vector to extract 
# header info i.e. ljs[list of required states]
# example ljs[c("State 3", "State 1", "State 3", "State 2")]
# result: -> State 3 State 1 State 3 State 2 
#		   2.0     1.0     2.0     1.5 

head(ljs[sims2])

# result: -> State 2 State 3 State 2 State 3 State 2 State 3 
#		    1.5     2.0     1.5     2.0     1.5     2.0 

# Now we can use rexp(n, vector of parms) to simulate the waiting times
# between each state

set.seed(20)
waiting.times=rexp(1000, ljs[sims2])
head(waiting.times)

# result: -> 0.12890834 0.02916369 0.04220462 1.10571660 0.66901533 0.58672267

# So the waiting time being in State 2 follows exp(1.5). The value sampled
# from this distribution is 0.12890834
# The process then jumpes to State 3 and the waiting time (or time spent
# in State 3 follows exp(2). The value sampled for the waiting time in State 3
# is 0.02916369, etc. Note the waiting time here depends on the unit of lambda

# Time Inhomogenous MJP
# Similar to homo MJP but we need to keep track of the generator matrix A
# -> the transition rates are not constant

B=function(t){
	matrix(c(-t, 0.4*t, 0.6*t, 0.5*t, -1.5*t, t, 0.8*t, 1.2*t, -2*t),
		nrow=3, byrow=TRUE, dimnames=list(states, states))
	}

B(0.2)
B(0.5)
B(0.8)

# Approximate P(t,t+h) by Id+h*A(t)
# Therefore P(s,t)=Product(j=0, (t-s)/h - 1){P(s+ h*j)xP(s+ h*(j+1))}
# Start from P(s,s+h)..products in between..P(t-1/h,t)
# So to estimate P(s,t) we need:
# -> Start time s, End time t, subdivision h

Pst=function(s,t,h){
	M=diag(length(states)) # Identitiy matrix
	dimnames(M)=list(states, states)
	n=(t-s)/h -1 
	for (j in 0:n){
		#M=M%*%Ph(h, B(s+h*j)) # Remember P(t,t+h)=Id+h*B(t)
		M=M%*%(diag(length(states))+h*B(s+h*j))
		}
	M }


Pst(1,2,0.1)
Pst(1,2,0.01)
Pst(1,2,0.0001)
Pst(1,2,0.000001)

#########################################################################################
# Question 1

states=c("State 1", "State 2", "State 3") # State 1 = Omonivore, State 2 = Vegetarian, State 3 = Vegan

mu12=0.12
mu13=0.02
mu11=-sum(mu12,mu13)


mu21=0.18
mu23=0.05
mu22=-sum(mu21,mu23)

mu31=0.013
mu32=0.05
mu33=-sum(mu31,mu32)

A=matrix(c(mu11,mu12,mu13,mu21,mu22,mu23,mu31,mu32,mu33),nrow=3, byrow=TRUE, dimnames=list(states,states))

Ph=function(h,A){
	I=diag(nrow(A))
	Y=I+h*A
	dimnames(Y)=dimnames(A)
	Y }
	
Ph(1/12,A)

# h=1/12

Pt=function(t,h,A){
	n=t/h
	M=diag(nrow(A))
	for (j in 1:n){
		M=M%*%Ph(h,A)
		}
	dimnames(M)=dimnames(A)
	M }

Pt(10,1/12,A)[1,2] # -> State 1 to State 2 in 10y time
Pt(10,1/12,A)[1,3] # -> State 1 to State 3 in 10y time
Pt(10,1/12,A)[3,1] # -> State 3 to State 1 in 10y time


mu12B=0.15
mu13B=0.08
mu11B=-sum(mu12B,mu13B)


mu21B=0.09
mu23B=0.07
mu22B=-sum(mu21B,mu23B)

mu31B=0.004
mu32B=0.07
mu33B=-sum(mu31B,mu32B)

B=matrix(c(mu11B,mu12B,mu13B,mu21B,mu22B,mu23B,mu31B,mu32B,mu33B),nrow=3, byrow=TRUE, dimnames=list(states,states))

Pt(10,1/12,B)[1,2] # -> State 1 to State 2 in 10y time
Pt(10,1/12,B)[1,3] # -> State 1 to State 3 in 10y time
Pt(10,1/12,B)[3,1] # -> State 3 to State 1 in 10y time

# See the probabilties change with from analysis of trends. Before trends, omnivore athletes were likely 
# to become either vegetarian or vegan. Read the solution

# Ways to improve the model:
# 1. Time homogenous models are restrictive. It is quite likely that the inclination to become vegan or Vegetarian
# will depend on age because prefernce do change with age
# 2. Factors such as geography, type of athlete, wealth, food allergies, religion influence inclination to change to 
# being a vegan or Vegetarian
# 3. There are only 3 states, restrictive as there are other diet types. For instance, carnivore, paleo, pescatarian, etc
# 4. P(10) could be approximated using a finer subdivison i.e. h=1/365 instead of h=1/12

#########################################################################################
# Question 2

states=("H", "S", "D")

mu12=0.1
mu13=0.05
mu11=-sum(mu12,mu13)


mu21=0.6
mu23=0.2
mu22=-sum(mu21,mu23)

mu31=0
mu32=0
mu33=-sum(mu31,mu32)

A=matrix(c(mu11,mu12,mu13,mu21,mu22,mu23,mu31,mu32,mu33),nrow=3, byrow=TRUE, dimnames=list(states,states))

# P(8)=P(1)^8 if you make the transition step h=1

Ph=function(h,A){
	Id=diag(nrow(A))
	M=Id+h*A
	dimnames(M)=dimnames(A)
	M}

Pt=function(t,h,A){
	n=t/h
	M=diag(nrow(A))
	for (j in 1:n){
		M=M%*%Ph(h,A)
		}
	dimnames(M)=dimnames(A)
	M }
	
Pt(8,1,A)
Pt(8,0.5,A)
Pt(8,0.25,A)
Pt(8,0.125,A)
Pt(8,0.0625,A)

# Smaller subdivions, provide better approximation of the distribution

# When h = 0.0625, each P(h) is an approximation over 22 days. The higher order terms o(h)
# that are being approximated as 0 will be things like the probability that a healthy life becomes sick
# and then returns to health within 22 days. So, this approximation is not too bad!

for (k in 0:15){
	print(Pt(16,2^(-k),A))
	}

# Still find that approximation error playng role with h=2^-15. Also it becomes computationally demanding.
# h=2^-15 is -> P(h) is an approxiamtion over 16 minutes. This level of accuracy is good enough.
# We should see probailities stabilizing (Consider the last few probability transition matrices printed)

h=2^(-15)

# H -> S in 16y [1,2]
Pt(16,h,A)[1,2]

# S -> H in 20y [2,1]
Pt(20,h,A)[2,1]

# S -> D in 13.33333y [2,3]
# Check if n is an integer n=t/h...in this example, 13.3333 * 2^15 is not. 
# The loop will stop calculating just before 13.33333y. This means that the probability
# we've calculated is for a time period 16 minutes before 13.33333y. You can choose h such that n=t/h is
# an integer. In this case, its not neccessary. Inaccuracy is small.
Pt(13.33333,h,A)[2,3]



# No because transition intentisies change with age.  If the transition rates are correct 
# and can be used for all the policyholders, this model would be an
# excellent way to calculate the expected claims and premium income accurately.

# However, the main problem with this model is that it’s time-homogeneous, which means that the
# transition rates are assumed to be constant. In reality, the transition rates would all be functions
# of age. For example, the difference between the force of mortality for a 20-year-old and a
# 60-year-old would be very different, so a model that assumed they were the same would lead to
# extremely inaccurate premiums.

# It’s possible to use this model over small age bands where the transition rates are more or less
#constant. However, far greater accuracy would be achieved by building the correct transition
# rates into the model. The advantage of computing power is that the probability calculations can
# all still be performed extremely quickly even if the transition rates are functions of age.


#########################################################################################
# Question 3

# Over the age of 18 can vote for:
# Reds (R)
# BLues (B)
# Xenophobes (X)

# Suitbale time set if a MC is used - 
# The time set needs to be discrete, and correspond to a time where there is reason to 
# count the number of voters in each of the parties. A good time suggestion will be:
# - every election
# - every time an opinion poll is collected

states=c("R", "B", "X")

muRB=function(t){
	0.05+0.01*t }

muRX=function(t){
	0.001+0.0002*t }

muBR=function(t){
	0.6-0.005*t }

muBX=function(t){
	0.004+0.001*t }

muXR=function(t){
	0.05-0.0003*t }
	
muXB=function(t){
	0.9-0.006*t }
	
muRR=function(t){
	-sum(muRB(t)+muRX(t)) }

muBB=function(t){
	-sum(muBR(t)+muBX(t)) }

muXX=function(t){
	-sum(muXR(t)+muXB(t)) }
	

A=function(t){
	matrix(c(muRR(t), muRB(t), muRX(t),
			 muBR(t), muBB(t), muBX(t),
			 muXR(t), muXB(t), muXX(t)), nrow=3, byrow=TRUE, dimnames=list(states,states))
	}

muBR(25)
muRB(60)
muBX(50)

Pst=function(s,t,h){
	Id=diag(length(states))
	M=Id
	dimnames(M)=list(states, states)
	n=(t-s)/h-1
	for (j in 0:n){
		M=M%*%(Id+h*A(s+j*h)) }
	M }
	
Pst(25,25+10,h=1/12)[2,1] # -> Blues move to Reds
Pst(60,60+5,h=1/365)[1,2] # -> Reds move to Blues
Pst(50,50+7,h=1/2)[3,1] # -> Xenos move to Reds

# The model is dependent on given transition rates being correct. If they were obtained
# from past data, might not necessarily be a good indicator of future behavior.
# The methodology used to find age-dependent transition rates may be flawed
# Consider Markov assumption...The probability of a voter changing affiliations
# from one party to another is independent of the history. Possible to find that 
# voters who stayed with one party for a long time will probably stay longer than those
# who kept swapping in the past.

# The time-inhomogenous assumption may be a good one, political party affiliation likely to
# be influened by age :
# - voters tend to move parties as they age
# - different generation of voters will have different attitudes. Politcal parties will appeal 
# differently to generations
# Age is not the only factor to consider here. Political affiliation influenced very strongly by 
# socio economic background, and other factors independent of age. The model here doesn't take that 
# into account

# The model also assumes the population of voters remain constant. In reality, people who didn't vote
# before, may start to. Voters may also stop voting either because they dont want to, become extremely ill,
# or die. So they can't vote. The model doesn't take that into account.

#########################################################################################

states = paste("state", 1:3)
A = matrix(c(-1, 0.4, 0.6, 0.5, -1.5, 1, 0.8, 1.2, -2), nrow = 3, ncol = 3, byrow=TRUE)
rownames(A) = states
colnames(A) = states

rowSums(A)

mu12 = 0.4
mu13 = 0.6
mu21 = 0.5
mu23 = 1
mu31 = 0.8
mu32 = 1.2

mu11 = -(mu12 + mu13)
mu22 = -(mu21 + mu23)
mu33 = -(mu31 + mu32)
A = matrix(c(mu11, mu12, mu13, mu21, mu22, mu23, mu31, mu32, mu33), nrow = 3, ncol = 3, byrow=TRUE)
rownames(A) = states
colnames(A) = states

# Estimating transition probabilities
# -> pij(h) = h*mu_ij + o(h)
# -> pii(h) = 1+h*mu_ii + o(h)
# -> P(h) = I + h*A + o(h) -> Matrix form where I is an identity matrix
# -> P(h) ~. I +h*A

# Want to estimate P(20) -> transition matrix over 20 years by using h=1/12
# -> (P(1/12))^(20*12) -> 20*12 period to transition over
# -> P(1/12)^240

# P(1/12) ->
Ph = function(A, h){
	I = diag(nrow(A))
	P = I +h*A
	dimnames(P) = dimnames(A)
	P
	}

P = Ph(A, 1/12)

library(markovchain)
mc = new("markovchain", transitionMatrix = P, name = "MC")
# This is a markov chain with h = 1/12
# To get P(20) -> mc^(20*12) -> P(1/12)^240
mc^240

Pn = function(P, n){
	M = diag(nrow(P))
	for (j in 1:n){
		M = M%*%P
		}
	dimnames(M) = dimnames(P)
	M
	}
	
Pn(P, 240)
# -> converges to stationary distribution so starting state is irrelevant

# The finer we make h i.e. h -> 0, the better the approxiamtion as it tends to the true value
h = 1/365
n = 20
P = Ph(A, h)
Pn(P, n/h)

# Functions to estimate transition probabilities -> two ways
Pt = function(t, h, A){
	P = Ph(A, h)
	Pt = Pn(P, t/h)
	dimnames(Pt) = dimnames(A)
	Pt
	}
	
Pt.MC = function(t, h, A){
	P = Ph(A, h)
	mc = new("markovchain", transitionMatrix = P, name = "MC")
	Pt = mc^(t/h)
	Pt
	}

# -> We can use these methods if h is a mutiple of t -> remember we are performing the operation t/h

## Simulating a time-homogenous MJP
# There are two component parts to a sample path of a MJP -> the order of states visited and the the time spent in each state
# We can simulate each of these components which together make up a sample path from a MJP

# Approximate method -> use small enough h
mc1 = new("markovchain", transitionMatrix = Ph(A, 1/100), name = "MCh")
# We can do this because Ph is a transition matrix for a Markov Chain

# Now we can generate a sample from the Markov Chain using the random generator -> rmarkochain(n, <mc object>, include.t0 = TRUE)
set.seed(25)
sims = rmarkovchain(1000, mc1, include.t0 = TRUE)
head(sims)
# -> "state 3" "state 3" "state 3" "state 3" "state 3" "state 2"...
# -> Process started in state 3 and stayed there for 5 * (1/100) -> count the number of times state 3 appears from start
# -> We know the time in present state before a jump follows an exponential distribution lambda = -sum(transtions out of state)

# Exact method -> 
# Simulate the order of states -> The order of states visited can be simulated by constructing a Markov chain with transition probabilities ->
# -> transition probablitlies pij = mu_ij / lambda_i for i <> j  and pii = 0

# Create a PROBAILITY Transition Matrix from the GENERATOR Matrix ->
P = generatorToTransitionMatrix(A)

# We can then create a MArkov chain object using this probability transition matrix generated from the Generator matrix A ->
mc2 = new("markovchain", transitionMatrix = P, name = "MC2")

# As before, we can generate a sample for this chain using rmarkovchain(n, <MC object>, include.t0=TRUE)
set.seed(100)
sims = rmarkovchain(1000, mc2, include.t0=TRUE)
head(sims)
# -> "state 2" "state 3" "state 2" "state 3" "state 2" "state 3"...
# We simulated the first component -> order of states. Now we need to determine the length of time spent in each State
# The distribution of the waiting time in each state is exponential with parameter lamda = -sum(transitions out of state) -> lambda_i
# This will be the absolute value of the diagonals of the Generator Matrix A

ljs = abs(diag(A))
ljs["state 2"]
# -> for state 2, the lambda parameter is 1.5 -> the length of time spent in state 2 ~ exponential(lambda_2 = 1.5))

# Knowing the simulated states, we can extract a vector of lambda_i that correspond to the simulated state ->
params = ljs[c(sims[1:length(sims)])]
head(params)

# We're now in a position to simulated the waiting times ->
set.seed(20)
waiting.times = rexp(sims, params)
head(waiting.times)

## Time Inhomogenous Markov Jump Processes
# Non-constant Generator Matrix -> transition rates change with time

# for 0 < t < 3 we require a time-dependent Generator matrix ->

states = paste("state", 1:3)
A = function(t){
		M = matrix(c(-t, 0.4*t, 0.6*t, 0.5*t, -1.5*t, t, 0.8*t, 1.2*t, -2*t), nrow = 3, ncol = 3, byrow=TRUE)
		dimnames(M) = list(states, states)
		M
		}

A(0.2)

A = function(t){
	
	sigma12 = 0.4*t
	sigma13 = 0.6*t
	sigma11 = -t
	sigma21 = 0.5*t
	sigma23 = t
	sigma22 = -1.5*t
	sigma31 = 0.8*t
	sigma32 = 1.2*t
	sigma33 = -2*t
	
	matrix(c(sigma11, sigma12, sigma13, sigma21, sigma22, sigma23, sigma31, sigma32, sigma33), nrow = 3, ncol = 3, byrow=TRUE, dimnames = list(states, states))
	}

A(0.2)

# Transition Matrices in time-inhomogenous Markov Jump Processes
# -> Over a very small time period h, the transition probability matrix P(t,t+h) can be approximated by -> P(t,t+h) = I + h * A(t)
# To emphasise -> P(t, t+h) = I + h * A(t)

# Probabilities over longer time periods can be estimated by successively multiplying probabilities over shorter time periods (increment of length h) ->
# P(s,t) = Product(j = 0, (t-s)/h -1) [P(s + h*j, s + h*j + h)]
# P(s,t) = Product(j = 0, (t-s)/h -1) [P(s + h*j, s + h*(j + 1))]

# So to estimate P(s,t) we need a function of 3 things ->
# -> the start time s
# -> the end time t
# -> the subdivision to be used : h

states = paste("state", 1:3)
Pst = function(s, t, h){
	M = diag(length(states))
	dimnames(M) = list(states, states)
	n = (t-s)/h - 1
	for (j in 0:n){
		M = M%*%Ph(A(s + h*j), h)
		}
	M
	}

# Calculate P(1,2) using h = 1/10	
Pst(1,2,1/10)

# Calculate P(1,2) using h = 1/10000
Pst(1,2,1/10000)

# The estimate are still differing with finer divisons in h -> the error terms are playing more of a role than they were in time-homogeneous examples

#########################################################################################
# Question 1

# Omnivore - state 1
# Vegetarian - state 2
# Vegan - state 3

mu12 = 0.12
mu13 = 0.02
mu21 = 0.18
mu23 = 0.05
mu31 = 0.013
mu32 = 0.05

mu11 = -(mu12 + mu13)
mu22 = -(mu21 + mu23)
mu33 = -(mu31 + mu32)

A = matrix(c(mu11, mu12, mu13, mu21, mu22, mu23, mu31, mu32, mu33), nrow = 3, ncol =3, byrow=TRUE)
states = paste("state", 1:3)
dimnames(A) = list(states, states)

# Estimate P(1/12) -> transition probabilities over a month
# P(h) = I + h*A

Ph = function(A, h){
	M = diag(nrow(A))
	dimnames(M) = dimnames(A)
	Ph = M + h*A
	Ph
	}
	
Ph(A, 1/12)

# Estimate using one-month steps that:
# state 1 -> state 2 in 10 years
# state 1 -> state 3 in 10 years
# state 3 -> state 1 in 10 years

Pn = function(A, n, h){
	P = Ph(A,h)
	M = diag(nrow(P))
	for (j in 1:(n/h)){
		M = M%*%P
		}
	M
	}

Pn(A, 10, 1/12)[1,2]
Pn(A, 10, 1/12)[1,3]
Pn(A, 10, 1/12)[3,1]

# New rates after analysis on trends  in diets of athletes ->

mu12 = 0.15
mu13 = 0.08
mu21 = 0.09
mu23 = 0.07
mu31 = 0.004
mu32 = 0.07

mu11 = -(mu12 + mu13)
mu22 = -(mu21 + mu23)
mu33 = -(mu31 + mu32)

A.update = matrix(c(mu11, mu12, mu13, mu21, mu22, mu23, mu31, mu32, mu33), nrow = 3, ncol =3, byrow=TRUE)
states = paste("state", 1:3)
dimnames(A.update) = list(states, states)

Pn(A.update, 10, 1/12)[1,2] # This is now 38% instead of 30%
Pn(A.update, 10, 1/12)[1,3] # This now 39% instead of 19%
Pn(A.update, 10, 1/12)[3,1] # This is now 10% instead of 20%

# Before the analysis, omnivore athletes more likely to become vegetarian rather than vegan
# With the new updates from trends in diets, there is a similar likelihood that omnivores are vegan or vegetarian
# Trends are suggesting a shift toward veganism
# Before the analysis, vegans going back to omnivores were about 20% -> with new trends in diets, its even lower at 10%

# Improvements in the model ->
# In reality, a time homogenous model is a bit restrictive -> its quite likfely that shifts in diet will depend on age
# -> Individuals change their preferences as they age, and it also can be a generational shift where diet has become important for longevity
# -> There are demographic factors that could be considered - country, type of athlete (sprinter vs long-distance runner), wealth (can athlete afford expensive vegan choice), food allergies, religious stance, etc
# -> There are only 3 states -> other diets could also be considered such as keto, paleo, carnivore, etc


#########################################################################################
# Question 2

# HSD time homogenous MJP model

muHS = 0.1
muHD = 0.05
muSH = 0.6
muSD = 0.2
muDH = 0
muDS = 0


muHH = -(muHS + muHD)
muSS = -(muSH + muSD)
muDD = 0

A = matrix(c(muHH, muHS, muHD, muSH, muSS, muSD, muDH, muDS, muDD), nrow = 3, ncol =3, byrow=TRUE)
rownames(A) = c("H", "S", "D")
colnames(A) = rownames(A)

# Construct P(8) by rasing P(1) to the power of 8 -> Because the transition rates are constant, we can do this
# if we use 1 as a small unit of time h -> construct Ph(A,h)
# -> Calculate P(8) = P(1)^(8) -> P(h)^(n/h)

Ph = function(A, h){
	M = diag(nrow(A)) + h*A
	dimnames(M) = dimnames(A)
	M
	}
	
Pt = function(A, t, h){
	M = Ph(A,h)
	P = diag(nrow(A))
	for (j in 1:(t/h)){
		P = P%*%M
		}
	dimnames(P) = dimnames(M)
	P
	}

Pt(A, 8, 1)
Pt(A, 8, 0.5)
Pt(A, 8, 0.25)
Pt(A, 8, 0.125)
Pt(A, 8, 0.0625)
Pt(A, 8, 1e-4)

# as the h gets smaller, the refinement of the approximation improves. There is still error present in the approximation as the transiton matrix doesn't converge to non-chaning probabilities
# as h is getting smaller. I tried h = 1e-4, and it shows there is still room for refinement from h = 0.0625

# The smaller the value of h , the more accurate the final answer. When h = 1 , we are using the approximation P(h =1 ) = I + 1*A , which is a crude approximation since the “error term”, o(h) , will
# not necessarily be small
# You can see that the probabilities have started to “stabilise” in the last two answers, when h = 0.125 and h = 0.0625 -> the last matrix will be pretty close to the actual probabilities


for (j in 0:15){
	print(Pt(A, 16, 2^(-j)))
	}
	
# There is still small error between transition matrices as h gets smaller -> see it in the decimals
# When h = 0.0625, each P(h) is an approximation over 22 days. When h = 2^(-15) , each P(h) is an approximation over 16 minutes! So, this level of accuracy will be more than good enough, and
# we should see that the probabilities have stabilised in the last few matrices that are printed -> not really if you want deep accuracy

h = 2^(-15)

Pt(A, 16, h)["H", "S"]
Pt(A, 20, h)["S", "H"]
Pt(A, 13.5, h)["S", "D"]

# This model makes the assumption that the duration of sickness doesn't affect the time to recover to the healthy state
# -> The time sick does affect recovery to the health state and the model should be revised to account for time-inhomogenity of these transitions
# The insurance company will likely overestimate the prob of recovery -> not great for writing life policies -> the mortality probability is a lot higher than predicted using this time-homogenous model

# The main problem with this model is that its time homogenous -> transitoin rates are assumed to be constant
# -> in reality, the transiton rates will be functions of age -> the difference between the force of mortality betwenn a 20y old and 60y old is very different -> a model that assumed they were would be highly inaccurate
# -> it is possible to use this model over small age bands where the transition rates are more or less constant
# -> Accuracy can be improved by using the correct transiton rates dependent on the correct factors such as age and sickness duration

#########################################################################################
# Question 3

# Political parties -> R, B, Xenos
states = c("Red", "Blue", "Xenos")

# Time set needs to be discrete -> should correspond to a time to where there is a reason to count the number of voters in each of the parties
# A good time set would include:
# -> every election
# -> every time an opinion poll is collected

A = function(t){
	states = c("Red", "Blue", "Xenos")
	M = matrix(c(-(0.05 + 0.01*t + 0.001 + 0.0002*t), 0.05 + 0.01*t, 0.001 + 0.0002*t, 
					0.6 - 0.005*t, -(0.6 - 0.005*t + 0.004 + 0.001*t), 0.004 + 0.001*t, 
					0.05 - 0.0003*t, 0.9 - 0.006*t, -(0.05 - 0.0003*t + 0.9 - 0.006*t)),
		nrow = 3, ncol = 3, byrow=TRUE)
	dimnames(M) = list(states, states)
	M
	}
	
A(25)["Blue", "Red"]
A(60)["Red", "Blue"]
A(50)["Blue", "Xenos"]

# Over a small time period h -> the transition probability P(t, t+h) = I + h*A(t)
# Probabiltilies over successivly longer periods can be estimated by:
# -> P(s,t) = Product(j = 0, (t-s)/h - 1)[P(s +h*j, s + h*(j+1))]

Pst = function(s, t, h){
	states = c("Red", "Blue", "Xenos")
	M = diag(length(states))
	dimnames(M) = list(states, states)
	n = (t-s)/h - 1
	for (j in 0:n){
		P = diag(length(states)) + h*A(s + h*j)
		M = M%*%P
		}
	M
	}
	
Pst(25,35, 1/12)["Blue", "Red"]
Pst(60,65, 1/365)["Red", "Blue"]
Pst(50,57, 1/2)["Xenos", "Red"]

# The model is dependent on given transition rates being correct. If they were obtained
# from past data, might not necessarily be a good indicator of future behavior.
# The methodology used to find age-dependent transition rates may be flawed
# Consider Markov assumption...The probability of a voter changing affiliations
# from one party to another is independent of the history. Possible to find that 
# voters who stayed with one party for a long time will probably stay longer than those
# who kept swapping in the past.

# The time-inhomogenous assumption may be a good one, political party affiliation likely to
# be influened by age :
# -> voters tend to move parties as they age
# -> different generation of voters will have different attitudes. Politcal parties will appeal 
# differently to generations
# Age is not the only factor to consider here. Political affiliation influenced very strongly by 
# socio economic background, and other factors independent of age. The model here doesn't take that 
# into account

# The model also assumes the population of voters remain constant. In reality, people who didn't vote
# before, may start to. Voters may also stop voting either because they dont want to, become extremely ill,
# or die. So they can't vote. The model doesn't take that into account.
