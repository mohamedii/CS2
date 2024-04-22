# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\2 Markov Chains")

rep(0,5)
seq(1,5)

A=matrix(c(1,2,3,4), nrow=2, byrow=TRUE)
rownames(A)=c("row 1", "row 2")
colnames(A)=c("col 1", "col 2")

B=matrix(c(5,6,7,8,9,10), nrow=3, byrow=TRUE)
rownames(B)=paste("row", 1:3)
colnames(B)=paste("col", 1:2)
B

rows=c("A","B","C","D")
cols=c("X","Y")
C=matrix(c(10,11,12,13,14,15,16,17), nrow=4, byrow=TRUE,
dimnames=list(rows, cols))
C

diag(5)

rowSums(A)
colSums(A)

A%*%A
A*A

data=c(1,2,3,4)
data*data

dot=function(v1, v2){
	v1%*%v2
	}

dot(data,data)

install.packages("markovchain")
library(markovchain)

P=matrix(c(0.2,0.8,0.1,0.9), nrow=2, ncol=2, byrow=TRUE)
states=c("A","B")
mc=new("markovchain", transitionMatrix=P, states=states, name="my.mc")

mc
mc@transitionMatrix

is.matrix(mc@transitionMatrix)
rowSums(mc@transitionMatrix)
mc@states
is.irreducible(mc)
period(mc)

B=matrix(c(1,0,0,1), nrow=2, byrow=TRUE)
mc2=new("markovchain", transitionMatrix=B, states=states, name="my.mc2")
is.irreducible(mc2)
period(mc2)


P=matrix(c(0.2,0.8,0.1,0.9), nrow=2, byrow=TRUE)
rownames(P)=c("A","B")
colnames(P)=rownames(P)
# check rowSums(P)


Pn=function(P,n){
	M=diag(nrow(P))
	for (j in 1:n){
		M=M%*%P
		}
	dimnames(M)=dimnames(P)
	M
	}

mc3=new("markovchain", transitionMatrix=P, states=c("A","B"), name="my.mc3")
mc3@transitionMatrix
mc3@states
is.irreducible(mc3)
period(mc3)

mc3^4
mc4=mc3^4
mc4@transitionMatrix
mc4@states
period(mc4)

# Initial distribution function d=(d1,d2) row vector. expected distribution
# after 1-step -> (d1,d2)*P {Always check dimensionality}

d=c(0.5,0.5)
d%*%P
d%*%Pn(P,10)

ExpDist=function(d, P, n){
	pos=d
	for (j in 1:n){
		pos=pos%*%P
		}
	pos
	}
	
d=c(1,0)
ExpDist(d,P,1)

# Using markovchani package functionaility

y=d*mc3^4
y[1]+y[2]

steadyStates(mc3) #Stationary distribution
steadyStates(mc4) #Created new mc4=mc3^4. Checking steadyStates function

# NCD example -> 0% discount L1, 25% discount L2

Premium=numeric(3)
Income=numeric(2)

disc=c(1,0.75) # discount vector
d=c(0.5,0.5)
Premium[1]=100*d%*%disc # -> 

	
	for (j in 1:2){
		d=d%*%P
		Income[j]=100*d%*%disc
		Premium[j+1]=Premium[j]+Income[j]
		}

Income
Premium

# Expected Reward functionality of markovchain package to answer NCD
# -> use d(d1,d2) to specify starting distrubution
# -> income=c(100,75) -> c(1-discount, 1-discount) on L1 and L2
expectedRewards(mc3,2,c(100,75))
d=c(0.5,0.5)
d%*%expectedRewards(mc3,2,c(100,75))

# Sampling from a Markov Chain
# Straightforward formula -> rmarkovchain(n, mc, t0="start state", 
#	include.t0=TRUE)
# Here, the start state will be included so output will be of size n+1...
# to exlcude the start state -> include.t0=FALSE. If you don't include the 
# the t0 argument, then the chain will state in a random state.

rmarkovchain(10, mc3, t0="A", include.t0=TRUE)
rmarkovchain(10, mc3)

# Fitting MC -> use the package and inculde the starting state
data=rmarkovchain(10, mc3, t0="A", include.t0=TRUE)

markovchainFit(data)
mc3@transitionMatrix
mc5=markovchainFit(data)
mc5@transitionMatrix


# Time inhomogenous MCs
# Example ->

prob1=function(x){
	0.015*x }

prob2=function(x){
	0.02*x }

Px=function(x){
	P=matrix(c(1-prob1(x), prob1(x), 1-prob2(x), prob2(x)), nrow=2, byrow=TRUE)
	rownames(P)=c("State 1","State 2")
	colnames(P)=rownames(P)
	P
	}

position=c(1,0)
for (j in 25:34){
	position=position%*%Px(j)}
	position

driver.dist=function(age, start, n){
	pos=start
		for (j in age:(age+n-1)){
			pos=pos%*%Px(j)}	
	pos
	}
	
driver.dist(25,c(1,0),10)

#########################################################################################
# Question 1
 
P=matrix(c(0.2,0.8,0,0.4,0.5,0.1,0.85,0,0.15),nrow=3, byrow=TRUE)
rownames(P)=c("state 1", "state 2", "state 3")
colnames(P)=rownames(P)

mc=new("markovchain", transitionMatrix=P, states=c("state 1", "state 2", "state 3"), name="Q1MC")
is.irreducible(mc)
period(mc)

rowSums(mc@transitionMatrix)

start=c(0,1,0)
dist.4step=start*mc^4
dist.4step[1]

d=c(0.2,0.3,0.5)
dist.8step=d*mc^8
dist.8step[2]

steadyStates(mc)

#########################################################################################
# Question 2
past18=c(2,1,2,1,3,1,1,2,2,1,1,3,3,1,2,1,2,1)

fit1=markovchainFit(past18)

fit1$estimate
fit1$logLikelihood

#########################################################################################
# Question 3

P=matrix(c(0.2,0.8,0,0,0,0.15,0,0.85,0,0,0,0.1,0,0,0.9,0.2,0,0,0,0.8,0,0,0,0.1,0.9), nrow=5, byrow=TRUE)
rownames(P)=c("1", "2", "3+", "3-", "4")
colnames(P)=rownames(P)

rowSums(P)==1

mc=new("markovchain", transitionMatrix=P, states=c("1", "2", "3+", "3-", "4"), name="Q3MC")

Pn=function(P,n){
	M=diag(nrow(P))
	for (j in 1:n){
		M=M%*%P }
	dimnames(M)=dimnames(P)	
	M }

init=c(1,0,0,0,0)
dist.5y=init*mc^5
dist.5y #Careful here...the question asked for the probability to dicsount
# rate. 3+ and 3- states both have 30% dicount rates...so need to add up the
# probabilities for 3+ and 3-

dist.20y=init*mc^20
dist.20y #Careful here...the question asked for the probability to dicsount
# rate. 3+ and 3- states both have 30% dicount rates...so need to add up the
# probabilities for 3+ and 3-. Same story as above

Y=mc^25
Y[1,]

is.irreducible(mc)
period(mc)
steadyStates(mc)

# Since P is irreducible and aperiodic, a steady sate distrubution exists.
# As the number n -> inf, the chain will converge to its steady state
# distribution. In this case convergence is close.

#########################################################################################
# Question 4

states=c("L1", "L1-", "L2", "L3", "L4")
P=matrix(c(0.15,0,0.85,0,0,0,0.15,0.85,0,0,0.11,0.04,0,0.85,0,0.04,0,0.11,0,0.85,0,0,0.04,0.11,0.85),
	nrow=5, byrow=TRUE)
rownames(P)=states
colnames(P)=rownames(P)

P

Pn=function(P,n){
	M=diag(nrow(P))
	for (j in 1:n){
		M=M%*%P
		}
	dimnames(M)=dimnames(P)
	M }
	
Pn(P,4)
Pn(P,7)

# New transition probabilities
P.update=matrix(c(0.17,0,0.83,0,0,0,0.17,0.83,0,0,0.12,0.05,0,0.83,0,0.05,0,0.12,0,0.83,0,0,0.05,0.12,0.83),
	nrow=5, byrow=TRUE)
rownames(P.update)=states
colnames(P.update)=rownames(P.update)
mc=new("markovchain", transitionMatrix=P.update, states=states,
	name="Q4MC")

mc

#Full premium -> 750

#Check the conditions for the stationary / steady states to exists
is.irreducible(mc) # require this to be TRUE
period(mc) # require aperiodicity i.e. -> 1
steadyStates(mc)

#Determine the Premium Income function over n years
Income=function(startDist, transitionMatrix, discountVector, nsteps, Premium){	
	PremInc=Premium*startDist%*%(1-discountVector)
	pos=startDist
	for (j in 1:(nsteps-1)){
		pos=pos%*%transitionMatrix
		PremInc=PremInc+Premium*pos%*%
			(1-discountVector)
		}
	PremInc }

# Expected Total Premium Income if..

# A new driver over the next 5 years
startDist=c(1,0,0,0,0)
transitionMatrix=P.update
discountVector=c(0,0,0.15,0.25,0.3)
nsteps=5
Premium=750

Income(startDist, transitionMatrix, discountVector, nsteps, Premium)

# A new driver over the next 10 years
startDist=c(1,0,0,0,0)
transitionMatrix=P.update
discountVector=c(0,0,0.15,0.25,0.3)
nsteps=10
Premium=750
		
Income(startDist, transitionMatrix, discountVector, nsteps, Premium)
	
# A new driver currently on Level 4 over the next 10 years
startDist=c(0,0,0,0,1)
transitionMatrix=P.update
discountVector=c(0,0,0.15,0.25,0.3)
nsteps=10
Premium=750

Income(startDist, transitionMatrix, discountVector, nsteps, Premium)

# Check Markov Package results -> startDist%*%expectedRewards(mc,n-1,income)
# In this case, income=premium*dist*(1-discVec)
# CHECK YOUR LOOPS! Remeber (j in 1:(n-...)){

# L4 clients get NCD insurance where they dont leave L4 discountif they cliam once,
# and move down only one notch if they claim more than once. This benefit cost a Premium
# which has to be determined

# update the transition matrix to relfect the new policy on L4 clients

P.update2=matrix(c(0.17,0,0.83,0,0,0,0.17,0.83,0,0,0.12,0.05,0,0.83,0,0.05,0,0.12,0,0.83,0,0,0,0.05,0.95),
	nrow=5, byrow=TRUE)
rownames(P.update2)=states
colnames(P.update2)=rownames(P.update)
mc2=new("markovchain", transitionMatrix=P.update2, states=states,
	name="Q4MC2")
	
# New Premium Income assuming for now that additional premium is equal to zero
# i.e. existing L4 clients get benefit for free

#Determine the Premium Income function over n years with Additional Premium
Income2=function(startDist, transitionMatrix, discountVector, nsteps, Premium, AddPrem){	
	L4_ind=c(0,0,0,0,1)
	PremInc=Premium*startDist%*%(1-discountVector)+AddPrem*(startDist%*%L4_ind)
	pos=startDist
	for (j in 1:(nsteps-1)){
		pos=pos%*%transitionMatrix
		PremInc=PremInc+Premium*pos%*%
			(1-discountVector)+AddPrem*(pos%*%L4_ind)
		}
	PremInc }

# A new driver currently on Level 4 over the next 10 years
startDist=c(0,0,0,0,1)
transitionMatrix=P.update2
discountVector=c(0,0,0.15,0.25,0.3)
nsteps=10
Premium=750
AddPrem=0

prem.AddPrem=Income2(startDist, transitionMatrix, discountVector, nsteps, Premium, AddPrem)

prem.10=Income2(startDist, transitionMatrix, discountVector, nsteps, Premium,10)
prem.15=Income2(startDist, transitionMatrix, discountVector, nsteps, Premium,15)

# In terms of premium income, the insurer should try and set the additional premium as hihg as possible
# to make up for the loss in income from otherwise before the insurance offer

linear_interpolation=function(p1,x1,x2,y1,y2){
	# Solve for p2 using linear interpolation
	m=(y2-y1)/(x2-x1)
		p2=m*(q1-x1)+y1 }


prem.0=5408.8624
m=(prem.15-prem.10)/(15-10)
AddPrem=(1/m)*(prem.0-prem.15)+15

AddPrem
prem.AddPrem=Income2(startDist, transitionMatrix, discountVector, nsteps, Premium, AddPrem)

# Moral Harzard - biggest risk of offering NCD protection is makes the probability
# of a cliam higher. With NCD protection in place, its possilbe that policyholders
# take less care than if they would if they if they had lost the NCD
# Insurance company will find policyholders are willing to pay the additional premium
# up to some point

#########################################################################################
# Question 5
states=c("L1", "L2", "L3", "L4", "L5")
P=matrix(c(0.1,0.9,0,0,0,0.1,0,0.9,0,0,0.02,0.08,0,0.9,0,0.02,0,0.08,0,0.9,0.02,0,0,0.08,0.9), nrow=5, byrow=TRUE)
rownames(P)=states
colnames(P)=states

mc=new("markovchain", transitionMatrix=P, states=states, name="Q5MC")

transProb = function(i,j,P,n){
	M=diag(nrow(P))
	for (k in 1:n){
		M=M%*%P }
	
	startState=numeric(nrow(P))
	endState=numeric(nrow(P))
	for (k in 1:nrow(P)){
		startState[k]=0^(abs(i-k))
		endState[k]=0^(abs(j-k))}
	
	M=startState%*%M%*%endState
	M	}


transProb(1,5,P,5)
transProb(5,3,P,4)
transProb(3,4,P,6)
	
Y=steadyStates(mc)
discVec=c(0,0.1,0.2,0.3,0.4)

PremInc=800*Y%*%(1-discVec)
PremInc.total=2000*PremInc

states.update=c("L1", "L2", "L3", "L3-", "L4", "L4-", "L5")
Q=matrix(c(0.1,0.9,0,0,0,0,0,0.1,0,0.9,0,0,0,0,0.02,0.08,0,0,0.9,0,0,0.1,0,0,0,0.9,
	0,0,0.02,0,0,0.08,0,0,0.9,0.02,0.08,0,0,0,0,0.9,0.02,0,0,0,0,0.08,0.9),nrow=7, byrow=TRUE)
rownames(Q)=states.update
colnames(Q)=states.update

mc2=new("markovchain", transitionMatrix=Q, states=states.update, name="Q5MC")

transProb(1,7,Q,5)
transProb(7,3,Q,4)
transProb(3,5,Q,6)+transProb(3,6,Q,6)

Z=steadyStates(mc2)
discVec.update=c(0,0.1,0.2,0.2,0.3,0.3,0.4)
PremInc.updateTotal=2000*800*Z%*%(1-discVec.update)

PremInc.total
PremInc.updateTotal

#########################################################################################
# Question 6 INCOMPLETE




#########################################################################################
# Question 1

P = matrix(c(0.2, 0.8, 0, 0.4, 0.5, 0.1, 0.85, 0, 0.15), nrow = 3, ncol = 3, byrow=TRUE)
rownames(P) = c("state 1", "state 2", "state 3")
colnames(P) = rownames(P)
P

install.packages("markovchain")
library(markovchain)

mc = new("markovchain", transitionMatrix = P, name = "Q1MC")

is.irreducible(mc)
period(mc)
# aperiodic and irreducible -> unique stationary distribution exists

rowSums(mc@transitionMatrix)

p0 = c(0,1,0)
p4 = p0 * mc^4
p4[1]

d = c(0.2, 0.3, 0.5)
p8 = d * mc^8
p8[2]

stat.dist = steadyStates(mc)
stat.dist

#########################################################################################
# Question 2

past18 = c(2,1,2,1,3,1,1,2,2,1,1,3,3,1,2,1,2,1)
length(past18)

fit1 = markovchainFit(past18)
str(fit1)

fit1$estimate
fit1$logLikelihood

# The $estimate component shows the estiamtes of the transition probabilties. The probability of transitioning from state 1 to state 2
# -> p12_hat = n12/n1 -> 4/8 -> 0.5
# -> n12 number of transitions from state 1 to state 2 and n1 is the number of transitions out of state 1

# The log Likelihood provides a measure of how likely we are to obtain the observed data based on the model. A less negative value indicates a model that better accounts for observed results


#########################################################################################
# Question 3

P = matrix(c(0.2, 0.8, 0, 0, 0, 0.15, 0, 0.85, 0, 0, 0, 0.1, 0, 0, 0.9, 0.2, 0, 0, 0, 0.8, 0, 0, 0, 0.1, 0.9), nrow = 5, ncol = 5, byrow=TRUE)
rownames(P) = c("State 1", "State 2", "State 3+", "State 3-", "State 4")
colnames(P) = rownames(P)

rowSums(P)

mc = new("markovchain", transitionMatrix = P, name = "Q2")

p0 = c(1,0,0,0,0)
p5 = p0 * mc^5
p5
# For the Disounts, 30% for both state 3- and state 3 + -> add probabilities together

is.irreducible(mc)
period(mc)

P25 = mc^25
P25
# PInf has converged to its stationary distribution. Since P is aperiodic and irreducible, a unique stationary distribution exists regardless of the starting point
# All the rows are identical

Pn = function(P, n){
	M = diag(nrow(P))
	for (j in 1:n){
		M = M%*%P
		}
	M
	}

p5 = p0%*%Pn(P,5)
p5
# For the Disounts, 30% for both state 3- and state 3 + -> add probabilities together

P25 = Pn(P,25)
P25


#########################################################################################
# Question 4

P = matrix(c(0.15, 0.85, 0, 0, 0.15, 0, 0.85, 0, 0.04, 0.11, 0, 0.85, 0, 0.04, 0.11, 0.85), nrow = 4, ncol  =4, byrow=TRUE)
rownames(P) = c("state 1", "state 2", "state 3", "state 4")
colnames(P) = rownames(P)

mc = new("markovchain", transitionMatrix = P, name="Q3")

Pn = function(P, n){
	M = diag(nrow(P))
	for (j in 1:n){
		M = M%*%P
		}
	M
	}

Pn(P,4)
Pn(P,7)

Q = matrix(c(0.17, 0.83, 0, 0, 0.17, 0, 0.83, 0, 0.05, 0.12, 0, 0.83, 0, 0.05, 0.12, 0.83), nrow = 4, ncol  =4, byrow=TRUE)
rownames(Q) = c("state 1", "state 2", "state 3", "state 4")
colnames(Q) = rownames(Q)

mcQ = new("markovchain", transitionMatrix = Q, name="Q3")

is.irreducible(mcQ)
period(mcQ)
# -> unique stationary distribution exisits

Pn(Q, 100)
steadyStates(mcQ)

prem0 = 750
discount = c(0, 0.15, 0.25, 0.30)
income = prem0*(1-discount)

total.prem = function(p0, income, Q, n){
	prem = 0
	position = p0
	for (j in 1:n){
		prem = prem + sum(position*income)
		position = position%*%Q
		}
	prem
	}

total.prem(c(1,0,0,0), income, Q, 5)
total.prem(c(1,0,0,0), income, Q, 10)
total.prem(c(0,0,0,1), income, Q, 10)

sum(c(1,0,0,0)*expectedRewards(mcQ, 4, income))
sum(c(1,0,0,0)*expectedRewards(mcQ, 9, income))
sum(c(0,0,0,1)*expectedRewards(mcQ, 9, income))

Q.update = matrix(c(0.17, 0.83, 0, 0, 0.17, 0, 0.83, 0, 0.05, 0.12, 0, 0.83, 0, 0, 0.05, 0.95), nrow = 4, ncol  =4, byrow=TRUE)
rownames(Q.update) = rownames(Q)
colnames(Q.update) = colnames(Q)

mcQ.update = new("markovchain", transitionMatrix = Q.update, name = "Q3")
total.prem(c(0,0,0,1), income, Q.update, 10)
sum(c(0,0,0,1)*expectedRewards(mcQ.update, 9, income))

total.prem.update = function(p0, income, Q, n, addP){
	prem = 0
	position = p0
	for (j in 1:n){
		prem = prem + sum(position*(income + c(0,0,0,addP)))
		position = position%*%Q
		}
	prem
	}

addP = 10
prem.10 = total.prem.update(c(0,0,0,1), income, Q.update, 10, addP)
prem.10
sum(c(0,0,0,1)*expectedRewards(mcQ.update, 9, income+c(0,0,0,addP)))

addP = 15
prem.15 = total.prem.update(c(0,0,0,1), income, Q.update, 10, addP)
prem.15
sum(c(0,0,0,1)*expectedRewards(mcQ.update, 9, income+c(0,0,0,addP)))

# The expected premium income of $5379.28 is less than $5408.86 (without the add premium and using Q matrix i.e. no change in claim policy)
# -> Insurer will make less money chargin additional $10 -> suggest insurer charges a higher premium or stick to old policy

# The overall expected premium income is linearly related to the additional premium amount ->

prem = numeric(11)
for (j in 10:20){
	addP = j
	prem[j] = total.prem.update(c(0,0,0,1), income, Q.update, 10, addP)
	print(c(j, prem[j]))
	}

addP = (14-13)/(5416.903-5407.497)*(5408.862-5407.497)+13 # -> addP = 13.14512
total.prem.update(c(0,0,0,1), income, Q.update, 10, addP) # -> 5408.862

# The additional premium set too low will influence the insured to take more risk given the cover. The Moral Hazard problem.
# -> The biggest risk of offering a NCD is that it makes the probability of a claim higher
# -> With NCD protection in place, possible that policyholdersr take less care to prevent a claim
# -> With NCD protection in place, policyholder is more lkely to make a small claim that wouldnt have done for fear of losing the NCD
# Profit - the company may find that many of it policyholders are willing to pay the aiidtional premium if its very higher
# Expenses - calculate such that it balances the insured from being a moral hazard and the insurer making a profit 

#########################################################################################
# Question 5

P = matrix(c(0.1, 0.9, 0, 0, 0, 0.1, 0, 0.9, 0, 0, 0.02, 0.08, 0, 0.9, 0, 0.02, 0, 0.08, 0, 0.9, 0.02, 0, 0, 0.08, 0.9), nrow =5, ncol = 5, byrow=TRUE)
rownames(P) = c("state 1", "state 2", "state 3", "state 4", "state 5")
colnames(P) =rownames(P)

mc = new("markovchain", transitionMatrix = P, name = "Q5")

nPij = function(P, i, j, n){
	M = diag(nrow(P))
	
	for (k in 1:n){
		M = M%*%P
	}
	M[i,j]
	}
	
nPij(P,1,5,5)
nPij(P,5,3,4)
nPij(P,3,4,6)

prem0 = 800*2000
discount = c(0, 0.1, 0.2, 0.3, 0.4)
income = prem0*(1- discount)

is.irreducible(mc)
period(mc)
# -> unique stationary distribution exists and can start with any intial distribution

stat.dist.mc = steadyStates(mc)
exp.prem = sum(income*stat.dist.mc[])
exp.prem

# Require two more states 3-, and 4-
Q = matrix(c(0.1, 0.9, 0, 0, 0, 0, 0, 0.1, 0, 0.9, 0, 0, 0, 0, 0.02, 0.08, 0, 0, 0.9, 0, 0, 0.1, 0, 0, 0, 0.9, 0, 0, 0.02, 0, 0, 0.08, 0, 0, 0.9, 0.02, 0.08, 0, 0, 0, 0, 0.9, 0.02, 0, 0, 0, 0, 0.08, 0.9),
			nrow = 7, ncol = 7, byrow=TRUE)
rownames(Q) = c("state 1", "state 2", "state 3-", "state 3+", "state 4-", "state 4+", "state 5")
colnames(Q) = rownames(Q)

nPij(Q,1,5,5)
nPij(Q,5,3,4)
nPij(Q,3,4,6)

mc.Q = new("markovchain", transitionMatrix = Q, name = "Q5")
is.irreducible(mc.Q)
period(mc.Q)
# -> unique stationary distribution exists and can start with any intial distribution

prem0 = 800*2000
discount = c(0, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4)
income = prem0*(1- discount)

stat.dist.mc.Q = steadyStates(mc.Q)
exp.prem.Q = sum(income*stat.dist.mc.Q[])
exp.prem.Q

# -> Change in NCD policy results 0.3% higher expected premium. Not a very big increase in annual income. The new policy may be perceived as negative
# -> Policyholders may take business elsewhere. Not worth changing the rules if you potentially lose business -> rules may not be policyholder friendly
# -> Rather a small change in existing premium (no rules change) could have a better effect as the new rules change for existing policyholders
