# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\4 & 5 Embedded Jump Chains")

data = read.csv("Example journey.csv")
head(data)

journey=data$State
n=length(journey)
# n=1001, meaning starting in state A, there are 1000 visits
# in total to other states

table(journey)

# Movements from one state to another -> called doubles

doubles=character(n-1)
for (i in 1:length(doubles)){
	doubles[i]=paste(journey[i], journey[i+1], sep="")
}

head(doubles)
table(doubles)

triplets=character(n-2)
for (i in 1:length(triplets)){
	triplets[i]=paste(journey[i], journey[i+1], journey[i+2],sep="")
}

head(triplets)
table(triplets)

ni=table(journey[-n])
# Here we need to remove the last state because we dont know where
# this state will transition to
sum(ni)

nij=table(doubles)

# R can do quite a bit easily. Read the notes again for a quick refresh
# Things to remember...
# tables()
# names()
# paste(...,sep"")
# Really quite cool ^+^


# Now want to calculated Pij -> nij/ni
# length nij=6
# length ni=3
# Remember nij...number of visitins to state j from state i
# Remember ni...number of times the process has left state i
# Pij will have the same dimensions as nij
# ni has 3 elements in its table...to represent 3 states {A,B,C}

# Set up Pij for calculation

Pij=numeric(length(nij))
names(pij)=names(nij)
Pij=nij/(ni[substr(names(
nij),1,1)])

# Nice way to think and do the calc. In the above code
# using the substring function links nij -> ni
# Check properties of the probabilities -> summing to 1
# You'll see here that the state never transitions to itself in this
# problem

# WE can also use the markov chain package -> markovchainFit(journey)

library(markovchain)
mcFit = markovchainFit(journey)
mcFit$estimate

# Now to calculate nijk
nijk=table(triplets)

# Calculate tnij -> the number of times a double i->j was observed as
# as the start of a triplet
# We use a similar calculation as nij but just need to remove the last double

tnij=table(doubles[-length(doubles)])
# Check tables(doubles) to tnij and confirm that the last double has been 
# removed

# Calculating the Expected Triplets
# To calculate the expected triplets, we need to consider the distribution 
# of the number of each triplet
# Under the hypothesis that the Markov property holds, we assume that the
# distribution for the number of trplets ABC~Bin(tnAB,p^BC) ->
# that is ABC is Binomial with n=tnAB, and p=p(hat)BC estimate

# tnAB is the number of times this possible triplet is set up by going from A to B
# p^BC = nBC/nB is the estimated probability of going from B->C given B has just left
# tnAB x p^BC gives the expected number of triplets ABC

# Create a table (or matrix) to show the oberseved 
# vesrus expected for each triplet

trip.table=as.matrix(nijk)

# as.matrix(nijk) -> nijk=table(triplets) which is summary of the triplets
# i.e. the triplet that occurs and the count of it

# Now add another column to the matrix

trip.table=cbind(trip.table, NA)

# check the row names of trip.table
rownames(trip.table)

# give the colums of trip.table names ->
colnames(trip.table)=c("Observed", "Expected")
head(trip.table)

# Determine the number of expected triplets using the null
# distribution that triplet ABC (generic order - dont' get confused!)
# -> ABC~Bin(tnAB,p^BC) -> E(ABC)=tnAB.p^BC

# We'll have to do something ABC to get info from tnij and Pij clearly

# Consider the substring function again: substr(ABC,...)
# We need to extract tnAB and pBC info from ABC -> the Goal
# for tnAB -> tnij[AB]
# fpr pBC -> Pij[AB]

for (ABC in rownames(trip.table)){
	AB=substr(ABC,1,2)
	BC=substr(ABC,2,3)
	tnAB=tnij[AB]
	pBC=Pij[BC]
	trip.table[ABC,"Expected"]=tnAB*pBC # i.e. E(ABC)=tnAB.p^BC
	}

trip.table

# Remember the point of all of this...the assumption ath beginning
# was that the MJP is a Markov Process. The Triplets test is to test for 
# that. Is this MJP process modelled Markov

# We use a Chi Sw GoF test to test the assumption of hte Null Hypothesis
# beause if the MJP is Marokov, then TS=sum((O-E)^2/E) follows a 
# ChiSq(r-2q+s) disctribution. More on the DoF later

# Calculating the ChiSq Test Statistic

O=trip.table[, "Observed"]
E=trip.table[, "Expected"]
# Assuming the Null is true, TS~ChiqSq(r-2q+s) distribution
TS=sum((O-E)^2/E)

# Calculating the DoF for the ChiSq RV -> DoF=r-2q+s -> [(r-q) - (q-s)]

r=length(nijk) # i.e. the number of unique triplets
q=length(nij)  # i.e. the number of unique doubles
s=length(unique(journey)) # or s=length(ni)

# we use the unique function here to get the individual
# states in journey -> table function on journey...we can us ni=table(journey)

DoF=r-2*q+s
alpha=0.05

ChiSqCV=qchisq(alpha,DoF,lower=FALSE) # 5% significance level
# or ChiSqCV=qchisq(1-alpha, DoF) # 5% significance level

# Compare the TS and CV

TS>ChiSqCV # If FALSE, then fail to reject...conclude the MJP is Markov

# Alternatively, calculate the p-value based on TS and compare to 
# alpha = 5%


pval=pchisq(TS, DoF, lower=FALSE)
pval>alpha # if you fail to reject Null, then p-value > alpha
# i.e. this should return TRUE

#########################################################################################
# Question 1

# States:

# Bedroom B
# Garden G
# Hall H
# Kitchen K
# Lounge L 
# Shower S
# Toilet T

# Want to test if room currently occupied follows a Markov Jump Process
# with seven states

journey=read.csv("House journey.csv")
names(journey)

Room=journey$Room 

length(Room)
ni=table(journey)

doubles=numeric(length(Room)-1)
for (j in 1:(length(Room))-1){
	doubles[j]=paste(Room[j],Room[j+1],sep="") }

nij=table(doubles)

triplets=numeric(length(Room)-2)

for (j in 1:(length(Room)-2)){
	triplets[j]=paste(Room[j],Room[j+1],Room[j+2],sep="") }

nijk=table(triplets)

Pij=nij/(ni[substr(names(nij),1,1)])

tnij=table(doubles[-length(doubles)])

# Check tables(doubles) to tnij and confirm that the last double has been 
# removed
# Use tail(..) function to see the last couple entries much like
# how you would use head(...)

# Now setup the Observed and Expected Table for triplets

trip.table=as.matrix(nijk)
trip.table=cbind(trip.table, NA)
colnames(trip.table)=c("Observed","Expected")

for (ABC in rownames(trip.table)){
	AB=substr(ABC,1,2)
	BC=substr(ABC,2,3)
	tnAB=tnij[AB]
	pBC=Pij[BC]
	trip.table[ABC,"Expected"]=tnAB*pBC }
	
# Calculate the Test Statistic

O=trip.table[, "Observed"]
E=trip.table[, "Expected"]

TS=sum((O-E)^2/E)

#DoF = r-2s+s
r=length(nijk)
q=length(nij)
s=length(ni)
DoF=r-2*q+s	

alpha=0.01
# ChiSq(Dof)
CV=qchisq(1-alpha, DoF)

if (TS<CV){
	print("Insufficient evidence to reject the null hypothesis. Conclude that process is Markov
			and follow a Markov Jump Process")
	} else {
	print("Reject the null hypothesis and conclude that the process does not have the Markov property")
	}
	
pvalue=pchisq(TS, DoF, lower=FALSE)

if (pvalue>alpha){
	print("Insufficient evidence to reject the null hypothesis. Conclude that process is Markov
			and follow a Markov Jump Process")
	} else {
	print("Reject the null hypothesis and conclude that the process does not have the Markov property")
	}

# Possible that certain activity will lead to predicable trnsitions. For instance, after you eat in the kitchen,
# you will probably go the lounge. Or if you wake up in the morning, you will leave your bedroom and go to the 
# bathroom. The history of past state visits can predict the next state visit. 


#########################################################################################

# Embedded jump chain starts life as a MJP. It is a sequence of states that the process is observed to take.
# The time spent in each state is ignored -> the time set effectively becomes discrete

# The embedded jump chain of a MJP can be treated as a Markov chain -> We can use the triplets test to investigate the Markov property
# -> The triplet test can be used to to investigate the Markov property for either a proposed Markov chain model or embedded chain of a proposed MJP model

# Sample Markov chain / embedded chain calculations
# -> summarise the sample jump chain paths
# -> extract the sample doubles
# -> extract the sample Triplets
# -> calculate ni, nij, pij^hat, nijk
# -> calculate observed expected Triplets
# -> perform a triplets test to investigate the Markov property

data = read.csv("Example journey.csv")
head(data)
# -> process start in state A (transition = 0)

# Interested in the states the process visited
journey = data$State
n  = length(journey)

table(journey) # -> to summarise the times each state was visited

# Extract sample doubles
# 1001 stops (singles) in this journey. Therefore 1000 movements from ones to another -> doubles!
# For instance ABACBBCA -> AB, BA, AC, CB, BB, BC, BA -> 8 singles, 7 [8-1] doubles

#setup empty character vector -> note not numeric b/c state is labelled A, B, C, etc

doubles = character(n-1) # -> because there are n-1 doubles
# we can loop over journey using paste function ->
for (i in 1:length(doubles)){
	doubles[i] = paste(journey[i], journey[i+1], sep="")
	}

table(doubles)

# Extract sample triplets
triplets = character(n-2) # -> follow same logic as doubles
for (j in 1:length(triplets)){
	triplets[j] = paste(journey[j], journey[j+1], journey[j+2], sep="")
	}

table(triplets)

# Calculating ni -> number of times a transition was observed out of state i
# -> There are 1001 singles but we need to remove the last state because we dont know the state occupied after this\
ni = table(journey[-n]) # -> remember n = length(journey) -> here we remove the last state in the journey by [-n]

# calculating nij -> tehre are two different definitions of nij that we need
# -> The first is the number of times each double was observed in the data set -> we can use the table function for this
# -> This is required to calculate pij_hat = nij / ni

# -> The second we define tnij -> the 't' represent triplets
# -> This is the number of times each double was observed at the start of a triplet
# -> This is the same as nij for each double except the last double, as this last one is not the start of a triplet
# -> We will calculate tnij later...

nij = table(doubles)
# -> Note that in this example there are no transitions from a state back to itself
# -> Remember that for a MJP, we define a transition to be when a process leaves a state
# -> We can expect to have a Markov chain with transitions back to itself -> so we may see doubles of this type, but not in this Example

# Calculating pij_hat -> nij / ni
# -> for example pAB = nij["AB"] / ni["A"] -> number of transitions from A to B divided by the number of transitions out of A
# We need a way to extract the first state in the double as this is the state the double is leaving

pij = numeric(length(nij))
names(pij) = names(nij)

# now use a for loop, and loop over the names in nij applying an extract function on doubles to get the transition out of state
for (AB in names(nij)){
	pij[AB] = nij[AB] / ni[substr(AB, 1, 1)]
	}
# substr("AB", 1, 1) -> "A" i.e. substr(<text>, <start character position>, <end position>)

# Alternative is to use Markov Chain Package to Fit the mcFit
mc.journey.Fit = markovchainFit(journey)
mc.journey.Fit$estimate
# -> transiton probability matrix showing pij_hat

# Calculating nijk -> observed Triplets
nijk = table(triplets)

# Calculating tnijk -> remember what this is -> this is the same as nij for each double except the last double, as this last one is not the start of a triplet
# The last double can't be a triplet -> we must remove the last index from doubles
tnijk = table(doubles[-length(doubles)]) # -> the last double that was removed was "AC"

# Calculating the Expected Triplets -> make distribution assumptions Now
# To calculate the expected triplets, we need to consider the distribution of the number of each triplet i.e. # Triplet ~ ...
# Example -> the number of triplet ABC ~ Bin(tnAB, pBC_hat)
# IMPORTANT -> The form of this distribution -> ABC ~ Bin(tnAB, pBC_hat) -> AB-BC -> ABC
# Therfore E(ABC) = tnAB * pBC_hat -> expected number of triplets ABC i.e. expected number of times ABC occurs

trip.table = as.matrix(nijk)
trip.table = cbind(trip.table, NA) # adding a column of NAs
colnames(trip.table) = c("Observed", "Expected")

head(trip.table)

# We now need to calculate the expected number of triplet -> tnAB * pBC_hat
# For each triplet from trip.table we need to -
# -> the first two states of the triplet to get tnAB
# -> the last two states of the triplet to get pBC_hat

for (ABC in rownames(trip.table)){
	AB = substr(ABC, 1, 2)
	BC = substr(ABC, 2, 3)
	trip.table[ABC, "Expected"] = tnijk[AB] * pij[BC]
	}

trip.table
	
# Read top bottow of page 11 and top of page 12 -> this example all possible triplets were observed. Sometimes this wont be the case ->
# a double AB can be observed
# a double BC can be observed
# the triplet ABC not observed
# -> The first two doubles imply ABC triplet is possible under the Markov assumption even when it wasn't actually observed -> Remember This!
# This example all possible triplets were observed

# Peform a Triplets Test -> Chi Sq GoF
# Null hypothesis is that the Markov property holds
# The ChiSq Null distrubution has r - 2q + s degrees of freedom
# -> r = length(nijk) i.e. the total number of distint triplets observed
# -> q = length(nij) i.e. the total number of distint doubles observed
# -> s = length(unique(journey)) -> the number of distint single states observed

O = trip.table[ , "Observed"]
E = trip.table[ , "Expected"]

r = length(nijk)
q = length(nij)
s = length(unique(journey))
dof = r - 2*q + s

TS = sum((O-E)^2 / E)
CV = qchisq(0.95, dof)

TS; CV
p.value = pchisq(TS, dof, lower=FALSE)
p.value # -> 0.5313967 therefore fail to reject, the process has the Markov property

#########################################################################################
# Question 1

journey = read.csv("House journey.csv")
Room = journey$Room

n = length(Room) 
ni = table(Room[-n]) # -> can't include the last state...where do you go from there? nobody knows

doubles = character(n-1)
triplets = character(n-2)

for (j in 1:length(doubles)){
	doubles[j] = paste(Room[j], Room[j+1], sep="")
	}

for (j in 1:length(triplets)){
	triplets[j] = paste(Room[j], Room[j+1], Room[j+2], sep="")
	}
	
nij = table(doubles)
nijk = table(triplets)
tnijk = table(doubles[-length(doubles)])

# Calculate pij_hat -> nij / ni
pij = numeric(length(nij))
names(pij) = names(nij)

for (AB in names(pij)){
	pij[AB] = nij[AB] / ni[substr(AB,1, 1)]
	}

mc.Fit = markovchainFit(Room)
mc.Fit$estimate

trip.table = as.matrix(nijk)
trip.table = cbind(trip.table, NA)
colnames(trip.table) = c("Observed", "Expected")

# ABC ~ Bin(tnAB,pBC_hat) -> E(ABC) = tnAB * pBC_hat)
for (ABC in rownames(trip.table)){
	trip.table[ABC, "Expected"] = tnijk[substr(ABC, 1, 2)] * pij[substr(ABC, 2, 3)]
	}

trip.table

Obs = trip.table[, "Observed"]
Exp = trip.table[, "Expected"]

r = length(nijk)
q = length(nij)
s = length(unique(Room))

dof = r -2*q + s

TS = sum((Obs-Exp)^2 / Exp)
CV = qchisq(0.95, dof)
p.value = pchisq(TS, dof, lower=FALSE)

TS;CV
p.value # -> 2.484316e-15 therfore reject the null hypothesis -> process does not have the markov property

# A lot of dependencies -> going from kitchen to living room, you become tired and then want to sleep -> Bedroom
# -> if you wake up at night, need to go to toilet
# -> We can say that the Markov property does not hold if the probability of going from one room to another is influenced by the room previously occupied
# -> Less likely to visit the toilet from the hall if you've just been to the toilet
