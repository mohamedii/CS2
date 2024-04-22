# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\2 Markov Chains")

#Poisson Processess

set.seed(123)
T=rexp(10,3)
T.sum=cumsum(T)

N=numeric(10)
for (i in 1:length(N)){
	N[i]=i
	}

plot(stepfun(T.sum, c(0,N)), xlab="T", ylab="N(t)",
 do.points=TRUE, pch = 16,col.points = "blue",verticals = FALSE)

#Boat Example on Compound Process
#l=3 per hour, 08h00->20h00 is 12 hours
#l=3*12=36
#Simluate for 20 days

nsims=10000
N=rpois(nsims, l=3*12) #Number of arrivals each day when operating

toll=numeric(nsims)

#for each day, the toll will be the number of arrivals x ppl disembarking x toll

for (i in 1:nsims){
	n=rbinom(N[i], 10, 0.7) # number of ppl disembarking on arrival on day i
	toll[i]=sum(n)*3
	}

toll.mean=mean(toll)
toll.sd=sqrt(var(toll))

toll.mean
toll.sd
hist(toll)

##############################################################################
#Question 1

#P(X>=200) where X~Poisson(l=50*4)
ppois(199,200, lower.tail=FALSE)

#P(Y=3) where Y~Poisson(l=1.805)
dpois(3,1.805)

#Median cusotmer in last 10 minutes of the day 
#  -> l=50 per hour, therefore l=50/6 per 10 minutes
qpois(0.5, 50/6)

#Histogram to show number of customers arriving in 5-minute period, 
# therefor l=50/12,maximum of 15 arrivals

sample.n=1000
sample.X=rpois(sample.n, 50/12)
sample.Xmin=pmin(sample.X,15)
hist(sample.Xmin, freq=FALSE, 
xlab="Number of Customers - Max of 15", ylab="Probability")

#CORRECTION:
barplot(dpois(0:15,50/12),names.arg = 0:15, main = "Distributuon of
customers arriving in a 5-minute period", xlab = "Number of
customers", ylab = "Probability", col = "blue")

#Simulate 1000 runs of customers arriving each week using 
# seed value = 999

set.seed(999)
sample.n=1000
sample.X=rpois(sample.n, 50*8*7)
hist(sample.X, freq=TRUE, xlab="Number of Customers", ylab="Frequency")

# Remember that Poisson(lamda) -> N(lambda, lambda) as lambda gets large...
# by the Central Limit Theorem

#Revenue simulaiton where each pasty is $4, 
# number of pasties bought ~ Poisson(1.805)

set.seed(123)
s=numeric(sample.n)
sample.X=rpois(sample.n, 50*8*7)
for (i in 1:sample.n){
	s[i]=sum(4*rpois(sample.X[i], 1.805))
	}

#Prob(s>=19800)
p=length(s[s>=19800])/length(s)
p


#Revenue simulaiton where each pasty is $4, 
# number of pasties bought ~ Poisson(1.805)

set.seed(123)
s2=numeric(sample.n)
sample.X=rpois(sample.n, 50*8.1666667*7)
for (i in 1:sample.n){
	s2[i]=sum(4*rpois(sample.X[i], 1.805))
	}

#Prob(s2>=19800)
p2=length(s2[s2>=19800])/length(s)
p2

# - Unlikely model in time-homogenous. There will be certain times of the day
# where customers more likely to visit shop especially weekends.
# - Number of pasties bought may not be indepedent of the time of day/week.
# You may find office workers buy one or two. Families buy more 
# - Keeping the shop open in reality may not have the effect calculated.
# People come to the shop when they know its open, not when its about to close
# Profit is more of a concern than revenue
# Consider the price of pasties on revune and profit growth

##############################################################################
#Question 2


claims = read.csv("Claims Data.csv")
claims$claim.date = as.Date(claims$claim.date, format("%d/%m/%Y"))
head(claims)

lambda=nrow(claims)/10

house=claims[claims$claim.type=="House", ]
motor=claims[claims$claim.type=="Motor", ]
travel=claims[claims$claim.type=="Travel", ]

l.house=nrow(house)/10
l.motor=nrow(motor)/10
l.travel=nrow(travel)/10

#Thinning the process

p.house=nrow(house)/nrow(claims)
p.motor=nrow(motor)/nrow(claims)
p.travel=nrow(travel)/nrow(claims)

l.house.thin=p.house*lambda
l.motor.thin=p.motor*lambda
l.travel.thin=p.travel*lambda

#Incorrect insurance claim data on house type up to end 2010
# -> 1 Jan 2011 onwards

new.start.date=as.Date("01/01/2011", format("%d/%m/%Y"))
new.end.date=as.Date(tail(claims$claim.date,1), format("%d/%m/%Y"))

new.claims=claims[claims$claim.date >= new.start.date & claims$claim.date <= new.end.date, ]
new.house=new.claims[new.claims$claim.type=="House", ]
new.l.house=nrow(new.house)/7 #Correction - dont make this mistake again -> the period has change to 7 years begining from the new claims

l.house.q=new.l.house*0.25
n=qpois(0.9,l.house.q)

l.house.m=new.l.house*1/12
pi=ppois(5,l.house.m, lower=FALSE)
prob=pi+(1-pi)*pi+(1-pi)^2*pi


##############################################################################
#Question 1

# Customers arrive according to a Poisson Process: N ~ Poisson(50/h * t) -> Shop open 8hrs a day, 7 days a week
# Pasties cost $4...aim to collect $19,800 each week3 ->
# The number of pasties bought by customer, NP ~ Poisson(1.805)

lN = 50
ppois(199, lambda * 4, lower=FALSE)

lNP = 1.805
dpois(3, lNP)

m = qpois(0.5, lN*8) # Median number of customers in a day
m

m = qpois(0.5, lN/6) # Median number of customers on 10 minutes
m

# Bar chart of showing probability distribution of number of customers arriving in any given 5-minute period up to maximum of 15 arrivals
barplot(dpois(0:15, lN/12), names.arg=0:15, main = "Distributuon of customers arriving in a 5-minute period", xlab = "Number of customers", ylab = "Probability", col = "red")

set.seed(999)
sample.N = rpois(1000, lN*8*7)
hist(sample.N, prob=FALSE, xlab="Number of Weekly Customers", ylab = "Frequency")
# This looks symmetrical, which makes sense -> CLT -> implies that Poisson(lambda) -> N(lambda, lambda) as lambda -> Inf

hist(sample.N, prob=TRUE, xlab="Number of Weekly Customers", ylab = "Frequency")
lines(2600:3000, dnorm(2600:3000, lN*8*7, sqrt(lN*8*7)), col="blue")

# Revenue ~ Compound Poisson
set.seed(123)
revenue = numeric(1000)

for (j in 1:1000){
	revenue[j] = 4*sum(rpois(sample.N[j], lNP))
	}

s = revenue

prob = length(s[s>=19800])/length(s)
# 0.805

# Keeping the shop open for 10 minutes extra each day
set.seed(123)
revenue = numeric(1000)

sample.N = rpois(1000, 50*7*(8+10/60))

for (j in 1:1000){
	revenue[j] = 4*sum(rpois(sample.N[j], lNP))
	}

s2 = revenue

prob = length(s2[s2>=19800])/length(s2)
# 0.964

# Comment on Assumptions
# -> The model is unlikely to be time homogenous. There will be certain times of the day when customers are much
# more likely to visit the shop, and there will be days of the week (probably weekends) that are more popular than others
# -> The number of pasties bought may also not be indepedent of the time of day or week either. During the week, might get lots of office workers
# buying on pasty for lunch, on weekends you might get more families buying 4 or 5 pasties
# -> Keeping the shop open for another 10 minutes may not increaes the number of customers arriving in the shop that much
# -> Owner might be more concerned with profit than revenue
# -> Investigate the cost of the pasty...increasing or decreasing the the $4 cost -> impact on number of pasties sold and where the optimal price is to maximize profits


##############################################################################
#Question 2

claims = read.csv("Claims Data.csv")
claims$claim.date = as.Date(claims$claim.date, format("%d/%m/%Y"))
head(claims)

lambda = nrow(claims) / 10

type = table(claims$claim.type)
# 1: House, 2: Motor, 3: Travel

lambda_H = type[1]/nrow(claims) * lambda
lambda_M = type[2]/nrow(claims) * lambda
lambda_T = type[3]/nrow(claims) * lambda # Don't forget, when you're thinning...the proportion must be mutiplied by process Lambda

correct.start.date = as.Date("01/01/2011", format("%d/%m/%Y"))
new.claims = claims[claims$claim.date >= correct.start.date, ]
new.type = table(new.claims$claim.type)
# 1: House, 2: Motor, 3: Travel

new.lambda = nrow(new.claims) / 7
new.lambda_H = new.type[1]/nrow(new.claims) * new.lambda

# Trigger investigation into received claims if either the following occur:
# -> The number of house insurance claims arriving within the quarter is within the upper 10% point of the distribution
# -> The number of house insurance claims arrive in at least on calendar month exceeds 5

# What is the quantile at 10%
q_H = qpois(0.9, new.lambda_H/4)
# 9

# Probability that investigation is triggered during the quarter due to more than 5 arrivals in any one calendar month
p = ppois(5, new.lambda_H/12, lower=FALSE)
# out of 3 months in a quarter, want at least ONE month to be a trigger month -> assume each month is indepedent -> trigger month ~ Bin(3, p)
dbinom(1,3, p) + dbinom(2,3, p) + dbinom(3,3, p)
# Alternatively ->
1 - dbinom(0, 3, p)
# Or...first month there is a trigger, second month, or third month ~ Geometric distribution with first succcess, then one failuure and second success, then two failures and third success
p + (1-p)*p + (1-p)^2*p
