# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\7 Estimating the Lifetime Distribution")

# Survival function estimation with the survival package
# survfit()

surv.data=read.csv("surv_data.csv")
surv.data

# install.packages("survival")
# library(survival)

# Create survival object using Surv() -> create survival objects in R using surv()
# Surv(event times, event code) -> surv(event times, event code)

# even times -> vector of times each life experienced an event -> event times is a vector of times each life experience an event
# event code -> vecotr indicating whether the event that each life experience was death (value = 1)
# or right censoring (value = 0)

# we will create the survival object using Surv() on surv_data.csv

# We need to calculate event times for each patient as the difference btw end and start time of operation

# event times
surv.data$Event.time=surv.data$End-surv.data$Op_time
surv.data

# event codes
# Surv() function requires a vecotr of 1s and 0s for deaths or censorings.
# The data has death or censoring but not in binary - concert to binary
surv.data$Event.code=ifelse(surv.data$Reason == "Death",1,0)


# Now create the survival object
surv.obj=Surv(surv.data$Event.time, surv.data$Event.code)

# Result -> surv.obj -> 120+  68   40  116+  30+  30  100+  71   40   35   50   30 
# Censored times have the + ... indicating time of death after some event time

# The survfit() function
# To calculate the estimate of the survival function,-> use survfit()
# survfit(formula = <survival object> ~1, conf.type="plain", conf.int=0.95, stype=1)

# We need a formula to indicate what data we want to use to estimate the survival function.
# The structure of a formula object is a reponse variable, followed by ~ sign, followed by terms to include
# terms such as explanatory survival functions. the response variable needs to be a survival object
# We want to fit a curve to the entire data set w/o using any explanatory variables. 
# We do this by inputting a 1 on the right hand side of the ~ tilda
# The method "plain" corresponds to the method used to construct confidence intervals for the KM, NA methods
# However the CI for the KM method doesn;t give the same approximate CI for the NA method.
# stype stands for survival model type. We set this:
# -> KM=1
# -> NA=2
# if we dont set this, the default is 1 i.e. KM

# Calculating KM_S(t)
# Use the survfit() to calc KM estimate of the survival function using survival object surv.obj
# We also approximate a 95% CI
fitKM=survfit(surv.obj ~1, conf.type="plain", conf.int=0.95, stype=1)
fitKM=survfit(surv.obj ~ Drug, conf.type="plain", conf.int=0.95, stype=1, data = data_main) # !!!!!!!!!! Here we use Drug as an explanatory variable!

# Remember that surv.obj=Surv(Event.time, Event.code)

# Use summary() to see the estimated values

summary(fitKM)
# time contains tj
# n.risk contains nj
# n.event contains dj
# survival contains S^KM(t)
# std.err is the estimate standard error of S~(t) -> estimator of S(t)
# lower 95% CI, upper 95% CI are the confidence intervals

# Therefore have the estiamte of the survival function
# We need to extract the summary info ->

fitKM.summary=summary(fitKM)
fitKM.summary

fitKM.summary$surv # -> survival probability
fitKM.summary$lower # -> lower CI bound
fitKM.summary$upper # -> upper CI bound

# we can also extract info from:

fitKM$time # fitKM$time includes the censoring at times 100, 116, 120
fitKM.summary$time # gives the time where deaths observed

# We can get estiamte probabilities -> estiamte probability that a patient survives
# until at least time 45 is 0.556
# extract it in the following way ->

min(fitKM.summary$surv[fitKM.summary$time <= 45])


# Calculating NA_S(t) -> stype=2

fitNA=survfit(surv.obj ~1, conf.type="plain", conf.int=0.95, stype=2)
summary(fitNA)
# time contains tj
# n.risk contains nj
# n.event contains dj
# survival contains S^NA(t)
# std.err is the estimate standard error of S~(t) -> estimator of S(t)
# lower 95% CI, upper 95% CI are the confidence intervals

fitNA.summary=summary(fitNA)
min(fitNA.summary$surv[fitNA.summary$time <= 45])

# Plotting survival functions
plot(fitKM, main="KM survival function estimate", xlab="SKM(t)", ylab="time(days)", conf.int=FALSE)
# plot also provides the confidence interval for the estimate

plot(fitNA, main="KM survival function estimate", xlab="SNA(t)", ylab="time(days)")
# plot also provides the confidence interval for the estimate

# We can suppress the confidence intervals lines in the plot by conf.int=FALSE

# Plotting both KM and NA
plot(fitKM, main="KM and NA survival function estimates", xlab="time(days)", ylab="S(t)", col="orange")
lines(fitNA, col="blue", lwd=1)
legend("topright", legend=c("KM", "NA"), col=c("orange", "blue"), lwd=1)

# Esimtaing survival function based on a grouping
# We may be interested to estimate survival function on a different group of lives

# Recall surv.data
surv.data

# Column "Group" split the patients into two categories - >
# Group A patients who had another operation 12 months prior to this One
# Group B are patients having their first operation in the last 12 months

# We can calculate the KM estimate fo the survival function for these individual groups
# using survfit() ->
# survfit(formula=<survival object> ~ <grouping variables>, conf.type="plain", conf.int=0.95, stype=1)

# If there are multiple grouping variables, then <grouping variables> -> <var 1> + <var 2> + ... <var n>

# So using Group as the explnatory / groupoing variable ->

fitKM.group=survfit(surv.obj ~ surv.data$Group, conf.type="plain", conf.int=0.95, stype=1)

plot(fitKM.group, col=c("blue", "orange"), main="KM estimate of survival function", xlab="Time", ylab="SKM(t)")
legend("topright", legend=c("A", "B"), col=c("blue", "orange"), lwd=1)


# Calculating tj and dj
# Manually constructed a table that contains j, tj, nj, dj, lambda_j
# First calculate tj and dj together
# The tj represents unique times at which one or more deaths were oberved

# Use the table function to calc counts of each of the values that appear in a vector. This will 
# give us the how amny deaths there were for each death time

# We use table() on Event.time being carful to only select rows corresponding to deaths
# Event.time -> corresponding to deaths

deaths=table(surv.data$Event.time[surv.data$Reason =="Death"]) # -> this will give tj's and the table function with summaries dj's
deaths

# Figures correspond exactly to tj and dj

# Now turn deaths into data frame ->

deaths=as.data.frame(deaths)
colnames(deaths)=c("tj", "dj")

# tj, even though numbers, are strings in the data frame
# We want R to recognise the entries as forming a numeric vector instead

deaths$tj=as.numeric(as.vector(deaths$tj))

# Imporatnt...use both the as.numeric() and as.vector() -> it will make vector operations
# easier to do

# Calculating nj
# To calculate nj, we need to work out how many lives were at risk just before each death occured.
# For our data set, this is the same as how many lives experience an event at or after each time of death
# If the data contained some entries, there would be left-truncated data. We would need to account for this in
# the calculations -> censored lives before first death -> nj

# surv.data$Event.time >= deaths$tj[i] -> this will work out how many lives are risk before death occured.
# How would you account for left-truncated data?

# Order surv.data by Event.time ->

ordered.surv.data=surv.data[order(surv.data$Event.time), ]
# You'll see that there is a censored life but occuring at Event.time also equal to two deaths. Treat this as death first
# Censor the life after the death has occured. So n1=12, d1=2, t1=30, c1=1
# If I were you, order the table...work with ordered Event times. Makes it easier to confirm the numbers

deaths$nj=numeric(nrow(deaths))

for (i in 1:nrow(deaths)){
	deaths$nj[i]=sum(surv.data$Event.time >= deaths$tj[i])
	}
	
deaths

# Calculating lambda j -> dj/nj
deaths$lambdaj=numeric(nrow(deaths))

deaths$lambdaj=deaths$dj/deaths$nj

deaths

# Calculating S^KM(t)
# S^KM(t)=Product(tj<t){1-lambdaj) -> use the cumprod() function

deaths$Skm=numeric(nrow(deaths))

deaths$Skm=cumprod(1-deaths$lambdaj)

deaths

# Alternative method -> Your method

deaths$Skm=numeric(nrow(deaths))
deaths$Skm[1]=1-deaths$lambdaj[1]
for (i in 2:nrow(deaths)){
	deaths$Skm[i]=deaths$Skm[i-1]*(1-deaths$lambdaj[i])
	}

deaths

# Calculating S^NA(t)
# S^NA(t)=exp(-Lambdat) -> exp(-sum(tj<t){lambdaj})
# -> determine the integrated hazard Lambdat

deaths$Lambda.tj=numeric(nrow(deaths))
deaths$Sna=numeric(nrow(deaths))

deaths$Lambda.tj[1]=deaths$lambdaj[1]
deaths$Sna[1]=exp(-deaths$Lambda.tj[1])

for (i in 2:nrow(deaths)){
	deaths$Lambda.tj[i]=deaths$Lambda.tj[i-1]+deaths$lambdaj[i]
	deaths$Sna[i]=exp(-deaths$Lambda.tj[i])
	}
	
deaths 

# Alternative -> faster but be careful way
deaths$Lambda.tj=numeric(nrow(deaths))
deaths$Sna=numeric(nrow(deaths))

deaths$Lambda.tj=cumsum(deaths$lambdaj)
deaths$Sna=exp(-deaths$Lambda.tj)

deaths

# Plotting SKM(t) and SNA(t)
# plot the survical functions with argument type="s" for a step function

# Need to complete the times...the first time in the deaths table is 30, last 71
# But the survival function estimate is from time 0 -> 120
# Time 120 is the last time of an even in the data and related to a patient being right censored
# To get the correct time values:

times=c(0, deaths$tj,120)

# Similarly for the surival function -> follow these steps

SKM = c(1, deaths$Skm, deaths$Skm[nrow(deaths)])
SNA = c(1, deaths$Sna, deaths$Sna[nrow(deaths)])

plot(times, SKM, type="s", main="KM and NA survival function estiamte", xlab="time (days)", ylab="Survival Function (t)", col="blue")
lines(times, SNA, type="s", col="red")
legend("topright", col=c("blue","red"), legend=c("KM", "NA"), lwd=2)

# Calculating Confidence intervals

# CI for SKM(t)
# S^KM(t) +- 1.96 * sqrt(Var(S~KM(t)))
# Var(S~KM(t)) -> (S^KM(t))^2 * sum(tj<t){dj/(nj*(nj-dj))} -> Greenwoods formula

# CI for SNA(t)
# Lambda.tj) +- 1.96 * sqrt(Var(Lambda.tj))
# Var(Lambda.tj)=sum(tj<t){dj*(nj-dj)/(nj^3)}
# exp(-Lambda.tj) -> make sure to SWAP the lower95, and upper95 CI for Lambda.tj -> for SNA CI

# Don't forget to clip!!


# First tackle the quotients)
deaths$Q.km=numeric(nrow(deaths))
deaths$Q.na=numeric(nrow(deaths))

deaths$Q.km=cumsum(deaths$dj/(deaths$nj*(deaths$nj-deaths$dj)))
deaths$Q.na=cumsum(deaths$dj*(deaths$nj-deaths$dj)/((deaths$nj)^3))

deaths$VarSkm=numeric(nrow(deaths))
deaths$VarSkm=((deaths$Skm)^2)*deaths$Q.km

deaths$VarLambda.tj=numeric(nrow(deaths))
deaths$VarLambda.tj=deaths$Q.na

# Standard errors
Z=qnorm(0.975) # -> Remember that its on both sides of the distribution -> alpha/2=0.025 -> 1-alpha/2 = 0.975

deaths$SEkm=numeric(nrow(deaths))
deaths$SEkm=sqrt(deaths$VarSkm)

deaths$SEna=numeric(nrow(deaths))
deaths$SEna=sqrt(deaths$VarLambda.tj)

KM_CI=data.frame(tj=deaths$tj, lower_95=pmax(deaths$Skm-Z*deaths$SEkm,0), Skm=deaths$Skm, upper_95=pmin(deaths$Skm+Z*deaths$SEkm,1))
NA_CI=data.frame(tj=deaths$tj, lower_95=exp(-pmax(deaths$Lambda.tj+Z*deaths$SEna,0)), Sna=exp(-deaths$Lambda.tj), upper_95=exp(-pmax(deaths$Lambda.tj-Z*deaths$SEna,0)))
# Be careful here with CI for NA -> the upper95 CI for the Lambda is used in the lower95 for the Survival and vv.

# A few things to always check -> having CI probabilities > 1 must be clipped to 1 hence the pmin, and pmax if CI probability < 0 for KM
# When working with NA CI...at the Lambda.tj level, integrated hazards cannot be < 0 i.e. CI for Lambda.tj must be >=0 hence the pmax.
# Then you need to swap the CI lower95 and upper 95 for Lambda.tj -> CI for Sna because of the probability transform -> exp(-Lambda.tj)

# Check

summary(fitKM)$lower
summary(fitKM)$upper

summary(fitNA)$lower
summary(fitNA)$upper


#########################################################################################
# Question 1

survival.data=read.csv("surv_table_7.1.csv")
survival.data
survival.data$Event.time=survival.data$observation_end-survival.data$exposure_time
survival.data$Event.code=ifelse(survival.data$reason == "D",1,0)

survival.obj=Surv(survival.data$Event.time, survival.data$Event.code)

fitKM=survfit(survival.obj ~1, conf.type="plain", conf.int=0.95, stype=1)
fitKM.summary=summary(fitKM)

plot(c(0, fitKM.summary$time), c(1, fitKM.summary$surv), type="s", main="KM Survival Function", xlab="weeks", ylab="probability")
plot(fitKM, main="KM Survival Function", xlab="weeks", ylab="probability")

#str(fitKM)
#str(fitKM.summary)

# 95% CI for SKM(7) ->
c(fitKM.summary$lower[3], fitKM.summary$upper[3])

fitNA=survfit(survival.obj ~1, conf.type="plain", conf.int=0.95, stype=2)
fitNA.summary=summary(fitNA)

plot(c(0, fitNA.summary$time), c(1, fitNA.summary$surv), type="s", main="NA, KM Survival Function", xlab="exposure_time", ylab="probability", col="blue")
lines(c(0, fitKM.summary$time), c(1, fitKM.summary$surv), type="s", col="red")
legend("topright", col=c("blue","red"), legend=c("NA", "KM"), lwd=2)

plot(fitNA, main="NA Survival Function", xlab="weeks", ylab="probability")


#########################################################################################
# Question 2

surv.data=read.csv("surv_table_7.2.csv")
surv.data

# 100 patients observed

deaths=data.frame(tj=surv.data$Time[surv.data$Deaths>0], dj=surv.data$Deaths[surv.data$Deaths>0], cj=c(0,0,5,0,10,0,2,66))

deaths$nj=numeric(nrow(deaths))
deaths$nj[1]=100
for (i in 2:nrow(deaths)){
	deaths$nj[i]=deaths$nj[i-1]-sum(deaths$dj[i-1], deaths$cj[i-1])
	}

deaths

# Calculating the KM survival estimates
# Require lambdaj

deaths$lambdaj=numeric(nrow(deaths))
deaths$lambdaj=deaths$dj/deaths$nj

deaths$Skm=numeric(nrow(deaths))
deaths$Skm=cumprod(1-deaths$lambdaj)

plot(c(0, deaths$tj, surv.data$Time[nrow(surv.data)]), c(1, deaths$Skm, deaths$Skm[nrow(deaths)]), xlab="Time (weeks)", ylab="Survival Probability",
	main="Plot of KM Survival Estimate", type="s", col="blue")

deaths$Lambda.tj=numeric(nrow(deaths))
deaths$Lambda.tj=cumsum(deaths$lambdaj) # -> integrated hazard

deaths$VarNA=numeric(nrow(deaths))
deaths$VarNA=cumsum(deaths$dj*(deaths$nj-deaths$dj)/(deaths$nj^3))

Z=qnorm(0.975)

CI_NA_Lj=data.frame(tj=deaths$tj, lower95=pmax(deaths$Lambda.tj-Z*sqrt(deaths$VarNA),0), Lj=deaths$Lambda.tj, upper95=pmax(deaths$Lambda.tj+Z*sqrt(deaths$VarNA),0))
CI_NA_Lj

plot(CI_NA_Lj$tj, CI_NA_Lj$lower95, type="s", lty=2, col="blue")
lines(CI_NA_Lj$tj, CI_NA_Lj$Lj, type="s", col="green")
lines(CI_NA_Lj$tj, CI_NA_Lj$upper95, type="s", lty=2, col="red")
# You need to fix start and end values...dont forget applying limits because the chart cuts off

event.times = c(rep(surv.data$Time, times = surv.data$Deaths),
rep(surv.data$Time, times = surv.data$Censorings))
event.codes = c(rep(1, times = sum(surv.data$Deaths)),
rep(0, times = sum(surv.data$Censorings)))

surv.obj=Surv(event.times, event.codes)
fitNA=survfit(surv.obj ~1, conf.type="plain", conf.int=0.99, stype=2)
fitNA.summary=summary(fitNA)

CI_NA_fitNA=data.frame(tj=fitNA.summary$time, lower99=fitNA.summary$lower, SNA=fitNA.summary$surv, upper99=fitNA.summary$upper)
CI_NA_fitNA

plot(CI_NA_fitNA$tj, CI_NA_fitNA$lower99, type="s", lty=2, col="blue")
lines(CI_NA_fitNA$tj, CI_NA_fitNA$SNA, type="s", col="green")
lines(CI_NA_fitNA$tj, CI_NA_fitNA$upper99, type="s", lty=2, col="red")
# You need to fix start and end values...dont forget applying limits because the chart cuts off

# SKM <= SNA

fitKM=survfit(surv.obj ~1, conf.type="plain", conf.int=0.99, stype=1)
fitKM.summary=summary(fitKM)

S=data.frame(SKM=fitKM.summary$surv, SNA=fitNA.summary$surv)
S["SKM"]<=S["SNA"]

#########################################################################################


# Survival function estimation with the survival package
# survfit()

surv.data=read.csv("surv_data.csv")
surv.data

# install.packages("survival")
# library(survival)

# Create survival objects using surv() ->
# Surv(event times, event code)
# -> event times vector of times that each life experience event
# -> event code - > vector indicating death (1) or censored (0)

# -> next step is to create a survival object using Surv() of surv_data.csv

# Event times ->
# Process data in terms of duration since having operation -> timeline staring at 0 (start time of operation) and records the events of death or censoring for each patient
surv.data
# -> create a column call Event.time
surv.data$Event.Time = surv.data$End - surv.data$Op_time
surv.data

# Event codes ->
# Surv() function expects a vector of 1s and 0s that denote death and censoring
# The data we have is recorded as "Death" or "Censored" -> need to convert
surv.data$Event.code = ifelse(surv.data$Reason == "Death", 1, 0)

# Now create the curvival object ->

surv.obj = Surv(surv.data$Event.Time, surv.data$Event.code)

# To calculate the estimate of the survival function -> use survfit()
# -> survfit(formula = survival object ~ 1, conf.type = "plain", conf.int = 0.95, stype = 1) ## -> NOTE THE ~ -> its 1 for the survival function estimation -> When we do PROPORTIONAL HAZARDS -> linear covariates
# tilde symbol,
# -> ~ followed by any terms we wish to include as explanatory survival factors
# -> ~ 1 means this is the structure used to fit the null model

# CI -> conf.tpye = "plain" -> KM confidence interval  method -> note that it doesn't give the same approximate CI for the NA method
# stype = 1 for KM and 2 for NA -> default is 1 i.e. default is KM

# Calculating S^KM() ->
# We will use survfit() on the surv.obj create -> surv.obj = Surv(event times, event codes)
# Fit survfit() to calc KM estimate of the survival function with 95% CI
fitKM = survfit(surv.obj ~ 1, conf.type = "plain", conf.int = 0.95, stype = 1)
str(fitKM)

summary(fitKM)
# time contain tj
# n.risk = nj
# n.event = dj
# survival = S^KM(t)
# std.error -> estimated standard error of S^~(t) -> the estimator of S(t)
# lower.95 and upper.95 -> CI for S(t) -> conf.int = 0.95 is by default

fitKM.summary = summary(fitKM)
# extract various quaitities from the summary ->

fitKM.summary$surv # -> surival function probabilities
fitKM.summary$lower
fitKM.summary$upper # -> CI 

# fitKM$surv -> can also be extracted from the fit itself but looks slightly different to the fitKM.summary$surv
# -> fitKm$surv contains estimates corresponding to all times when death or censoring occured
# -> fitKM.summary$surv contains only estimates only corresponding to death times

# We can see this by outputting time component of each object ->

fitKM$time
fitKM.summary$time

# We can read directly off the table -> estimated probability that a patient survives until at least tim 45 is 0.556
# Also can extract ->

min(fitKM.summary$surv[fitKM.summary$time <= 45])
# min(0.8333333 0.7407407 0.5555556) -> 0.5555556

# Calculating S^NA(t)
fitNA = survfit(surv.obj ~ 1, conf.type = "plain", conf.int = 0.95, stype = 2)
fitNA.summary = summary(fitNA)

# As before ->
# time contain tj
# n.risk contain nj
# n.event contains dj
# survival constains S^NA(t)
# std.error contain estimated standard error S^~NA(t)
# R calculates the CI slightly differnt than core reading

# We can extract information as we did for fitKM and fitKM.summary


# Plotting the Survival Function estimates -> S(t)
plot(fitKM, main = "KM Survival Function Estimate", xlab = "time (days)", ylab = "SKM(t)") # -> with CI
plot(fitKM, main = "KM Survival Function Estimate", xlab = "time (days)", ylab = "SKM(t)", conf.int=FALSE) # -> without CI

plot(fitNA, main = "NA Survival Function Estimate", xlab = "time (days)", ylab = "SNA(t)") # -> with CI
plot(fitNA, main = "NA Survival Function Estimate", xlab = "time (days)", ylab = "SNA(t)", conf.int=FALSE) # -> without CI

plot(fitKM, main = "KM and NA Survival Finction Estimates", xlab = "time (days)", ylab = "S(t)", col = "orange", conf.int = FALSE)
lines(fitNA, col = "blue", lwd = 1, conf.int = FALSE)
legend("topright", legend = c("KM", "NA"), col = c("orange", "blue"), lwd = 1)

# Estimating the Survival Function based on a Grouping -> A, B
# Examining the data set ... surv.data -> column Group that splits patients into two categories
# -> A relates to patients who had another operation in the 12 months prior to this One
# -> B are ptients who did not

# We can calculate the KM estimate of the survival function for these individual groups usig survfit() ->
# survfit(survival object (using Surv...event time, event code) ~ <groupoing of variables>, conf.type = "plain", conf.int = 0.95, stype = 1)
# See what comes after the ~ -> <grouping of variables>

# If there are multiple grouping of variables -> <var 1> + <var 2> + ... + <var n>
# Using Group  as the EXPLANATORY  / GROUPING variable ->

fitKM.group = survfit(surv.obj ~ surv.data$Group, conf.type = "plain", conf.int = 0.95)
summary(fitKM.group)

# So here we have two separate estimates -> One base on patients in Group A and the other for those in Group base
# We can plot both of these ->

plot(fitKM.group, col = c("orange", "blue"), main = "KM estimate of the survival function by group", xlab = "Time", ylab = "SKM(t)")
legend("topright", legend = c("Group A", "Group B"), col = c("orange", "blue"), lwd = 1)

# It appears from the plot that the survival is generally worse for those patients who had a previoud operation ni the 12 months prior to this One
# However note that the sample size is quite small -> Always check the information
# Note -> the estimate for Group A ends at time 68 with the last section being a vertical line because the last event in Group A was a death at this time
# Note -> the last even in Group B was at time 120 and the life was censored -> survival function estimate ends at time 120 with the last section being a horizontal line


# R Code  - Survival Function Estimation in R
surv.data = read.csv("surv_data.csv")
surv.data$Event.time = surv.data$End - surv.data$Op_time

# Calculating tj and dj
# Manually construct a table that contains:
# -> j
# -> tj
# -> dj
# -> nj
# and -> lambda^j

# First calculate tj and dj togehter ->
# Recall that tj representsthe unique times at which one or more deaths were observed

# The table(<vector>) function calculates the counts of each of the value in <vector>
# -> Exactly what we need -> how many deaths there were dj for each death time tj

# use table() on Event Times ->
deaths = table(surv.data$Event.time[surv.data$Reason == "Death"]) # -> this excludes the censoring! -> note the query -> Event.time[Reason == Death]
# Shows the deaths in surv.data -> surv.data[surv.data$Reason == "Death", ]

deaths = as.data.frame(deaths)
colnames(deaths) = c("tj", "dj")
deaths

str(deaths)
# R recognises tj as a factor -> more helpful to ensure R recognises the entries forming a NUMERIC VECTOR instead ->
# use -> as.numeric() and as.vector()

deaths$tj = as.numeric(as.vector(deaths$tj))
str(deaths) # -> tj now a a numeric vector

# Alternatively use the aggregate() function -> aggregate([variable] ! [groupong variable], FUN = [functions], data = [data set])
deaths = aggregate(Event.time ~ c(Event.time), FUN  = length, data = surv.data[surv.data$Reason == "Death", ])
# -> Clever function -> LEARN HOW TO USE! especially how Even.time ~ c(Event.time) -> you require the c(...) vector of Event.time -> otherwise it doesn't give correct results
colnames(deaths) = c("tj", "dj")
deaths

# Another alternative ->
deaths = aggregate(Reason ~ Event.time, FUN = length, data = surv.data[surv.data$Reason == "Death", ]) # -> didnt c(...) -> only c(...) when you aggregate on itself!
colnames(deaths) = c("tj", "dj")
deaths

# Calculating nj
# To calculate nj -> need to know how many lives were at risk just before each death
# -> For data set this is the same as how many lives esperienced an event at or after each time of death
# -> NOTE: if data contained truncated entreis -> left truncated data then we would need to account for this in calculations

# To caculate nj -> use logical expression -> returns TRUE of FALSE
# Example: below expression will return TRUE or FALSE for each event time to indicate whether it is at or after the second death time
surv.data$Event.time >=deaths$tj[2]
# -> TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
# -> I dont like theat Event Time is nor sorted ascendingly -> use sort(surv.data$Event.time)

# -> we want collect all the TRUE in the logical expression ->
sum(surv.data$Event.time >= deaths$tj[2]) # -> 9
# That is -> n2 = 9

# We can work out nj using sapply ->
deaths$nj = sapply(1:nrow(deaths), function(j){sum(surv.data$Event.time >= deaths$tj[j])})
# Sapply is a powerful function!

# Alternatively -> Easier to interpret...you got two methods
deaths$nj = sapply(deaths$tj, function(tj){sum(surv.data$Event.time >= tj)})

# For loop -> breaking it down to basics
deaths$nj = numeric(nrow(deaths))
for (j in 1:nrow(deaths)){
	deaths$nj[j] = sum(surv.data$Event.time >= deaths$tj[j])
	}
	
deaths

# Calculating lambdaj
deaths$lamj = deaths$dj / deaths$nj
deaths

# Calcuating S^KM(t) -> Product[tj <t](1 - lambda[tj])
deaths$SKM = cumprod(1-deaths$lamj)
deaths

# Calculating S^NA(t) -> exp(-sum[tj < t](lamda[tj]))
deaths$Lj = cumsum(deaths$lamj)
deaths$SNA = exp(-deaths$Lj)
deaths

# Plotting S^KM(t) and S^NA(t) ->
# We need to plot a step function -> set type = "s" instead of type = "l"
# -> type = "s" is for step whereas type = "l" is for line

# Note that deaths$tj is incomeplete in a sense -> unless the first and last tj are deaths (tj is complete) we need to account for the first and last time point in Event.time
c(0, deaths$tj, 120)
length(c(0, deaths$tj, 120))

# Also include that we start from 1 with SKM, and the last point in SKM will depend on nj -> if nj > 0 at end...we need to repeat the last SKM value to keep the line horizontal
# This is also to match the Even.time of 120...because nj[end] > 0 -> nj[end] = 4
c(1, deaths$SKM, deaths$SKM[nrow(deaths)])
length(c(1, deaths$SKM, deaths$SKM[nrow(deaths)]))

plot(c(0, deaths$tj, 120), c(1, deaths$SKM, deaths$SKM[nrow(deaths)]), type = "s", main = "KM and NA Survival Function Estimate", xlab = "time (days)", ylab = "S(t)", col = "blue")
lines(c(0, deaths$tj, 120), c(1, deaths$SNA, deaths$SNA[nrow(deaths)]), type = "s", col = "red") # -> dont forget to change KM to NA
legend("topright", legend = c("SKM(t)", "SNA(t)"), col = c("blue", "red"), lwd = 1)

# Calculating SKM(t) Confidence Intervals -> require the Var(S^~KM(t))
# -> Greenwoods formula for variance
# CI for SKM(t) -> S^KM(t) +- 1.96 * sqrt(Var(S^~KM(t)))

# Greenwoods Formula for Var(S^~KM(t)) -> [S^KM(t)]^2 * sum(tj < t)[dj / (nj * (nj - dj))]
# To get the standard error -> sqrt([S^KM(t)]^2 * sum(tj < t)[dj / (nj * (nj - dj))])
# -> S^KM(t) * sqrt(sum(tj < t)[dj / (nj * (nj - dj))])

temp = cumsum(deaths$dj / (deaths$nj * (deaths$nj - deaths$dj)))
deaths$KM.se = deaths$SKM * sqrt(temp) # -> this is the standard error as defined two lines above -> dont forget the sqrt!

# To get the 1.96 value accurately -> z = qnorm(0.975)
z = qnorm(0.975) # -> both tails sum to 5%

KM_CI = data.frame(tj = deaths$tj, lower_95 = (deaths$SKM - z * deaths$KM.se), estimate = deaths$SKM, upper_95 = (deaths$SKM + z * deaths$KM.se))
# On inspection, we see the upper_95 has valus greater than 1 -> remember SKM is an estimate of a probability -> its max value is 1

# Remember that: YOU HAVE TO THINK
# pmin(vector , 1) -> for the upper end
# pmax(vector, 0) -> for the lower end

KM_CI = data.frame(tj = deaths$tj, lower_95 = pmax((deaths$SKM - z * deaths$KM.se), 0), estimate = deaths$SKM, upper_95 = pmin((deaths$SKM + z * deaths$KM.se), 1))

# We can check our estimates for the CI against the fitKM model from survfit()
summary(fitKM)$lower
summary(fitKM)$upper
# -> The same...good

# Plot the CI with the Survival Funciton Estimate ->
plot(c(0, deaths$tj, 120), c(1, deaths$SKM, deaths$SKM[nrow(deaths)]), type = "s", main = "KM Survival Function Estimate with Lower and Upper 95% CI", xlab = "time (days)", ylab = "S(t)", col = "blue", ylim = c(0,1))
lines(c(0, deaths$tj, 120), c(1, KM_CI$lower_95, KM_CI$lower_95[nrow(KM_CI)]), type = "s", lty = 2, col = "orange")
lines(c(0, deaths$tj, 120), c(1, KM_CI$upper_95, KM_CI$upper_95[nrow(KM_CI)]), type = "s", lty = 2, col = "orange")
legend("topright", legend = c("KM lower 95", "KM Estimate", "KM upper 95"), col = c("orange", "blue", "orange"), lwd = 1)

# Calculating SNA(t) Confidence intervals
# -> Here we first determine the CI for Lambda(t) for tj < t -> remeber tha Lambda(t) is the INTEGRATED HAZARD
# -> Lambda^(t) +- 1.96 * sqrt(Var(Lambda^~(t)))

# Var(Lambda^~(t)) -> sum[tj < t] (dj * (nj - dj) / (nj^3)) -> Refer to notes quickly to get the flow of the equations -> computer notation is tricky

# Once we get the CI for Lambda(t) -> we can determine the CI for SNA(t) -> and remember we can have negative probability and we cant have probability > 1

temp = cumsum(deaths$dj * (deaths$nj - deaths$dj) / (deaths$nj^3))
deaths$NA.se = sqrt(temp)

NA_CI = data.frame(tj = deaths$tj, lower_95 = pmax(exp(-(deaths$Lj + z * deaths$NA.se)), 0), estimate = deaths$SNA, upper_95 = pmin(exp(-pmax(deaths$Lj - z * deaths$NA.se, 0)), 1))

# Check the CI against the survfit() ->
summary(fitNA)$lower
summary(fitNA)$upper
# -> There will be a differnce for the CI for NA estimates -> R uses a differnt appromixation to what we calculated

# Plot the CI with the Survival Funciton Estimate ->
plot(c(0, deaths$tj, 120), c(1, deaths$SNA, deaths$SNA[nrow(deaths)]), type = "s", main = "NA Survival Function Estimate with Lower and Upper 95% CI", xlab = "time (days)", ylab = "S(t)", col = "blue", ylim = c(0,1))
lines(c(0, deaths$tj, 120), c(1, NA_CI$lower_95, NA_CI$lower_95[nrow(NA_CI)]), type = "s", lty = 2, col = "orange")
lines(c(0, deaths$tj, 120), c(1, NA_CI$upper_95, NA_CI$upper_95[nrow(NA_CI)]), type = "s", lty = 2, col = "orange")
legend("topright", legend = c("NA lower 95", "NA Estimate", "NA upper 95"), col = c("orange", "blue", "orange"), lwd = 1)

#########################################################################################
# Question 1

surv.data = read.csv("surv_table_7.1.csv")
surv.data

surv.data$Event.time = surv.data$observation_end - surv.data$exposure_time
surv.data$Event.code = ifelse(surv.data$reason == "D", 1, 0)

library(survival)

surv.obj = Surv(surv.data$Event.time, surv.data$Event.code)

# Two ways of doing this...

fitKM = survfit(surv.obj ~ 1, stype = 1, conf.type = "plain", conf.int = 0.95)
fitKM.summary = summary(fitKM)

SKM = fitKM$surv # -> c(1, fitKM$surv) -> (1, 0.80 0.60 0.48 0.36 0.36 0.00)
plot(fitKM, main = "KM Survival Function Estimate", xlab = "Time (weeks)", ylab = "S(t)", col = "blue", conf.int = FALSE) # -> type = "s" not allowed in this function when plotting fitKM model

# From FitKm.summary -> S(7) CI
# (0.1587, 0.801)
c(fitKM.summary$lower[3], fitKM.summary$upper[3]) # -> 0.1586615, 0.8013385

fitNA = survfit(surv.obj ~ 1, stype = 2, conf.type ="plain", conf.int = 0.95)
fitNA.summary = summary(fitNA)

SNA = fitNA$surv # -> c(1, fitNA$surv) -> (1, 0.8187308 0.6376282 0.5220458 0.4065697 0.4065697 0.1495686)
plot(fitNA, main = "NA Survival Function Estimate", xlab = "Time (weeks)", ylab = "S(t)", col = "red", conf.int = FALSE) # -> type = "s" not allowed in this function when plotting fitKM model

plot(c(0, fitKM.summary$time), c(1, fitKM.summary$surv), type = "s", xlab = "Time (weeks)", ylab = "S(t)", main = "KM and NA Survival Function Estimates", col = "blue")
lines(c(0, fitNA.summary$time), c(1, fitNA.summary$surv), type = "s", col = "red")
legend("topright", legend = c("KM Estimate", "NA Estimate"), col = c("blue", "red"), lwd = 1)


#########################################################################################
# Question 2

surv.data2 = read.csv("surv_table_7.2.csv")
surv.data2

# A total of 100 patients observed since the day illness was contracted -> Observation of patients stopped after 25 weeks following contracting the condition -> end time of investigation

m = nrow(surv.data2[surv.data2$Deaths != 0, ])
event.data = surv.data2[surv.data2$Deat

deaths = data.frame(tj = rep(0, m), dj = rep(0, m), nj = rep(0, m))
deaths$tj = surv.data2$Time[surv.data2$Deaths != 0]
deaths$dj = surv.data2$Deaths[surv.data2$Deaths != 0]
deaths$cj = c(0, 0, 5, 0, 10, 0, 2, 0)

deaths$nj[1] = 100
for (i in 2:nrow(deaths)){
	deaths$nj[i] = deaths$nj[i-1] - deaths$dj[i-1] - deaths$cj[i-1]
	}

deaths$lambdaj = deaths$dj / deaths$nj
deaths$SKM = cumprod(1 - deaths$lambdaj)

plot(c(0, deaths$tj, surv.data2$Time[nrow(surv.data2)]), c(1, deaths$SKM, deaths$SKM[nrow(deaths)]), xlab = "Time (weeks)", ylab = "S(t)", main = "KM Survival Function Estimate", type = "s", col = "blue")

deaths$Lj = cumsum(deaths$lambdaj)
NA_CI = data.frame(lower = rep(0, nrow(deaths)), estimate = rep(0, nrow(deaths)), upper = rep(0, nrow(deaths)))
NA_CI$estimate = deaths$Lj

temp = cumsum(deaths$dj * (deaths$nj - deaths$dj) / (deaths$nj^3))
NA_CI$se = sqrt(temp)

NA_CI$lower = pmax(NA_CI$estimate - qnorm(0.975)*NA_CI$se,0)
NA_CI$upper = NA_CI$estimate + qnorm(0.975)*NA_CI$se
	
plot(c(0, deaths$tj, surv.data2$Time[nrow(surv.data2)]), c(0, deaths$Lj, deaths$Lj[nrow(deaths)]), xlab = "Time (weeks)", ylab = "Lj", main = "NA Integrated Hazard Rate with 95% lower and upper CI", col = "blue", type = "s", ylim = c(0, 0.3))
lines(c(0, deaths$tj,surv.data2$Time[nrow(surv.data2)]), c(0, NA_CI$lower, NA_CI$lower[nrow(NA_CI)]), col = "red", type = "s")
lines(c(0, deaths$tj,surv.data2$Time[nrow(surv.data2)]), c(0, NA_CI$upper, NA_CI$upper[nrow(NA_CI)]), col = "green", type = "s")
legend("topleft", legend = c("NA Integrated Hazard", "NA IH 95% Lower", "NA IH 95% Upper"), col = c("blue", "red", "green"), lwd = 1)

event.times = c(rep(surv.data2$Time, times = surv.data2$Deaths), rep(surv.data2$Time, times = surv.data2$Censorings))
event.codes = c(rep(1, times = sum(surv.data2$Deaths)), rep(0, times = sum(surv.data2$Censorings)))

surv.obj = Surv(event.times, event.codes)

FitNA = survfit(surv.obj ~ 1, conf.type = "plain", conf.int = 0.99, stype = 2)
FitNA.summary = summary(FitNA)

c(1, FitNA$surv) # -> 1.0000000 0.9801987 0.9603974 0.9504451 0.9192858 0.9087798 0.9087798 0.8509160 0.8272810 0.8150252 0.8150252

FitNA.summary$lower
FitNA.summary$upper

plot(FitNA, xlab = "Time (weeks)", ylab = "S(t)", main = "NA Fusvival Function Estimate with 99% Lower and Upper CI", ylim = c(0.7,1))

# SKM(t) <= SNA(t)

FitKM = survfit(surv.obj ~ 1, conf.type = "plain", conf.int = 0.99, stype = 1)

t = data.frame(time = deaths$tj, SKM = deaths$SKM, SNA = FitNA.summary$surv) # -> use the summary of Fit model to get survival probabilties
t$SKM <= t$SNA # -> TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
