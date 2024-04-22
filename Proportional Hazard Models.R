# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\8 Proportional Hazard Models")

# Fit a Cox model with Survial package -> coxph()

# Differences in patients suffering from winter flue admitted to hospital
# Interested in differences btw patients who were compromised when admitted
# i.e. already suffering from another condition that would be expected
# to have a significant impact on their recovery than those who were not compromised this way

# use Cox model to model the hazard rate of dying from flu from these patients
# -> h(t,x) = h0(t)exp(Beta*x) -> Beta*x = sum{beta_i*x_i}

# t-> number of days since hte parient contracted illness
# h0(t) -> baseline hazard rate of dying from flu -> Beta_i's=0
# x -> takes the value 1 if patient is compromised, 0 if not
# Beta -> regression parameter whose value is to be estimated

# The study followed 30 patients who were compromised and 17 patients who were not
# compromised from the time they contracted the illness for a maximum of 10 days.

# n compromised -> 30
# n not compromised -> 17
# investigation of study -> 10 days
# right censoring done at end of study

part.lik=function(b){
	exp(3*b) / (30*exp(b) + 17)^4 * exp(4*b) / (27*exp(b) + 14)^4 *
exp(b) / (23*exp(b) + 14) * exp(b) / (22*exp(b) + 12)^2 *
1 / (20 * exp(b) + 10)^2 * exp(5*b) / (18*exp(b) + 8)^5 *
exp(2*b) / (12*exp(b) + 8)^2 * 1/(10*exp(b) + 8) *
exp(2*b) / (7*exp(b) + 4)^2
	} # Notice the last term...constant K -> 1 ...because no deaths, no tied deaths
	
part.log.lik=function(b){
	log(part.lik(b))
	}
	
plot(part.lik, 0,3, lwd=2, main="Partial Likeliihood", xlab="beta", ylab="L(beta)")
plot(part.log.lik, 0,3, lwd=2, main="Partial Log-Likeliihood", xlab="beta", ylab="log-L(beta)")


# Non-Linear Minimization -> nlm
# we've constructed the partial Likeliihood and Log-Likeliihood functions
# Use nlm to find the value of beta -> b^

# nlm(function, p=<start values>)
# function -> to be minimized
# start values -> vecotr of starting values for the parameters of the function
# We minimize the negative of the Likelihood/Log-Likelihood to get the maximum

nlm(-part.log.lik, p=0.7) # -> will give an error. we need to define a function for the nlm -> we need to have the negative in the function

neg.part.log.lik=function(b){
	-part.log.lik(b)
	}
	
# Now run nlm with nlm.part.log.lik
MLE =  nlm(neg.part.log.lik, p=0.7)

# $minimum goves the value of the partial log-likelihood at b^
# $estimate gives the b^
# use ?nlm for help

b_hat=MLE$estimate

# Going back to the problem...when b=0, baseline hazard is for uncompromised patients
# the key value of interest now is exp(b) which tells us the ratio of the harzard rates
# i.e. exp(b) -> x=1, the compromised hazard as a ratio to the uncompromised Hazard

exp(b_hat)
# -> 1.952935
# This is saying that the hard for comprmised patients is nearly 95.3% more than the hazard for uncompromised patients i.e baseline
# However be careful...the sample is small -> we can't be sure that the true value of b is significantly different from 0 -> try hypothesis test using
# asymptotic distribution of the MLE i.e. estimator(b)~N(0, Var(estimator(b))) -> becuase its one paramenter, the Cramer-Rao LB -> check section on information matrix

# Proportional hazard models with survival package

library(survival)

PH.data=read.csv("PH_data.csv")
head(PH.data)

# don't forget to binarise Deaths / Censored
surv.obj=Surv(PH.data$Event.time, PH.data$Event.code)

# Fit Cox proportional hazard model -> coxph()
# coxph(formula = <survival object> ~ (tilda sign) <explantory variables>, ties="breslow")
# Need to end a forumla to inidcate what explanatory variables we want to use in Cox model
# -> following the ~ we wish to include explanatory survival factors
# -> The response variable will always be a survival object

# If are multiple exaplanatory variables, -> <var1> + <var2>} + <var3>
# You can also add in an interaction term -> <var1>:<var2> or <var1>*<var2>
# -> can have <var1> + <var2> + <var1>:<var2>
# 3-way interaction can also be extended -> <var1>:<var2>:<var3>


cox.fit=coxph(surv.obj ~ PH.data$Status, ties="breslow") # -> indicating one explnatory variable, Status...expect one parameter to estimate
# Alternative way to fit -> cox.fit=coxph(surv.obj ~ Status, ties="breslow", data=PH.data)

summary(cox.fit)

# Recall that exp(b^hat) gives the an estimate of the ratio of the hazard of a compromised patient and the hazard of a non-comprimised patient.
# Output from summary also includes exp(-b^hat) -> estimte of the inverse ratio. This shows the hazard of an uncompromised patient is 51.2% 
# of that of a comprimised patient according to the model

# Note the regression parameter...the MLE is asympotically normal...so any hypothesis testing on it it a two-tailed test
# If we need to change the CI ->

confint(cox.fit, level=0.95)
exp(confint(cox.fit, level=0.95))
# which is what we have in the summary(cox.fit)

# Use str(cox.fit) to see what can be extracted

# for instance, std error - >

Z=qnorm(0.975)
se=Z*sqrt(cox.fit$var)
CI=c(cox.fit$coefficients - se, cox.fit$coefficients + se)

# For the LR test -> value = 1.96 -> We can calc this by
# b^=0 -> when there are no parameters
# b^=cox.fit$coefficients
# Plus these into part.log.lik function -> then calc the LR -> -2(logL0 - logL1)

logL0=part.log.lik(0)
logL1=part.log.lik(cox.fit$coefficients)
-2*(logL0-logL1)

# either calculate qchisq(0.95,DoF=q) or the p-value -> pchisq(-2*(logL0-logL1), DoF=1..in this case, lower.tail=FALSE)

qchisq(0.95,1)
pchisq(-2*(logL0-logL1), 1, lower.tail=FALSE)


summary(cox.fit)$logtest
# use str(summary(cox.fit)) to see what can be extracted from the object summary

# Now build another model using the prmary physician column from PH.data

cox.fit2=coxph(surv.obj ~ Status +Pri.phys, ties="breslow", data=PH.data)
summary(cox.fit2)

# Prim.phys is a categorical value -> A or B -> R assign A=0, B=1 in the model
# It calcs b^(prim.physB) -> which is esentially the parameter for prim.physician

# On the logtest -> LR test...using -2(L1-L2) -> p=1 i.e. one parameter as it was, and p+q=2 -> q=1 and therefore ChiSq(DoF=1) for the null

# We can calculate the LR values using cox.fit objects

logL1=cox.fit$loglik[2] # Note here...this is from cox.fit becuae it is measuring the one parameter model
logL2=cox.fit2$loglik[2]
LR=-2*(logL1-logL2)

p.value.cox2=pchisq(LR, 1, lower.tail=FALSE)
# here the p-value < 5% so we can rject the null, and conclude the addition of the prim.phys exaplantory variable is an improvement

# We can also use ANOVA to help with the LR test results ->

anova(cox.fit, cox.fit2, test="Chi")
# str(anova(cox.fit, cox.fit2, test="Chi")) can help wioth extracting values out of the anova analysis
# Confirm the models being compared in the anova summary

#########################################################################################
# Question 1

baseline.h=function(lambda,t){
	lambda*exp(-lambda*t)
	} # Uncompromised Hazard rate

compromised.h=function(b,lambda,t){
	lambda*exp(-lambda*t)*exp(b)
	}
	
lambda=0.2439
b=0.66933
	
plot(0:40, baseline.h(lambda, 0:40), type="l", xlab="time", ylab="hazard rate", main="Hazard rates for compromised versus uncompromised", col="blue", ylim=c(0,0.5))
lines(0:40, compromised.h(b,lambda,0:40), col="red", ylim=c(0,0.5))


PH.data=read.csv("PH_data_8.1.csv")
head(PH.data)

# Status takes the value 1 if the patient is comprimised, 0 otherwise
# Event.code takes the value 1 if the patient dies, and 0 if censored
# Event.time -> the time that each patient experience an event (death or censoring)
# Ward takes the value 0 if the patient was admitted to ward 1, and 1 otherwise i.e other wards

library(survival)

surv.obj=Surv(PH.data$Event.time, PH.data$Event.code)
cox.fit=coxph(surv.obj ~ Status + Ward, ties="breslow", data=PH.data)
cox.fit.summary=summary(cox.fit)
cox.fit.summary$coefficients

# baseline hazard -> Status=0, Ward=1 -> Uncompromised patient, admitted to ward 1.

# baseline.h -> lambda*exp(-lambda*t)

# Hazard rate for dying from flu for compromised patients, to wards 1 and ward 2
# -> x=1, 

hazard=function(lambda, t, bStatus, bWard, x,y){
	lambda*exp(-lambda*t)*exp(bStatus*x + bWard*y)
	}
	
t=seq(0,40,0.1)
lambda=0.2439
bStatus=cox.fit.summary$coefficients[1]
bWard=cox.fit.summary$coefficients[2]


plot(t, hazard(lambda, t, bStatus, bWard, 1, 0), col="blue", type="l", xlab="time (days)", ylab="Fitted Hazard", main="Hazard Rate for Compromised Patients admitted to Ward 1 and Ward 2")
lines(t, hazard(lambda, t, bStatus, bWard, 1,1), col="red", type="l")

plot(t, hazard(lambda, t, bStatus, bWard, 0, 0), col="blue", type="l", xlab="time (days)", ylab="Fitted Hazard", main="Hazard Rate for Unompromised Patients admitted to Ward 1 and Ward 2")
lines(t, hazard(lambda, t, bStatus, bWard, 0,1), col="red", type="l")

# Compromised patients admitted to Ward 2 have a lower hazard rate of dying than compromised patients admitted to ward 1 in early durations
# Over time, the harzards decrease -> the longer the patients survive, the closer the hazard rate of patients in ward 1 gets to compromised patients in ward 2
# Hazard rate for patients on Ward 2 lower than Ward 1 for both compromised and uncimpromised patients
# Appears that patients on Ward 2 have a much better chance of survival in the first few days after contracting the flu

cox.fit.StatusOnly=coxph(surv.obj ~ Status, ties="breslow", data=PH.data)

anova(cox.fit.StatusOnly, cox.fit, test="Chi")
# We reject the null that there is no improvement. The addition of the Ward covariate appears to make an improvement to the Fit

# Alternative model -> <Status>:<Ward> i.e. if patients are compromised in the ward
# -> surv.obj ~ Status + Ward + Status:Ward, ties="breslow", data=PH.data

# Try fitting a model with just the Ward as the only explanatory variable. The parameter for the compromised status (Status) does not appear significant
# in the model with Status + Ward. In fact, Status wasn't significant in the compromised only model.

# so try ->
# cox.fit.ward = coxph(surv.obj ~ Ward, ties="breslow", data=PH.data)

#########################################################################################
# Question 2

recruits=data.frame(recruit=seq(1,10), age=c(17,18,18,19,20,21,21,24,25,30), sex=c(0,0,0,1,0,0,1,1,1,1), circuits=c(5,5,5,4,3,3,6,3,3,2), status=c(1,1,1,1,1,1,1,1,1,1))

surv.obj=Surv(recruits$circuits, recruits$status)

# Model 1 -> surv.obj ~ sex
cox.fit.m1=coxph(surv.obj ~ sex, ties="breslow", data=recruits)
summary(cox.fit.m1)
cox.fit.m1$coefficients
# Coefficient doesn't appear to be significant in explaining a hazard rate different to baseline -> male recruit completing 6 circuits, can't conclude that 
# female recruits drop out than 31% more than males

# Model 2 -> surv.obj ~ sex + age
cox.fit.m2=coxph(surv.obj ~ sex + age, ties="breslow", data=recruits)
summary(cox.fit.m2)
cox.fit.m2$coefficients
# Again it seems that sex isn't significant but age seems to have some effect. 
# The negative cofficeint on sex -> drop out rate is lower for female than male recruits? again the p-value is too high
# Consdering the parameter for age -> exp(b2) = 1.776 -> 77.6% drop out rate increases with age! p-values look ok...so this may have some basis

anova(cox.fit.m1, cox.fit.m2, test="Chi")
# LR test confirms adding age does improve the fit

# Model 3 -> surv.obj ~ sex + age + sex:age
cox.fit.m3=coxph(surv.obj ~ age + sex + age:sex, ties="breslow", data=recruits)
summary(cox.fit.m3)
cox.fit.m3$coefficients
# Accodring to model 2, age has more of an impact on male than on female recruits

# For males and age -> exp(beta1z1 + beta2z2 + beta3z1z2) -> z1=0 -> exp(beta2z2) ->exp(beta2) considering age covariate -> 1.9912
# For females and age -> exp(beta1z1 + beta2z2 + beta3z1z2) ->z1=1 -> exp(beta1 + beta2z2 + beta3) -> exp(beta2 + beta3) considering age covariate -> 1.9912*0.8723 = 1.7369

anova(cox.fit.m2, cox.fit.m3, test="Chi")

#########################################################################################
# Question 3

part.log.lik=function(b){
	4*b-log(5*exp(b)+5)-4*log(4*exp(b)+5)-log(2*exp(b)+3)-3*log(exp(b)+3)
	}

b=0.3
part.log.lik(b)

b=seq(-2,2,0.01)
plot(b, part.log.lik(b), type="l", xlab="values of b", ylab="partial log-likelihood", main="plot of partial log-likelihood for model 1", col="blue")

nlm.part.log.lik=function(b){
	-part.log.lik(b)
	}

MLE=nlm(nlm.part.log.lik, p=0)
MLE$estimate

# MLE estimate -> 0.2737846 versus 0.3

########################################################################################

# Objectives ->
# Plotting the partial likelihood and partial log-likelihood
# Graphically maximizing the partial likelihood
# Using non-linear linear minimization to estiamte parameters

# Proportional hazrd models with survival package ->
# Crete a survival object
# Fiting Cox model with coxph()

library(survival)

# Example in Notes ->
# h(t,x) = h0(t) * exp(Bx)
# B is the parameter that has to be estimated

# Study has 30 patients who were compromizd -> Z = 1
# Study has 17 patients who were not compromized -> Z = 0
# Construct the partial loglikehood with ties -> Breslow
# Derive the L(B) function -> done in examination pad

# Create R function for likelihood and log-likelihood ->

part.lik = function(b){
	exp(3*b) / (30*exp(b) + 17)^4 * exp(4*b) / (27*exp(b) + 14)^4 *
	exp(b) / (23*exp(b) + 14) * exp(b) / (22*exp(b) + 12)^2 *
	1 / (20 * exp(b) + 10)^2 * exp(5*b) / (18*exp(b) + 8)^5 *
	exp(2*b) / (12*exp(b) + 8)^2 * 1/(10*exp(b) + 8) *
	exp(2*b) / (7*exp(b) + 4)^2
	}	

part.log.lik = function(b){
	log(part.lik(b))
	}

# with respect to functions -> always indicate the operation at the end of the line followed by what the operator is operating on

# Plotting the partial likelihood and the partial log-likelihood

plot(part.lik, 0, 3, lwd = 2, main = "Partial Likelihood", xlab = "beta", ylab = "L(B)", col ="blue")
plot(part.log.lik, 0, 3, lwd = 2, main = "Partial Log-Likelihood", xlab = "beta", ylab = "log L(B)", col ="red")

# Non-Linear Minimization
# Obtain estimate of B using nlm()
# We can minimize a function using the following structure ->
# nlm(<function>, p = <start vector>)

# Remember we want to maximize the log-likehood -> minimize the negative log-likelihood
# -> nlm(-part.log.lik, p = 0.7)
# But we get an error -> create another function that takes in the parameter into the log-likelihood function

neg.part.log.lik = function(params){
	- part.log.lik(params)
	}

params0 = 0.7

nlm(neg.part.log.lik, params0) # -> we want the estimate
MLE = nlm(neg.part.log.lik, params0)$estimate # -> 0.6693335 as the estimate of B

# ?nlm -> help in R for $code output -> We either want a 1 or 2 -> anything else mean algo was termianted for some reason other than successfully stopping
# -> Also important to check ITERATIONS! if the algo stops and found a solution -> check the number of iterations -> chances are it stopped at the intial paramaters given

# Check the ML Estimate and the graph of the log partial likelihood -> sense check that it is correct

# Interpreting the Estimated Parameter

# The Hazard rate for compromised patients is h0(t) * exp(B) -> h0(t) is the baseline Hazard that applied to non-compromised patients -> set Z = 0 -> get Baseline Hazard
# Key value of interest is exp(B)

B = MLE
exp(B) # -> 1.952935

# This tells us that the estimated hazard rate is approximately double for the compromised patients as compated to the non-compromised patients
# -> 95% higher value than baseline
# Check the sample size! -> if it is small, we cannot be sure the the true value of B is significantly different to zero (0)
# Need to apply a statistical test to decide ->

# Proportional Hazard Models with the Survival Package
# Review on creating survival objects -> Surv(event time, event code)

# Load the data file ->

PH.data = read.csv("PH_data.csv")
head(PH.data)

surv.obj = Surv(PH.data$Event.time, PH.data$Event.code)

# We can now use the Cox Proportional Hazards Model -> coxph()
# -> coxph(formula = <survival object> ~ <explanatory variables>, ties = "breslow")
# REMEMBER THE TILDA -> ~ on EXPLANATORY VARIABLES <var1> + <var2> + <var3> + ... + <varn> or even interactionary -> <var1> + <var2> + <var1>:<var2> -> LOOK AT FORM BELOW
# For SURVIVAL FUNCTIONS -> survfit(surv.obj ~ 1 or what ever explanatory variables eg. group A or B)

# The structure can be extended ->
# <var1>:<var2>:<var3> -> three-way interaction between these variables
# <var1>*<var2>*<var3> -> represents all the main effect and all possible combinations of interactions between these variables

cox.fit = coxph(surv.obj ~ PH.data$Status, ties = "breslow") # -> in this example we are only considering status but there is the primary physician variable we can consider later
# or
cox.fit = coxph(surv.obj ~ Status, ties = "breslow", data = PH.data)
summary(cox.fit)

# exp(B) gives the ratio of the hazard of a compromised patient and the hazard of a non-compromised patient

# Standard Error and Confidence Interval ->
# Output shows:
# se.coef -> estimate of standard error of B
# lower .95, upper .95 -> approx 95% CI for exp(B) -> exp(B - 1.96*coef.se); exp(B + 1.96*coef.se)

# Using the model -> cox.fit -> we can extract the CI at different CI levels ->
confint(cox.fit, level = 0.95)

# To get the exp of the CI -> exp CI
exp(confint(cox.fit, level = 0.95)) # -> compare with summary(cox.fit) -> exact

# The standard error is the square root of the variance component ->
sqrt(cox.fit$var) # -> matches the coef.se in summary(cox.fit)

# If we had a model of more than one variable -> can't do this so easily as the variance will be a matrix - variance-covariance matrix
# To extract teh variance for the parameter -> sqrt(cox.fit$var[i,i]) -> IMPORTANT TO REMEMBER

# To extract the coefficients from cox.fit ->
cox.fit$coefficients

# To manually construct the CI at 5% sig level -> two tail -> 97.5% tail
l = cox.fit$coefficients - qnorm(0.975)*sqrt(cox.fit$var)
u = cox.fit$coefficients + qnorm(0.975)*sqrt(cox.fit$var)

y = c(l,u)
y; exp(y)

# Intervale for exp(B) contains 1 -> equivalently interval for B include 0 (zero) -> which corresponds to the hazards for both compromised and non-compromised patients being the same
# This suggest that the parameter B may not be significantly different to zero -> fail to reject the null that B = 0

# Significan Tests ->
# Output Pr(>|z|) -> p-value
# Likelihood Ratio Test -> 1.96 on 1 dof -> Remember that adding another variable -> ChiSq with 2 DoF
# NOTE - The LRT -> gives an overall indication of significance of the model and not consider individual parameters! -> Like the GoF -> the LR Test is an overall Test
# In this example we only have one parameter so the LRT is useful

# The LR test considers the value -> -2 * (l(p) - l(p+q)) -> l: log-likelihood
# -> l(p) considers the value of the maximized log-likelihood for the model with p parameters
# -> l(p+q) considers the value of the maximized log-likelihood for the model with p + q parameters
# The value of -2 * (l(0) - l(q)) -> REMEMBER THE MODEL CAN HAVE ZERO PARAMTERS -> JUST AN INTERCEPT! -> hence the 1 DoF here when comparing the LR Test
# -> The higher the value of 2 * (l(p) - l(p+q)) then the extra parameters do have more explanatory power -> reject the null that the extra parameters are significantly different to zero (0)

# In a way we can calculate it ourselves using the part.log.lik() function evualted at B = 0 -> i.e. no parameters and B = MLE = 0.6693 ->
part.log.lik(0)
part.log.lik(MLE)

-2*(part.log.lik(0) - part.log.lik(MLE)) # -> ~ Chiq(1 Dof) under the null
qchisq(0.95, 1) # -> 95% quantile of ChiSq(1 Dof)
# -> fail to rejec the null -> the parameter B insn't statistically different to zero
# There are also other ways to get the log likelihood evaluate using cox.fit model -> see notes pg 16

# Alternatively calc the p-value
p = pchisq(-2*(part.log.lik(0) - part.log.lik(MLE)), 1, lower=FALSE)
p # -> 0.1617152 which corresponds to the output 0.2 in summary(cox.fit)

# IMPORTANT -> to get more accurate value from the test itself we can select the logtest from the summary ->
summary(cox.fit)$logtest # -> Summary of the LR test -> Need to try this with 2-variable model

# Comparing Models ->
# Fit a Cox proportional hazards model using both Status and Pri.phys ->
cox.fit2 = coxph(surv.obj ~ Status + Pri.phys, ties = "breslow", data = PH.data)
summary(cox.fit2)

# Pri.phys is a categorical variable -> A or B
# R has chose a varible Z = 0 if Prim.phys = A, and Z = 1 if Pri.phys = B
# We can do the LR test ourselves -> Note now the l(1) -> Status, l(2) -> Status + Pri.phys

l1 = cox.fit$loglik[2] # -> p = 1 and the log likelihood come from the FIRST model -> cox.fit! IMPORTANT NOT TO MIX UP
l2 = cox.fit2$loglik[2] # -> p + q = 2 -> q = 1 
-2 * (l1 - l2) # -> ~ ChiSq(1 DoF because q = 1)
qchisq(0.95, 1) # -> CV at 95% with DoF = 1
p = pchisq(-2 * (l1 - l2), 1, lower=FALSE)
p # -> 0.005650032
# As this is smaller than 5% there is sufficent evidence to reject the null hypothesis at 5% level -> reasonable to conclude that the model with Pri.phys is a significant improvement

summary(cox.fit2)$logtest # -> Now I understand this! It assumes an l(0) i.e. intercept only, and l(2) with two parameters -> explains the DoF = 2 !

# We can also use the anova(<model 1>, <model 2>, test = "Chi")
anova(cox.fit, cox.fit2, test = "Chi")
# This will give a good summary of the LR test and the decision on model improvment between models
# Output gives the log likelihood for cox.fit and cox.fit2 -> remember how to extract -> cox.fit$loglik[2] and cox.fit2$loglik[2]
# -> LR = -2 * (l(p) - l(p+q))
# -> CV = qchisq(0.95, DoF)
# -> p-value = pchisq(-2 * (l(p) - l(p+q)), DoF = q)

# The rest follows...

########################################################################################
# Question 1

# h(t, x) = h0(t) * exp(Bx)

# estiamte B = 0.66933
# Baseline hazard is h0(t( = lambda * exp (-lambda *t( -> DONT CONFUSE YOURSELF -> THIS IS A HAZARD RATE, NOT A DISTROBUTION
# Estimate lambda = 0.2439

# -> Baseline hazard is for non-compromised patients 

l = 0.2439
B = 0.6933

h0 = function(t){
	l * exp(-l * t)
	}

t = seq(0, 40, 0.1)
plot(t, h0(t), xlab ="time(days)", main = "Hazard Rates - Non Compromised and Compromised Patients", ylab = "Hazard Rate", type = "l", col = "blue")
lines(t, h0(t)*exp(B), col = "red", type = "l")
legend("topright", legend = c("Non-Compromised HR", "Compromised HR"), col = c("blue", "red"), lwd = 1)

# In early days, compromised patients have a higher HR -> contract the illness more severely than non-comprimised patients
# Can see the HR in the fitted model decrease with duration -> HR for compromised and non-compromised patients remain in the same proportio at all durations
# -> with cimpromised patients experiencing mortality rates exp(0.66933) more than non-compromised patients -> PH model

# h(t, x) = h0(t) * exp(Bx + Dy)
# -> x = {0,1} for non-compromised and compromised patients
# -> y = {0,1} 0 when patients admitted to ward 1 and 1 when patients admitted to ward 2

# Data file ->

PH.data =  read.csv("PH_data_8.1.csv")
PH.data

# Fit Cox model to the data -> two co-variates ->
# First creat the survival object:

surv.obj = Surv(PH.data$Event.time, PH.data$Event.code)
cox.fit = coxph(surv.obj ~ Status + Ward, ties = "breslow", data = PH.data)
cox.fit.summary = summary(cox.fit)
coeffs = cox.fit$coefficients


# Baseline still h0(t) -> lambda * exp(-lambda *t)
# lambda -> l = 0.2439 -> doctors still believe this -> baseline: x = 0, y = 0 -> non-compromised patients in ward 1

l = 0.2439
B = coeffs[1]
D = coeffs[2]

HR = function(t, x, y){
	h0(t) * exp(B*x + D*y)
	}

# Plot HR from dying from flu for compromised patients admitted to ward 1 and ward 2 ->
par(mfrow=c(2,1))

plot(t, HR(t, 1, 0), main = "HR for compromised patients in Ward 1 and Ward 2", xlab = "time (days)", ylab = "Hazard Rate", type = "l", col = "blue")
lines(t, HR(t, 1, 1), col = "red", type = "l")
legend("topright", legend = c("Compromised patients in ward 1", "Compromised patients in ward 2"), col = c("blue", "red"), lwd = 1)

plot(t, HR(t, 0, 0), main = "HR for non-compromised patients in Ward 1 and Ward 2", xlab = "time (days)", ylab = "Hazard Rate", type = "l", col = "blue")
lines(t, HR(t, 0, 1), col = "red", type = "l")
legend("topright", legend = c("non-compromised patients in ward 1", "non-compromised patients in ward 2"), col = c("blue", "red"), lwd = 1)
	
par(mfrow=c(1,1))

# Being admitted to ward 2 has the effect of lowering the mortality rate from flu for both compromised and non-compromised patients
# HR for patients in ward 2 looks to quite low than hazard rate for patients in ward 1
# -> Patients in ward 2 have a better chance of survival in the first few days after getting the flu

# Testing the improvement of Ward to the model with Status only ->
# Consider the LR for this ->

model.1 = coxph(surv.obj ~ Status, ties = "breslow", data = PH.data)
model.2 = coxph(surv.obj ~ Status + Ward, ties = "breslow", data = PH.data)

l1 = model.1$loglik[2]
l2 = model.2$loglik[2]

TS = -2*(l1 - l2) # Under Null hypothesis -> TS ~ Chisq(q = 1 DoF)
CV = qchisq(0.95, 1) # -> 1 DoF
p = pchisq(TS, 1, lower=FALSE)
p # -> 0.0128297 -> Reject null at 5% sig level -> Adding co-variate ward adds statistical improvement to mortality over Status

anova(model.1, model.2, test = "Chi") # -> Confirms the results above

model.3 = coxph(surv.obj ~ Status + Status:Ward, ties = "breslow", data = PH.data)
anova(model.1, model.3)

model.4 = coxph(surv.obj ~ Ward, ties = "breslow", data = PH.data)
anova(model.1, model.4)
summary(model.4) # -> Go with This

########################################################################################
# Question 2

# Table shows the number of circuits a group of 10 army recruits were able to complete in training excerises
# -> number of exercise: split by ages and gender

# Two HR models being considered ->
# model 1 -> log h(t; z1) = log h0(t) + B1z1
# model 2 -> log h(t; z1, z2) = log h0(t) + B1z1 B2z2

# These HR are for dropping out -> reaching their limit in terms of number of circuits they can complete
# -> z1 = {0,1} -> 0 for males, 1 for females
# -> z2 = {age} -> age of the recruit

Circ.Tbl = data.frame(Age = rep(0, 10), Sex = rep(0,10), Circuits = rep(0,10), Status = rep(0,10))

Circ.Tbl$Age =c(17, 18, 18, 19, 20, 21, 21, 24, 25, 30)
Circ.Tbl$Sex = c(0, 0, 0, 1, 0, 0, 1, 1, 1, 1) 
Circ.Tbl$Circuits = c(5, 5, 5, 4, 3, 3, 6, 3, 3, 2)
Circ.Tbl$Status = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

surv.obj = Surv(Circ.Tbl$Circuits, Circ.Tbl$Status)

model.1 = coxph(surv.obj ~ Sex, ties = "breslow", data = Circ.Tbl)
B1 = model.1$coefficients
B1 # -> 0.2737851 ... females drop out 31.5% more than males -> coeffecient not significantly different to zero...bear in mind

model.2 = coxph(surv.obj ~ Sex + Age, ties = "breslow", data = Circ.Tbl)
B1 = model.2$coefficients[1]
B2 = model.2$coefficients[2]
B1 # -> -2.00863 ... females drop out 13.42% less than males -> coeffecient not significantly different to zero...bear in mind
B2 # -> 0.5744168 ... older you are, the more likely to drop out -> drop out rate increase with age -> 77.6% per year of age -> effect is significant based on p-value of coefficient

anova(model.1, model.2, test = "Chi")
# model.2 is an improvement to model.1 as determined by the LR test

l1 = model.1$loglik[2]
l2 = model.2$loglik[2]
TS = -2 * (l1 - l2) # ~ Chisq(1 DoF)
p = pchisq(TS, 1, lower=FALSE)
p # -> 0.009618477 so we can rejec the null and conclude the adding co-variate Age statiticall improves the model for drop out

model.3 = coxph(surv.obj ~ Sex + Age + Sex:Age, ties = "breslow", data = Circ.Tbl)
# With increasing age, the drop out rate increases. If you consider the gender in this case, although drop increases with age, it increase less for females that with males

# Alternatively using sex*age -> sex + age + sex:age DONT FORGET THE DIFFERNCE BETWEEN : and *

anova(model.2, model.3, test = "Chi")
# Based on the LR test, model 3 is not a statistical improvement over model 2

########################################################################################
# Question 3

l = function(B){
	4*B - log(5*exp(B)+5) - 4*log(4*exp(B)+5) - log(2*exp(B)+3) - 3*log(exp(B)+3)
	}

B = 0.3
l(B) # -> -16.78165

B = seq(-5, 5, 0.1)
plot(B, l(B), main = "Partial Log-Likelihood of Model 1", xlab = "Beta 1", ylab = "l(Beta 1)", col = "blue", type = "l")
# -> B close to 0.3

neg.l = function(params){
	-l(params)
	}

params0 = 0.3
B.mle = nlm(neg.l, params0)$estimate
B.mle # -> 0.2737846

model.1$coefficients # -> 2737851
# very close to the linear estimatate -> exactly the same
