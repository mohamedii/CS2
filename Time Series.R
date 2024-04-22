# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\13 & 14 Time Series")

# Simulating Time Series data
# We simulate a stationary ts using arima.sim()

# Simulate 100 obesrvations from ARMA(2,1) from ->
# Y(t) = 0.5*Y(t-1) - 0.1Y(t-2) + err(t) + 0.2err(t-1) -> Check that its ARMA(2,1)...ok

# n = 100
# ar = c(0.5, -0.1)
# ma = 0.2
# sd = 3 -> the std deviation sigma of the white noise terms

Yt=arima.sim(n=100, model=list(ar=c(0.5,-0.1), ma=0.2), sd=3) # careful where you put sd...don't add it to model arugment

Wt=10+arima.sim(n=50, model=list(ar=0.8, ma=c(0.4,0.1)), sd=sqrt(5))

# Always check the stationary of the ts when simulating
# If you have to simulate from sctach i.e. do it manually, the time series needs time to stablize
# The arima.sim in R includes a burn period to achieve this
# Manually simulating a ts from starting point 0 -> requires time to stablize -> make sense if the series is stationary

# We use arima.sim to simulate stationary ts
# To simulate non-stationary ts, we do so manually


# Deterministic Trend
# To incorporate a Deterministic trend, its easy to create a ts Yt
# -> Create Yt by adding stationary ts Xt onto a straight line: a + c*t
# -> Yt = a +c*t + Xt -> Xt is a stationary ts generated using arima.sim()

set.seed(1215)
n = 50
xt = arima.sim(n, model=list(ar=0.9, ma=-0.2))
Yt = xt + 1805 + 0.5*seq(1,n) # -> can easily see the trend coming from 0.5*seq(1,n)

# this is a Xt stationary ARMA(1,1) model added to trend line 1805 + 0.5*t
# The err(t) in Xt is ~ N(0,1) -> white noise process IID


# Stochastic Trend
# Alternatively, we can create a non-stationary ARMIMA(p,d,q) ts which is ARMA(p,q) stationary
# We do this by incorporating a Stochastic trend into the data

# First generate stationary ARMA(p,q) dataset using arima.sim() -> think of this as differnced ts Xt
# Undifference the data to calculate Yt such that: Xt = (1-B)Yt using the cumsum() function

# Simulate 100 values from ARIMA(2,1,1)
# Assume that Xt = (1-B)Yt = 0.5*X(t-1) + 0.2*X(t-2) +err(t) - 0.2*err(t-1) with std normal white noise terms

set.seed(8143)
n=100
Xt=arima.sim(n, model=list(ar=c(0.5,0.2), ma=-0.2))

# Remove the differencing by taking the cumulative sum of the data ->
# Y2 = X1 + X2, Y3 = X1 + X2 + X3, ...
# Therefore Yt=cumsum(Xt)

Yt = 80 + cumsum(Xt)
# R has made Yt a vector instead of a ts -> but we can transform

Yt=ts(Yt)

# par(mfrow = c(2,1)) two rows of graphs, one column stack

par(mfrow = c(2,1)) # -> two rows of graphs, one column
plot(Xt, main = "Differenced time series", col = "red") # -> stationary arima.sim() simualted ARMA(2,1) process
plot(Yt, main = "ARIMA(2, 1, 1)", col = "blue") # -> Undifferenced, non-stationary, stochastic trend ts ARIMA(2,1,1)
par(mfrow = c(1, 1)) # -> reset graphical settings

# Intercept versus mean
# Note that the first value of Yt is approximately 80 as expected. Call this the intercept
# This is not the same as the sample mean of Yt -> mean(Yt) = 105.2462

# In contrast, the sample mean of Xt and the intercept are both approx zero. This is because arima.sim() generates
# stationary data centred around zero

# Only for Stationary ts can we say the INTERCEPT is the long-term mean...DONT FORGET THIS

Xt = read.table("Xt.csv", sep =",")
# R has formatted Xt as a data fram...we need a ts
# Check str(Xt)
# Also...there aren't dates associated with the series...need to add it in

ts.Xt=ts(Xt, start=c(2000,1), frequency=12) # -> frequency denotes monthly data

# Adapting the data...
# Weekly -> frequency=52  and also start=c(2000,...)
# Quarterly -> frequency=4, start=c(2000, whichever quarter)
# Annually=1 start=c(2000, whichever start)
# The start=c(2000,1) starts in Jan 2000. To start in Q2 of Jan -> c(2000, 6), frequency = 12...play around to get the formating to work

ts.quarterlyXt <- ts(Xt, start = c(1912, 2), frequency = 4)

# Output ts over a window of time -> window(ts, start, end)

window(ts.Xt, c(2004,2), c(2007,5)) # -> R will know its monthly becuaes of how ts.Xt is setup

# Plotting Time Series
# Use ts.plot

ts.plot(ts.Xt, main="Time Series Xt", ylab="Value", col="blue")
points(ts.Xt, col = "red", cex = 0.7) # -> cex must go on existing graph
# Insert points on chart to show the ts value...shown above

# Plotting sample ACF and sample PACF
# plot these using acf() and pacf() functions
# Use the par(mfrow=c(2,1)) for graphical layout

# REMEMBER...These plots below are SAMPLE ACF, and SAMPLE PACF
par(mfrow=c(2,1))
acf(ts.Xt, lag.max=60, main="Time Series Xt", ylab="Saomple ACF") # -> Notice here...this will display the chart. If you dont want it, set plot=FALSE
pacf(ts.Xt, lag.max=60, main="Time Series Xt", ylab="Saomple PACF")

# From the plots, this is not a stationary time series. 
# R indicates a 95% confidence interval if the data were generated from a white noise process
# -> Autocorrelations or partial autocorrelations that fall outside the interval give a broad indication of significance at different lags -> Read Core for details

# Plotting the theoretical ACF, and PACF
# -> theoretical and sample acf, pacf wont be the same because of sampling error
# To calcuate the theoretical ACF and PACF for an ARMA(p,q) -> use ARMAacf()

# -> ARMA(2,1): Yt=0.5*Y(t-1)-0.1Y(t-2)+err(t)+0.2*err(t-1)
# -> Calculate the ACF for lags k=0,1,2,...,12

modelacf = ARMAacf(ar=c(0.5,-0.1), ma=0.2, lag.max=12)

modelpacf = ARMAacf(ar=c(0.5,-0.1), ma=0.2, lag.max=12, pacf=TRUE) # -> Use pacf=TRUE to get the PACF function out of ARMAacf

barplot(modelacf[-1], main = "ACF of ARMA(2,1)", col = "red", xlab = "Lag")
barplot(modelpacf, main = "PACF of ARMA(2,1)", col = "blue", xlab = "Lag", names.arg = seq(1, 12))


# Extracting Numbers from Time Series data
 
frequency(ts.Xt) # -> will return the frequency
start(ts.Xt) # -> start of ts
end(ts.Xt) # -> end of ts

a=acf(ts.Xt, lag.max=60, plot=FALSE) # -> setting plot=FALSE will allow us to access the sample acf numbers
a$acf[2] # -> will give the autocorrelation at lag 2
# acf() is a tricky function onthe output when plot=FALSE -> be careful
# In the code aboe, lag.max=60 -> specifies how many ACFs to compute - > data is monthly...so calculate rho(k) for lags up to 5 years -> 60/12=5

# Similar story for PACF
b=pacf(ts.Xt, lag.max=125, plot=FALSE) # -> we set lag.max=125 since we want to get the PACF for onserbations exatcly 10 yeras apart -> 12*10 = 120
b$acf[120] # -> 0.02423828 i.e. the pacf for observations exactly 10 years apart


#########################################################################################
# Question 1

# Plot the ACF -> lag 20 on:
# Yt=15 + 0.8*Y(t-1) -0.6*Y(t-2) +Zt

Yt=arima.sim(n=1000, model=list(ar=c(0.8, -0.6)), sd=1)
Yt=Yt+15
ts.Yt=ts(Yt)

par(mfrow=c(2,1))
acf(ts.Yt, lag.max=20, main="Time Series Yt", ylab="Sample ACF", col="blue")
pacf(ts.Yt, lag.max=20, main="Time Series Yt", ylab="Sample PACF", col="red")
par(mfrow=c(1,1))

# Ok so you did hte sample acf, pacf plots -> do the theoretical plots using ARMAacf()

AR2acf=ARMAacf(ar=c(0.8,-0.6), lag.max=20)
AR2pacf=ARMAacf(ar=c(0.8,-0.6), lag.max=20, pacf=TRUE)

par(mfrow=c(2,1))
barplot(AR2acf[-1], main="ACF of AR(2)", col="blue", xlab="Lag") # -> notice here that we exlude the first autocorrelation because rho(0)=1
barplot(AR2pacf, main="PACF of AR(2)", col="red", xlab="Lag", names.arg = seq(1, 20))  #-> we use the names.arg(seq,1,2) to label bars explcitly
par(mfrow=c(1,1))

#########################################################################################
# Question 2

# Use ts -> ldeaths in R
frequency(ldeaths)
start(ldeaths)
end(ldeaths)

# We want rho(2) -> but the ts is monthly...so the lag is 2*12=24 -> 25th position in vector...because of the rho(0)=1
acf(ldeaths, lag.max=72, plot=FALSE)$acf[25] # -> rho(k): lags k -> {0, 1/12, 2/12, 3/12, ..., 24/12=2, 25/12, ..., 71,12, 72/12=6}
# or 
acf(ldeaths, lag.max=72, plot=FALSE)$lag[72] # -> will tell us the index just before the end for rho -> 5.9166667 which is 0.083333 i.e. 1/12 before 6 

ldeaths[50] # -> alternatively use window(ts, <start=c()>, <end=c()>)
as.numeric(window(ldeaths, c(1978,2), c(1978,2))) # if this is the same as ldeaths[50]

sum(window(ldeaths, c(1976,1), c(1976,12)))


#########################################################################################
# Question 3

# 5y gap -> 5*12 = 60 lags
par(mfrow=c(1,2))
acf(ldeaths, lag.max=72, main="Time Series Yt", ylab="Sample ACF", col="blue")
pacf(ldeaths, lag.max=72, main="Time Series Yt", ylab="Sample PACF", col="red")
par(mfrow=c(1,1))


#########################################################################################
# Question 4

set.seed(2224)
sim=arima.sim(n=480, model=list(ar=c(0.8,-0.4), ma=0.6), sd=100)+2000
sim=ts(sim, start=c(2020,1), end=c(2059,12), frequency=12)

ts.plot(window(sim,c(2024,1), c(2025,12)),main = "Values of the process from Jan 2024 to Dec 2025", ylab = "Values")
points(window(sim,c(2024,1), c(2025,12)), col = "red", cex = 0.7)

#########################################################################################

# Removing Trends in Time Series
# -> Analyse if ts is stationary
# -> Differencing time series data
# -> Remove linear trend using LS regression -> abline
# -> Identify seasonality by ploitting ACFs
# -> Remove seasonal variation by seasonal differencing
# -> Use decompose() and stl() to apply method of seasonal means, and the method of moving averages

testing.stationarity = read.table("testing.stationarity.txt")

ts.stationarity=ts(testing.stationarity)

# If ts is staionary, then sample ACF should converge exponentially towards zero
# If the sample ACF decreases slowlym need to differnce the ts before fitting the model

acf(ts.stationarity, main="Data: Testing Stationarity", ylab="Sample ACF") # -> we didn't include the lag.max here...default chosen by R
acf(ts.stationarity, lag.max=365, main="Data: Testing Stationarity", ylab="Sample ACF") # -> we didn't include the lag.max here...default chosen by R
# The conclusion is the same though...the sample ACF decreases slowly, then cycles back up and down again -> perhaps also presence of seasonality...determine later

# We know we have to differnce the data...but how many times? WE know at least once given this plot of the sample acf

# Statistical test for Stationarity ->
# Phillips-Perron test for stationarity
# This test whether the CP of the ts has a unit root. If so, the ts can be differenced -> If lambda = 1 for any root -> conclude the
# ts is non-stationary...and can be differenced once. Then repeat again until we show there no unit roots?

# For PP test, 
# null -> the times series has unit root and therefore can be Differenced
# alternative -> the ts doesn't need to be Differenced

# Use the PP.test() in R

PP.test(ts.stationarity)
# p-value = 0.5922 so we fail to reject the null -> so the ts can be differenced

set.seed(1901)
n=365
data=arima.sim(n, model=list(ma=c(-1.4,0.8)))

acf(data, main="simulated MA(2) process", ylab="Sample ACF")
# the Sample ACF wuikcly falls inside the confidence interval -.=> ACF appears to cut off after lag 2 -> expected for MA(2) process
# Applying the PP test on a MA process will naturally be stationary because of the white noise processes of the error

PP.test(data)
# p-value -> 0.01 so we reject null at 5% level -> this ts process doesn't need to be Differenced


# Differencing Data
# Difference ts using diff() function

Xt=diff(ts.stationarity, lag=1, differences=1)  # -> note the function...lag and differences
# -> This is the process Xt = (1-B)ts.stationary

# Some plots
par(mfrow = c(3, 2))
ts.plot(ts.stationarity, main="Data: ts.stationarity", ylab="value")
ts.plot(Xt, main="Differenced data", ylab="change in value")
acf(ts.stationarity, main="", ylab="Sample ACF")
acf(Xt, main="", ylab="Sample ACF")
pacf(ts.stationarity, main="", ylab="Sample PACF")
pacf(Xt, main="", ylab="Sample PACF")
par(mfrow=c(1,1))

# Test Xt for stationariy using PP-test
PP.test(Xt)
# p-value -> 0.01 so we reject null at 5% level -> this ts process doesn't need to be Differenced 


# Choosing d -> the number of differences to make
# Start with calculating the variances of ts.stationarity and Xt ->

var(ts.stationarity); var(Xt)
# -> You'll find the var(Xt) < Var(ts.stationarity)
# -> so it was correct to difference the data
# Try differencing again -> (1-B)(1-B)ts.stationarity 
# -> (1-B)Xt

Yt=diff(Xt,lag=1, differences=1)
# Compare variances again for all 3 series
# Alternatively -> Yt=diff(ts.stationarity, lag=1, differences=2)

var(ts.stationarity); var(Xt); var(Yt)
# Differenced series, Yt has a variance greater than var(Xt)...do d=1 gives the lowest varainced time series


# Least Square Trend Removal
# Remove a linear trend in the data using OLS regression
# Involves fitting a model -> xt = a + bt + yt
# -> a,b are constants, yt a process with zero mean

set.seed(123)
n=1000
sim = arima.sim(n, model=list(ar=0.9))
xt=sim+2+0.05*time(sim)

ts.plot(xt, col="blue", main="Time series with trend", ylab="Data")
# This is a time series with linear trend clearly

# Now use least squares regression to fit a line yt = alpha + beta *t
t=time(xt)
fit=lm(xt ~ t) # -> vector t corresponds to time in the straight line yt = alpha + beta *t

# extract the fitted values for yt and zt -> residuals
yt=fit$fitted.values
zt=xt-yt # -> calculated yt to get the residuals

# plot the results ->
par(mfrow=c(2,1))
plot(xt, col="blue", main="Regression example", ylab="Data")
abline(fit, col="red")
plot(zt, col="red") # -> useful to know pch=** for different plot symbols
par(mfrow=c(1,1))

# So is zt the stationary series?

# Identifying seasonality
# -> We can check if seasonality is present by plotting the data
# -> plotting the sample ACF

# Example data will ldeaths

plot(ldeaths, main="Monthly deaths from bronchitis, emphysema and asthma in the UK", ylab="Deaths")
points(ldeaths, pch=20)

# Can idenify a seasonal effect -> more death in the winter months and fewer deaths in the middle of the year
# use abline(v=x) to plot a vertical line at x

abline(v=1974:1979, col="red", lwd=2) # -> draw vertical red lines at start of each year to clearly separate each year. Seasonal effect it present
# There is a clear annual trend

# plot the Sample ACF -> up to lag 36...should get 3 cycles of seasonal change
acf(ldeaths, lag.max=36, main="Sample ACF of ldeaths", ylab="Sample ACF") # -> you should understand now the lag explaination
# There are positive spikes at lag 1,2,3 (12,24,36) indicating postive correlations between values 1 year, 2 years, and 3 years apart
# -> Similarly for other lags i.e. 11,23,35 -> 10,22,34

# Removing Seasonility
# Three key methods for removing seasonality ->
# - seasonal Differencing
# - method of seasonal means
# - method of moving averages

# Seasonal differencing
# For ts Xt that exhibits seasonality -> apply sesasonal differencing:
# -> Yt = Xt - X(t-s) -> where s is the period of seasonality

# For instance, ldeaths showed annual (or 12-month) seasonality -> s=12
# Always check data first using frequency(data) -> frequency(ldeaths)

frequency(ldeaths) # -> so the ts is monthly and therfore s=12

sdiff.ldeaths=diff(ldeaths, lag=12, differences=1)

# plot the results ->
par(mfrow=c(2,2))
plot(ldeaths, main = "Data: ldeaths", ylab = "number of deaths")
acf(ldeaths, main = "Sample ACF of ldeaths", ylab = "")
plot(sdiff.ldeaths, main = " Data: sdiff.ldeaths", ylab = "increase in number of deaths")
acf(sdiff.ldeaths, main = "Sample ACF of sdiff.ldeaths", ylab = "")
par(mfrow = c(1, 1))

# try new plot layout -> ma
m=matrix(2,2,data=c(1,2,3,4)) # -> each graph has its own quadrant
# m=matrix(2,2,data=c(1,1,2,3), byrow=TRUE) -> graph 1 is across two quads, graphs 2 & 3 have their own quads below

layout(m)
plot(ldeaths, main = "Data: ldeaths", ylab = "number of deaths")
acf(ldeaths, main = "Sample ACF of ldeaths", ylab = "")
plot(sdiff.ldeaths, main = " Data: sdiff.ldeaths", ylab = "increase in number of deaths")
acf(sdiff.ldeaths, main = "Sample ACF of sdiff.ldeaths", ylab = "")
par(mfrow = c(1, 1))

# Seasonal means
# -> apply seasonal means on ts that exhibits seasonality
# Assume xt = mu + yt + theta.t -> theta.t is a periodic function with the period of the seasonal variation exhbited by Xt
# yt -> is a non-seasonal Series

# -> yt = xt - mu - theta.t

# Say that we have monthly data, xt -> exhibits annual trend say
# -> theta.t is a period function with period = 12
# Estimate the value of theta.t for any particular calendar month by considering the averages of the observed values of xt for the month
# -> Assume that there are n.Jan observatons -> x.Jan1, x.Jan2, ..., x.Jann
# -> xbar.Jan = (1/n.Jan)*sum(i=1, n.Jan){x.Jani}

# Therefore theta.Jan = xbar.Jan - mu -> xbar.Jan - (1/n)sum(i=1,n){xi}

# We can do this because of the seasonal variation that occurs annually ->
# -> y.Jani = x.Jani - xbar.Jan
# -> y.Febi = x.Febi - xbar.Feb
# etc

ldeaths.df = data.frame(year = rep(1974:1979, each = 12), month = rep(1:12, 6), value = ldeaths) # -> show the data frame 

# Be careful! If series does not have a complete number of months for all years -> say for instance that there are some months in 1980
# -> account for this by:
# -> year=c(rep(1974:1979, each=12), 1980)
# -> month=c(rep(1:12,6), 1)...if there was only one month in 1980 given. If there were 4...then c(rep(1:12,6),1,2,3,4)

# We can now average for each calendar month -> aggregate()
# -> aggregate(vector of values ~ vector to aggregate by, data, function)

xbars=aggregate(value ~ month, data=ldeaths.df, FUN=mean) # -> very cool function! it calculates the mean thats required

yt = ldeaths - xbars$value # -> xbars$value will return the value in xbars

# Here we are taking a vector of length 72 (ldeaths) and subtracting a vector of length 12 (xbars$value). R will automatically recycle the shorter vector
# You were thining about this...now you know what R does

# Plot yt and the sample acf of yt

par(mfrow=c(2,1))
plot(yt, main = "Monthly deaths from bronchitis, emphysema and asthmain the UK less seasonal means", ylab = "Deaths")
points(yt, pch = 20)
acf(yt, main = "Sample ACF of the series ldeaths less seasonal means", ylab = "sample ACF", lag.max = 36)
par(mfrow=c(1,1))

# Graphs will show that there is no longer seasonality present

# Using R to do the hard work! -> Use the built-in functions to separate out trend, sesonality and white noise
# decompose()

# If seasonility remains constant over time i.e regularly occuring -> type="additive"
# If the magnitude of seasonality appears to change over time i.e. increase then use type="multiplicative"

plot(decompose(ldeaths, type="additive"))
# -> trend in decompose() uses method of moving averages to determine
# -> sesonality in decompose() -> subtracts the trend from the original series and applies seasonal means as done above
# -> the last graph, random, are the residual values -> original series minus trend minus seasonal

decomp=decompose(ldeaths, type="additive")
str(decomp) # -> get the values that can be extraceted

trend=decomp$trend # -> See if you can get this by ldeaths - diff(ldeaths, lag=1, differences=1)
seasonal=decomp$seasonal# -> See if you can get this by ldeaths - diff(ldeaths, lag=1, differences=1) - xbars
residual=decomp$random # -> should be ldeaths - trend - seasonality

# What is nice is you can plot these decomposed values ->

plot(ldeaths, ylab = "", main = "Components of time series:ldeaths", col = "dark grey", ylim=c(-2000,4500))
points(ldeaths, cex = 0.5, col = "dark grey")
#lines(trend, col = "red")
lines(seasonal, col="green")
#lines(seasonal + trend, col = "blue")

# stl() function
# stl() is similar to decompose() -> it separates out the seasonal variation, trend, and white noise
# We include the argument s.window = "periodic". This tells R to calculate the seasonal component

plot(stl(ldeaths, s.window = "periodic"), main = "Components of time series: ldeaths")

stl = stl(ldeaths, s.window = "periodic") # -> s.window="periodic" is important for stl() to work on the data
str(stl) # -> get the values that be extracted

stl$time.series # -> gives us the ts for each compenent: seasonal, trend, remainder

trend = stl$time.series[, "trend"] # -> essentially the columd of trend
seasonal = stl$time.series[, "seasonal"] # -> essentially the columd of seasonal
remainder = stl$time.series[, "remainder"] # -> essentially the columd of remainder

plot(ldeaths, ylab = "", main = "Components of time series:ldeaths", col = "dark grey")
points(ldeaths, cex = 0.5, col = "dark grey")
lines(trend, col = "red")
lines(seasonal + trend, col = "blue")

# Advanatges & Disadvantages of decompose() and stl()
# -> decompose() doesn't work well if data spans a non-integer number of time periods
# -> decompose() is a little closer to the methods of moving averages, and method of seasonal means in the Core Reading
# -> stl() should only be used for additive time series -> if the ts is multiplicative, transform the data using log -> must do this before applying stl()


#########################################################################################
# Question 5

xx=numeric(1003)
set.seed(4567)
ww=rnorm(1003)
xx[1:3]=ww[1:3]

for (t in 4:1003){
	xx[t]=0.8*xx[t-1]+0.2*xx[t-3]+ww[t]+2*ww[t-1]
	}

s=ts(xx[4:1003])

# -> first run PP.test()
PP.test(s)
# -> p-value = 0.117...this says that we fail to reject null...the data has to be differenced
var(s) # -> quite large

d.s=diff(s, lag=1, differences=1)
PP.test(d.s)
# -> p-value = 0.01...ok getting smaller...so we reject null...data doesn't need to be differenced further
var(d.s) # -> very much smaller

# try one more Difference
dd.s=diff(d.s, lag=1, differences=1)
PP.test(dd.s) # -> p-value = 0.01...ok getting smaller...so we reject null...data doesn't need to be differenced further
var(dd.s) # -> increasing...so d=1 difference is optimal

#########################################################################################
# Question 6

sales=read.table("sales.txt")
ts.sales=ts(sales, c(2005,1), c(2014,12), frequency=12)

ts.plot(ts.sales)

decomp=decompose(ts.sales, type="additive")
sales.trend=decomp$trend
sales.seasonal=decomp$seasonal

sales.remainder = window(ts.sales - sales.trend - sales.seasonal, c(2006,1), c(2013,12))
# untrended <- decomposed$random
# r = untrended[!is.na(untrended)]
# run acf(r, lag.max=120, main...), pacf(r, lag.max=120, main...)

par(mfrow=c(2,1))
acf(sales.remainder, lag.max=120, main = "Sample ACF of sales.remainder", ylab = "acf")
pacf(sales.remainder, lag.max=120, main = "Sample PACF of sales.remainder", ylab = "pacf")
par(mfrow=c(1,1))

# The remainder data is referred to as the untrended data
# -> Your graphical results match the solution even though your approach is different
# -> The pacf chart cuts off after lag 2 or lag 2*(1/2) -> indicative of AR(2)?

#########################################################################################
# Question 7

volume=read.table("seasonality.txt")
head(volume)

ts.volume=ts(volume, c(2010,1), c(2014,12), frequency=12)

par(mfrow=c(3,1))
ts.plot(ts.volume, col="blue", main="Time Series of Resevoir Volumes", ylab="millions of gallons")
acf(ts.volume, lag.max=60, main="Sample ACF of Resevoir Volumes", ylab="acf")
pacf(ts.volume, lag.max=60, main="Sample PACF of Resevoir Volumes", ylab="pacf")
par(mfrow=c(1,1))

# Assuming annual seasonal variation

ts.volume.df = data.frame(year = rep(2010:2014, each = 12), month = rep(1:12, 5), value = ts.volume) # -> show the data frame
# -> Careful with month=rep(1:12,5) -> rememebr the ts has 60 obs...so 5 years!
xbars=aggregate(V1 ~ month, data=ts.volume.df, FUN=mean)
yt = ts.volume - xbars$V1
sdiff.ts.volume=diff(ts.volume, lag=12, differences=1)

m=matrix(c(1,4,2,5,3,6), nrow=3, byrow=TRUE)
layout(m)

ts.plot(yt, col="blue", main="Seasonally mean-adjusted volume", ylab="volume")
acf(yt, lag.max=60, main="ACF of Seasonally mean-adjusted volume", ylab="acf")
pacf(yt, lag.max=60, main="ACF of Seasonally mean-adjusted volume", ylab="pacf")

ts.plot(sdiff.ts.volume, col="blue", main="Seasonally differenced volume", ylab="volume")
acf(sdiff.ts.volume, lag.max=60, main="ACF of Seasonally differenced volume", ylab="acf")
pacf(sdiff.ts.volume, lag.max=60, main="ACF of Seasonally differenced volume", ylab="pacf")

par(mfrow=c(1,1))

#########################################################################################

# Fitting a ts model
# -> Use the ACF, PACF o decide on model fit
# -> for amodel to ts data
# -> discuss how graphs can analyse GoF of a ts model
# -> Analyse GoF of a model usin:
#		> Ljung-Box or Portmanteau Test
#		> Tha Akaike Info Creiterion (AIC)
#		> Larger sample size, the better reduction in sampling unceratinty and we should get a lower AIC -> low AIC is good!

# Having obtained a stationary ts (after some tranforms)
# -> we can identify what order model to fit by examining the ACF (SACF) and PACF (SPACF)
# -> It the SACF and SPACF don't appear to show significant correlations, try fitting a white noise process
# -> If the SACF appeasr to cut off for lags k > q and SPACF decays -> fit MA(q) to stationary data
# -> If the SPACF cut off for lags k > p and SACF decays -> fit AR(p) to stationary data
# -> 'If both SACF and SPACF decay, then fit an ARMA(p,q) model using trial and error to find optimal values of (p,q) based on chosen criterion
# -> The PACF gives the best linear approximation to Xt based on Xt-1 -> we can show this by minimizing E[(Xt - pXt-1)^2)] 
# -> For MA(1) case -> Xt = et + b*e_t-1 -> show that p =0.5 -> best linear estimator for Xt is 0.5*Xt-1 when b=1
# -> Use the ARMAacf and ARMApacf to get the theoretical values

# Example 1 - White noise WN model
# For a large enough sample from a WN process, we have:
# -> rho ~ N(0,1/n)
# -> phi ~ N(0, 1/n)..yes if the process follows WN...hence the CI bands! Very cool

# expect 95% of the time, SACF and SPACF of a WN process to lie in interval: -1.96*sqrt(1/n); 1.96*sqrt(1/n)
# Recall the CI on the acf, pacf charts -> if majority of SACF and SPACF lie in this interval, then a WN model may be appropriate
# 95% CI -> expect 1 in 20 of samples to fall outside the interval when data comes from WN process
# -> If there more than 5% of values outside the interval, consider instead to fit ARMA(p,q) model
# -> If the samples shows a significant early spike or spikes in either SACF or SPACF, then, even if the overall
# number of values outside the interval is less than 5%, this may indicate a more complicated ARMA(p,q) model...more appropriate than WN

data = ts(read.csv("fittingmodelEg1.csv", header = FALSE))

# Make a few plots, including correlogram and partial correlogram

m=matrix(c(1,2,1,3), nrow=2, byrow=TRUE)
layout(m)
ts.plot(data, main="What model to fit?", ylab="Data: Example 1")
acf(data, main="")
pacf(data, main="")
par(mfrow=c(1,1))

# Looks like a WN model will be suitable? There is no obvious trend in the data itself, and the variability appears to be reasonably constant
# over time
# Bot SACF and SPACF appear consistent with sample from WN process. Both correlogram show one spike in 26 lags...< 5% expected
# -> i.e. outside interval: -1.96*sqrt(1/n); 1.96*sqrt(1/n) -> data doesn not show any significant correlations
# Try fittting a WN model

# Example 2 - MA model
# For a large enough sample from MA(q) -> 
# rho(k) ~ N(0,1/n)
# phi(k) ~ N(0, 1/n*(1+2*sum(i=1,q){rho(i)^2}))
# and so has CI for the estiamte of phi(k) -> [-1.96*sqrt(1/n*(1+2*sum(i=1,q){rho(i)^2})); 1.96*1/n*(1+2*sum(i=1,q){rho(i)^2})]
# This is wider than default interval [-1.96*sqrt(1/n); 1.96*sqrt(1/n)] -> plot of acf

# So...for acf -> [-1.96*sqrt(1/n); 1.96*sqrt(1/n)]
# -> pacf -> [-1.96*sqrt(1/n*(1+2*sum(i=1,q){rho(i)^2})); 1.96*1/n*(1+2*sum(i=1,q){rho(i)^2})]

# If the SACF at a particular lag falls within the default interval, then it must also fall within the wider interval above

data2 = ts(read.csv("fittingmodelEg2.csv", header = FALSE))
head(data2)

m=matrix(c(1,2,1,3), nrow=2, byrow=TRUE)
layout(m)
ts.plot(data2, main="What model to fit?", ylab="Data: Example 2")
acf(data2, main="")
pacf(data2, main="")
par(mfrow=c(1,1))

# What ARIMA model would you fit to data2? -> See the lag cuts off after lag 3 and PACF decrease to zero across lags
# -> There doesn't appear to be any trend so...ARIMA(0,0,3)

# Example 3 - AR model
# For a large enough sample from AR(p) model:
# -> phi(k) ~ N(0,1/n) for k>p -> think WN
# -> rho(k) will tend to Inf as k tends to Inf -> expect the SACF to cut off at lag k > p-value
# For a large enough sample from AR(p) -> expect 95% of time the SPACF past lage p to be interval: [-1.96*sqrt(1/n); 1.96*sqrt(1/n)]


data3 = ts(read.csv("fittingmodelEg3.csv", header = FALSE))
head(data3)

m=matrix(c(1,2,1,3), nrow=2, byrow=TRUE)
layout(m)
ts.plot(data3, main="What model to fit?", ylab="Data: Example 3")
acf(data3, main="")
pacf(data3, main="")
par(mfrow=c(1,1))

# I dont like the SACF...decreasing too slowly -> difference the data3

df.data3=diff(data3, lag=1, differences=1)
var(data3); var(df.data3) # ...lower variance in df.data3

m=matrix(c(1,2,1,3), nrow=2, byrow=TRUE)
layout(m)
ts.plot(df.data3, main="What model to fit?", ylab="Data: Example 3")
acf(df.data3, main="")
pacf(df.data3, main="")
par(mfrow=c(1,1))

# Ideally fit ARIMA(1,1,0) or less stationary AR(2)

# Example 4 - ARMA(p,q) model
# What we look for is what lags do SACF and SPACF cut off after
# But first make sure the ts is stationary first before fitting (p,q)

data4 = ts(read.csv("fittingmodelEg4.csv", header = FALSE))
head(data4)

plot(data4) # -> can see trend effect
# Need to difference but PP.test() will confirm that we should

diff.data4=diff(data4, lag=1, differences=1)
plot(diff.data4)

m=matrix(c(1,2,1,3), nrow=2, byrow=TRUE)
layout(m)
ts.plot(diff.data4, main="What model to fit?", ylab="Data: Example 3")
acf(diff.data4, main="")
pacf(diff.data4, main="")
par(mfrow=c(1,1))

# Looking at SACF and SPACF -> looks like ARIMA(6,1,5) or stationary ARMA(6,5)
# Better to choose the best (p,q) on the basis of chosen criterion ... VERY IMPORTANT!

# Fitting Time Series in R
# Use the arima() function to fit ARIMA(p,d,q) model in R
# -> ARIMA(1,0,1) -> arima(x, order=c(1,0,1))
# The order=c(1,0,1) argument is what we're trying to fit

# Using Example 2
# We'll fit MA(3) model -> ARIMA(0,0,3) -> order=c(0,0,3)

e2.model=arima(data2, order=c(0,0,3))
e2.model
# Testing the white noise?

# Fit AR(2) to Example 3

e3.model=arima(data3, order=c(2,0,0))
e3.model

# Notice the estiamted variance given in the model output -> it is the estimated variance of the white noise terms Zt -> simga^2
# Don't confuse with gamma_0 -> variance of the ts

# Alternatively, use ar() funtions to fit AR(2)
m3=ar(data3) # R chose the parameter for us instead of it being specified in order = c(2,0,0)
# Fitted parameters using ar() doesn't matcht eh estimates from arima() -> ar() uses the YuleWalker equations wheres arima() is fit differently
m3$x.mean # -> 499.3974

# model: Xt = mu + a1*[X(t-1) - mu] + a2*[X(t-2) - mu] + err(t)
# -> Xt = 499.3971 + 0.6473*(X(t-1) - 499.3971) + 0.2486*(X(t-2) - 499.3971) + err(t)
# estimated variance of err(t) -> 0.9568

# Note that in all the examples, the models that R fit, R gave intercept or mean of the underlying process
# However, if you indicate a difference in c(p,d,q) -> d > 0 -> R will not give an intercept term if the data is Differenced

# Model of Example 4 -> try to fit ARIMA(1,1,1) to this...
m4=arima(data4, order=c(1,1,1))
m4
# -> don't see an intercept or mean Output ... why? arima() function is fitting a zero-mean model to differenced d>0 data
# This may not be a good fit if a zero-mean model is not appropriate for the differenced data

# To get around this...dont let R difference in arima()...rather manually difference, then fit arima() with order=c(p,0,q))
# -> you difference the data and then fit an ARMA(p,q) using arima() on differenced data set with order=c(p,0,q)

# diff.data4 -> you did this earlier

m4.alt=arima(diff.data4, order=c(1,0,1))
m4.alt # -> here we will have an intercept or mean term

# Testing the fit...
# Graph of the residuals
# The Ljung-Box or Portmanteau test
# The AIC  -> penalized AIC -> you want the lowest value here - AIC = -2*L + 2*#parameters

# Starting point when analysing GoF is to analyse residuals
# arima() gives the residuals

ma3=arima(data2, order=c(0,0,3))
e=ma3$residuals
# Alternatively, e=residuals(m3)

# Plot the residuals
par(mfrow = c(2,1))
ts.plot(e, main = "MA(3): analysis of residuals", ylab = "Residuals",col = "blue") # -> plot of the residual ts
acf(e, main = "", ylab = "ACF of residuals") # -> acf of the residuals... check if the residuals follow a WN process
par(mfrow = c(1, 1))

# we should expect it to...MA(q) are stationary WN terms
# Looking athe ACF, the primary determinant of q for MA(q) models, we see the correlations are all within the interval:
# -> [-1.96*sqrt(1/n*(1+2*sum(i=1,q){rho(i)^2})); 1.96*1/n*(1+2*sum(i=1,q){rho(i)^2})]

# Alternative to plotting graphs of residuals -> tsdiag() function
# Function contains a bug in R -> Ljung-Box test  p-values are incorrect
# Rather use Box.test() -> Box.test(e, lag=k, type=Ljung", fitdf=3)...more on this later

tsdiag(ma3) # -> makes the plots of ts and residuals easier to obtain. Ignore the p-value chart...that is wrong. Use Box.test() alternative
# -> Note that we use ts.diag() on the model ma3 -> MA(3) process rather than e, the residuals
# -> We are interested in the GoF of the MA(3) model
# -> For GoF... we expect the mean and variance of the residuals to be broadly constant over time -> stationarity
# -> The ACF of the residuals are small, patternless, appear to be uncorrelated and lie within the interval

# Ljung-Box Test or Portmanteau Test
# -> Box.test() function with type="Ljung"
# -> Here you apply the test on the residuals e -> DO NOT CONFUSE tsdiag() with Box.test()
# -> tsdiag() uses the model i.e. ma3 whereas Box.test() uses the residuals, e

Box.test(e, lag=5, type="Ljung", fitdf=3)

# Compre this results with the formula for Box-Ljung on pg 42 ->
# Since we have fitted MA(3) -> p+q=3 -> so we set fitdf = p+q -> 3 ... IMPORTANT TO SET fitdf
# fitdf represents the DoF lost in the fitting process -> aligns with ChiSq(DoF: m-(p+q)) Critical value
# Speficied a lag = 5 -> tells R to use five autocorrelations (r1,r2,r3,r4,r5) in the the calculation -> Corresponds to m=5
# Note in the output of Box.test() -> DoF = 2 whcih is correct -> m-(P+q) -> 5-3 -> 2
# p-value: 0.2025 so we fail to reject null: residuals follow WN process

# Output from Box.test() -> X-Squared is the ChiSq TS -> 3.194 and is assumed to follow ChiSq(m-(p+q)) null distribution
# -> ChiSq(5-3) -> ChiqSq(2) null
# Null: residuals are independent

# Example on AR(2) model
ar2=arima(data3, order=c(2,0,0))
e=residuals(ar2)

tsdiag(ar2)
Box.test(e, lag=10, type="Ljung", fitdf=2)
# p-value -> 0.7047 so fail to reject null

# How do you choose the number of correlation coefficients?
# -> the choice for m -> the lag=m argument in the Box.test()
# Comes down to the power of the test and whether we have enough DoF to still follow a ChiSq distribution
# If we include too many m -> the TS will resemble a ChiSq but the power of the test will be lower -> because of the higehr DoF
# Too few DoF i.e. too low m, will have higher power but the TS will less likely resemble a ChiSq RV or come from a ChiSq distribution

# Power of a test is the probability that you reject the null given that it is FALSE

# AIC -> Akaike Information Crierion
# Use the AIC to anlayse GoF
# AIC is a measure of the trade-off between the GoF of a model and te number of parameters of the model
# AIC = -2*logL + 2*#parameters used in the model

# Trade off -> 
# A model with many parameters will be a very good fit but not necessarily an accurate predictor of future experience -> High variance
# A model with few parameters will be have a pooer fir or capture significant patterns in the data but provide more accurate predictions than an over-fitted model
# -> Lead to high bias

# using AIC to decide between ARMA(2,1) or ARMA(1,2) on dataset diffdata4 -> remember that arima in R will calculate a zero-mean using arima when c(p,d>0,q)
arma12=arima(diff.data4, order=c(1,0,2))
arma12$aic

arma21=arima(diff.data4, order=c(2,0,1))
arma21$aic

# Choose the model with the lower AIC -> ARMA(1,2)

#########################################################################################
# Question 8

TS = read.table("TS.txt", sep = "\t")
TS = ts(TS[, 1], start = 1, end = length(TS[, 1]), frequency = 1)

PP.test(TS) # -> Check if the significance...small rejection
var(TS)

diff.TS=diff(TS, lag=1, differences=1)
PP.test(diff.TS)
var(diff.TS)
# Ok so dont differnce

ar2=arima(TS, order=c(2,0,0))
ar2.coef=ar2$coef

e=residuals(ar2)
Box.test(e, type="Ljung", lag=5, fitdf=2) # -> expected DoF = m-(p+q) -> 5 - 2 = 3
# p-value -> 0.2625 therfore fail to reject null @ 5% level


#########################################################################################
# Question 9

Wt = read.table("Wt.csv", sep = "", header = TRUE)
Wt = ts(Wt$value, start = min(Wt$day), end = max(Wt$day)) # -> I hope R links it correctly

par(mfrow=c(3,1))
ts.plot(Wt, main="Wt Time Series", ylab="value", col="blue")
acf(Wt, lag.max=100, main="Sample ACF of Wt", ylab="acf")
pacf(Wt, lag.max=100, main="Sample PACF of Wt", ylab="pacf")
par(mfrow=c(1,1))

# Based only on the SACF and SPACF plots, may suggest AR(1) or ARIMA(1,0,0)


#########################################################################################
# Question 10

Xt = read.table("Xt.csv", sep = ",", header = TRUE)
Xt = ts(Xt, 1990, frequency = 12)

aic=array(0, dim = c(3, 2, 3)) # -> create a tensor

for (p in 0:2){
	for (q in 0:2){
		for (d in 1:2){
			aic[p+1,d,q+1]=arima(Xt, order=c(p,d,q))$aic
			}
		}
	}
aic

# min aic -> 872.1794 ... [p=1,d=1,q=2] -> ARIMA(1,1,2)


#########################################################################################
# Question 11

Yt = read.table("Yt.csv", sep = ",", header = TRUE)
Yt = ts(Yt[,2], 2005, frequency = 12)

# Plot Yt...trended series, can't fit ARMA to non-stationary series
# -> need to difference

diff.Yt=diff(Yt, lag=1, differences=1)
PP.test(diff.Yt)
# p-value -> 0.01 therfore reject the null, dont have to differnce further

arma.11=arima(diff.Yt, order=c(1,0,1))
arma.11.coef=arma.11$coef # -> On the differnced series we have non-zero mean if we did the differencing manually
# instead of letting R do it in arima(Yt, order=c(1,1,1))

e=residuals(arma.11) # -> run the residuals on the model! not on the time series
tsdiag(arma.11) # -> run the tsdiag() on the model! not on the time series

# Happy with the ACF of the residuals -> correlations are within the interval, appears to follow WN process

Box.test(e, type="Ljung", lag=6, fitdf=2)
# Confirms that residuals e follows a WN process -> p-value @ lag = 8 -> 0.09753 so fail to reject null

# Turning Points test on Residuals -> Remember the different ways on testing the WN process
e=residuals(arma.11)
n=length(e)

turns=0
# What does a turn mean? if e(t-1) < e(t) and e(t)> e(t+1) -> this is a turn
# Similarly e(t-1) > e(t) and e(t) < e(t+1) -> this is a turn

for (i in 2:(n - 1)) {
	if (e[i] > e[i + 1] & e[i] > e[i - 1]){
		turns = turns + 1
		}
	else if (e[i] < e[i + 1] & e[i] < e[i - 1]){
		turns = turns + 1
		}
	}

turns

# Expected Value ->
E = 2/3*(n-2)
V = (16*n-29)/90

# Test Statistic: Accounting for continutity correct if using a continuos distribution
TS = (turns-E+0.5)/sqrt(V) # -> assuming asymtotic normality if n is large -> n=179 in this case
pnorm(TS, lower.tail=FALSE)
# or using CV
CV = qnorm(0.975)
# Comapre TS and CV -> 
abs(TS)>CV # -> because of the two-tail...use the abs() because there are two rejection regions
# If TRUE, reject null
# Else, FALSE -> fail to reject null

# Plot of Yt...will confirm what you thought alrady about trend. Cant fit ARMA model to non-stationary ts

#########################################################################################

# Forecasting Time Series

# -> Forecast using the step-ahead method
# -> Forcast using the basic exponential smoothing

# Recall that the Bonx-Jenkins method of forcasting is called the k-step aherad forecast

# Example 1: Stationary data

set.seed(476)
abc = sort(rbinom(3,1,0.6), decreasing = TRUE)*c(0.86, -0.4, 0.15) # -> sample 3 bernoulli(0.6) numbers -> {0,1} where P(X=1)=0.6
abc

d = 50 + arima.sim(n=1000, list(ar=abc))
head(d)

# fit AR model using ar() on the data
fit = ar(d)
fit.coef=fit$ar # -> AR model coefficients
fit.mean=fit$x.mean

# To forecast the next 20 values of this time series, set n.ahead=20 ->
# Use the predict() function ON THE MODEL!

p = predict(fit, n.ahead=20) # -> you need to put n.ahead in the function -> can't define it outside 
str(p) # -> the values we can extract -> $pred and $se

# You can clearly see the prediction converging to the long-run average fit.mean
fit.mean
pred=p$pred
pred[length(pred)]

# Similarly, the std errors are also converging toward sqrt(gamma_0), the std deviation of the fitted model
se=p$se
se[length(se)]
# Calculate this manually using Yule Walker equations...you have value of the coefficients -> solve for gamma_0
# Refer to notes -> good exercise

str(fit) # -> check chat gpt to find out how to get covariances from the model

# Calculating this in R using Yule-Walker equations
a = fit$ar[1]
b = fit$ar[2]
sig = fit$var.pred
g1.coeff = a / (1 - b) # -> gamma_1
g2.coeff = a * g1.coeff + b # -> gamma_2
g0 = sig / (1 - a * g1.coeff - b * g2.coeff) # -> gamma_0
# finally take the sqrt() ->
sqrt(g0)
# Compare this with se[lenght(se)]
se[length(se)] # Works

# Get the value at t = 1010
# This is observation 10 after the last in the time series -> because length(d) = 1000
# Essentially, we want p$pred[10]

p$pred[10] # -> 50.05137

# Plots with xlim...

ts.plot(d, col="blue", xlim=c(900,1020), main="Forescasting Example", ylanb="Data")
lines(p$pred, col="red", type="l", lty=2)

# Notice how the predicted values quickly converge to a sationary i.e. flat line equal to the estimated mean of the ts ->
# -> mu = 50.07051 ... confirm but its true. Be aware of these features
# -> fit.mean


# Example 2: Non-Stationary data Forecasting
# If a ts is non-stationary, we must make it stationary by removing the trend effects from the data
# -> then we can fit a ts model to the result

# To estimate future values, we must forecast the stationary time series and then REINSERT the TREND ->
# -> Forecast using stationary ts, then add trend effect back to get the forecasts for the original series

# Example: time series when d=1
# Code below is for ARIMA(0,1,2)

set.seed(476)
a=runif(1, 0.6, 0.8)
b=runif(1, -0.3, 1)
d2 = 50 + cumsum(arima.sim(list(ma = c(a, b)), n = 1460))
d2 = ts(d2, start=c(2015,1), frequency=365)
# We've generated an ARIMA(0,1,2) model

# Now we must difference it, and fit MA(2) model

diff.d2=diff(d2, lag=1, differences=1)
ma2=arima(diff.d2, order=c(0,0,2))

# So we have fitted model -> (1-B)d2 = mu + err(t) + b1*err(t-1) + b2*err(t-2)
ma2.coef=ma2$coef
ma2.mean=ma2.coef[3]

p2 = predict(ma2, n.ahead=180)$pred # -> if you get stuck, use str() to help

# p2 is the forecast of the differnced data -> to forecast the original data, UNDO the differencing ->
# using cumsum() then adding the result onto the last value of the original time series ->

p2.with.trend = tail(d2,1)+cumsum(p2) # -> tail(d2,1) is the last value in d2 ts

# Problem now...cumsum() function has transformed ts into vector -> we need to make the p2.with.trend a ts ->
p2.with.trend=ts(p2.with.trend, start=c(2019,1), frequency=365)

# Now we can use ts.plot() but adjust the xlim for the forecasted values to be added to original ts plot
ts.plot(d2, col = "blue", xlim = c(2015, 2019 + 179/365), main = "Forecasting example 2", ylab = "d2", ylim = c(-42, 153))
lines(p2.with.trend, col = "red")


# Forcasting with Exponential Smoothing
# x^hat_n(1) = alpha*x_n + (1-alpha)*x^hat_[n-1](1)
# Complicated but easy -> the 1-step ahead forecast is alpha*current value + (1-alpha)*(1-step ahead forecast at n-1)

# Use HoltWinters() function with parameters:
# -> alpha smoothing parameter
# -> beta smoothing parameter for trend
# -> gamma smoothing parameter for seasonality

# We will assume no trend or seasonality -> beta, gamma = FALSE

# Example 1 on Exponential Smoothing

series = read.csv("forecasting.csv", header = FALSE)
head(series)

series = ts(series, start=c(2017,20), frequency=365)

# The last observation of the series -> end(series)
end(series)

# We want to estimate the value of the time series on 4th June 2018 using exponential smoothing with 
# alpha = 0.7

HW = HoltWinters(series, alpha=0.7, beta=FALSE, gamma=FALSE)
predict(HW, n.ahead=1) # -> remember the end of the series was c(2018, 154) -> 154th day of 2018 or 3rd June 2018
# -> the step ahead foecast is for 4th June 2018 -> i.e. 1 day ahead therefore because frequency = 1 -> n.ahead = 1

# Calculate the upper and lower bound for the approximate 95% prediction interval
predict(HW, level = 0.95, prediction.interval = TRUE)

# Evaluating the estiamted value on 8th June 2018 -> give us the same stiamte as n.ahead = 1 ... why?
predict(HW, n.ahead=5) 3 # -> the fitted value is the same for each future time period

# Example 2
# Calculate the 95% prediction interval for the value of the time series on 4th June 2018, using the
# optimal smoothing parameter. Comment on your answer

HW2=HoltWinters(series, beta=FALSE, gamma=FALSE)
predict(HW2, n.ahead=1, level=0.95, prediction.interval=TRUE)
str(HW2) # -> useful
# optimal alpha determined by R:
HW2$alpha # -> alpha = 0.04646942

#########################################################################################
# Question 12

set.seed(1558)
data = 100 + arima.sim(list(ar = runif(1, -0.3, 0.6), ma = runif(1, 12, 15)), n = 40)
data = ts(data, start = c(2008, 1), frequency = 4)

PP.test(data)

fit=arima(data, order=c(1,0,1))
p = predict(fit, n.ahead=8)$pred

HW=HoltWinters(data, beta=FALSE, gamma=FALSE)
hw=predict(HW, n.ahead=8)


ts.plot(data, xlim=c(2008,2020), main="Time Series of Quarterly Revenue", ylab="Revenue", col="blue")
lines(p, col="red")
lines(hw, col="green")
par(mfrow=c(1,1))


#########################################################################################
# Question 13

set.seed(1952)
x = arima.sim(list(ar = 0.7), n = 240)
x = 1800 + cumsum(x)
x = ts(x, start = c(1990, 1), frequency = 12)

y=diff(x, lag=1, differences=1)

model=ar(y)
p=predict(model, n.ahead=60)
p.model=p$pred
x.pred=tail(x,1) + cumsum(p.model)
x.pred=ts(x.pred, c(2010,1), frequency=12)
tail(x.pred,1)

par(mfrow=c(1,1))
ts.plot(x, xlim=c(1990,2015), main="Number of Diagnoses of Rare Disease", ylab="Number of diagnoses", col="red")
lines(x.pred, col="blue")
par(mfrow=c(1,1))

#########################################################################################
# Question 14

set.seed(1952)
y = 90 + round(arima.sim(list(ma = c(15, 7)), n = 40))
y = ts(y, start = c(2007, 3), frequency = 4)

model=arima(y, order=c(0,0,2))
p.y=predict(model, n.ahead=86)$pred
tail(p.y,1)

# Dont be fooled by this question -> the process will evetually move toward its mean and expected err(t+) -> 0


#########################################################################################

# Multivariate Time Series
# -> Determine whether a MVts is Stationary
# -> Determine whether two ts are cointegrated...pay attention to this one

# We will use Capital Notation to denote matrices -> Please dont get confused with RVs
# We will sick to usual RV variables -> X, Y, Z
# We will use A, B, C etc for matrices

# Refer to notes on the ts forms -> difficult to write out here

# Mvts process is Stationary if all EIGENVALUES of coeffcient matrix A are strictly less than 1 in magnitude
# -> That means abs(lambda) < 1
# Remember that there is a relationship between the CP and Eigen basis

# To calculate the eigenvalues in R, use the eigen() function

# Example 1 
# Refer to equations in notes

A=matrix(c(0.2, 0.4, 0.1, 0.3), nrow=2, by=TRUE)
# str again is useful 
ev=eigen(A)$values
abs(ev)<1
# TRUE -> MVts is stationary

# Example 2
S=matrix(c(-0.6, 0.5, 0.8 -0.1), nrow=2, byrow=TRUE)
ev=eigen(S)$values
abs(ev)<1
# one of the eigenvalues is not stricly less than 1 -> process is not stationary


# Example 3
A=matrix(c(0.7, 0.9, 0.4, 1, 0.2, 0.2, 3, 0.2, 0.7), nrow=3, byrow=TRUE)
ev=eigen(A)$values
abs(ev)<1
# one of the eigenvalues is not stricly less than 1 -> process is not stationary


# Cointegrated Time Series
# Recall that two ts X and Y are cointegrated if ->
# - X and Y are I(1) random processes -> That X and Y can be differenced once
# - There exists a non-zero co-integrating vector [alpha, beta] such that:
# -> alpha*X + beta*Y is stationary

# To check if X and Y are cointegrated, we need to check these conditions above

# We can test for cointegration with packagaes...but this CS2...punishment
# Write your own test for cointegration

# To check the first condition, that is that both ts are I(1)
# -> check that the two datasets should be differenced
# -> then difference the data and check if the results are stationary

# We will use the PP.test() to check if diffences should be done

# For the second condition, we will find a cointegrating vector [alpha, beta] ->
# then check if alpha*X + beta*Y is stationary using the PP.test() again

# Example
xy = read.table("cointegration.txt", sep = " ", header = TRUE)
head(xy)

x=xy[,1]
y=xy[,2]

# Check the first condition -> check whther x and y are I(1)
PP.test(x) # -> p-value = 0.4258 therefore fail to reject null...there is a unit root and we need to difference
PP.test(y) # -> p-value = 0.6912 therefore fail to reject null...there is a unit root and we need to difference

PP.test(diff(x)) # -> p-value = 0.01 so we can reject null, no need to difference further
PP.test(diff(y)) # -> p-value = 0.01 so we can reject null, no need to difference further

# Condition 1 is met with x and y ts

# Now check the second condition...
# Does a vecotr [alpha, beta] exist such that alpha*x + beta*y is stationary?
# We will use the PP.test() again on the linear combination to see if the system is Stationary

# write the following function:

find.coint = function(coint){
	comb = coint[1]*x + coint[2]*y
	test = PP.test(comb)$p.value
	test
	}
	
# This is smart...we will use the nlm() function i.e. find coint vector that minimizes the find.coint()
# starting point -> v=c(1,1)

v=c(1,1)
fit=nlm(find.coint, v)
fit

coint=c(fit$estimate[1], fit$estimate[2])

# Check, but only for practice, that coint*(x,y) is stationary using PP.test()
PP.test(coint[1]*x + coint[2]*y)$p.value
# -> p-value = 0.01 therefore reject null, linear combinbation doesn't need differencing and therfore Stationary

# Note that there may be other vectors that also satisfy the condition for cointegration

# Question
# Write a general function that tests whether any given pair of time series x and y are 
# cointegrated. The output of the function should be the conclusion of the test (ie whether or 
# not the series are cointegrated), and the cointegrating vector (if there is one)

set.seed(1234)
z = rep(0, 1000)
for (i in 2:1000){
	z[i] = z[i-1] + rnorm(1)
	}
w = v = rep(0, 1000)
w = runif(1, 5, 7)*z + rnorm(1000)
v = -runif(1, 2, 4)*z + rnorm(1000)

is.cointegrated = function(x, y){
	xp = PP.test(x)$p.value >= 0.05 && PP.test(diff(x))$p.value < 0.05
	yp = PP.test(y)$p.value >= 0.05 && PP.test(diff(y))$p.value < 0.05
	
	find.coint = function(coint){
		comb = coint[1]*x + coint[2]*y
		test = PP.test(comb)$p.value
		test
		}
		
	v = c(1, 1)
	fit = nlm(find.coint, v)
	a = fit$estimate[1]
	b = fit$estimate[2]
	v.fit = c(a, b)
	v.fit.check = round(a, 6) == 0 && round(b, 6) == 0
	comb = a*x + b*y
	combp = PP.test(comb)$p.value < 0.05
		if (xp == TRUE && yp == TRUE && combp == TRUE && v.fit.check == FALSE){
				print("the vectors are cointegrated with cointegrating vector");
				print(v.fit)
		} else {
				print("x and y are not cointegrated")
		}
	}

is.cointegrated(w,v)

#########################################################################################
# Question 15

A=matrix(c(0.3, 0.1, -0.2, 0.6), nrow=2, byrow=TRUE)
ev=eigen(A)$values
abs(ev)<1
# -> eigenvalues are both stricly less than 1 -> MVts is Stationary

#########################################################################################
# Question 16

set.seed(1234)

z = rep(0, 1000)
	for (i in 2:1000){
		z[i] = z[i-1] + rnorm(1)
		}
		
s = runif(1, 16, 20)*z + rnorm(1000)
t = -runif(1, 1, 10)*z + rnorm(1000)

# Is s and t I(1) stationary?

PP.test(s)$p.value # -> need to difference
PP.test(t)$p.value # -> need to difference

PP.test(diff(s))$p.value # -> s is I(1) stationary
PP.test(diff(t))$p.value # -> t is I(1) stationary

v=c(0.2, 1.5)
PP.test(v[1]*s + v[2]*t)$p.value
# -> v=[0.2, 1.5] is not a cointegratiing vector

v=is.cointegrated(s,t) # -> s and t are cointegrated with cointegrating vector [0.426, 1.361]
v=c(v[1], v[2])

# -> fit = nlm(find.coint, v) -> this is the way

#########################################################################################

# Time Series Basics

# Simulate a stationary TS using arima.sim()
# eg. Y(t) = 0.5*Y(y-1) - 0.1*Y(t-2) + e(t) + 0.2*e(t-1)

# -> ARMA(2,1)
Yt = arima.sim(n = 100, model = list(ar = c(0.5,-0.1), ma = 0.2), sd = 3)
# default for sd = 1 -> think WN process variance

# ARMA(1,2
Wt = 10 + arima.sim(n = 50, model = list(ar = c(0.8), ma = c(0.4, 0.1)), sd = sqrt(5))

# Simulate 200 obs of AR(1): X(t) = 10 + 0.9*X(t-1) + e(t)
c = 10
n = 200

xt = numeric(n)

set.seed(1789)
et = rnorm(n)

for (j in 2:n){
	xt[j] = 10 + 0.9*xt[j-1] + et[j]
	}
xt

# Disdvanatges of simulating in the manner above:
# -> we simulate from a defined constant xt[1] = 0
# R has not recognised this as a TS object
# In general we would assum a TS has reached equilibrium i.e. started some time ago. However the method has not given the time series enough time to stabilise
# Plot xt to see why -> only stablises around 30th observation -> arima.sim() includes a 'burn-in' period to allow for this -> equilibrium

# Simulating non-stationary TS
# arima.sim works well to simulate stationary TS
# To simulate a non-stationary TS, we do manually

# Examples of non-stationary TS:
# TS with deterministic trend
# TS with stochastic trend

# To incorporate Deterministc Trend, create a TS Y(t) by adding a stationary TS X(t) onto a straight line: a + c*t
# Y(t) = a + c*t + X(t)
# where X(t) is a stationary TS generated from arima.sim()

set.seed(1215)
n = 50
xt = arima.sim(n, model = iist(ar = 0.9, ma = - 0.2)) # Stationary series abs(0.9) < 1
yt = xt + 1805 + 0.5*seq(1, n) # -> stationary series adding deterministic trend -> non-stationary Series

# Stocahstic Trend
# We can alternatively create and ARIMA(p,d,q) TS with d != 0 by incorporating stochastic trend

# -> First generate ARMA(p,q) using arima.sim() -> stationary series
# -> Then UNDIFFERENCE the data to calculate Yt such that:
# Yt = (1-B)Xt -> i.e. X(t) - X(t-1)

# Simulate 100 values of ARIMA(2,1,1) first ->
set.seed(8143)
n = 100
xt = arima.sim(model = list(ar = c(0.5, 0.2), ma = c(-0.2)), n) # Stationary ARMA(2,1) after it has been difference i.e. ARIMA(2,1,1)
# We want to recover ARIMA(2,1,1) by undifferencing -> we know then the resulting TS is non-stationary -> has a unit root 
# How do you undiffernce? Take the cumulative sum of the data

Yt = 80 + cumsum(xt) # -> you can show why we can do this...notes. DONT FORGET THE INTERCEPT 
# Convert Yt into a TS
Yt = ts(Yt)

# Intercept verus mean
# Note that the first value of Yt is 80 -> as expected
# Because Yt increases with over time, Yt is not stationary -> E(Yt) depends on time
mean(Yt) # -> 105.2462 which is not equal to 80

# In contrast, the sample mean and intercept of xt are both close to zero -> this is because arima.sim() geneartes stationary data centred around zero

# Importing TS data
Xt = read.csv("Xt.csv")
head(Xt)

Xt = read.table("Xt.csv", sep = ",") # or just use read.csv -> R will intepret the comma separated value
head(Xt)

# No attached dates to the data -> we can do that by converting Xt into a TS with frequency
ts.Xt = ts(Xt, start = c(2000, 1), frequency = 12)
ts.Xt

# weekly -> frequency = 52
# quarterly -> frequency = 4
# annual -> frequency = 1

# state = c(2000, 1) -> January 2000
ts.quarterlyXt = ts(Xt, start = c(1912, 2), frequency = 4)

# to get the output of the TS over a window of tim ->
window(ts.Xt, start = c(2004, 2), end = c(2007, 5))

# Plotting TS is straightforward ->
ts.plot(ts.Xt, main = "Time Series Xt", ylab = "Value", col = "blue")
points(ts.Xt, col = "red", cex = 0.7)

# Plotting sample ACF and sample PACF
# acf() and pacf() are sample functions -> Remember these functions will have sampling errors because they're based on the sample -> acf and pacf distorted by sampling error

par(mfrow = c(2,1))
acf(ts.Xt, lag.max = 60, main = "Time series Xt", ylab = "Sample ACF") 
pacf(ts.Xt, lag.max = 60, main = "Time series Xt", ylab = "Sample PACF")
par(mfrow = c(1,1))

# Plotting Theoretical ACF and PACF
# Example model: Y(t) = 0.5*Y(t-1) - 0.1*Y(t-2) + e(t) + 0.2*e(t-1) - ARMA(2,1)
# Calculate the theoretical ACF for lags k = 0,1,2,3,...12 ->
modelACF = ARMAacf(ar = c(0.5, -0.1), ma = c(0.2), lag.max = 12) # -> plot and confirm
# and for the PACF
modelPACF = ARMAacf(ar = c(0.5, -0.1), ma = c(0.2), lag.max = 12, pacf = TRUE) # -> plot and confirm

par(mfrow = c(2,1))
barplot(modelACF[-1], main = "Theoretical ACF of ARMA(2,1)", col = "red", xlab = "Lag") # -> remove the lag k = 0 and put labels for the lag on the x axis
barplot(modelPACF, main = "Theoretical PACF of ARMA(2,1)", col = "blue", xlab = "Lag", names.arg = seq(1,12)) # -> put labels for the lag on the x axis
par(mfrow = c(1,1))

# Extracting key numbers from the TS data
frequency(ts.Xt) # -> 12
start(ts.Xt) # -> c(2000, 1)
end(ts.Xt) # -> c(2016, 8)

# ACF and PACF Calculations
# Calculate the sample ACF rho^(k) for given lag k using acf()

a = acf(ts.Xt, plot = FALSE, lag.max = 60)

# lag.max = 60 -> how many ACFs to compute -> 60
# -> 60 / 12 since the TS is monthly -> calculated rho^(k) for up to 5 years
# -> rho^(0), rho^(1/12), ..., rho^(59/12), rho^(60/12)
# You see in R how its shown -> autocorrelations by lage but its denoted in 1/12 ths

# rho^(0) -> will be 1 i.e. a$acf[1] = 1
# to get acf value 12 months apart say -> a$acf[13] -> 0.5897812

b = pacf(ts.Xt, plot = FALSE, lag.max = 125) # REMEMBER THAT THE TIME SERIES IS DEFINED ACCORDING TO FREQUENCY! NEED MORE LAGS TO EXTRACT DATA FURTHER OUT
b$acf[10*12] # -> PACF exactly 10 year apart ->  0.02423828 BE CAREFUL -> to get the pacf value, str(b) -> you'll see $acf even though pacf -> IMPORTANT


#########################################################################################
# Question 1

# AR(2) -> Y(t) = 15 + 0.8*Y(t-1) - 0.6*Y(t-2) + Z(t)
# Plot the theoretical functions

par(mfrow = c(2,1))
barplot(ARMAacf(ar = c(0.8, -0.6), lag.max = 20)[-1], main = "Theoretical ACF of AR(2)", xlab = "Lag", ylab = "ACF", col = "red")
barplot(ARMAacf(ar = c(0.8, -0.6), lag.max = 20, pacf = TRUE), main = "Theoretical PACF of AR(2)", xlab = "Lag", ylab = "ACF", col = "blue", names.arg = seq(1, 20))
par(mfrow = c(1,1))


#########################################################################################
# Question 2

frequency(ldeaths) # -> 12
start(ldeaths) # -> c(1974, 1)
end(ldeaths) # -> c(1979, 12)
acf(ldeaths, plot = FALSE, lag.max = 30)$acf[2*12 + 1] # -> 0.6001395
window(ldeaths, start = c(1978, 2), end = c(1978, 2)) # -> 3137
sum(window(ldeaths, start = c(1976, 1), end = c(1976, 12))) # -> 25718


#########################################################################################
# Question 3

# 5y gap based on 12-month freq -> lag.max = 60

par(mfrow = c(2,1))
acf(ldeaths, lag.max = 60, main = "ACF of ldeaths", xlab = "Lag", ylab = "ACF", col = "red")
pacf(ldeaths, lag.max = 60, main = "PACF of ldeaths", xlab = "Lag", ylab = "PACF", col = "blue")
par(mfrow = c(1,1))


#########################################################################################
# Question 4

set.seed(2224)
sim = arima.sim(n = 480, model = list(ar = c(0.8,-0.4), ma = 0.6), sd = 100) + 2000
sim = ts(sim, start = c(2020,1), end = c(2059, 12), frequency = 12)

ts.plot(window(sim, start = c(2024, 1), end = c(2025, 12)), type = "l", main = "TS of sim from January 2024 - December 2025", ylab = "Value")
points(window(sim, start = c(2024, 1), end = c(2025, 12)), col = "red")


#########################################################################################

# Time Series Removing Trends

# Analyse there TS is Stationary
# Differencing TS data
# Removing linear trend in the data using LS Regression
# Identifying seasonality in TS data by plotting ACFs
# Removing Seasonal variation from data by means of seasonal differencing
# Using R function decompose() and stl() to apply:
#	-> method of seasonal means
# 	-> method of moving averages

testing.stationarity = read.table("testing.stationarity.txt")
head(testing.stationarity)

testing.stationarity = ts(testing.stationarity)

# How to test if a TS is stationary?
# How to difference a dataset

# If a TS is stationary, the sample ACF should decay exponentially to zero with increasing lag
# -> if the ACF decays slowly but steadily -> required to difference to remove Trend

acf(testing.stationarity, lag.max = 30, main = "Sample CF of testing.stationarity", xlab = "Lag", ylab = "Sample ACF", col = "red")
# -> Slow and Steady decay -> Not Stationary
# -> Data should be differenced but how many times?

diff.test.stat = diff(testing.stationarity) 

# Statistical Test for Stationarity -> Phillips-Perron Test: PP.test()
# H0: the times series HAS a unit root (and hence must be differnced) -> we want to reject this null when the TS is Stationary
# H1: the TS does not need to be differnced i.e. TS is Stationary -> no unit root
# Use PP.test() to test for Stationarity

PP.test(testing.stationarity)$p.value # -> 0.5921636 fail to reject null -> there is a unit root i.e. TS not Stationary
PP.test(diff.test.stat)$p.value # -> 0.01 -> reject null at 5% level -> difference TS is stationary or doesn't have a unit root

# Example:
set.seed(1901)
n = 365
data = arima.sim(n, model = list(ma = c(-1.4, 0.8)))
acf(data, main = "TS: data", ylab = "Sample ACF")

# Don't need to run PP.test() on this process -> All MA processes are stationary -> question of invertibility and giving the unique ACF
# Do it anyway -> conclusion is that we reject null for PP.test()
PP.test(data)$p.value # -> 0.01 ... reject null at 5% level -> there is no unit root -> therefore Stationary

# Differencing TS data
Xt = diff(testing.stationarity, lag = 1, differences = 1) 
# -> differencing if (1-B) on testing.stationarity

par(mfrow = c(2, 2))
ts.plot(testing.stationarity, main = "Data:testing.stationarity", ylab = "value")
ts.plot(Xt, main = "Differenced data", ylab = "change in value")
acf(testing.stationarity, main = "", ylab = "Sample ACF")
acf(Xt, main = "", ylab = "Sample ACF")
par(mfrow = c(1, 1))

# Choosing d
# You difference the data until you achieve stationarity -> when to stop?
# Consider the variance of the differenced series -> set d that minimizes the variance of the difference data

var(testing.stationarity)
var(Xt)
var(diff(Xt, 1, 1))
# -> can see the variance decreases after d = 1 but increases again when d = 2 -> conclude that d = 1
# NOTE: diff(data, lag = lag on the value to difference, difference = how many times you want to difference)
var(testing.stationarity)
var(Xt)
var(diff(Xt, 1, 1))
var(diff(Xt, 1, 2)) # -> difference Xt twice

# Least Squares Trend Removal
# We can remove linear trend in the data using Ordinary Least Squares Regression
# Involves fitting: xt = a + bt + yt where yt is a process with zero mean, a and b are constants

set.seed(123)
n = 1000

sim = arima.sim(model = list(ar = 0.9), n)
xt = sim + 2 + 0.05 * time(sim)

ts.plot(xt, col = "blue", main = "TS with trend", ylab = "Data")
# we can either use the time from the TS -> time(xt) or time = seq(1, 1000)

t = time(xt)
# the t corresponds to a straight line: yt = a + b*t

fit = lm(xt ~ t)
# a -> 1.94842 from regression
# b -> 0.05055 from regression
summary(fit) # -> make sure the coefficients a and b are significant to use

yt = fit$fitted.values
zt = fit$residuals

# Alternative:
# yt = fit$fit
# zt = xt - yt -> xt - fitted values yt -> residuals is the stationary process after removing trend

par(mfrow = c(1,1))
plot(xt, col = "blue", main = "Regression example", ylab = "Data")
abline(fit, col = "red")
par(mfrow = c(1,1))

par(mfrow = c(2,1))
plot(xt, col = "blue", main = "Regression example", ylab = "Data")
plot(zt, col = "dark green", xlab = "Time", ylab = "Residuals")
par(mfrow = c(1,1))

# Identifying Seasonality 
# Quick check for seasonility: plot the data and plot the sample ACF -> see patterns in correlation thats repeatable -> seasonality

plot(ldeaths, main = "Monthly deaths from bronchitis, emphysema, and asthma in the UK", ylab = "Deaths")
points(ldeaths, pch = 20)
abline (v = 1974:1979, col = "red", lwd = 2) # -> abline here is v = x

# Chart appears to have yearly seasonal effect -> deaths in winter
# We can add dividers to assits with seeing the seasonal period

# Look at the ACF
acf(ldeaths, main = "Sample ACF of ldeaths", ylab = "sample ACF", lag.max = 36)
# -> lag axis is labelled up to 3 instead of 36 -> remember the frequency of ldeaths = 12
# Here again we can see an annual trend -> YoY the correlation is postive, and every 6 months it goes negative
# So there is seaonality in ldeaths -> how do we remove it for the TS to be stationary?

# Removing Seasonality
# Remove seasonlity through seasonal differencing
# Method of seasonal means
# Method of moving averages

# Seasonal differencing -> ldeaths exhibit yearly seasonality so difference at lag 12 (frequency = 12)
sdiff.ldeaths = diff(ldeaths, 12, 1)

par(mfrow = c(2, 2))

plot(ldeaths, main = "Data: ldeaths", ylab = "number of deaths")
acf(ldeaths, main = "Sample ACF of ldeaths", ylab = "")

plot(sdiff.ldeaths, main = " Data: sdiff.ldeaths", ylab = "increase in number of deaths")
acf(sdiff.ldeaths, main = "Sample ACF of sdiff.ldeaths", ylab = "")

par(mfrow = c(1, 1))

# The ACF has no pattern of seasonality any longer from the seasonal Difference
# We can check the differenced series for stationarity if necessary

# We can use layout for frid graphs ->

m = matrix(c(1, 2, 3, 4), nrow = 2, ncol =2, byrow = TRUE) # -> layout order is the matrix order row by row

layout(m)

plot(ldeaths, main = "Data: ldeaths", ylab = "number of deaths")
acf(ldeaths, main = "Sample ACF of ldeaths", ylab = "")
plot(sdiff.ldeaths, main = " Data: sdiff.ldeaths", ylab = "increase in number of deaths")
acf(sdiff.ldeaths, main = "Sample ACF of sdiff.ldeaths", ylab = "")

par(mfrow = c(1, 1))

# If you want a single graph on top, and two at the bottom ->
m = matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE)
layout(m()
# and the rest of the graph Code
par(mfrow = c(1, 1))

# Seasonal Means
# For a TS xt that has seasonal variation, we can apply the mthod of seasonal means ->
# xt = mu + yt + theta_t 
# yt is a non-seasonal TS
# theta_t -> is a periodic function with period of the seasonal variation exhbited by xt

# Therfore -> yt = xt - mu - theta_t
# See notes pg 20
# x^bar_JAN = (1 / n(all JAN data points) * sum[i =1, n](xi_JAN points)

# yt_JAN = xt_JAN - x^bar_JAN

# Look at data set ldeaths
# Create a data frame to separate the month and year columns using rep() and each

ldeaths.df = data.frame(year = rep(1974:1979, each = 12), month = rep(1:12, 6), value = ldeaths)
# Now we can calculate the averages for each calendar month using aggregate()

# Functional form: aggregate(<vector of values>, ~ <vector to aggregate by>, data = <data frame>, FUN = <function>)

xbars = aggregate(value ~ month, data = ldeaths.df, FUN = mean)

# Therfore -> yt = xt - xbars
yt = ldeaths - xbars$value # -> we need the $value to extract the elemets in xbars -> xbars is still a data fram inheritted from ldeaths.df
# R does what you were worried about -> ldeaths has length 72 but xbars has lenght 12 -> R does the correction

plot(yt, main = "Monthly deaths from bronchitis, emphysema and asthma in the UK less seasonal means", ylab = "Deaths")
points(yt, pch = 20)

acf(yt, main = "Sample ACF of the series ldeaths less seasonal means", ylab = "sample ACF", lag.max = 36)

# Uss built in function in R to separate out Trend, Seasonality, and White Noise
# decompose()
# stl()

decompose(ldeaths, type = "additive") # -> can only do this on TS
plot(decompose(ldeaths, type = "additive")) # -> can only do this on TS
# if Seasonality is constant through time -> "additive"
# If Seasonality appears to change over time -> "multiplicative"
# pg 24

# Extracting data from the decompose function
decomp = decompose(ldeaths, type = "additive")
str(decomp)

trend = decomp$trend
# you'll see that trend has NA values at the beginning and en
head(trend, 12)
tail(trend, 12)
# As expected -> trend calculated using Moving Averages -> read example on pg 24

seasonal = decomp$seasonal
random = decomp$random # -> THE WHITE NOISE

plot(ldeaths, ylab = "", main = "Components of time series: ldeaths", col = "dark grey")
points(ldeaths, cex = 0.5, col = "dark grey")
lines(trend, col = "red")
lines(seasonal + trend, col = "blue")

# stl() function # -> only for additive TS
# If TS is multiplicative -> log TS -> transform to additive before using stl()
# -> stl() is simlar to decompose() as it separates the seasonal variation, trend, and white Noise from the data

stl(ldeaths, s.window = "periodic") # -> s.window = "periodic" tells R to calculate the seasonal component
plot(stl(ldeaths, s.window = "periodic"), main = "Components of time series: ldeaths")

# Bars on the right hand side of the graphs indicate the size of each compoenent

# As with decompose(), we can store values of stl() ->
stl = stl(ldeaths, s.window = "periodic")
str(stl) # -> "seasonal" "trend" "remainder" : NOT STRAIGHTFORWARD AS YOU'LL SEE

trend = stl$time.series[, "trend"]
seasonal = stl$time.series[, "seasonal"]
remainder = stl$time.series[, "remainder"]

plot(ldeaths, ylab = "", main = "Components of time series: ldeaths", col = "dark grey")
points(ldeaths, cex = 0.5, col = "dark grey")
lines(trend, col = "red")
lines(seasonal + trend, col = "blue")
# lines(seasonal + trend + remainder, col = "green")
# remainder is the stationary process -> The White Noise

# decompose() uses the method of Moving Averages (pg 24) and Seasonal Means -> Covered in Core Reading
# stl() only uses additive TS -> if TS is multiplicative, log tranform TS and apply as additive before using stl()

#########################################################################################
# Question 5

xx = numeric(1003)
set.seed(4567)

ww = rnorm(1003)
xx[1:3] = ww[1:3]

for (t in 4:1003) {
	xx[t] = 0.8*xx[t-1] +0.2* xx[t-3] + ww[t]+ 2*ww[t-1]
	}

s = ts(xx[4:1003])
var(s)

s.d1 = diff(s, 1, 1)
var(s.d1)

s.d2 = diff(s, 1, 2)
var(s.d2)

# Differencd once -> d = 1

#########################################################################################
# Question 6

sales = read.table("sales.txt")

sales = ts(sales, start = c(2005, 1), end = c(2014, 12), frequency = 12)
ts.plot(sales)

sales.diff = diff(sales, 1, 1)
PP.test(sales.diff) # -> reject null, no unit root -> stationary

decomp = decompose(sales, type = "additive")
sales.ex.trend = sales[7:(length(sales) - 6)] - decomp$trend[7:(length(sales) - 6)] # -> Can use window(ts, start, end) instead of the indexing
sales.ex.trend.seasonal = sales[7:(length(sales) - 6)] - decomp$trend[7:(length(sales) - 6)] - decomp$seasonal[7:(length(sales) - 6)] # -> Can use window(ts, start, end) instead of the indexing

par(mfrow = c(2,1))
acf(sales.ex.trend, lag.max = 60)
acf(sales.ex.trend.seasonal, lag.max = 60)
par(mfrow = c(1,1))

par(mfrow = c(2,1))
acf(sales.ex.trend.seasonal, lag.max = 60, main = "ACF of sales ex trend and seasonality", ylab = "sample ACF")
pacf(sales.ex.trend.seasonal, lag.max = 60, main = "PACF of sales ex trend and seasonality", ylab = "sample PACF")
par(mfrow = c(1,1))

# The ACF does decay over lag and exhbits osicallation -> AR(p) process with p > 1
# The PACF cuts off after lag 2 -> the pacf after lag 2 are not signficant at 5% level -> comfortable that p = 2 -> AR(2)


#########################################################################################
# Question 7

# Volume of water in reservoir measure every month from 2010 for 5 years

vol = read.table("seasonality.txt")
head(vol)

vol = ts(vol, start = c(2010, 1), end = c(2014, 12), frequency = 12)

par(mfrow = c(2,1))
acf(vol, lag.max = 60, main = "Sample ACF of Resevoir Volume from 2010 - 2014", ylab = "Sample ACF")
pacf(vol, lag.max = 60, main = "Sample PACF of Resevoir Volume from 2010 - 2014", ylab = "Sample PACF")
par(mfrow = c(1,1))

seasonal.diff.vol = diff(vol, 12, 1) # -> lag = 12 i.e. annual seasonality but also frequency = 12

par(mfrow = c(2,1))
acf(seasonal.diff.vol, lag.max = 60, main = "Sample ACF of Seasonally adjusted Resevoir Volume from 2010 - 2014", ylab = "Sample ACF")
pacf(seasonal.diff.vol, lag.max = 60, main = "Sample PACF of of Seasonally adjusted Resevoir Volume from 2010 - 2014", ylab = "Sample PACF")
par(mfrow = c(1,1))

# Another way -> Seasonal Difference of the Means
# -> focus on the periodic function and calc the seasonal mean -> adjust vol accordingly
# Assuming annual seasonal variation

vol.df = data.frame(year = rep(2010:2014, each = 12), month = rep(1:12, 5), value = vol) # -> Careful with the data.frame rep functions, one is each =, the other jsut simple rep for a number of times
vol.bars = aggregate(V1 ~ month, data = vol.df, FUN = mean)
yt = vol - vol.bars$V1 # -> DONT BE AN ASS -> vol.bars inherit data frame from vol.df

m = matrix(c(1, 4, 2, 5, 3, 6), nrow = 3, ncol = 2, byrow = TRUE)
layout(m)

ts.plot(yt, col = "blue", main = "Seasonally mean-adjusted volume", ylab = "volume")
acf(yt, lag.max = 60, main = "ACF of Seasonally mean-adjusted volume", ylab = "ACF")
pacf(yt, lag.max = 60, main = "ACF of Seasonally mean-adjusted volume", ylab = "Sample PACF")

ts.plot(seasonal.diff.vol, col = "blue", main = "Seasonally Differenced Resevoir Volume from 2010 - 2014", ylab = "volume")
acf(seasonal.diff.vol, lag.max = 60, main = "Sample ACF of Seasonally Differenced Resevoir Volume from 2010 - 2014", ylab = "Sample ACF")
pacf(seasonal.diff.vol, lag.max = 60, main = "Sample PACF of of Seasonally Differenced Resevoir Volume from 2010 - 2014", ylab = "Sample PACF")

par(mfrow=c(1,1))


#########################################################################################

# Times Series Fitting a Model

# Use graphs of ACF and PACF tp decide which model to Fit
# fit a model to a TS
# Discuss how graphs can help analyse GoF ot TS Model
# Analyse Gof using:
# -> Boj-Ljung Test 
# -> AIC

# To fit TS models, we require stationary TS -> start first by ensuring the TS is stationary either from ACF plots or PP.test()
# Having obtained statioanry TS we can identify what order model by examining the Sample ACF or Sample PACF
# -> If the SACF and SPACF dont show signficant correlations, we fit White Noise
# -> If the SACF decays exponentially, SPACF cut off after lag p -> AR(P)
# -> If the SACF cuts off after lag q, SPACF oscillates -> MA(Q)
# -> If both the SACF and SPACF decay, we fitr ARMA(p,q) using trial and error to find optimal values of (p,q) based on chosen criterion -> AIC for instance

# White Noise ->
# For a large enough sample from a WN process ->
# rho ~ N(0, 1/n) and phi ~ N(0, 1/n)
# CI -> estimate of rho or phi +- 1.96*sqrt(1/n)
# From the ACF, PACF plots, the interval shown is the CI and outside are type I error i.e. reject null that estimates of correlation = 0

# Example 1
data = read.csv("fittingmodelEg1.csv", header = FALSE)
head(data)

data = ts(data)

m = matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE)
layout(m)

ts.plot(data, main = "What model to fit?", ylab = "Data: Example 1")
acf(data, main ="")
pacf(data, main ="")

par(mfrow = c(1,1))
# Based on the non-signficant ACFs and PACFs -> process looks like White Noise
# No obvious trend of Seasonality

# Example 2
# For a large enough sample from MA(q) ->
# rho(k) ~ N(0, 1 / n * (1+ 2 * sum[i=1, q](rho(i)^2))for k > q  ... and phi(k) -> 0 as k -> Inf
# You can constuct CIs from the distribution above
# You can see that the CI interval for MA(q) is wider than AR(p) -> use the same appraoch at looking at correlations that are not signficant

data2 = read.csv("fittingmodelEg2.csv", header = FALSE)
head(data2)

data2 = ts(data2)

m = matrix(c(1, 2, 1, 3), nrow = 2, ncol = 2, byrow=TRUE)
layout(m)

ts.plot(data2, main = "What model to fit?", ylab = "Data: Example 2")
acf(data2, main = "")
pacf(data2, main = "")

par(mfrow = c(1, 1))

# Based on our defintion of an MA(q) process -> this looks suggestive of MA(3) from ACF
# -> PACF for MA(q) will osciallte and tend to zero quickly as the lag increases
# There is no obvious trend of seaonality -> ARIMA(0,0,3)
# You could look at MA(2) and MA(3) AIC for GoF analysis

# Example 3
# For a large enough sample from AR(P) model ->
# phi(k) ~ N(0, 1 / n)) for k > p and rho(k) -> 0 as k -> Inf
# CI -> estimate of phi +- 1.96*sqrt(1/n) same as before

data3 = read.csv("fittingmodelEg3.csv", header = FALSE)

data3 = ts(data3)

m  = matrix(c(1, 2, 1, 3), nrow = 2, ncol = 2, byrow=TRUE)
layout(m)

ts.plot(data3, main = "What model to fit?", ylab = "Data: Example 2")
acf(data3, main = "", ylab = "Sample ACF")
pacf(data3, main = "", ylab = "Sample PACF")

par(mfrow = c(1,1))

# No trend, seasonality -> ACF has exponential decay as lag increase
# PACF cuts off after lag 2 -> phi(k) for k > 2 fall inside interval -> no significant pacfs
# Conclude AR(2) or ARIMA(2,0,0) process


# Example 4 - ARMA MODEL

data4 = ts(read.csv("fittingmodelEg4.csv", header = FALSE))
# initial plot shows trend -> difference
# ACF shows slow decay which confirm the trend and non-stationarity -> need to difference

data4.d = diff(data4, 1, 1)

m = matrix(c(1, 4, 2, 5, 3, 6), nrow = 3, ncol = 2, byrow=TRUE)
layout(m)

ts.plot(data4, main = "Data without differencing", ylab = "Data: Example 3")
acf(data4, main = "", ylab = "Sample ACF")
pacf(data4, main = "", ylab = "Sample PACF")

ts.plot(data4.d, main = "Differenced data", ylab = "increase in data3")
acf(data4.d, main = "", ylab = "Sample ACF")
pacf(data4.d, main= "", ylab = "Sample PACF")

par(mfrow = c(1, 1))

# We differenced the TS -> d = 1 and we can see the expoenential decay in the ACF
# The PACF oscillates for at least 8 lags before the pacfs become non-significant
# We've got no clear cutoffs in both ACF and PACF -> need to fit other models and decide on a criterion to use to select the best fit

# Fitting a Model
# We use arima() function to fit ARIMA(p,d,q) model in R
# -> For instance, to fit ARIMA(1,0,1) -> arima(ts, order = c(1, 0, 1))

# DONT GET CONFUSED -> 
# arima.sim(n, model = list(ar = c(,), ma = c(,)), sd)
# arima(ts, order = c(p, d, q))

# Example 2 continued...fitting MA(3) ->
m2 = arima(data2, order = c(0, 0, 3))

# Output is easy to understand -> from the model -> write out equations
m2$coef # ->              ma1           ma2           ma3           intercept 
					 # 0.7506091559 -0.9096455763 -0.8409611163  0.0004682244 

# sig^2 - 140.1
# -> WN {Zt} assumed to have zero mean and variance sig^2 = 140.1

# Example 3 continued...fitting AR(2) ->
m3 = arima(data3, order = c(2, 0, 0))
m3$coef
# DONT FORGET HOW TO WRITE OUT AR(p) PROCESS with mean mu -> (X(t) - mu) = a1*(X(t-1) - mu) + a2*(X(t-2) - mu) + ... + ap*(X(t-p) - mu) + err(t)

# Alternative method for fitting AR(p) proceesses ->
ar(data3)
# -> see Output
# -> This method has chosen an AR(2) as the best fit
# ar() uses Yule-Walker to fit the model -> theoretical ACF to fit the MODEL
# arima() uses a different method to estimate the coefficients
# ar() doesn't give a mean
str(ar(data3)) # -> we see the x.mean in there
ar(data3)$x.mean # -> 499.3974 which the same result from arima() -> because TS data3 is stationary


# Example 4 continued...fitting ARIMA(1, 1, 1) ->
m4 = arima(data4, order = c(1, 1, 1)) # -> R will difference then fit ARMA(1,1)
str(m4)
# -> see output for m4
# The output doesn't include the mean or intercept -> because of the differencing -> function fits a zero mean  to difference data if d > 0
# This may be a bad model if a zero mean model is not appropriate for the differenced data

# We can manually differnce -> then fit ARMA(1,1)
m4.d = arima(data4.d, order = c(1, 0, 1))
# -> see output for m4.d
# -> we see an intercept term -> perhaps rather difference manually and then fit model -> might be important when selecting appropriate model

# Testing the Fit 
# -> What I remember:
# Box-Ljung Portmanteau
# Inpsection of graph of residuals -> evidence of WN
# Turning Points
# AIC AIC 

# Calculating Residuals 
# Starting point for analysing Goodness of Fit -> analyse the Residuals

# arima() does the Calculations
# For model 3 -> m3
e = m3$residuals

# Alternatively ->
e = residuals(m3)

par(mfrow = c(2,1))

ts.plot(e, main = "MA(3): analysis of residuals", ylab = "Residuals", col = "blue")
acf(e, main = "", ylab = "ACF of residuals")

par(mfrow = c(1, 1))

# -> all correlations are non-signifcant -> evidence of WN process

# tsdiag() function
# Alternative to plotting the residuals -> use tsdiag() as suggested by Core Reading
tsdiag(m3)
# Don't focus on the last graph - Ljung-Box p-values is incorrect
# On inspection:
# -> The mean and variance of the residuals are braodly constant over time -> important because of Stationarity
# -> the ACFs of the residuals are small and no significant pattern -> appear to be uncorrelated -> which is what we want for constant variance or at least no variance that changes with the size of the Residual
# Fail to reject the null -> no evidence that this is not a WN process -> good fit

# Bx Ljung Test -> Portmanteau test
# Here we need to be careful -> there is a lot going on that needs attention especailly with (p,q) and degree of freedom
# Box.test(<residuals>, lag = , type = "Ljung", fitdf = )

# This is for model 3 -> AR(3)
w = Box.test(e, lag = 5, type = "Ljung", fitdf = 3) # -> fitdf = p + q = 3 + 0 = 3
# lag = 5 -> always consider the power of the test here -> hihgher lag i.e. more correlatios used, reduces the power
# lower lag -> increases the power but the Test Statistic less resembles a ChiSq RV
# Null Hypothesis -> WN process -> so we want to fail to reject the null if it is a WN and a Good Fit
w$p.value # -> 0.2363  so we fail to reject the Null -> residuals are indepdendent

# Box Ljung Test Statistic ~ ChiSq(m -(p +q)) distribution -> Total DoF = m - (p + q) -> lag - fitdf
# lag = 5 -> use the first 5 ACF values

test.stat = w$statistic
test.stat # -> 2.884927 
p.value = pchisq(test.stat, 5 - 3, lower=FALSE)
p.value # -> 0.2363448 ... same as above

# The Null Hypothesis for the Box Ljung test -> H0 : the residuals are independent

# Choosing the number of correlation coefficients -> IMPORTANT
# Choice of lag m in the Box Ljung test is arbitrary
# -> any number for lag m provided that m - (p + q) > 0
# If we include a lot of correlation coefficients in the Box Ljung test, the test statistic will be more likely to follow a ChiSq distribution 
# The problem though with higher DoF -> lower the power of the Test
# The converse is also true -> lower the DoF, the higher the power of the Test
# With lower Dof -> the power of the test is higher but the test statistic will be less likely to come from a ChiSq distribution

# Power -> Pr(reject the null | the null is false)
# -> 1 - Pr(type II error)

# The AIC -> IMPORTANT CRITERION
# Method of analysing the GoF -> the LOWER THE AIC -> better fit expected
# AIC is a measure of the trade-off between:
# -> Goodness of Fit
# -> Number of parameters in the model

# A model with many parameters will probably be a good fit but not an accurate predictor of future experience -> high variance
# A model with fewer parameters will probably be a poorer fit -> not capture the significant patterns in the data -> but have less variance in future predictions
# Strike a balance between these two effects

# Example using data4.d
# Remember for AIC -> want the lower AIC for GoF

m5 = arima(data4.d, order = c(2, 0, 1))
m6 = arima(data4.d, order = c(1, 0, 2))

m5$aic # -> 2177.172
m6$aic # -> 2060.051

# m6 -> ARMA(1, 2) is a better fit to data4.d than ARMA(2, 1) based on AIC 

#########################################################################################
# Question 8

TS = read.table("TS.txt", sep = "\t")
TS = ts(TS[, 1], start = 1, end = length(TS[, 1]), frequency = 1)

# Fitting an AR(2) model ->
m.TS = arima(TS, order = c(2, 0 ,0))

# Ok, when commenting -> stat the model
# str(m.TS) 
# mu = 500.1856 (intercept)
# a1 = 0.2193
# a2 = 0.4475
# sig^2 = 0.9245 -> estimated variance of the WN terms Z(t)


par(mfrow = c(2, 1))
acf(TS)
pacf(TS)
par(mfrow = c(1, 1))

# p = 2, q = 0 -> p + q = 2
e = residuals(m.TS) # -> remember residuals must be on the model and not the TS -> can use residuals(model) or model$residuals
w = Box.test(e, lag = 5, type ="Ljung", fitdf = 2
w$p.value # -> 0.2625162 fail to reject null -> residuals are indepdendent and follow WN process

#########################################################################################
# Question 9

Wt = read.table("Wt.csv", sep = "", header = TRUE)
Wt = ts(Wt$value, start = min(Wt$day), end = max(Wt$day))

par(mfrow = c(2, 1))

acf(Wt, lag.max = 40, main = "Sample ACF of Commodity TS", ylab = "Sample ACF")
pacf(Wt, lag.max = 40, main = "Sample PACF of Commodity TS", ylab = "Sample PACF")

par(mfrow = c(1, 1))

# ACF does decay quickly
# No trend of seasonality present
# PACF cut offs after lag 1 -> suggest AR(1) or ARIMA(1,0,0)

#########################################################################################
# Question 10

Xt = read.table("Xt.csv", sep = ",", header = TRUE)
Xt = ts(Xt, 1990, frequency = 12) # -> frequency = 12

# Xt is a TS recorded monthly from Jan 1990 to Aug 2006
# Calculate the AIC for p <= 2, d <= 2, q <= 2

# Create a data.frame with combinations of p, d, q 
# 3 x 3 x 3 = 27 combinations

params = expand.grid(p = 0:2, d = 0:2, q = 0:2) # -> data frame
for (j in 1:nrow(params)){
	params$AIC[j] = arima(Xt, order = c(params[j, "p"], params[j, "d"], params[j, "q"]))$aic
	}

params
# The best model will have the lowest AIC -> wuse which.min() to get the row index of params

index = which.min(params$AIC) # -> 23
params[index, ] # -> ARIMA(1, 1, 2) AIC 872.1794


#########################################################################################
# Question 11

Yt = read.table("Yt.csv", sep = ",", header = TRUE)
Yt = ts(Yt[,2], 2005, frequency = 12)

# Intial inspection -> Trend
PP.test(Yt)$p.value # -> 0.01 hmmm
# Yt is clearly not stationary -> plot and ACF confirm so
# Can only fir ARMA(1,1) if TS Stationary

d.Yt = diff(Yt, 1, 1)

# Fit ARMA(1,1)
d.Yt.m = arima(d.Yt, order = c(1, 0, 1))
# a1 = -0.4377
# b1 = 0.7410 
# mu = 0.1003 interecpt

# Now have to undifference the model -> Yt.m

e = residuals(d.Yt.m)

par(mfrow = c(2, 1))
ts.plot(e, ylab = "residual value", main = "TS plot of residuals of d.Yt.m", col = "blue")
acf(e, ylab = "Sample ACF", main = "Sample ACF of residualss of d.Yt.m")
par(mfrow = c(1, 1))
# no non-signficant correlations at any lag -> residual correaltionsa are very low -> residuals are idependent -> differenced model for Yt is a good fit

# Turning Points Test
# Turns ~ N(2/3 * (n - 2), (16*n - 29) / 90) # -> if n is large enough 
# Calculate the test statistic -> need the number of turns

n = length(Yt)

E = 2/3 * (n -2)
Var = (16*n - 29) / 90

turns = 0
# What does a turn mean? if e(t-1) < e(t) and e(t)> e(t+1) -> this is a turn
# Similarly e(t-1) > e(t) and e(t) < e(t+1) -> this is a turn

for (i in 2:(n - 1)) {
	if ((e[i] > e[i + 1]) & (e[i] > e[i - 1])){
		turns = turns + 1
		}
	else if ((e[i] < e[i + 1]) & (e[i] < e[i - 1])){
		turns = turns + 1
		}
	}

turns # -> 109

# Altenrative and the easier -> Just understand what the Turning Point Test is doing -> 
# What does a turn mean? if e(t-1) < e(t) and e(t)> e(t+1) -> this is a turn -> switching from + to -
# Similarly e(t-1) > e(t) and e(t) < e(t+1) -> this is a turn -> switching from - to +

signs = sign(diff(e))
turns = sum(abs(diff(signs)) == 2)
turns # -> 109

# Why different to 110 in the notes? Because you differnced the Time Series Yt to get a stationary Time Series

TS = (turns - E) / sqrt(Var) # ~ N(0,1)
p.value = pnorm(TS, lower=FALSE)
p.value # -> 0.957057
# Null is that the residuals are independent -> here we fail to reject the null -> conclude the residuals are indepdent under the null Hypothesis

ts.plot(Yt, ylab = "Value", main = "TS plot of Yt", col = "blue")
# Comments already made

#########################################################################################

# Time Series Forecasting

# Focus on how to forcast time series data using:
# -> step-ahead method
# -> basic exponential smoothing

# Box Jenkins Method ->
# k-step ahead forecast
# easy to obtain using predict() function

# Example 1 Stationary Data

set.seed(476)
abc = sort(rbinom(3, 1, 0.6), decreasing = TRUE)*c(0.86, -0.4, 0.15)
data = 50 + arima.sim(model = list(ar = abc), n = 1000)

# Find an AR model to fit -> think! use ar() function
ar.m = ar(data)
ar.m
# -> R found AR(2) model  that fits the data:
# a1 = 0.8894
# a2 = -0.4214
# sig^2 = 0.9886 -> variance of independent WN terms
# mu -> 50.07051 (model has no intercept term but we can get mean from ar.m$x.mean)
ar.m$x.mean # -> 50.07051
 
# To forecast the next 20 values of the TS -> n.ahead = 20

predict(ar.m, n.ahead = 20) # this gives us the estimated forecasts and standard error from 2o times period -> 1001 : 1020
pred = predict(ar.m, n.ahead = 20)$pred # -> give us the predicted values

# As you will notice -> the forecasts are converging toward the mean mu -> look at ar.m$pred
# The standard error os also converging to the unconitional se of the MODEL -> sqrt(gamma_0)
# To see this -> calculate gamma_0 using the fitted model and Yule-Walker equations 

# X(t) = 50.07051 + 0.8894*(X(t-1) - 50.07051) - 0.4241*(X(t-2) - 50.07051) + err(t)
# Write out Yule-Walker equations and solve for gamma_0
# -> sqrt(gamma_0) = sqrt(1.1976) = 1.406 (can see the se in ar.m is converging toward this number)

a1 = ar.m$ar[1]
a2 = ar.m$ar[2]
sig.sq = ar.m$var.pred

g1 = a1 / (1 - a2)
g2 = a1 * g1 + a2
g0 = sig.sq / (1 - a1*g1 - a2*g2)

g0 # -> 1.975682
sqrt(g0) # -> 1.405589

# Estimated value of TS at time = 1010
predict(ar.m, n.ahead = 20)$pred[10] # -> 50.05137

# Plot forecast values ->
# Need to ensure the plotting area is wide enough to include future time periods -> use xlim()

# Plot the step ahead forcasts calculated ->
ts.plot(data, col = "blue", xlim = c(900,1020), main = "Forecasting example", ylab = "Data")
lines(pred, col = "red")
# Notice that the predicted values quickly converge to a stationary (flat line) equal to the estimated mean of the times series -> mu = 50.07051


# Example 2 Non-Stationary Data

# If a TS is not stationary, we must first remove the causes of non-stationarity such as trend, seasonality, etc.
# We fit a TS model to a stationary series -> otherwise models won't work
# In order to estimate future values for a non-stationary TS, we must forecast the stationary time series -> then add back the trend, seasonality, etc

# Consider ARIMA(0,1,2) -> MA(2) Stationary
set.seed(476)
a = runif(1, 0.6, 0.8); b = runif(1, -0.3, 1)

data = 50 + cumsum(arima.sim(list(ma = c(a, b)), n = 1460))
data = ts(data,start = c(2015, 1), frequency = 365)
head(data)
# plotting the TS data -> can clearly see trend

data.d = diff(data, 1, 1) # Difference data

par(mfrow = c(3, 1))
ts.plot(data.d)
acf(data.d)
pacf(data.d)
par(mfrow = c(1, 1))

# Now fit ARMA(0,0,2) -> MA(2) on data.d
m.data.d = arima(data.d, order = c(0, 0, 2))
m.data.d
# b1 = 0.7054
# b2 = 0.5669 
# intercept = 0.0471

# Therefore ->
# (1- B)data = 0.0471 + e(t) 0.7054*e(t-1) + 0.5669*e(t-2)
# -> see the differnce operator being applied above

# Forecast the value for next 180 days
pred = predict(m.data.d, n.ahead = 180)$pred
# quickly converging to unconditional mean -> 0.0471
# But this is the forecast on the differenced data -> need to undifference it

# To undifference:
# -> Use cumsum()
# -> Then add the result onto the last value of TS data

pred.with.trend = cumsum(pred) + tail(data, 1) # I understand what is going on -> cumsum() is summing up the differnces as you go along -> add it back to the last point in data -> UNDIFFERENCING
# Because we undifferenced -> R has lost the timestamp for each Forecast
# Just make pred.with.trend a TS again!

pred.with.trend =ts(pred.with.trend, start = c(2019, 1), frequency = 365) # -> just give it a start date and ensure frequency is correct - confirm with initial TS data
# Start date will conincide with date of first predition -> 180 days starting Jan 2019

# As before, we need a wide enough area on time to plot -> adjust xlim() to cover the length

ts.plot(data, xlim = c(2015, 2019 + 179 / 365), main = "Forecasting example 2", ylab = "Data")
lines(pred.with.trend, col ="red")
# as expected -> the trend features and constant 

# Exponetial Smoothing
# Forecast values of TS using Exponential Smoothing

# x^[n](1) = a*x[n] (1 - a)*x^[n-1](1)
# -> a is the smoothing paramater
# -> x^[n-1](1) is the estimate as at time n-1 of the value at time n i.e. estimate x[n] using x[n-1]
# -> x[n] is the oberved value at time n 
# So essentially you forcaset x[n](1) and use x[n] oberved -> blend with a and (1-a)

# R uses HoltWinters():
# -> alpha smoothign paramter
# -> beta smoothing parameter for Trend
# -> gamma smoothing parameter for Seasonality

# Since there is no trend or seasonality in the TS stationary data -> set beta and gamma to FALSE

# Example 1

series = read.csv("forecasting.csv", header = FALSE)
head(series)

# make series into TS
series = ts(series, start = c(2017, 20), frequency = 365) # -> daily TS

# end date of series ->
end(series)
# -> year: 2018, day: 154 -> 3rd June 2018

# Say we want to estimate the series on 4th June 2018 -> next day after last observation
# Using a = 0.7 ->
HW = HoltWinters(series, alpha = 0.7, beta = FALSE, gamma = FALSE) # -> this is an object that we will use to make the predictions with

# To make predition on 4th June 2018 ->
predict(HW, n.ahead = 1) # -> 314.6618
# or predict(HW) # default for n.ahead = 1

# We can also include a prediction interval for the prediction ->
predict(HW, level = 0.95, prediction.interval = TRUE) # 95 Confidence interval

# Whats interesting -> since there is no trend or seasonality -> predicting to longer dates will still be the same 1-step prediction
predict(HW, n.ahead = 8)
# See its the same as the n.ahead = 1 prediction -> makes sense if you look the formula -> it can only change if get new information ie. update on latest observation
# But it will only give a 1-step ahead forcast whcih will be the same unless we get updates on latest observations


# Example 2

# We've specified the smoothing paramter alpha = 0.7
# If we omit it, R will smooth based on an optimal parameter

# Question: Calculate 95% prediction interval for the value on 4th June 2018 using optimal smoothing paramter ->
# If we don't speficy alpha in the predict() function, R will use an optimal paramater for alpha
# We need to create a new HW without specifying alpha ->

HW2 = HoltWinters(series, beta = FALSE, gamma = FALSE)
predict(HW2, level = 0.95, prediction.interval = TRUE)
# The prediction interval here: (255.68, 341.51) is narrower that the prediction interval derived using alpha = 0.7

# To find the alpha value R used -> 
HW2$alpha # -> 0.04646942


#########################################################################################
# Question 12

set.seed(1558)

data = 100 + arima.sim(list(ar = runif(1, -0.3, 0.6), ma = runif(1, 12, 15)), n = 40)
data = ts(data, start = c(2008, 1), frequency = 4) # Quarterly over 10y period -> 40 periods

# Fit ARMA(1,1)
rev.m = arima(data, order = c(1, 0, 1))

# Box Jenkins Forecasts
pred = predict(rev.m, n.ahead = 2 * 4)$pred
pred

# Exponential Smoothing Forecasts
HW = HoltWinters(data, beta = FALSE, gamma = FALSE)
pred.xs = predict(HW, n.ahead = 2 * 4)
pred.xs

ts.plot(data, xlim = c(2008, 2018 + 8*0.25), main = "Plot of TS data & Box-Jenkins and Expo Smoothing Forecats", ylab = "data")
lines(pred, col = "red")
lines(pred.xs, col = "blue")
legend("bottomright", legend = c("TS data", "Box-Jenkins Forecast", "Expo Smoothing Forecast"), col = c("black", "red", "blue"), lwd = 2)


#########################################################################################
# Question 13

set.seed(1952)
x = arima.sim(list(ar = 0.7), n = 240)
x = 1800 + cumsum(x)
x = ts(x, start = c(1990, 1), frequency = 12) # -> monthly frequency: January 1990 until December 2009

# -> can see this has trend already -> got to difference before fitting model

x.d = diff(x, 1, 1)

# fit AR model ->
ar.x.d = ar(x.d)
# R fit an AR(3) model -> strange given the PACF cutsoff after lag 1

pred = predict(ar.x.d, n.ahead = 5 * 12)$pred
pred[length(pred)] # -> 0.1040751 ... which is useless because its on the differenced data
# Need to undifference ->

pred.un = tail(x, 1) + cumsum(pred)
pred.un[length(pred.un)] # -> 1832.144

# Need wide enough length on time to plot TS x and pred.un
# Before we can do that -> no timestamp on pred.un -> make this a TS

pred.un = ts(pred.un, start = c(2010, 1), frequency = 12)

ts.plot(x, xlim = c(1990, 2010 + 60 / 12), main = "TS plot of Diagnoses from 1990-2009 and Box-Jenskins trended forecasts from 2010-2014", ylab = "data", ylim = c(1750, 1900))
lines(pred.un, col = "red")
legend("bottomright", legend = c("TS plot of Diagnoses", "Box-Jenkins Forecast"), col = c("black", "Red"), lwd = 2)


#########################################################################################
# Question 14

set.seed(1952)
y = 90 + round(arima.sim(list(ma = c(15, 7)), n = 40))
y = ts(y, start = c(2007, 3), frequency = 4) # Quarterly 

# Data start after two quarters in 2007
# Data stops after two quarters in 2017

# Given the TS y is stationary -> we can fit MA(2)

y.ma = arima(y, order = c(0, 0, 2)) # -> fitting MA(2)

pred.y = predict(y.ma, n.ahead = 2 + 21*4)$pred
pred.y[length(pred.y)] # -> 94.09817
# As expected -> Forcasts will tend to unconditional mean -> mu = 94.09817
y.ma$coef["intercept"] # -> 94.09817 

#########################################################################################

# Time Series Multivariate Time Series

# Determine whether a MV TS is stationary -> Eigenvalues: det(I - lam*A) = 0
# Determine whether two TS are conintegrated

# X(t) = A*X(t-1) + E(t)
# -> Vector of x(t)  = Matrix A %*& Vector of x(t-1) + Vector of err(t)

# Example 1

# x(t) = 0.2*x(t-1) + 0.4*y(t-1) + err(t)
# y(t) = 0.1*x(t-1) + 0.3*y(t-1) + err'(t)

A = matrix(c(0.2, 0.4, 0.1, 0.3), nrow = 2, ncol = 2, byrow=TRUE)

# now calculate the eigenvalues of A ->
eig = eigen(A)
eig$values # -> 0.45615528 0.04384472 -> abs(eigenvalues) < 1 
# -> X(t) is Stationary because eignevalues are stricly less than 1 ... ALL eigenvalues < 1 for stationarity

# Example 2

A = matrix(c(-0.6, 0.5, 0.8, -0.1), nrow = 2, ncol = 2, byrow=TRUE)
eig = eigen(A)
eig$values # -> -1.0300735  0.3300735 -> MV TS not stationary -> not all absolute eigenvalues are stricly less than 1

# Example 3

A = matrix(c(0.7, 0.9, 0.4, 1 ,0.2, 0.2, 3, 0.2, 0.7), nrow = 3, ncol =3, byrow=TRUE)
eig = eigen(A)
eig$values # -> 2.1884342 -0.7050872  0.1166531 -> MV TS process not stationary -> not all abs eigenvalues are strictly less than 1

# Cointegrated Time Series
# Two TS X and Y are called conintegrated if the following two conditions hold:
# 1. X and Y and I(1) stationary ie. if you difference X and difference Y -> both differnced processess are stationary but X and Y ARE NOT
# 2. There exists a non-zero co-integratingvector [a, b] wuch that a*X + b*Y is Stationary

# To check if X and Y are I(1) stationary ->
# First check if X and Y are stationary using PP.test()
# We will difference X and Y and confirm stationarity using PP.test()

# Second condition of a vector [a, b] -> attempt to find this using PP.test() again

xy = read.table("cointegration.txt", sep = " ", header = TRUE)
head(xy)

x = xy[, 1]
y = xy[, 2]

# Check first condition -> is x and y I(1)?
PP.test(x)$p.value # -> 0.4925217 fail to reject null -> there is a unit root and thereore not stationary
PP.test(y)$p.value # -> 0.6911769 fail to reject null -> there is a unit root and thereore not stationary

# Now differnce x and y and check stationary using PP.test()
PP.test(diff(x))$p.value # -> 0.01 reject null -> there is no unit root therefore stationary
PP.test(diff(y))$p.value # -> 0.01 reject null -> there is no unit root therefore stationary

# x and y are I(1) Stationary

# Checking the second condition
# Need to find a vector [a, b] such that process a*x + b*y is stationary

find.coint = function(coint) {
	comb  = coint[1]*x + coint[2]*y
	test = PP.test(comb)$p.value
	test
	}

# We will minimize the p.value of the PP.test() to reject the null hypothesis of the PP.test() -> found the cointegrating vector [a, b]

v = c(1, 1) # -> starting vector as input
# like params0

fit = nlm(find.coint, v)
fit # -> the nlm found estiamtes of [a, b] that give the lowest p.value in PP test()

a = fit$estimate[1]
b = fit$estimate[2]

# -> conintegrating vector is
c(a,b)

# Checking the stationary of the combindation a*x + b*y ->
comb = a*x + b*y
PP.test(comb)$p.value # -> 0.01 so we reject the null -> there is no unit root and the combindation process is stationary

is.cointegrated = function(x, y){
	
	xp = PP.test(x)$p.value >= 0.05 && PP.test(diff(x))$p.value < 0.05 # Logical for later
	yp = PP.test(y)$p.value >= 0.05 && PP.test(diff(y))$p.value < 0.05 # Logical for later
	
	find.coint = function(coint){
		comb = coint[1]*x + coint[2]*y
		test = PP.test(comb)$p.value
		test
		}
		
	v = c(1, 1) # like params0 -> solve for a vector
	
	fit = nlm(find.coint, v) # does it converge? check
	
	a = fit$estimate[1]
	b = fit$estimate[2]
	
	v.fit = c(a, b)
	v.fit.check = round(a, 6) == 0 && round(b, 6) == 0
	
	comb = a*x + b*y
	
	combp = PP.test(comb)$p.value < 0.05
		
		if (xp == TRUE && yp == TRUE && combp == TRUE && v.fit.check == FALSE){
				print("the vectors are cointegrated with cointegrating vector");
				print(v.fit)
				} 
		else {
				print("x and y are not cointegrated")
			}
	}
	
set.seed(1234)
z = rep(0, 1000)
for (i in 2:1000) z[i] = z[i-1] + rnorm(1)
w = v = rep(0, 1000)
w = runif(1,5,7)*z + rnorm(1000)
v = -runif(1,2,4)*z + rnorm(1000)

is.cointegrated(w,v)

# There can be many conintegrating vectors -> depends on where you start -> Remember this

#########################################################################################
# Question 15

A = matrix(c(0.3, 0.1, -0.2, 0.6), nrow = 2, ncol = 2, byrow=TRUE)
eig = eigen(A)$values # -> 0.5 and 0.4 -> (X,Y)' is a stationary process -> abs eigenvalues are strictly less than 1

#########################################################################################
# Question 16

set.seed(1234)
z = rep(0, 1000)

for (i in 2:1000) {
	z[i] = z[i-1] + rnorm(1)
	}
	
s = runif(1, 16, 20)*z + rnorm(1000)
t = -runif(1, 1, 10)*z + rnorm(1000)

v = c(0.2, 1.5)
comb = v[1]*s + v[2]*t
PP.test(comb)$p.value # -> 0.6894946 fail to rejct null that there is a unit root and the process is not stationary -> v = [0.2, 1.5] is not a conintegrating vector

is.cointegrated(s,t)
# -> s and t are conintegrated I(1) processes with cointegrating vector (0.4261247 , 1.3611548)
