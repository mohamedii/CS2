# setwd("C:\\Users\\User\\OneDrive\\Desktop\\IFoA PBOR\\CS2\\CS2 Paper B\\PBOR\\***")

# Calculating exposed to risk using exact method or census method
# estimating transition intensities

FuneralData=read.csv("FuneralData.csv")
head(FuneralData)

FuneralData$BIRTH=as.Date(FuneralData$BIRTH, format="%d/%m/%Y")
FuneralData$ENTRY=as.Date(FuneralData$ENTRY, format="%d/%m/%Y")
FuneralData$DEATH=as.Date(FuneralData$DEATH, format="%d/%m/%Y")

str(FuneralData)

nrow(FuneralData)

# Start date of the investigation period
inv_start=as.Date("01/01/2013", format="%d/%m/%Y")

# End date of the investigation period
inv_end=as.Date("31/12/2017", format="%d/%m/%Y")

n.deaths=sum(FuneralData$DEATH >= inv_start & FuneralData$DEATH <= inv_end)
n.deaths

# Polociholders alive during the investigation
# -> Either bought policy on or prior to the investigation period
# -> die on or after the investigation start date

Alive=FuneralData[FuneralData$ENTRY <=inv_end & FuneralData$DEATH >=inv_start, ] # -> the ,] end is important here...
nrow(Alive)

# Code given to add 70th birthdays
interim = as.POSIXlt(Alive$BIRTH)
interim$year = interim$year + 70
Alive$B70 = as.Date(interim)

head(Alive)

# Update code for B71 -> 71st birthdays
interim = as.POSIXlt(Alive$BIRTH)
interim$year = interim$year + 71
Alive$B71 = as.Date(interim)

head(Alive)

# Policyholders contributing to the exposed to risk at age 70 last birthday (x,x+1)
# are those within the Alive data set that:
# bought policy before 71st birthday
# died after 70th birthday (right censored if survived the investigation)
# turned 70 on or before investigation end Date
# turned 71 after the investigation start Date

Alive70=Alive[Alive$DEATH > Alive$B70 & Alive$B70 <= inv_end & Alive$B71 >=inv_start & Alive$ENTRY < Alive$B71, ]

# The start date for the exposed to risk at age 70 last birthday is the latest of:
# -> the life's 70th birthday
# -> the date this life's  policy started i.e. the entry Date
# -> the start date of the investigation

# The end date for the exposed to risk at 70 last birthday is the earliest of:
# -> the life's 71st birthday
# -> the date the life exits the investigation either through death or censored (withdrawal)
# -> the end date of the investigation

Alive70$ETR_START=numeric(nrow(Alive70))
Alive70$ETR_START=pmax(Alive70$B70, Alive70$ENTRY, inv_start)

Alive70$ETR_END=numeric(nrow(Alive70))
Alive70$ETR_END=pmin(Alive70$B71, Alive70$DEATH, inv_end)

head(Alive70)

Alive70$ETR=Alive70$ETR_END-Alive70$ETR_START
cE70.exact=as.numeric(sum(Alive70$ETR)/365.25)

# Therefore the exposed to risk using the exact method is 70.39836 years

# Number of lives who dies age 70 last birthday during the invetigation period
d70=nrow(Alive70[Alive70$DEATH <= inv_end & Alive70$DEATH < Alive70$B71, ])

# Therefore the estimate of the force of mortality mu(x+0.5) for aged x last birthday -> exact age (x,x+1)
# mu_70.5

mu.exact=d70/cE70.exact
mu.exact

# Function for Census contribution

my.census=function(CD, DoE, DoD, DB70, DB71){
	if ((DoE<CD) & (DoD>CD) & (DB70<=CD) & (DB71>CD)) {
		1 }
	else {
		0 }
	}

census=function(CD, DoE, DoD, DB70, DB71){
	ifelse(DoE<CD & DoD>CD & DB70<=CD & DB71>CD, 1,0)
	}


# Number of lives aged 70 last birthday at the start of the investigation period
# Census Date -> CD = inv_start

CD=inv_start
DoE=Alive70$ENTRY
DoD=Alive70$DEATH
DB70=Alive70$B70
DB71=Alive70$B71


census(CD, DoE, DoD, DB70, DB71) # -> This tells us which policyholders contribute to the census data at this date

# We need to sum this vector to count the 1's

P70.start=sum(census(CD, DoE, DoD, DB70, DB71))

# Alternatively, we could've used the original Alive data set but obviosuly need the B70, and B71 columns
# ->
CD=inv_start
DoE=Alive$ENTRY
DoD=Alive$DEATH
DB70=Alive$B70
DB71=Alive$B71
P70.start=sum(census(CD, DoE, DoD, DB70, DB71))
# <-

# Number of lives aged 70 last birthday at the 1 Jan 2014, 2015, 2016, 2017, 2018
# Census Date -> CD = 1 Jan 2014, 2015, 2016, 2017, 2018

P70.2014=sum(census(CD=as.Date("01/01/2014", format="%d/%m/%Y"), DoE, DoD, DB70, DB71))
P70.2015=sum(census(CD=as.Date("01/01/2015", format="%d/%m/%Y"), DoE, DoD, DB70, DB71))
P70.2016=sum(census(CD=as.Date("01/01/2016", format="%d/%m/%Y"), DoE, DoD, DB70, DB71))
P70.2017=sum(census(CD=as.Date("01/01/2017", format="%d/%m/%Y"), DoE, DoD, DB70, DB71))
P70.2018=sum(census(CD=as.Date("01/01/2018", format="%d/%m/%Y"), DoE, DoD, DB70, DB71))

inforce=c(P70.start, P70.2014, P70.2015, P70.2016, P70.2017, P70.2018)

t=c(inv_start, as.Date("01/01/2014", format="%d/%m/%Y"), as.Date("01/01/2015", format="%d/%m/%Y"), as.Date("01/01/2016", format="%d/%m/%Y"),
	 as.Date("01/01/2017", format="%d/%m/%Y"), as.Date("01/01/2018", format="%d/%m/%Y"))
t=as.numeric(t)

add=function(v){
	v[-length(v)] + v[-1]
	}

cE70.census=0.5*sum(add(inforce)*diff(t))/365.25 # -> using exact time intervals, and function 'add' to add pairwise elements in the inforce vector
cE70.census

# Now require d70, the number of deaths for lives age 70 last birthday -> number of lives who die before investigation ends, and dies before age 71 next birthday
d70=nrow(Alive70[Alive70$DEATH < inv_end & Alive70$DEATH < Alive70$B71, ])

# mu for age 70 last birthday will estimate mu(70.5)

mu.census=d70/cE70.census

# Comapring differnt force of mortality mu(70.5) calculations for exact versus census ->
mu=data.frame(exact=mu.exact, census=mu.census)
mu

# The census method assumes that the number of in-force policies varies over each calendar year. Even though there were some big changes in these numbers
# from year to year in the example, the census estimate of the ETR, cE70.census, was quite close to the exact value, cE70.exact = 70.39863
# The two estimates of the force of mortalitywere quite similar.