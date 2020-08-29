library(rcarbon)
library(tidyverse)

# Import all dates
all_dates <- read_csv("./data/raw_data/all_dates.csv")

# Filter by defined criteria and dates acceptance in the literature
filtered_dates <- all_dates %>%
  filter(Accepted == "Yes",
         !Method %in% c("OSL", "TL"))

# Calibrate filtered dates
filtered_caldates <- calibrate(x = filtered_dates$Age,errors = filtered_dates$`Standard Deviation`, calCurves='intcal13', ncores = 3)

# Plot SPDs within the LGM timeframe
#all_spd <- spd(filtered_caldates,timeRange=c(26500,19000))
#plot(all_spd)
# Plot rolling average of 200 years for smoothing
#plot(all_spd,runm=200,add=TRUE,type="simple",col="indianred",lwd=2,lty=2)


# Binning
all_dates_bins <- binPrep(sites = filtered_dates$Site, ages = filtered_caldates,h=200)

all_dates_bins_spd <- spd(filtered_caldates,bins = all_dates_bins,timeRange=c(26500,19000))

plot(all_dates_bins_spd)

# Plot sensibility analysis of binning process (from 0 to 500 in steps of 50)
#binsense(x=filtered_caldates,y=filtered_dates$Site,h=seq(0,500,50),
         timeRange=c(26500,19000))


# Compare empirical SPDs

perm_all_dates <- permTest(x = filtered_caldates, marks = filtered_dates$Region,
                           timeRange=c(26500,19000),
                           bins = all_dates_bins, nsim = 500, runm = 200)

summary(perm_all_dates)


# Plot
par(mfrow=c(2,2))
plot(perm_all_dates,focalm = 4,main="Northern")
plot(perm_all_dates,focalm = 2,main="Inland")
plot(perm_all_dates,focalm = 1,main="Mediterranean")
plot(perm_all_dates,focalm = 3,main="Western Atlantic")



