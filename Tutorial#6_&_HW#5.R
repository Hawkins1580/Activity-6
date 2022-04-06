# Start of Tutorial #6

# read in greenhouse gas data from reservoirs
ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")
ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")

# Installing Packages
# install.packages(c("dplyr", "ggplot2", "olsrr", "PerformanceAnalytics"))
# install.packages(c("lubridate", "forecast"))

library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)

unique(ETdat$crop)

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)

acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)

pacf.plot <- pacf(na.omit(almond_ts))


almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1


model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4


# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")


newAlmond <- forecast(model4)
newAlmond


#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))


# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")







# Start Homework #5

# Question #1
# Use the transformation and design a regression analysis to present to water managers about the impact of reservoir characteristics on carbon dioxide fluxes. 
# In designing your regression, you should consider the environmental conditions that impact carbon dioxide fluxes, the availability of data, and the assumptions of ordinary least squares regression

# log transformations 
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)
ghg$log.SA <- log(ghg$surface.area)
ghg$log.MeanDepth <- log(ghg$mean.depth)
ghg$log.airTemp <- log(ghg$airTemp)



unique(ghg$Region)

# binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)

# binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)

# binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)

# binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)

# Reservoir Characteristics that Affects CO2 Flux
# depth of the reservoir, age of the reservoir, chlorophyll A measurements, 
# surface area, volume, precipitation, runoff

ghg$NEW.co2 <- ((1)/(ghg$co2+1000))

ggplot(ghg, aes(x=co2)) + 
  geom_histogram(binwidth=100)

ggplot(ghg, aes(x=NEW.co2)) + 
  geom_histogram(binwidth=.0001)

# Visualizing data to see if I need to make transformations 
ggplot(ghg, aes(x=mean.depth, y=NEW.co2)) + geom_point()
ggplot(ghg, aes(x=log.MeanDepth, y=NEW.co2)) + geom_point()

ggplot(ghg, aes(x=surface.area, y=NEW.co2)) + geom_point()
ggplot(ghg, aes(x=log.SA, y=NEW.co2)) + geom_point()

ggplot(ghg, aes(x=age, y=NEW.co2)) + geom_point()
ggplot(ghg, aes(x=log.age, y=NEW.co2)) + geom_point()

ggplot(ghg, aes(x=DIP, y=NEW.co2)) + geom_point()
ggplot(ghg, aes(x=log.DIP, y=NEW.co2)) + geom_point()

ggplot(ghg, aes(x=precipitation, y=NEW.co2)) + geom_point()
ggplot(ghg, aes(x=log.precip, y=NEW.co2)) + geom_point()

ggplot(ghg, aes(x=airTemp, y=NEW.co2)) + geom_point()
ggplot(ghg, aes(x=log.airTemp, y=NEW.co2)) + geom_point()


mod.QUESTION1 <- lm(NEW.co2 ~ mean.depth+
                      log.SA+
                      log.age+
                      log.DIP+
                      log.precip+ 
                      airTemp, data = ghg)
summary(mod.QUESTION1)

# Isolating residuals and fitted values
res.full <- rstandard(mod.QUESTION1)
fit.full <- fitted.values(mod.QUESTION1)

# Checking for normality of residuals 
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)

# Checking assumptions 2 through 4 with residual plot
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)

# shapiro-wilks test
shapiro.test(res.full)

# Checking for Multicollinearity of explanatory variables
reg.data <- data.frame(ghg$mean.depth,
                       ghg$log.SA,
                       ghg$log.age,
                       ghg$log.DIP,
                       ghg$log.precip,
                       ghg$airTemp)
# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# run stepwise
full.step <- ols_step_forward_aic(mod.QUESTION1)
# view table
full.step

# check full model
full.step$model

# plot AIC over time
plot(full.step)

# exporting regression
regTable <- summary(mod.QUESTION1)$coefficients
# write to file then click more>export
write.csv(regTable, "/cloud/project/reg_out.csv")






# Question #2
# Decomposing Time Series for different crops

# Almonds

# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)





# Pistachios 

# average fields for each month for pistachios
pistachios <- ETdat %>% # ET data
  filter(crop == "Pistachios") %>% # only use pisachios fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(pistachios, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# pistachio ET time series
pistachios_ts <- ts(pistachios$ET.in, # data
                    start = c(2016,1), #start year 2016, month 1
                    #first number is unit of time and second is observations within a unit
                    frequency= 12) # frequency of observations in a unit

# decompose pistachios ET time series
pistachios_dec <- decompose(pistachios_ts)
# plot decomposition
plot(pistachios_dec)



# Fallow / Idle

# average fields for each month for Fallow / Idle
fallow <- ETdat %>% # ET data
  filter(crop == "Fallow/Idle Cropland") %>% # only use Fallow/Idle Cropland fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(fallow, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# fallow / idle ET time series
fallow_ts <- ts(fallow$ET.in, # data
                    start = c(2016,1), #start year 2016, month 1
                    #first number is unit of time and second is observations within a unit
                    frequency= 12) # frequency of observations in a unit

# decompose fallow / idle ET time series
fallow_dec <- decompose(fallow_ts)
# plot decomposition
plot(fallow_dec)



# Corn

# average fields for each month for corn
corn <- ETdat %>% # ET data
  filter(crop == "Corn") %>% # only use Corn fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(corn, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# corn ET time series
corn_ts <- ts(corn$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit

# decompose corn ET time series
corn_dec <- decompose(corn_ts)
# plot decomposition
plot(corn_dec)



# Grapes

# average fields for each month for Grapes
grapes <- ETdat %>% # ET data
  filter(crop == "Grapes (Table/Raisin)") %>% # only use grape fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(grapes, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# grapes ET time series
grapes_ts <- ts(grapes$ET.in, # data
              start = c(2016,1), #start year 2016, month 1
              #first number is unit of time and second is observations within a unit
              frequency= 12) # frequency of observations in a unit

# decompose grapes ET time series
grapes_dec <- decompose(grapes_ts)
# plot decomposition
plot(grapes_dec)




# Question #3
# Design an autoregressive model for pistachios

PISTACHIOS.pacf.plot <- pacf(na.omit(pistachios_ts))

# Running a first order AR
pistachios_y <- na.omit(pistachios_ts)
pistachios.model1 <- arima(pistachios_y ,  
                order = c(1,0,0)) 
pistachios.model1

# Running a second order AR
pistachios.model2 <- arima(pistachios_y ,  
                order = c(2,0,0)) 
pistachios.model2

# Running a third order AR
pistachios.model3 <- arima(pistachios_y ,  
                order = c(3,0,0)) 
pistachios.model3

# Running a fourth order AR
pistachios.model4 <- arima(pistachios_y ,  
                order = c(4,0,0)) 
pistachios.model4

# Looking at fitted values
# calculate fit
p.AR_fit1 <- pistachios_y - residuals(pistachios.model1) 
p.AR_fit2 <- pistachios_y - residuals(pistachios.model2)
p.AR_fit3 <- pistachios_y - residuals(pistachios.model3)
p.AR_fit4 <- pistachios_y - residuals(pistachios.model4)
#plot data
plot(pistachios_y)
# plot fit
points(p.AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(p.AR_fit2, type = "l", col = "green3", lty = 2, lwd=2)
points(p.AR_fit3, type = "l", col = "orchid2", lty = 2, lwd=2)
points(p.AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data", "AR1", "AR2", "AR3", "AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3", "green3", "orchid2", "darkgoldenrod4"),
       bty="n")

# Forecast using AR3
newPistachios <- forecast(pistachios.model3)
newPistachios

#make dataframe for plotting
newPistachiosF <- data.frame(newPistachios)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistachiosF$dateF <- ymd(paste(years,"/",month,"/",1))

# making a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = pistachios, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(pistachios$date[1]),newPistachiosF$dateF[24])+  # Plotting original data
  geom_line(data = newPistachiosF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newPistachiosF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="Year", y="Evapotranspiration (in)")



# and fallow/idle fields

# Design an autoregressive model for fallow / idle

FALLOW.pacf.plot <- pacf(na.omit(fallow_ts))

# Running a first order AR
fallow_y <- na.omit(fallow_ts)
fallow.model1 <- arima(fallow_y ,  
                           order = c(1,0,0)) 
fallow.model1

# Running a second order AR
fallow.model2 <- arima(fallow_y ,  
                           order = c(2,0,0)) 
fallow.model2

# Running a third order AR
fallow.model3 <- arima(fallow_y ,  
                           order = c(3,0,0)) 
fallow.model3

# Running a fourth order AR
fallow.model4 <- arima(fallow_y ,  
                           order = c(4,0,0)) 
fallow.model4

# Looking at fitted values
# calculate fit
f.AR_fit1 <- fallow_y - residuals(fallow.model1) 
f.AR_fit2 <- fallow_y - residuals(fallow.model2)
f.AR_fit3 <- fallow_y - residuals(fallow.model3)
f.AR_fit4 <- fallow_y - residuals(fallow.model4)
#plot data
plot(fallow_y)
# plot fit
points(f.AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(f.AR_fit2, type = "l", col = "green3", lty = 2, lwd=2)
points(f.AR_fit3, type = "l", col = "orchid2", lty = 2, lwd=2)
points(f.AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data", "AR1", "AR2", "AR3", "AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3", "green3", "orchid2", "darkgoldenrod4"),
       bty="n")

# Forecast using AR2
newFallow <- forecast(fallow.model2)
newFallow

#make dataframe for plotting
newFallowF <- data.frame(newFallow)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallowF$dateF <- ymd(paste(years,"/",month,"/",1))


# making a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(fallow$date[1]),newFallowF$dateF[24])+  # Plotting original data
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newFallowF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="Year", y="Evapotranspiration (in)")



