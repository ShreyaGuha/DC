####Meteorological detrending for PM2.5 for Washington DC 2011-20####
###Goal: To check relations between different meteorological signals and atmospheric pollutants###
#*To check differences between satellite vs observed datasets for meteorological data

##using specific library functions##
install.packages("data.table") #installing package
library(data.table) #calling library function

##read data
#*Note on Data: Data collected from EPA
#*For years 2014-16, data is collected from McMillan
#*For other years, PM2.5 data is collected from River Terrace
#*Only Primary Sampler used, determined by POC (Parameter Occurence Code)
#*Erroneous values manipulated for the data set, meteorological data merged
#*Met data, case 2 = daily observational data from ncdc.noaa.gov
dc_obs = read.csv("DC_airport_obs.csv")

##create new column for date##
#installing specific library function for date
install.packages("lubridate")
library(lubridate)
dc_obs$date <- seq(ymd('2011-01-01'), ymd('2021-01-01'), by = "1 day")
#assigning numbers 1-7 to each day of the week
dc_obs$days <- wday(dc_obs$date)

#replace NAs with zero
dc_obs[is.na(dc_obs)] = 0


##GAM##
##General Additive Models##
library(mgcv) #calling library function


##Recreating Models from papers##
###GAMs recreated from papers###

##GAM_Beijing without optimized inputs##
##From Hua & Zhang et al (2021)##
#checking distributions of the parameters involved
hist(dc_obs$PM2.5, breaks = 50)
hist(dc_obs$PRCP, breaks = 100)

##PM2.5 and PRCPip have logarithmic distributions, linear regression models assume normal distributions
#Transforming into normal distribution
dc_obs$PM2.5 <- log10(dc_obs$PM2.5 + 1)
dc_obs$PRCP <- log10(dc_obs$PRCP + 1)
#checking summary and adding 1 inside log10 to avoid infinity in summary
summary(dc_obs$PM2.5)
summary(dc_obs$PM2.5)
#check distribution again
hist(dc_obs$PM2.5)
hist(dc_obs$PRCP, breaks = 50)

#distribution of PRCPipitation is still skewed
#changing it to categorical variable
dc_obs$PRCPBool [dc_obs$PRCP != 0.0000] <- 1
dc_obs$PRCPBool [dc_obs$PRCP == 0.0000] <- 0
dc_obs$PRCPBool <- as.factor(dc_obs$PRCPBool)
#check number of rainy days
summary(dc_obs$PRCPBool)

#introducing some time variables, because we are not detrending the data
dc_obs$year <- as.factor(format(dc_obs$date, "%y"))
dc_obs$month <- as.numeric(format(dc_obs$date, "%m"))
dc_obs$dayno <- seq(1,3654, by = 1) #dummy variable for days


##changing wind direction to 4 coordinates##
dc_obs$WDF2 [dc_obs$WDF2 <= 90.00] <- 1
dc_obs$WDF2 [dc_obs$WDF2 > 90.00 & dc_obs$WDF2 <= 180.00] <- 2
dc_obs$WDF2 [dc_obs$WDF2 > 180.00 & dc_obs$WDF2 <= 270.00] <- 3
dc_obs$WDF2 [dc_obs$WDF2 > 270.00 & dc_obs$WDF2 <= 360.00] <- 4
summary(dc_obs$WDF2)


#*****Important considerations*****
#Here, offset for log transform = 1, compared to 3 in study #
#For temporal categorical values, here days = 1, 2,.., 7 
#compared to weekdays = 1, weekends = 2, holidays = 3 to 5
#There is no hourly factor here
#Meteorological components with highest fitting performance for the PARTICULAR study included in the model
#All meteorological components = T2M, RH, Precipitation, Wind, BLH, SP, Dew Point Temperature
#s(pbl) not considered

gam_bj <- gam(dc_obs$PM2.5 ~ dc_obs$year + s(dc_obs$month) 
              + dc_obs$days + s(dc_obs$WSF2))

summary(gam_bj)

#plot model
plot(gam_bj, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)

#show residuals
plot(gam_bj, pages = 1, all.terms = TRUE,
     rug = TRUE, residuals = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)



##GAM_Wind_Precipitation##
## From Zhang, Jiao, Xu, Zhao, Tang, Zhou & Chen (2018) ##

#*****Important considerations*****
#No information about ?? in paper#
#Here, Jenks natural classification method not followed, only Boolean 1,0 followed for precipitation#
#cubic splines, non-linearly correlated used in the study; hence cubic regression splines used#


gam_wsprec <- gam(dc_obs$PM2.5 ~ s(dc_obs$WSF2, bs = "cr") + 
                  s(dc_obs$dayno, bs = "cr") +  dc_obs$PRCPBool + dc_obs$WDF2 , 
                  method = "REML")
summary(gam_wsprec)

#plot model
plot(gam_wsprec, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


#show residuals
plot(gam_wsprec, pages = 1, all.terms = TRUE,
     rug = TRUE, residuals = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


###Introducing Detrending by KZ filter###
###Using KZ filter###
install.packages("kza")
library("kza")

###Long-term###
dc_obs$kz_PM_annual <- kz(dc_obs$PM2.5, m = 365, k = 3)
dc_obs$kz_PRCP_annual <- kz(dc_obs$PRCP, m = 365, k = 3)
dc_obs$kz_TAVG_annual <- kz(dc_obs$TAVG, m = 365, k = 3)
dc_obs$kz_WSF2_annual <- kz(dc_obs$WSF2, m = 365, k = 3)
dc_obs$kz_rh_annual <- kz(dc_obs$rh, m = 365, k = 3)
dc_obs$kz_dew_annual <- kz(dc_obs$dewpttemp, m = 365, k = 3)

###Seasonal###
##Detrending calculations for seasonal##
##longterm, remaining##
dc_obs$PM_LT_rem <- dc_obs$PM2.5 - dc_obs$kz_PM_annual
dc_obs$PRCP_LT_rem <- dc_obs$PRCP - dc_obs$kz_PRCP_annual
dc_obs$TAVG_LT_rem <- dc_obs$TAVG - dc_obs$kz_TAVG_annual
dc_obs$WSF2_LT_rem <- dc_obs$WSF2 - dc_obs$kz_WSF2_annual
dc_obs$rh_LT_rem <- dc_obs$rh - dc_obs$kz_rh_annual
dc_obs$dew_LT_rem <- dc_obs$dewpttemp - dc_obs$kz_dew_annual

##Seasonal##
dc_obs$kz_PM_seasonal <- kz(dc_obs$PM_LT_rem, m = 15, k = 5)
dc_obs$kz_PRCP_seasonal <- kz(dc_obs$PRCP_LT_rem, m = 15, k = 5)
dc_obs$kz_TAVG_seasonal <- kz(dc_obs$TAVG_LT_rem, m = 15, k = 5)
dc_obs$kz_WSF2_seasonal <- kz(dc_obs$WSF2_LT_rem, m = 15, k = 5)
dc_obs$kz_rh_seasonal <- kz(dc_obs$rh_LT_rem, m = 15, k = 5)
dc_obs$kz_dew_seasonal <- kz(dc_obs$dew_LT_rem, m = 15, k = 5)

###Short-term###
##Detrending##
dc_obs$PM_STM <- dc_obs$PM_LT_rem - dc_obs$kz_PM_seasonal
dc_obs$PRCP_STM <- dc_obs$PRCP_LT_rem - dc_obs$kz_PRCP_seasonal
dc_obs$TAVG_STM <- dc_obs$TAVG_LT_rem - dc_obs$kz_TAVG_seasonal
dc_obs$WSF2_STM <- dc_obs$WSF2_LT_rem - dc_obs$kz_WSF2_annual
dc_obs$rh_STM <- dc_obs$rh_LT_rem - dc_obs$kz_rh_annual
dc_obs$dew_STM <- dc_obs$dew_LT_rem - dc_obs$kz_dew_annual

##Short-term_correlations##
dc_STM <- subset(dc_obs, select = c(PM_STM, PRCP_STM, TAVG_STM, 
                                    WSF2_STM, WDF2, rh_STM, dew_STM, days))
cor(dc_STM,  use = 'complete.obs')
plot(dc_STM)

##Linear model for PM2.5 for short-term data, detrended##
lm_PM2.5 <- lm(dc_obs$PM_STM ~ dc_obs$TAVG_STM + dc_obs$PRCP_STM + 
                       dc_obs$days)
summary(lm_PM2.5) 
plot(lm_PM2.5)

lm_PM2.5 <- lm(dc_obs$PM_STM ~ dc_obs$TAVG_STM + dc_obs$PRCP_STM + dc_obs$rh_STM
                 + dc_obs$WSF2_STM + dc_obs$dew_STM + dc_obs$WDF2 + dc_obs$days, 
                 use = 'complete.obs')
summary(lm_PM2.5) 
plot(lm_PM2.5)

#GAM
gam4 <- gam(dc_obs$PM_STM ~ s(dc_obs$PRCP_STM, dc_obs$WSF2_STM) + 
              dc_obs$TAVG_STM, method = "REML")
summary(gam4)

#plot model
plot(gam4, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 0.45 , cey = 0.3)



###Predicting Weather_Types###
##Weather type: smoke or haze##
summary(dc_obs$WT08)

#Using Linear Model
lm_haze_1 <- lm(dc_obs$WT08 ~ dc_obs$PM_STM + dc_obs$WSF2_STM + dc_obs$WDF2 +
                        dc_obs$PRCP_STM)
summary(lm_haze_1)


lm_haze_2 <- lm(dc_obs$WT08 ~ dc_obs$PM_STM + dc_obs$WSF2_STM + dc_obs$WDF2 +
                dc_obs$PRCP_STM + dc_obs$rh_STM + dc_obs$dew_STM)
summary(lm_haze_2)

#Using GAM
gam_haze_1 <- gam(dc_obs$WT08 ~ s(dc_obs$PM2.5, bs = "cc") + s(dc_obs$WSF2, bs = "cc") +
                                + dc_obs$WDF2 + dc_obs$PRCP_STM, method = "REML")
summary(gam_haze_1)

#plot model
plot(gam_haze_1, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


gam_haze_2 <- gam(dc_obs$WT08 ~ s(dc_obs$PM2.5, bs = "cc") + s(dc_obs$WSF2, bs = "cc") +
                              s(dc_obs$dewpttemp, bs = "cc") + dc_obs$WDF2 +
                              dc_obs$PRCP_STM + dc_obs$rh_STM, method = "REML")
summary(gam_haze_2)


gam_haze_3 <- gam(dc_obs$WT08 ~ s(dc_obs$PM2.5, bs = "cc") + s(dc_obs$WSF2, bs = "cc") +
                          s(dc_obs$dewpttemp, bs = "cc") + dc_obs$WDF2 +
                          dc_obs$PRCP_STM + dc_obs$rh_STM + s(dc_obs$month) +dc_obs$year, 
                          method = "REML")
summary(gam_haze_3)
plot(gam_haze_3)



##Weather type: Fog##
#Linear Models
lm_fog_1 <- lm(dc_obs$WT02 ~ dc_obs$PM_STM + dc_obs$WSF2_STM + dc_obs$WDF2 +
                dc_obs$PRCP_STM + dc_obs$TAVG_STM)
summary(lm_fog_1)

lm_fog_2 <- lm(dc_obs$WT02 ~ dc_obs$PM_STM + dc_obs$WSF2_STM + dc_obs$WDF2 +
                             dc_obs$PRCP_STM + dc_obs$TAVG_STM + dc_obs$rh_STM
                             + dc_obs$dew_STM)
summary(lm_fog_2)

#GAMs
gam_fog_1 <- gam(dc_obs$WT02 ~ dc_obs$PM_STM + s(dc_obs$WSF2, bs = "cc") + 
                 dc_obs$PRCP_STM + dc_obs$TAVG_STM, method = "REML")
summary(gam_fog_1)

gam_fog_2 <- gam(dc_obs$WT02 ~ s(dc_obs$PM_STM, bs = "cp") + s(dc_obs$WSF2, bs = "cc")
                 + s(dc_obs$PRCP_STM, bs = "cp") + dc_obs$TAVG_STM + dc_obs$WDF2 
                 + dc_obs$rh_STM + dc_obs$dew_STM, method = "REML")
summary(gam_fog_2)

##Predicting visibility##
#Linear Model
lm_vis <- lm(dc_obs$vis ~ dc_obs$PM_STM + dc_obs$WT02 + dc_obs$WT08 + dc_obs$PRCPBool 
             + dc_obs$WSF2_STM + dc_obs$rh_STM + dc_obs$TAVG_STM + dc_obs$days)
summary(lm_vis)



