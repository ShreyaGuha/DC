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
#*Met data, case 1 = 6-hourly satellite data, from NCEP CFSv2 
dc_1120 = read.csv("dc_11_20.csv")

#calculate wind speed
dc_1120$wind <- sqrt (dc_1120$u^2 + dc_1120$v^2)

##create new column for date##
#installing specific library function for date
install.packages("lubridate")
library(lubridate)
dc_1120$date <- seq(ymd('2011-01-01'), ymd('2020-12-31'), by = "1 day")
#assigning numbers 1-7 to each day of the week
dc_1120$days <- wday(dc_1120$date)


###Raw Data###
##checking basic correlation for whole time period, raw data##
dc_numeric <- subset(dc_1120, select = -c(ï..date, date, days, u, v)) #subseting to remove non-numeric column
cor(dc_numeric)
plot(dc_numeric)

###Introducing Detrending by KZ filter###
###Using KZ filter###
install.packages("kza")
library("kza")

###Long-term###
dc_1120$kz_PM_annual <- kz(dc_1120$PM2.5, m = 365, k = 3)
dc_1120$kz_prec_annual <- kz(dc_1120$prec, m = 365, k = 3)
dc_1120$kz_temp_annual <- kz(dc_1120$temp, m = 365, k = 3)
dc_1120$kz_rh_annual <- kz(dc_1120$rh, m = 365, k = 3)
dc_1120$kz_wind_annual <- kz(dc_1120$wind, m = 365, k =3)
dc_1120$kz_pbl_annual <- kz(dc_1120$pbl, m = 365, k =3)

###Seasonal###
##Detrending calculations for seasonal##
##longterm, remaining##
dc_1120$PM_LT_rem <- dc_1120$PM2.5 - dc_1120$kz_PM_annual
dc_1120$prec_LT_rem <- dc_1120$prec - dc_1120$kz_prec_annual
dc_1120$temp_LT_rem <- dc_1120$temp - dc_1120$kz_temp_annual
dc_1120$rh_LT_rem <- dc_1120$rh - dc_1120$kz_rh_annual
dc_1120$wind_LT_rem <- dc_1120$wind - dc_1120$kz_wind_annual
dc_1120$pbl_LT_rem <- dc_1120$pbl - dc_1120$kz_pbl_annual

##Seasonal##
dc_1120$kz_PM_seasonal <- kz(dc_1120$PM_LT_rem, m = 15, k = 5)
dc_1120$kz_prec_seasonal <- kz(dc_1120$prec_LT_rem, m = 15, k = 5)
dc_1120$kz_temp_seasonal <- kz(dc_1120$temp_LT_rem, m = 15, k = 5)
dc_1120$kz_rh_seasonal <- kz(dc_1120$rh_LT_rem, m = 15, k = 5)
dc_1120$kz_wind_seasonal <- kz(dc_1120$wind_LT_rem, m = 15, k = 5)
dc_1120$kz_pbl_seasonal <- kz(dc_1120$pbl_LT_rem, m = 15, k = 5)

###Short-term###
##Detrending##
dc_1120$PM_STM <- dc_1120$PM_LT_rem - dc_1120$kz_PM_seasonal
dc_1120$prec_STM <- dc_1120$prec_LT_rem - dc_1120$kz_prec_seasonal
dc_1120$temp_STM <- dc_1120$temp_LT_rem - dc_1120$kz_temp_seasonal
dc_1120$rh_STM <- dc_1120$rh_LT_rem - dc_1120$kz_rh_seasonal
dc_1120$wind_STM <- dc_1120$wind_LT_rem - dc_1120$kz_wind_annual
dc_1120$pbl_STM <- dc_1120$pbl_LT_rem - dc_1120$kz_pbl_annual


##Short-term_correlations##
dc_STM <- subset(dc_1120, select = c(PM_STM, prec_STM, temp_STM, 
                                     rh_STM, wind_STM, pbl_STM, days))
cor(dc_STM)
plot(dc_STM)

##Linear model for PM2.5 for short-term data, detrended##
lm_PM2.5 <- lm(dc_1120$PM_STM ~ dc_1120$temp_STM + dc_1120$prec_STM + 
                 dc_1120$rh_STM + dc_1120$wind_STM + dc_1120$pbl_STM + dc_1120$days)
summary(lm_PM2.5) 
plot(lm_PM2.5)

##GAM##
##General Additive Models##
library(mgcv) #calling library function
gam4 <- gam(dc_1120$PM_STM ~ s(dc_1120$rh_STM, dc_1120$prec_STM, dc_1120$wind_STM) + 
              dc_1120$temp_STM, method = "REML")
summary(gam4)

#plot model
plot(gam4, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 0.45 , cey = 0.3)


##Recreating Models from papers##
###GAMs recreated from papers###

##GAM_Beijing without optimized inputs##
##From Hua & Zhang et al (2021)##

#checking distributions of the parameters involved
hist(dc_1120$PM2.5, breaks = 50)
hist(dc_1120$prec, breaks = 100)

##PM2.5 and Precip have logarithmic distributions, linear regression models assume normal distributions
#Transforming into normal distribution
dc_1120$PM2.5 <- log10(dc_1120$PM2.5 + 1)
dc_1120$prec <- log10(dc_1120$prec + 1)
#checking summary and adding 1 inside log10 to avoid infinity in summary
summary(dc_1120$PM2.5)
summary(dc_1120$PM2.5)
#check distribution again
hist(dc_1120$PM2.5)
hist(dc_1120$prec, breaks = 50)

#distribution of precipitation is still skewed
#changing it to categorical variable
dc_1120$PRCPBool [dc_1120$prec != 0.0000] <- 1
dc_1120$PRCPBool [dc_1120$prec == 0.0000] <- 0
dc_1120$PRCPBool <- as.factor(dc_1120$PRCPBool)
#check number of rainy days
summary(dc_1120$PRCPBool)

#introducing some time variables, because we are not detrending the data
dc_1120$year <- as.factor(format(dc_1120$date, "%y"))
dc_1120$month <- as.numeric(format(dc_1120$date, "%m"))
dc_1120$dayno <- seq(1,3653, by = 1) #dummy variable for days

#*****Important considerations*****
#Here, offset for log transform = 1, compared to 3 in study #
#For temporal categorical values, here days = 1, 2,.., 7 
#compared to weekdays = 1, weekends = 2, holidays = 3 to 5
#There is no hourly factor here
#Meteorological components with highest fitting performance for the PARTICULAR study included in the model
#All meteorological components = T2M, RH, Precipitation, Wind, BLH, SP, Dew Point Temperature

gam_bj <- gam(dc_1120$PM2.5 ~ dc_1120$year + s(dc_1120$month) 
              + dc_1120$days + s(dc_1120$pbl) + s(dc_1120$wind))

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
#Wind Direction not considered#
#No information about ?? in paper#
#Here, Jenks natural classification method not followed, only Boolean 1,0 followed for precipitation#
#cubic splines, non-linearly correlated used in the study; hence cubic regression splines used#

gam_wsprec <- gam(dc_1120$PM2.5 ~ s(dc_1120$wind, bs = "cr") + 
                    s(dc_1120$dayno, bs = "cr") +  dc_1120$PRCPBool, method = "REML")
summary(gam_wsprec)

#plot model
plot(gam_wsprec, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


#show residuals
plot(gam_wsprec, pages = 1, all.terms = TRUE,
     rug = TRUE, residuals = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


