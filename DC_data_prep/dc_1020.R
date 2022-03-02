####Meteorological detrending for PM2.5 for Washington DC 2010-2020####
###Goal: To check relations between different meteorological signals and atmospheric pollutants###


###Preparing and assembling the Data###
##using specific library functions##
install.packages("data.table") #installing package
library(data.table) #calling library function

##read data
#*Note on Data: Data collected from EPA
#*For years 2014-16, data is collected from McMillan
#*For other years, PM2.5 data is collected from River Terrace
#*Only Primary Sampler used, determined by POC (Parameter Occurence Code)
dc_1020 = read.csv("DC2010_20.csv")


##create new data frame for date##
#installing specific library function for date
install.packages("lubridate")
library(lubridate)
date <- seq(ymd('2010-01-01'), ymd('2020-12-31'), by = "1 day")

#merge the two data frames
dc_pm <- merge(data.frame(dc_1020, row.names=NULL), data.frame(date, row.names=NULL), 
      by = 0, all = TRUE)[-1]

##Manipulate the erroneous values##
#Replace negative numbers in PM2.5 data with NA
dc_pm$PM2.5[dc_pm$PM2.5 < 0] <- NA

#replace NAs with zero
dc_pm[is.na(dc_pm)] = 0

#save file
write.csv(dc_pm,"C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\dc_pm.csv", row.names = FALSE)

##Assembling other meteorological data##
library(dplyr) #used for grouping function
#*library(data.table), library(lubridate) already in use

#read data
dc_prec = read.csv("dc_prec.csv")
dc_temp = read.csv("dc_temp.csv")
dc_rh = read.csv("dc_rh.csv")
dc_u = read.csv("u-wind_dc.csv")
dc_v = read.csv("v-wind_dc.csv")
dc_pbl = read.csv("dc_pbl.csv")

#Find daily averages from 6-hourly averages
dc_prec <- aggregate.data.frame(dc_prec$Prec, list(dc_prec$Date), FUN = mean)
dc_temp <- aggregate.data.frame(dc_temp$Temp, list(dc_temp$Date), FUN = mean)
dc_rh <- aggregate.data.frame(dc_rh$rh, list(dc_rh$Date), FUN = mean)
dc_u <- aggregate.data.frame(dc_u$u_1000, list(dc_u$Date), FUN = mean)
dc_v <- aggregate.data.frame(dc_v$v_1000, list(dc_v$Date), FUN = mean)
dc_pbl <- aggregate.data.frame(dc_pbl$pbl, list(dc_pbl$Date), FUN = mean)


#save data in csv format in PC
write.csv(dc_prec, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\dc_dailyprec.csv", row.names = FALSE)
write.csv(dc_temp, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\dc_dailytemp.csv", row.names = FALSE)
write.csv(dc_rh, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\dc_dailyrh.csv", row.names = FALSE)
write.csv(dc_u, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\dc_dailyu.csv", row.names = FALSE)
write.csv(dc_v, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\dc_dailypbl.csv", row.names = FALSE)





