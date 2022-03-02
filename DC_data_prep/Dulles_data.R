#using specific library functions
install.packages("data.table") #installing package
library(data.table) #calling library function
library(dplyr) #used for grouping function
install.packages("lubridate")
library(lubridate) #used for dates

#read data
Dulles_obs = read.csv("DC_Dulles_obs.csv")

#manipulate data type
Dulles_obs$date <- as.Date(Dulles_obs$DATE)
Dulles_obs$HourlyRelativeHumidity <- as.numeric(Dulles_obs$HourlyRelativeHumidity)
Dulles_obs$HourlyDewPointTemperature <- as.numeric(Dulles_obs$HourlyDewPointTemperature)
Dulles_obs$HourlyVisibility <- as.numeric(Dulles_obs$HourlyVisibility)

#Find daily averages from hourly averages
DC_rh <- aggregate(Dulles_obs$HourlyRelativeHumidity, 
                              list(Dulles_obs$date), FUN = mean, na.rm = TRUE)
DC_dew <- aggregate(Dulles_obs$HourlyDewPointTemperature, 
                    list(Dulles_obs$date), FUN = mean, na.rm = TRUE)
DC_vis <- aggregate(Dulles_obs$HourlyVisibility, 
                    list(Dulles_obs$date), FUN = mean, na.rm = TRUE)

#save data in csv format in PC
write.csv(DC_rh, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\DC_rh.csv", row.names = FALSE)
write.csv(DC_dew, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\DC_dew.csv", row.names = FALSE)
write.csv(DC_vis, "C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents\\DC_vis.csv", row.names = FALSE)


