library(tidyverse)
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(here)
library(plotly)
library(Metrics)
library(ggpubr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(htmltools)
# load data, name by site ID


#####Sylvania, US-Syv####
# 2001 - 2021, FLUXNET daily means available
data_Syv_01_21 <- read.csv(here("data/AMF_US-Syv_FLUXNET_SUBSET_DD_2001-2021_3-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = substr(TIMESTAMP, 1, 4),
         Month = substr(TIMESTAMP, 5, 6),
         Day = substr(TIMESTAMP, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day),
                                        format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime))

data_Syv_2002 <- read.csv(here("data/AMF_US-Syv_FLUXNET_SUBSET_DD_2001-2021_3-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP, 1, 4)), 
         Month = substr(TIMESTAMP, 5, 6),
         Day = substr(TIMESTAMP, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2002)  


data_Syv_2016 <- read.csv(here("data/AMF_US-Syv_FLUXNET_SUBSET_DD_2001-2021_3-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP, 1, 4)), 
         Month = substr(TIMESTAMP, 5, 6),
         Day = substr(TIMESTAMP, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2016)  


data_Syv_2021 <- read.csv(here("data/AMF_US-Syv_FLUXNET_SUBSET_DD_2001-2021_3-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP, 1, 4)),  
         Month = substr(TIMESTAMP, 5, 6),
         Day = substr(TIMESTAMP, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2021)  

#Plotting Syv NEE
plot(data_Syv_2016$DoY, data_Syv_2016$NEE_VUT_25, 
     type = "o", col = "black", 
     xlab = "DOY", ylab = "NEE", 
, main = "SYV:NEE", pch=16)

points(data_Syv_2002$DoY, data_Syv_2002$NEE_VUT_25, 
       col = "red", type = "o", pch=16)

points(data_Syv_2021$DoY, data_Syv_2021$NEE_VUT_25, 
       col = "purple", type = "o",pch=16)


legend("topleft", legend = c("2016", "2002", "2021"),
       col = c("black", "red", "purple"), lty = 1)

#######US-Willow Creek (WCr) ########
library(dplyr)
#loaded .csv file of willow creek and also averaged out half hourly data to daily.
data_WCr_01_21 <- read.csv(here("data/AMF_US-WCr_BASE_HH_30-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = substr(TIMESTAMP_START, 1, 4),
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day),
                                        format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime))%>%
  group_by(Date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

data_WCr_2001_2004 <- data_WCr_01_21 %>%
  filter(Date >= as.Date("2001-01-01") & Date <= as.Date("2004-12-31"))

data_WCr_2020_2024 <- data_WCr_01_21 %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2024-12-31"))

data_WCr_2016_2020 <- data_WCr_01_21 %>%
  filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2020-12-31"))



#Plot NEE of WCr
plot(data_WCr_2020_2024$DoY, data_WCr_2020_2024$NEE_PI, 
     type = "o", col = "black", 
     xlab = "Datetime", ylab = "NEE", 
     main = "Willow Creek: NEE", ylim = c(-30,30))

points(data_WCr_2001_2004$DoY, data_WCr_2001_2004$NEE_PI, 
      col = "red", type = "o")

points(data_WCr_2016_2020$DoY, data_WCr_2016_2020$NEE_PI, 
      col = "purple", type = "o")

legend("topright", legend = c("2020-2024", "2001-2004", "2016-2020"),
       col = c("black", "red","purple"), lty = 1, cex=0.9)


