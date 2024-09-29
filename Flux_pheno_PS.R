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

# Willow Creek, US-WCr
# 2024
# FLUXNET data not available; we have HH data only
data_WCr_2024 <- read.csv(here("data/US-WCr_HH_202401010000_202406190000.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = substr(TIMESTAMP_START, 1, 4),
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Hour = substr(TIMESTAMP_START, 9, 10),
         Minute = substr(TIMESTAMP_START, 11, 12),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day,
                                              paste(Hour, Minute, sep=":")),
                                        format = "%Y %m %d %H:%M")),
         Date = make_date(Year, Month, Day),
         Time = format(Datetime, format = "%H:%M"),
         DoY = yday(Datetime)) %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Sylvania, US-Syv
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
  mutate(Year = as.numeric(substr(TIMESTAMP, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP, 5, 6),
         Day = substr(TIMESTAMP, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2002)  # Filter for years 2020 to 2024


data_Syv_2016 <- read.csv(here("data/AMF_US-Syv_FLUXNET_SUBSET_DD_2001-2021_3-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP, 5, 6),
         Day = substr(TIMESTAMP, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2016)  # Filter for years 2020 to 2024


data_Syv_2021 <- read.csv(here("data/AMF_US-Syv_FLUXNET_SUBSET_DD_2001-2021_3-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP, 5, 6),
         Day = substr(TIMESTAMP, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2021)  # Filter for years 2020 to 2024


# Set up the plotting area
plot(data_Syv_2016$DoY, data_Syv_2016$NEE_VUT_25, 
     type = "o", col = "black", 
     xlab = "DOY", ylab = "NEE", 
, main = "SYV:NEE", pch=16)

# Add the 2020-2024 data to the plot using lines()
points(data_Syv_2002$DoY, data_Syv_2002$NEE_VUT_25, 
       col = "red", type = "o", pch=16)

points(data_Syv_2021$DoY, data_Syv_2021$NEE_VUT_25, 
       col = "purple", type = "o",pch=16)


# Optional: Add a legend to distinguish the datasets
legend("topleft", legend = c("2016", "2002", "2021"),
       col = c("black", "red", "purple"), lty = 1)


library(dplyr)
WCr <- read.csv("data/AMF_US-WCr_BASE_HH_30-5.csv", header = T)
# Sylvania, US-Syv
# 2001 - 2021, FLUXNET daily means available
data_WCr_01_21 <- read.csv(here("data/AMF_US-WCr_BASE_HH_30-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = substr(TIMESTAMP_START, 1, 4),
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day),
                                        format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime))

data_WCr_2020_2024 <- read.csv(here("data/AMF_US-WCr_BASE_HH_30-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP_START, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year >= 2020 & Year <= 2024)  # Filter for years 2020 to 2024

data_WCr_2016_2020 <- read.csv(here("data/AMF_US-WCr_BASE_HH_30-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP_START, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year >= 2016 & Year <= 2020)  # Filter for years 2020 to 2024

data_WCr_2023 <- read.csv(here("data/AMF_US-WCr_BASE_HH_30-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP_START, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2023)  # Filter for years 2020 to 2024

data_WCr_2022 <- read.csv(here("data/AMF_US-WCr_BASE_HH_30-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP_START, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2022)  # Filter for years 2020 to 2024



par(mfrow=c(2,1))
# Initialize the plot
plot(data_WCr_2023$DoY, data_WCr_2023$NEE_PI, 
     type = "l", col = "blue", 
     xlab = "Datetime", ylab = "NEE_PI", 
     ylim = c(-70,70))

# Add the 2020-2024 data to the plot
plot(data_WCr_2022$DoY, data_WCr_2022$NEE_PI, 
     col = "red", type = "l", 
     ylim = c(-70,70))
)



data_WCr_2001 <- read.csv(here("data/AMF_US-WCr_BASE_HH_30-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP_START, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2001)  # Filter for years 2020 to 2024


data_WCr_2002 <- read.csv(here("data/AMF_US-WCr_BASE_HH_30-5.csv")) %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  mutate(across(everything(), ~na_if(., -9999))) %>%
  mutate(Year = as.numeric(substr(TIMESTAMP_START, 1, 4)),  # Ensure 'Year' is numeric
         Month = substr(TIMESTAMP_START, 5, 6),
         Day = substr(TIMESTAMP_START, 7, 8),
         Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
         Date = make_date(Year, Month, Day),
         DoY = yday(Datetime)) %>%
  filter(Year == 2002)  # Filter for years 2020 to 2024

# Set up the plotting area
plot(data_WCr_2023$DoY, data_WCr_2023$NEE_PI, 
     type = "o", col = "black", 
     xlab = "Datetime", ylab = "NEE_PI", 
     ylim = c(-70, 70), main = "NEE_PI for 2016-2020 and 2020-2024")

# Add the 2020-2024 data to the plot using lines()
points(data_WCr_2001$DoY, data_WCr_2001$NEE_PI, 
      col = "red", type = "o")

points(data_WCr_2002$DoY, data_WCr_2002$NEE_PI, 
      col = "purple", type = "o")

# Optional: Add a legend to distinguish the datasets
legend("topright", legend = c("2023", "2001", "2002"),
       col = c("black", "red","purple"), lty = 1)

####################


# Set up the plotting area
plot(data_WCr_2023$DoY, data_WCr_2023$TA_1_1_1, 
     type = "o", col = "black", 
     xlab = "Datetime", ylab = "NEE_PI", 
     ylim = c(-70, 70), main = "NEE_PI for 2016-2020 and 2020-2024")

# Add the 2020-2024 data to the plot using lines()
points(data_WCr_2001$DoY, data_WCr_2001$TA_1_1_1, 
       col = "red", type = "o")

points(data_WCr_2002$DoY, data_WCr_2002$TA_1_1_1, 
       col = "purple", type = "o")

# Optional: Add a legend to distinguish the datasets
legend("topright", legend = c("2023", "2001", "2002"),
       col = c("black", "red","purple"), lty = 1)
