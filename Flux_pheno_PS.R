
setwd("/Users/pujasharma/Downloads/UGLR_warm_winter_fluxes/data")
library(tidyverse)
library(lubridate) 
library(dplyr)    
library(ggplot2)  
library(here)
library(plotly)
library(Metrics)
library(ggpubr)
library(patchwork) 
library(hrbrthemes)
library(htmltools)
# load data, name by site ID

library(dplyr)
library(lubridate)

# Read data
###SyV#####

SyV <- read.csv("AMF_US-Syv_BASE_HH_29-5.csv") %>%
  mutate(across(where(is.numeric), ~na_if(., -9999)))


# Converting TIMESTAMP_START to datetime
SyV <- SyV %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  filter(!is.na(TS_2_2_1) & !is.na(RECO_PI_F)) %>%
  mutate(
    TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)),
    Date = as.Date(TIMESTAMP_START),
    Year = year(Date),  # getting year from Date column
    Month = month(Date)  # getting year from Date column
    )
SyV <- SyV %>%
  mutate(Winter_Year = if_else(Month %in% c(1, 2), Year - 1L, Year)) %>%
  filter(Month %in% c(11, 12, 1, 2))

#  group by Date and lets do daily averages
SyV_daily <- SyV %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

fit_nls_Q10_SyV <- function(df) {
  model <- nls(RECO_PI_F ~ a * exp(b * TS_2_2_1), data = df, start = list(a = 1, b = 0.01))
  coefficients <- coef(model)
  Q10_value <- exp(10 * coefficients["b"])
  return(data.frame(a = coefficients["a"], b = coefficients["b"], Q10 = Q10_value))
}

results_SyV <- SyV_daily %>%
  group_by(Winter_Year) %>%
  do(fit_nls_Q10_SyV(.)) %>%
  ungroup()

# View the results
print(results_SyV)

#####Wcr#####
# Load the Wcr data

data_Wcr_01_21 <- read.csv("AMF_US-Wcr_FLUXNET_SUBSET_DD_2007-2021_3-5.csv") %>%
  mutate(across(where(is.numeric), ~na_if(.x, -9999))) %>%      # if needed
  filter(!is.na(TS_1_1_1) & !is.na(RECO_PI_F)) %>%
  mutate(
    Year  = as.integer(substr(TIMESTAMP_START, 1, 4)),
    Month = as.integer(substr(TIMESTAMP_START, 5, 6)),
    Day   = as.integer(substr(TIMESTAMP_START, 7, 8)),
    Datetime = as.POSIXct(strptime(paste(Year, Month, Day), format = "%Y %m %d")),
    Date     = make_date(Year, Month, Day),
    DoY      = yday(Datetime)
  ) %>%
  filter(Year >= 2012 & Year <= 2024) %>%
  mutate(Winter_Year = if_else(Month %in% c(1, 2), Year - 1L, Year)) %>%
  filter(Month %in% c(11, 12, 1, 2))

str(data_Wcr_01_21)

#function to fit model and calculate q10
fit_nls_Q10_Wcr <- function(df) {
  model <- nls(RECO_PI_F ~ a * exp(b * TS_1_1_1), data = df, start = list(a = 1, b = 0.01))
  coefficients <- coef(model)
  Q10_value <- exp(10 * coefficients["b"])
  return(data.frame(a = coefficients["a"], b = coefficients["b"], Q10 = Q10_value))
}

# Apply the function to each year in the dataset
results_Wcr <- data_Wcr_01_21 %>%
  group_by(Winter_Year) %>%
  do(fit_nls_Q10_Wcr(.)) %>%
  ungroup()

# View the results
print(results_Wcr)


###UMB#####
UMB <- read.csv("AMF_US-UMB_BASE_HH_21-5.csv") %>%
  mutate(across(where(is.numeric), ~na_if(., -9999)))

# Converting TIMESTAMP_START to datetime
UMB <- UMB %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  filter(!is.na(TS_1_1_1) & !is.na(RECO_PI_F)) %>%
  mutate(
    TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)),
    Date = as.Date(TIMESTAMP_START),
    Year = year(Date),  # getting year from Date column
    Month = month(Date)  # getting year from Date column
  )
UMB <- UMB %>%
  mutate(Winter_Year = if_else(Month %in% c(1, 2), Year - 1L, Year)) %>%
  filter(Month %in% c(11, 12, 1, 2))
#  group by Date and lets do daily averages
UMB_daily <- UMB %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

#function to fit model and calculate q10
fit_nls_Q10_UMB <- function(df) {
  model <- nls(RECO_PI_F ~ a * exp(b * TS_1_1_1), data = df, start = list(a = 1, b = 0.01))
  coefficients <- coef(model)
  Q10_value <- exp(10 * coefficients["b"])
  return(data.frame(a = coefficients["a"], b = coefficients["b"], Q10 = Q10_value))
}

results_UMB <- UMB_daily %>%
  group_by(Winter_Year) %>%
  do(fit_nls_Q10_UMB(.)) %>%
  ungroup()

print(results_UMB)



###UMd#####
UMd <- read.csv("AMF_US-UMd_BASE_HH_15-5.csv") %>%
  mutate(across(where(is.numeric), ~na_if(., -9999)))

# Converting TIMESTAMP_START to datetime
UMd <- UMd %>%
  mutate_if(is.logical, ~as.numeric(.)) %>%
  filter(!is.na(TS_1_1_1) & !is.na(RECO_PI_F)) %>%
  mutate(
    TIMESTAMP_START = ymd_hm(as.character(TIMESTAMP_START)),
    Date = as.Date(TIMESTAMP_START),
    Year = year(Date),  # getting year from Date column
    Month = month(Date)  # getting year from Date column
  )
UMd <- UMd %>%
  mutate(Winter_Year = if_else(Month %in% c(1, 2), Year - 1L, Year)) %>%
  filter(Month %in% c(11, 12, 1, 2))

#  group by Date and lets do daily averages
UMd_daily <- UMd %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

#function to fit model and calculate q10
fit_nls_Q10_UMd <- function(df) {
  model <- nls(RECO_PI_F ~ a * exp(b * TS_1_1_1), data = df, start = list(a = 1, b = 0.01))
  coefficients <- coef(model)
  Q10_value <- exp(10 * coefficients["b"])
  return(data.frame(a = coefficients["a"], b = coefficients["b"], Q10 = Q10_value))
}

results_UMd <- UMd_daily %>%
  group_by(Winter_Year) %>%
  do(fit_nls_Q10_UMd(.)) %>%
  ungroup()

print(results_UMd)



###PFa#####
# Load the PFa data
# Read the CSV file and process data


data_PFa_01_21 <- read.csv("AMF_US-PFa_BASE_HR_29-5.csv", header = TRUE, sep = ",", skip = 2) %>%
  mutate(
    TIMESTAMP_START = ymd_hm(TIMESTAMP_START),  # Convert to datetime
    Date = as.Date(TIMESTAMP_START)
  ) %>%
  group_by(Date) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~na_if(.x, -9999))) %>%
  filter(!is.na(TS_1_1_1) & !is.na(RECO_PI_F)) %>%
  mutate(
    Year = year(Date),
    Month = month(Date),
    Day = day(Date),
    DoY = yday(Date)
  ) %>%
  filter(Year >= 2012 & Year <= 2024) %>%       # filter by years
  mutate(Winter_Year = if_else(Month %in% c(1, 2), Year - 1L, Year)) %>%
  filter(Month %in% c(11, 12, 1, 2))            # keep only Nov, Dec, Jan, Feb


#function to fit model and calculate q10
fit_nls_Q10_PFa <- function(df) {
  model <- nls(RECO_PI_F ~ a * exp(b * TS_1_1_1), data = df, start = list(a = 1, b = 0.01))
  coefficients <- coef(model)
  Q10_value <- exp(10 * coefficients["b"])
  return(data.frame(a = coefficients["a"], b = coefficients["b"], Q10 = Q10_value))
}

results_PFa <- data_PFa_01_21 %>%
  group_by(Winter_Year) %>%
  do(fit_nls_Q10_PFa(.)) %>%
  ungroup()

print(results_PFa)

######NEON BUNDLED .h5 UNDE site#####
library(rhdf5)
setwd("/Users/pujasharma/Downloads")
f_Nov23 <- "NEON.D05.UNDE.DP4.00200.001.nsae.2023-11.basic.20250122T184756Z.h5"
f_Dec23 <- "NEON.D05.UNDE.DP4.00200.001.nsae.2023-12.basic.20250122T185000Z.h5"
f_Jan24 <- "NEON.D05.UNDE.DP4.00200.001.nsae.2024-01.basic.20250122T185102Z.h5"
f_Feb24 <- "NEON.D05.UNDE.DP4.00200.001.nsae.2024-02.basic.20250122T184723Z.h5"

dat_main_stor_f_Nov23 <- h5read(f_Nov23, "/UNDE/dp04/data") 

plot(results_UMB$Winter_Year, results_UMB$Q10, type = "o", pch = 15, col = "red",ylim = c(0,20))

points(results_SyV$Winter_Year, results_SyV$Q10,
     type = "o", pch = 14, col = "blue",
     ylab = "Q10", xlab = "Year",
     main = "Winter Months Q10: NDJF")

#other sites
points(results_UMd$Winter_Year, results_UMd$Q10, type = "o", pch = 16, col = "darkgreen")
points(results_PFa$Winter_Year, results_PFa$Q10, type = "o", pch = 17, col = "purple")
points(results_Wcr$Winter_Year, results_Wcr$Q10, type = "o", pch = 18, col = "orange")

# legend
legend("topright",                                  
       legend = c("SyV", "UMB", "UMd", "PFa", "Wcr"), 
       col = c("blue", "red", "darkgreen", "purple", "orange"),  
       pch = c(14, 15, 16, 17, 18), 
       lty = 1,                     
       bty = "n",                   
       cex = 0.8)                   

