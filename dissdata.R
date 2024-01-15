# Script for Dissertation Project
# Author: Amy Tramontozzi
# Date: 18/10/2023

library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)
library(stringdist)

# Load and tidy prod data 
## Summing area, prod, and yield across crops
prod1997 <- read_excel("prod1997.xls", guess_max = 20000)
prod1997[is.na(prod1997)] <- 0
area <- select(prod1997, contains("Area"))
prod1997$area <- rowSums(area) 
prod <- select(prod1997, contains("Production"))
prod1997$prod <- rowSums(prod)
yield <- select(prod1997, contains("Yield"))
prod1997$yield <- rowSums(yield)
# Only select total values
prod1997 <- prod1997[,-4:-240]

# Get rid of listed numbers from state and district
prod1997$state <- sub("^\\d+\\.\\s", "", prod1997$state)
prod1997$district <- sub("^\\d+\\.\\s", "", prod1997$district)
# String to nice title
prod1997$state <- str_to_title(prod1997$state)
prod1997$district <- str_to_title(prod1997$district)
# Just the first year
prod1997$year <- str_sub(prod1997$year, end = -8)
# Convert state, district, and year into factors
prod1997$state <- as.factor(prod1997$state)
prod1997$year <- as.factor(prod1997$year)
prod1997$district <- as.factor(prod1997$district)

# Load and tidy insurance data
ins <- read_excel("districtdata.xlsx")
ins <- ins[,-24:-41]
ins$ins.units <- as.numeric(gsub(",","",ins$ins.units))
ins$farmers <- as.numeric(gsub(",","",ins$farmers))
ins$loanee <- as.numeric(gsub(",","",ins$loanee))
ins$nonloanee <- as.numeric(gsub(",","",ins$nonloanee))
ins$sum.insured <- as.numeric(gsub(",","",ins$sum.insured))
ins$area.ins <- round(ins$area.ins, digits = 2)
ins$farmers.premium <- round(ins$farmers.premium, digits = 2)
ins$state.premium <- round(ins$state.premium, digits = 2)
ins$goi.premium <- round(ins$goi.premium, digits = 2)
ins$sum.insured <- round(ins$sum.insured, digits = 2)
ins$state <- sub("&", "and", ins$state)
ins$district <- sub("&", "and", ins$district)
ins$state <- as.factor(ins$state)
ins$district <- as.factor(ins$district)

# Load and tidy CRIS rainfall data
rain <- read_excel("indian_rain_data.xlsx")
names(rain)[names(rain) == "fed.ptdef"] <- "feb.ptdef"
rain$year <- as.numeric(rain$year)
rain$state <- str_to_title(rain$state)
rain$district <- str_to_title(rain$district)
rain$state <- sub("&", "and", rain$state)
rain$district <- sub("&", "and", rain$district)
rain$state <- as.factor(rain$state)
rain$district <- as.factor(rain$district)
rain$year <- as.factor(rain$year)

# Load and tidy climate data
climate_data <- read_excel("climate_data_v5.xlsx")
climate_data$station <- str_to_title(climate_data$station)
climate_data$station <- as.factor(climate_data$station)
climate_data$stationID <- as.factor(climate_data$stationID)

### RENAMING STATIONS TO DISTRICTS
stat_dis <- read_excel("station_district.xlsx")
stat_dis$station <- str_to_title(stat_dis$station)
climate_data <- merge(climate_data, stat_dis, by = "station")
climate_data$district <- as.factor(climate_data$district)
### RENAMING DISTRICTS TO MERGE
# Rename insurance states

# Rename insurance districts
insmissing.dis <- levels(climate_data$district)[!(levels(climate_data$district) %in% levels(ins$district))]
levels(ins$district)[levels(ins$district)=='Ahmadabad'] <- 'Ahmedabad'
levels(ins$district)[levels(ins$district)=='Anantapur'] <- 'Anantapuramu'
levels(ins$district)[levels(ins$district)=='Baleshwar'] <- 'Balasore'
levels(ins$district)[levels(ins$district)=='Banas Kantha'] <- 'Banaskantha'
levels(ins$district)[levels(ins$district)=='Bengaluru Urban'] <- 'Bengalore'
levels(ins$district)[levels(ins$district)=='Bengaluru Rural'] <- 'Bengalore Rural'
levels(ins$district)[levels(ins$district)=='Koch Bihar'] <- 'Cooch Behar'
levels(ins$district)[levels(ins$district)=='DakshinaKannada'] <- 'Dakshina Kannada'
levels(ins$district)[levels(ins$district)=='Darjiling'] <- 'Darjeeling'
levels(ins$district)[levels(ins$district)=='Gaurella Pendra Marwahi'] <- 'Gaurela-Pendra-Marwahi'
levels(ins$district)[levels(ins$district)=='Jagatsinghapur'] <- 'Jagatsinghpur'
levels(ins$district)[levels(ins$district)=='Kalaburgi'] <- 'Kalaburagi'
levels(ins$district)[levels(ins$district)=='Kendujhar'] <- 'Keonjhar'
levels(ins$district)[levels(ins$district)=='Maldah'] <- 'Malda'
levels(ins$district)[levels(ins$district)=='Sri Potti Sriramulu Nellore'] <- 'Nellore'
levels(ins$district)[levels(ins$district)=='North Twenty Four Parganas'] <- 'North 24 Paraganas'
levels(ins$district)[levels(ins$district)=='Rangareddy'] <- 'Ranga Reddy'
levels(ins$district)[levels(ins$district)=='South Twenty Four Parganas'] <- 'South 24 Parganas'
levels(ins$district)[levels(ins$district)=='UttarKannada'] <- 'Uttara Kannada'

# Create climate/ins 2018 dataframe
climate_data_noNA <- na.omit(climate_data) # Omit all climate NAs
climate_data2018_noNA <- filter(climate_data_noNA, year > 2017) # All climate data from 2018
climate_data2018_agg <- aggregate(. ~ district + year, FUN = mean, data=climate_data2018_noNA) # Aggregate stations to districts
climate_data2018_agg <- climate_data2018[,-3:-4] # Remove stations and station ID
climate_data2018_agg <- climate_data2018 %>% mutate_if(is.numeric, ~round(., 1)) # Round temps to 1 digit
climate_ins_2018 <- inner_join(ins, climate_data2018, by=c("district" = "district", "year" = "year")) # Merge ins and 2018 climate
## Result is 223 observations for primary regression

# Create climate/ins full dataframe
climate_data_agg <- aggregate(. ~ district + year, FUN = mean, data=climate_data_noNA) # Aggregate stations to districts
climate_data_agg <- climate_data_agg[,-3:-4] # Remove stations and station ID
climate_data_agg <- climate_data_agg %>% mutate_if(is.numeric, ~round(., 1)) # Round temps to 1 digit
## Result is 7723 observations for full regression
