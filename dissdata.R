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

# Rename insurance districts to match climate districts
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



# Rename rain states to ins states
rainmissing.st <- levels(ins$state)[!(levels(ins$state) %in% levels(rain$state))]

levels(rain$state)[levels(rain$state)=='A and N Island (Ut)'] <- 'Andaman and Nicobar Islands'
levels(rain$state)[levels(rain$state)=='Puducherry (Ut)'] <- 'Puducherry'
levels(rain$state)[levels(rain$state)=='Tamilnadu'] <- 'Tamil Nadu'
# Rename rain districts to  ins districts
rainmissing.dis <- levels(ins$district)[!(levels(ins$district) %in% levels(rain$district))]

levels(ins$district)[levels(ins$district)=='Ahmednagar'] <- 'Ahmadnagar' # within ins
levels(ins$district)[levels(ins$district)=='Boudh'] <- 'Baudh'
levels(ins$district)[levels(ins$district)=='Beed'] <- 'Bid'
levels(ins$district)[levels(ins$district)=='Barabanki'] <- 'Bara Banki'
levels(ins$district)[levels(ins$district)=='Buldhana'] <- 'Buldana'
levels(ins$district)[levels(ins$district)=='Chittorgarh'] <- 'Chittaurgarh'
levels(ins$district)[levels(ins$district)=='Gangtok'] <- 'East District'
levels(ins$district)[levels(ins$district)=='Gyalshing'] <- 'West District'
levels(ins$district)[levels(ins$district)=='Pauri Garhwal'] <- 'Garhwal'
levels(ins$district)[levels(ins$district)=='Deogarh'] <- 'Debagarh'
levels(ins$district)[levels(ins$district)=='Dholpur'] <- 'Dhaulpur'
levels(ins$district)[levels(ins$district)=='Ayodhya'] <- 'Faizabad'

levels(rain$district)[levels(rain$district)=='Agar-Malwa'] <- 'Agar Malwa'
levels(rain$district)[levels(rain$district)=='Ahmednagar'] <- 'Ahmadnagar'
levels(rain$district)[levels(rain$district)=='Alapuzha'] <- 'Alappuzha'
levels(rain$district)[levels(rain$district)=='Prayagraj'] <- 'Allahabad'
levels(rain$district)[levels(rain$district)=='Amraoti'] <- 'Amravati'
levels(rain$district)[levels(rain$district)=='Angul'] <- 'Anugul'
levels(rain$district)[levels(rain$district)=='Bagalkote'] <- 'Bagalkot'
levels(rain$district)[levels(rain$district)=='Bolangir'] <- 'Balangir'
levels(rain$district)[levels(rain$district)=='Barabanki'] <- 'Bara Banki'
levels(rain$district)[levels(rain$district)=='Boudhgarh'] <- 'Baudh'
levels(rain$district)[levels(rain$district)=='Bengaluru Rural'] <- 'Bengalore Rural'
levels(rain$district)[levels(rain$district)=='Bengaluru Urban'] <- 'Bengalore'
levels(rain$district)[levels(rain$district)=='B. Kothagudem'] <- 'Bhadradri'
levels(rain$district)[levels(rain$district)=='Beed'] <- 'Bid'
levels(rain$district)[levels(rain$district)=='Bulandshahar'] <- 'Bulandshahr'
levels(rain$district)[levels(rain$district)=='Buldhana'] <- 'Buldana'
levels(rain$district)[levels(rain$district)=='Chamarajanagar'] <- 'ChamarajNagar'
levels(rain$district)[levels(rain$district)=='Chikaballapura'] <- 'Chikkaballapur'
levels(rain$district)[levels(rain$district)=='Chittorgarh'] <- 'Chittaurgarh'
levels(rain$district)[levels(rain$district)=='Dantewada'] <- 'Dakshin Bastar Dantewada'
levels(rain$district)[levels(rain$district)=='South Dinajpur'] <- 'Dakshin Dinajpur'
levels(rain$district)[levels(rain$district)=='Deogarh'] <- 'Debagarh'
levels(rain$district)[levels(rain$district)=='Dholpur'] <- 'Dhaulpur'
levels(rain$district)[levels(rain$district)=='N. C. Hills'] <- 'Dima Hasao'
levels(rain$district)[levels(rain$district)=='East Sikkim'] <- 'East District'
levels(rain$district)[levels(rain$district)=='Khandwa'] <- 'East Nimar'
levels(rain$district)[levels(rain$district)=='Garhwal Pauri'] <- 'Garhwal'
levels(rain$district)[levels(rain$district)=='West Sikkim'] <- 'West District'
levels(rain$district)[levels(rain$district)=='West Sikkim'] <- 'West District'

levels(rain$district)[levels(rain$district)=='Haridwar'] <- 'Haridwar'
levels(rain$district)[levels(rain$district)=='Hazaribag'] <- 'Hazaribagh'
levels(rain$district)[levels(rain$district)=='Janjgir'] <- 'Janjgir-Champa'
levels(rain$district)[levels(rain$district)=='J. Bhupalpally'] <- 'Jayashankar Bhupalpally'
levels(rain$district)[levels(rain$district)=='Kamrup Metro'] <- 'Kamrup (Metro)'
levels(rain$district)[levels(rain$district)=='Cannur'] <- 'Kannur'
levels(rain$district)[levels(rain$district)=='Kasargod'] <- 'Kasaragod'
levels(rain$district)[levels(rain$district)=='Keonjhargarh'] <- 'Kendujhar'
levels(rain$district)[levels(rain$district)=='Kistwar'] <- 'Kishtwar'
levels(rain$district)[levels(rain$district)=='Korea'] <- 'Koriya'
levels(rain$district)[levels(rain$district)=='M. Malkajgiri'] <- 'Medchal-Malkajgiri'
levels(rain$district)[levels(rain$district)=='Nawarangpur'] <- 'Nabarangpur'
levels(rain$district)[levels(rain$district)=='South Sikkim'] <- 'Namchi'
levels(rain$district)[levels(rain$district)=='North 24 Parganas'] <- 'North Twenty Four Parganas'
levels(rain$district)[levels(rain$district)=='Mewat'] <- 'Nuh'
levels(rain$district)[levels(rain$district)=='Peddapalle'] <- 'Peddapalli'
levels(rain$district)[levels(rain$district)=='Rae Bareilly'] <- 'Raebareli'
levels(rain$district)[levels(rain$district)=='Ri-Bhoi'] <- 'Ri Bhoi'
levels(rain$district)[levels(rain$district)=='Seraikela-Kharsawan'] <- 'Seraikela-Kharsawan'
levels(rain$district)[levels(rain$district)=='Shrawasti Nagar'] <- 'Shravasti'
levels(rain$district)[levels(rain$district)=='Siddharth Nagar'] <- 'Siddharthnagar'
levels(rain$district)[levels(rain$district)=='Sibsagar'] <- 'Sivasagar'
levels(rain$district)[levels(rain$district)=='Sonepat'] <- 'Sonipat'
levels(rain$district)[levels(rain$district)=='South 24 Parganas'] <- 'South Twenty Four Parganas'
levels(rain$district)[levels(rain$district)=='Spsr Nellore'] <- 'Sri Potti Sriramulu Nellore'
levels(rain$district)[levels(rain$district)=='Sonepur'] <- 'Subarnapur'
levels(rain$district)[levels(rain$district)=='Thenkasi'] <- 'Tenkasi'
levels(rain$district)[levels(rain$district)=='Tirupattur'] <- 'Tirupathur'
levels(rain$district)[levels(rain$district)=='North Dinajpur'] <- 'Uttar Dinajpur'
levels(rain$district)[levels(rain$district)=='Villupuram'] <- 'Viluppuram'
levels(rain$district)[levels(rain$district)=='Vishakhapatnam'] <- 'Visakhapatanam'
levels(rain$district)[levels(rain$district)=='Wynad'] <- 'Wayanad'
levels(rain$district)[levels(rain$district)=='Ysr District'] <- 'Y.S.R.'
levels(rain$district)[levels(rain$district)=='Yadgir'] <- 'Yadgiri'
levels(rain$district)[levels(rain$district)=='Yamuna Nagar'] <- 'Yamunanagar'
levels(rain$district)[levels(rain$district)=='Yeotmal'] <- 'Yavatmal'
levels(rain$district)[levels(rain$district)=='Anantapuramu'] <- 'Anantapur'
levels(rain$district)[levels(rain$district)=='Hardwar'] <- 'Hardiwar'
levels(rain$district)[levels(rain$district)=='Kanpur City'] <- 'Kanpur Nagar'
levels(rain$district)[levels(rain$district)=='West Midnapore'] <- 'Paschim Medinipur'
levels(rain$district)[levels(rain$district)=='West Singbhum'] <- 'Paschim Singhbhum'
levels(rain$district)[levels(rain$district)=='East Midnapore'] <- 'Purba Medinipur'
levels(rain$district)[levels(rain$district)=='East Singbhum'] <- 'Purbi Singhbhum'
levels(rain$district)[levels(rain$district)=='Sholapur'] <- 'Solapur'
levels(rain$district)[levels(rain$district)=='Garhwal Tehri'] <- 'Tehri Garhwal'
levels(rain$district)[levels(rain$district)=='Trichy'] <- 'Tiruchirappalli '
# Rename prod states to ins states

# Rename prod districts to ins districts


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
