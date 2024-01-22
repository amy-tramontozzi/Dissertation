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
prod1997$year <- as.numeric(prod1997$year)
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
#rain$year <- as.factor(rain$year)

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

same_name_diff_state <- ins %>%
  group_by(district) %>%
  filter(n_distinct(state) > 1) %>%
  select(district, state) %>%
  distinct()

# Rename rain states to ins states
rainmissing.st <- levels(ins$state)[!(levels(ins$state) %in% levels(rain$state))]

levels(rain$state)[levels(rain$state)=='A and N Island (Ut)'] <- 'Andaman and Nicobar Islands'
levels(rain$state)[levels(rain$state)=='Puducherry (Ut)'] <- 'Puducherry'
levels(rain$state)[levels(rain$state)=='Tamilnadu'] <- 'Tamil Nadu'
# Rename rain districts to  ins districts
rainmissing.dis <- levels(ins$district)[!(levels(ins$district) %in% levels(rain$district))]

# Rename Deogarh district 
rain <- rain %>%
  mutate(district = if_else(state == "Jharkhand" & district == "Deogarh", "Deoghar", district))
rain$district <- as.factor(rain$district)

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
levels(ins$district)[levels(ins$district)=='Gorella-Pendra-Marwahi'] <- 'Gaurela-Pendra-Marwahi'
levels(ins$district)[levels(ins$district)=='Janjgir - Champa'] <- 'Janjgir-Champa'
levels(ins$district)[levels(ins$district)=='Nabarangpur'] <- 'Nabarangapur'
levels(ins$district)[levels(ins$district)=='Gondia'] <- 'Gondiya'
levels(ins$district)[levels(ins$district)=='Gurugram'] <- 'Gurgaon'
levels(ins$district)[levels(ins$district)=='Haridwar'] <- 'Hardwar'
levels(ins$district)[levels(ins$district)=='Jalore'] <- 'Jalor'
levels(ins$district)[levels(ins$district)=='Kabirdham'] <- 'Kabeerdham'
levels(ins$district)[levels(ins$district)=='Kamrup Metro'] <- 'Kamrup Metropolitan'
levels(ins$district)[levels(ins$district)=='East Nimar'] <- 'Khandwa (East Nimar)'
levels(ins$district)[levels(ins$district)=='Khargone'] <- 'Khargone (West Nimar)'
levels(ins$district)[levels(ins$district)=='Korea'] <- 'Koriya'
levels(ins$district)[levels(ins$district)=='Maharajganj'] <- 'Mahrajganj'
levels(ins$district)[levels(ins$district)=='Marigaon'] <- 'Morigaon'
levels(ins$district)[levels(ins$district)=='Narsinghpur'] <- 'Narsimhapur'
levels(ins$district)[levels(ins$district)=='Nuh'] <- 'Mewat'
levels(ins$district)[levels(ins$district)=='Pondicherry'] <- 'Puducherry'
levels(ins$district)[levels(ins$district)=='Prayagraj'] <- 'Allahabad'
levels(ins$district)[levels(ins$district)=='Ri Bhoi'] <- 'Ribhoi'
levels(ins$district)[levels(ins$district)=='Shravasti'] <- 'Shrawasti'
levels(ins$district)[levels(ins$district)=='Namchi'] <- 'South District'
levels(ins$district)[levels(ins$district)=='Nellore'] <- 'Spsr Nellore'
levels(ins$district)[levels(ins$district)=='Sonepur'] <- 'Subarnapur'
levels(ins$district)[levels(ins$district)=='Thanjavur I'] <- 'Thanjavur'
levels(ins$district)[levels(ins$district)=='Thanjavur II'] <- 'Thanjavur'
levels(ins$district)[levels(ins$district)=='Thiruvarur II'] <- 'Thiruvarur'
levels(ins$district)[levels(ins$district)=='Visakhapatanam'] <- 'Visakhapatnam'
levels(ins$district)[levels(ins$district)=='Warangal Rural'] <- 'Warangal'
levels(ins$district)[levels(ins$district)=='Warangal Urban'] <- 'Warangal'

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
levels(rain$district)[levels(rain$district)=='Garhwal Pauri'] <- 'Garhwal'
levels(rain$district)[levels(rain$district)=='West Sikkim'] <- 'West District'
levels(rain$district)[levels(rain$district)=='Gariaband'] <- 'Gariyaband'
levels(rain$district)[levels(rain$district)=='Gondia'] <- 'Gondiya'
levels(rain$district)[levels(rain$district)=='Howrah'] <- 'Haora'
levels(rain$district)[levels(rain$district)=='Hassan'] <- 'Hasan'
levels(rain$district)[levels(rain$district)=='Hazaribag'] <- 'Hazaribagh'
levels(rain$district)[levels(rain$district)=='Narmadapuram'] <- 'Hoshangabad'
levels(rain$district)[levels(rain$district)=='Hooghly'] <- 'Hugli'
levels(rain$district)[levels(rain$district)=='Jagatsinghapur'] <- 'Jagatsinghpur'
levels(rain$district)[levels(rain$district)=='Jajpur'] <- 'Jajapur'
levels(rain$district)[levels(rain$district)=='Jalore'] <- 'Jalor'
levels(rain$district)[levels(rain$district)=='Janjgir'] <- 'Janjgir-Champa'
levels(rain$district)[levels(rain$district)=='J. Bhupalpally'] <- 'Jayashankar'
levels(rain$district)[levels(rain$district)=='Jogulamba Gadwal'] <- 'Jogulamba'
levels(rain$district)[levels(rain$district)=='Kabirdham'] <- 'Kabeerdham'
levels(rain$district)[levels(rain$district)=='Kalaburgi'] <- 'Kalaburagi'
levels(rain$district)[levels(rain$district)=='Kamrup (Rural)'] <- 'Kamrup'
levels(rain$district)[levels(rain$district)=='Kamrup Metro'] <- 'Kamrup Metropolitan'
levels(rain$district)[levels(rain$district)=='Kanyakumari'] <- 'Kanniyakumari'
levels(rain$district)[levels(rain$district)=='Cannur'] <- 'Kannur'
levels(rain$district)[levels(rain$district)=='Kanpur City'] <- 'Kanpur Nagar'
levels(rain$district)[levels(rain$district)=='Kasargod'] <- 'Kasaragod'
levels(rain$district)[levels(rain$district)=='Keonjhargarh'] <- 'Keonjhar'
levels(rain$district)[levels(rain$district)=='Khandwa'] <- 'Khandwa (East Nimar)'
levels(rain$district)[levels(rain$district)=='Khargone'] <- 'Khargone (West Nimar)'
levels(rain$district)[levels(rain$district)=='Keonjhargarh'] <- 'Keonjhar'
levels(rain$district)[levels(rain$district)=='Kistwar'] <- 'Kishtwar'
levels(rain$district)[levels(rain$district)=='Koderma'] <- 'Kodarma'
levels(rain$district)[levels(rain$district)=='Kumaram Bheem'] <- 'Komaram Bheem'
levels(rain$district)[levels(rain$district)=='Korea'] <- 'Koriya'
levels(rain$district)[levels(rain$district)=='Kushi Nagar'] <- 'Kushinagar'
levels(rain$district)[levels(rain$district)=='Maharajganj'] <- 'Mahrajganj'
levels(rain$district)[levels(rain$district)=='M. Malkajgiri'] <- 'Medchal-Malkajgiri'
levels(rain$district)[levels(rain$district)=='Nuh'] <- 'Mewat'
levels(rain$district)[levels(rain$district)=='Nawarangpur'] <- 'Nabarangapur'
levels(rain$district)[levels(rain$district)=='Narsinghpur'] <- 'Narsimhapur'
levels(rain$district)[levels(rain$district)=='Nicobar'] <- 'Nicobars'
levels(rain$district)[levels(rain$district)=='North 24 Parganas'] <- 'North 24 Paraganas'
levels(rain$district)[levels(rain$district)=='North Sikkim'] <- 'North District'
levels(rain$district)[levels(rain$district)=='Dharashiv'] <- 'Osmanabad'
levels(rain$district)[levels(rain$district)=='West Midnapore'] <- 'Paschim Medinipur'
levels(rain$district)[levels(rain$district)=='West Singbhum'] <- 'Pashchimi Singhbhum'
levels(rain$district)[levels(rain$district)=='East Midnapore'] <- 'Purba Medinipur'
levels(rain$district)[levels(rain$district)=='East Singbhum'] <- 'Purbi Singhbhum'
levels(rain$district)[levels(rain$district)=='Peddapalle'] <- 'Peddapalli'
levels(rain$district)[levels(rain$district)=='Poonch'] <- 'Punch'
levels(rain$district)[levels(rain$district)=='Purulia'] <- 'Puruliya'
levels(rain$district)[levels(rain$district)=='Rae Bareilly'] <- 'Rae Bareli'
levels(rain$district)[levels(rain$district)=='Rajanna Sircilla'] <- 'Rajanna'
levels(rain$district)[levels(rain$district)=='Rangareddy'] <- 'Ranga Reddy'
levels(rain$district)[levels(rain$district)=='Ri-Bhoi'] <- 'Ribhoi'
levels(rain$district)[levels(rain$district)=='Seraikela-Kharsawan'] <- 'Saraikela-Kharsawan'
levels(rain$district)[levels(rain$district)=='Shrawasti Nagar'] <- 'Shrawasti'
levels(rain$district)[levels(rain$district)=='Siddharth Nagar'] <- 'Siddharthnagar'
levels(rain$district)[levels(rain$district)=='Sibsagar'] <- 'Sivasagar'
levels(rain$district)[levels(rain$district)=='Sholapur'] <- 'Solapur'
levels(rain$district)[levels(rain$district)=='Sonepat'] <- 'Sonipat'
levels(rain$district)[levels(rain$district)=='Namchi'] <- 'South District'
levels(rain$district)[levels(rain$district)=='Sonepur'] <- 'Subarnapur'
levels(rain$district)[levels(rain$district)=='Garhwal Tehri'] <- 'Tehri Garhwal'
levels(rain$district)[levels(rain$district)=='Thenkasi'] <- 'Tenkasi'
levels(rain$district)[levels(rain$district)=='Nilgiris'] <- 'The Nilgiris'
levels(rain$district)[levels(rain$district)=='Tiruvallur'] <- 'Thiruvallur'
levels(rain$district)[levels(rain$district)=='Tiruvarur'] <- 'Thiruvarur'
levels(rain$district)[levels(rain$district)=='Toothukudi'] <- 'Thoothukkudi'
levels(rain$district)[levels(rain$district)=='Trichy'] <- 'Tiruchirappalli'
levels(rain$district)[levels(rain$district)=='Tirupattur'] <- 'Tirupathur'
levels(rain$district)[levels(rain$district)=='Kanker'] <- 'Uttar Bastar Kanker'
levels(rain$district)[levels(rain$district)=='North Dinajpur'] <- 'Uttar Dinajpur'
levels(rain$district)[levels(rain$district)=='Villupuram'] <- 'Viluppuram'
levels(rain$district)[levels(rain$district)=='Vishakhapatnam'] <- 'Visakhapatnam'
levels(rain$district)[levels(rain$district)=='Wynad'] <- 'Wayanad'
levels(rain$district)[levels(rain$district)=='Ysr District'] <- 'Y.S.R.'
levels(rain$district)[levels(rain$district)=='Yadgir'] <- 'Yadgiri'
levels(rain$district)[levels(rain$district)=='Yamuna Nagar'] <- 'Yamunanagar'
levels(rain$district)[levels(rain$district)=='Yeotmal'] <- 'Yavatmal'

# 643 districts in common

# Rename prod states to ins states

# Rename prod districts to ins districts

# Create ins/rain/prod dataframe
test <-merge(ins, rain, by=c("district", "state", "year"))
test <- inner_join(test, prod1997, by = c("district" = "district", "state" = "state", "year" = "year"))
test$f.it <- test$area.ins/test$area

test.glm <- glm(log(prod) ~ jul.ptdef + aug.ptdef + I(jul.ptdef**2) + I(aug.ptdef**2)
                + jul.ptdef*f.it + aug.ptdef*f.it + I(jul.ptdef**2)*f.it + I(aug.ptdef**2)*f.it + factor(year) + factor(district), data = test)

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
