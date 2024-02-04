# Script for Dissertation Project
# Author: Amy Tramontozzi
# Date: 18/10/2023

library(tidyverse)
library(readxl)
library(stringr)
library(dplyr)
library(stringdist)
library(expss)

# Load and tidy prod data 
## Summing area, prod, and yield across crops
prod1997 <- read_excel("prod1997.xls", guess_max = 20000)
#prod1997[is.na(prod1997)] <- 0
area <- select(prod1997, contains("Area"))
prod1997$area <- rowSums(area, na.rm = TRUE)
prod <- select(prod1997, contains("Production"))
prod1997$prod <- rowSums(prod, na.rm = TRUE)
yield <- select(prod1997, contains("Yield"))
prod1997$yield <- rowSums(yield, na.rm = TRUE)
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

prod1997$area <- prod1997$area/1000

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


ins <- ins %>%
  group_by(district, state, year) %>%
  summarise(across(everything(), sum))

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

rain$jan.rf <- rain$jan.rf/1000
rain$feb.rf <- rain$feb.rf/1000
rain$mar.rf <- rain$mar.rf/1000
rain$apr.rf <- rain$apr.rf/1000
rain$may.rf <- rain$may.rf/1000
rain$jun.rf <- rain$jun.rf/1000
rain$jul.rf <- rain$jul.rf/1000
rain$aug.rf <- rain$aug.rf/1000
rain$sep.rf <- rain$sep.rf/1000
rain$oct.rf <- rain$oct.rf/1000
rain$nov.rf <- rain$nov.rf/1000
rain$dec.rf <- rain$dec.rf/1000
# Load and tidy climate data
climate_data <- read_excel("climate_data_v5.xlsx")
climate_data$station <- str_to_title(climate_data$station)
climate_data$station <- as.factor(climate_data$station)
climate_data$stationID <- as.factor(climate_data$stationID)

# Load and tidy ICRISAT data
icrisat <- read_csv("ICRISAT-District Level Data.csv")
names(icrisat)[names(icrisat) == "Dist Name"] <- "district"
names(icrisat)[names(icrisat) == "State Name"] <- "state"
names(icrisat)[names(icrisat) == "Year"] <- "year"
icrisat[, 6:80][icrisat[, 6:80] == -1.00] <- NA
icr_area <- select(icrisat, contains("AREA"))
icrisat$icr2017_area <- rowSums(icr_area, na.rm = TRUE) 
icr_prod <- select(icrisat, contains("PRODUCTION"))
icrisat$icr2017_prod <- rowSums(icr_prod, na.rm = TRUE)
icr_yield <- select(icrisat, contains("YIELD"))
icrisat$icr2017_yield <- rowSums(icr_yield, na.rm = TRUE)
icrisat <- icrisat[,-6:-80]
icrisat <- icrisat[,-1:-3]

icrisat$state <- as.factor(icrisat$state)
icrisat$district <- as.factor(icrisat$district)

# Load and tidy irrigation data
irrigation <- read_csv("irrigation.csv")
names(irrigation)[names(irrigation) == "Dist Name"] <- "district"
names(irrigation)[names(irrigation) == "State Name"] <- "state"
irrigation[, 6:25][irrigation[, 6:25] == -1.00] <- NA
total_irr_area <- irrigation[,6:25]
irrigation$irr_area <- rowSums(total_irr_area, na.rm = TRUE)
irrigation <- irrigation[,-6:-25]
irrigation <- irrigation[,-1:-3]

# Load and tidy ICRISAT seasonal
icrisat_2015 <- read_excel("icrisat_2015_season.xlsx")
names(icrisat_2015)[names(icrisat_2015) == "DISTNAME"] <- "district"
names(icrisat_2015)[names(icrisat_2015) == "STNAME"] <- "state"
names(icrisat_2015)[names(icrisat_2015) == "YEAR"] <- "year"
icr15_area <- select(icrisat_2015, contains("KA"))
icrisat_2015$icr2015_area <- rowSums(icr15_area) 
icr15_prod <- select(icrisat_2015, contains("KQ"))
icrisat_2015$icr2015_prod <- rowSums(icr15_prod)

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
levels(ins$district)[levels(ins$district)=='Dantewada'] <- 'Dakshin Bastar Dantewada'
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
levels(ins$district)[levels(ins$district)=='Kanker'] <- 'Uttar Bastar Kanker'

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

prod1997missing.st <- levels(ins$state)[!(levels(ins$state) %in% levels(prod1997$state))]

levels(prod1997$state)[levels(prod1997$state)=='Andaman And Nicobar Islands'] <- 'Andaman and Nicobar Islands'
levels(prod1997$state)[levels(prod1997$state)=='Jammu And Kashmir'] <- 'Jammu and Kashmir'

# Rename prod districts to ins districts
prod1997missing.dis <- levels(ins$district)[!(levels(ins$district) %in% levels(prod1997$district))]

prod1997 <- prod1997 %>%
  mutate(district = if_else(state == "Karnataka" & district == "Bijapur", "Vijayapura", district))
prod1997$district <- as.factor(prod1997$district)

levels(prod1997$district)[levels(prod1997$district)=='Ahmadabad'] <- 'Ahmedabad'
levels(prod1997$district)[levels(prod1997$district)=='Ahmednagar'] <- 'Ahmadnagar'
levels(prod1997$district)[levels(prod1997$district)=='Anantapur'] <- 'Anantapuramu'
levels(prod1997$district)[levels(prod1997$district)=='Baleshwar'] <- 'Balasore'
levels(prod1997$district)[levels(prod1997$district)=='Banas Kantha'] <- 'Banaskantha'
levels(prod1997$district)[levels(prod1997$district)=='Barabanki'] <- 'Bara Banki'
levels(prod1997$district)[levels(prod1997$district)=='Boudh'] <- 'Baudh'
levels(prod1997$district)[levels(prod1997$district)=='Beed'] <- 'Bid'
levels(prod1997$district)[levels(prod1997$district)=='Beed'] <- 'Bijapur'
levels(prod1997$district)[levels(prod1997$district)=='Bengaluru Urban'] <- 'Bengalore'
levels(prod1997$district)[levels(prod1997$district)=='Sant Ravidas Nagar'] <- 'Bhadohi'
levels(prod1997$district)[levels(prod1997$district)=='Buldhana'] <- 'Buldana'
levels(prod1997$district)[levels(prod1997$district)=='Chamarajanagar'] <- 'ChamarajNagar'
levels(prod1997$district)[levels(prod1997$district)=='Charki Dadri'] <- 'Charkhi Dadri'
levels(prod1997$district)[levels(prod1997$district)=='Chikballapur'] <- 'Chikkaballapur'
levels(prod1997$district)[levels(prod1997$district)=='Chikmagalur'] <- 'Chikkamagaluru'
levels(prod1997$district)[levels(prod1997$district)=='Chittorgarh'] <- 'Chittaurgarh'
levels(prod1997$district)[levels(prod1997$district)=='Dantewada'] <- 'Dakshin Bastar Dantewada'
levels(prod1997$district)[levels(prod1997$district)=='Dinajpur Dakshin'] <- 'Dakshin Dinajpur'
levels(prod1997$district)[levels(prod1997$district)=='Dakshin Kannad'] <- 'Dakshina Kannada'
levels(prod1997$district)[levels(prod1997$district)=='Deogarh'] <- 'Debagarh'
levels(prod1997$district)[levels(prod1997$district)=='Dholpur'] <- 'Dhaulpur'
levels(prod1997$district)[levels(prod1997$district)=='Gangtok'] <- 'East District'
levels(prod1997$district)[levels(prod1997$district)=='Khandwa'] <- 'Khandwa (East Nimar)'
levels(prod1997$district)[levels(prod1997$district)=='Pauri Garhwal'] <- 'Garhwal'
levels(prod1997$district)[levels(prod1997$district)=='Gaurella-Pendra-Marwahi'] <- 'Gaurela-Pendra-Marwahi'
levels(prod1997$district)[levels(prod1997$district)=='Gondia'] <- 'Gondiya'
levels(prod1997$district)[levels(prod1997$district)=='Geyzing'] <- 'West District'
levels(prod1997$district)[levels(prod1997$district)=='Howrah'] <- 'Haora'
levels(prod1997$district)[levels(prod1997$district)=='Haridwar'] <- 'Hardwar'
levels(prod1997$district)[levels(prod1997$district)=='Hassan'] <- 'Hasan'
levels(prod1997$district)[levels(prod1997$district)=='Hooghly'] <- 'Hugli'
levels(prod1997$district)[levels(prod1997$district)=='Jagatsinghapur'] <- 'Jagatsinghpur'
levels(prod1997$district)[levels(prod1997$district)=='Jagitial'] <- 'Jagtial'
levels(prod1997$district)[levels(prod1997$district)=='Jalore'] <- 'Jalor'
levels(prod1997$district)[levels(prod1997$district)=='Jangoan'] <- 'Jangaon'
levels(prod1997$district)[levels(prod1997$district)=='Kabirdham'] <- 'Kabeerdham'
levels(prod1997$district)[levels(prod1997$district)=='Gulbarga'] <- 'Kalaburagi'
levels(prod1997$district)[levels(prod1997$district)=='Kamrup Metro'] <- 'Kamrup Metropolitan'
levels(prod1997$district)[levels(prod1997$district)=='Kanchipuram'] <- 'Kancheepuram'
levels(prod1997$district)[levels(prod1997$district)=='Kendujhar'] <- 'Keonjhar'
levels(prod1997$district)[levels(prod1997$district)=='Khargone'] <- 'Khargone (West Nimar)'
levels(prod1997$district)[levels(prod1997$district)=='Coochbehar'] <- 'Cooch Behar'
levels(prod1997$district)[levels(prod1997$district)=='Koderma'] <- 'Kodarma'
levels(prod1997$district)[levels(prod1997$district)=='Komaram Bheem Asifabad'] <- 'Komaram Bheem'
levels(prod1997$district)[levels(prod1997$district)=='Korea'] <- 'Koriya'
levels(prod1997$district)[levels(prod1997$district)=='Kushi Nagar'] <- 'Kushinagar'
levels(prod1997$district)[levels(prod1997$district)=='Maharajganj'] <- 'Mahrajganj'
levels(prod1997$district)[levels(prod1997$district)=='Maldah'] <- 'Malda'
levels(prod1997$district)[levels(prod1997$district)=='Marigaon'] <- 'Morigaon'
levels(prod1997$district)[levels(prod1997$district)=='Medchal'] <- 'Medchal-Malkajgiri'
levels(prod1997$district)[levels(prod1997$district)=='Mysore'] <- 'Mysuru'
levels(prod1997$district)[levels(prod1997$district)=='Nabarangpur'] <- 'Nabarangapur'
levels(prod1997$district)[levels(prod1997$district)=='Namchi'] <- 'South District'
levels(prod1997$district)[levels(prod1997$district)=='Narayanapet'] <- 'Narayanpet'
levels(prod1997$district)[levels(prod1997$district)=='Narsinghpur'] <- 'Narsimhapur'
levels(prod1997$district)[levels(prod1997$district)=='North And Middle Andaman'] <- 'North and Middle Andaman'
levels(prod1997$district)[levels(prod1997$district)=='Mangan'] <- 'North District'
levels(prod1997$district)[levels(prod1997$district)=='24 Paraganas North'] <- 'North 24 Paraganas'
levels(prod1997$district)[levels(prod1997$district)=='Medinipur West'] <- 'Paschim Medinipur'
levels(prod1997$district)[levels(prod1997$district)=='Pondicherry'] <- 'Puducherry'
levels(prod1997$district)[levels(prod1997$district)=='Poonch'] <- 'Punch'
levels(prod1997$district)[levels(prod1997$district)=='Medinipur East'] <- 'Purba Medinipur'
levels(prod1997$district)[levels(prod1997$district)=='Purulia'] <- 'Puruliya'
levels(prod1997$district)[levels(prod1997$district)=='Rajauri'] <- 'Rajouri'
levels(prod1997$district)[levels(prod1997$district)=='Rangareddi'] <- 'Ranga Reddy'
levels(prod1997$district)[levels(prod1997$district)=='Ri Bhoi'] <- 'Ribhoi'
levels(prod1997$district)[levels(prod1997$district)=='Rudra Prayag'] <- 'Rudraprayag'
levels(prod1997$district)[levels(prod1997$district)=='Sahebganj'] <- 'Sahibganj'
levels(prod1997$district)[levels(prod1997$district)=='Sant Kabeer Nagar'] <- 'Sant Kabir Nagar'
levels(prod1997$district)[levels(prod1997$district)=='Saraikela Kharsawan'] <- 'Saraikela-Kharsawan'
levels(prod1997$district)[levels(prod1997$district)=='Shimoga'] <- 'Shivamogga'
levels(prod1997$district)[levels(prod1997$district)=='Shravasti'] <- 'Shrawasti'
levels(prod1997$district)[levels(prod1997$district)=='Siddharth Nagar'] <- 'Siddharthnagar'
levels(prod1997$district)[levels(prod1997$district)=='Sonepur'] <- 'Subarnapur'
levels(prod1997$district)[levels(prod1997$district)=='South Andamans'] <- 'South Andaman'
levels(prod1997$district)[levels(prod1997$district)=='24 Paraganas South'] <- 'South 24 Parganas'
levels(prod1997$district)[levels(prod1997$district)=='Ganganagar'] <- 'Sri Ganganagar'
levels(prod1997$district)[levels(prod1997$district)=='Thenkasi'] <- 'Tenkasi'
levels(prod1997$district)[levels(prod1997$district)=='Thoothukudi'] <- 'Thoothukkudi'
levels(prod1997$district)[levels(prod1997$district)=='Tumkur'] <- 'Tumakuru'
levels(prod1997$district)[levels(prod1997$district)=='Udam Singh Nagar'] <- 'Udham Singh Nagar'
levels(prod1997$district)[levels(prod1997$district)=='Kanker'] <- 'Uttar Bastar Kanker'
levels(prod1997$district)[levels(prod1997$district)=='Dinajpur Uttar'] <- 'Uttar Dinajpur'
levels(prod1997$district)[levels(prod1997$district)=='Uttar Kannad'] <- 'Uttara Kannada'
levels(prod1997$district)[levels(prod1997$district)=='Uttar Kashi'] <- 'Uttarkashi'
levels(prod1997$district)[levels(prod1997$district)=='Villupuram'] <- 'Viluppuram'
levels(prod1997$district)[levels(prod1997$district)=='Visakhapatanam'] <- 'Visakhapatnam'
levels(prod1997$district)[levels(prod1997$district)=='Kadapa'] <- 'Y.S.R.'
levels(prod1997$district)[levels(prod1997$district)=='Yadgir'] <- 'Yadgiri'

# Rename ICRISAT districts
icrmissing.dis <- levels(ins$district)[!(levels(ins$district) %in% levels(icrisat$district))]

levels(icrisat$district)[levels(icrisat$district)=='Ahmednagar'] <- 'Ahmadnagar'
levels(icrisat$district)[levels(icrisat$district)=='Almorah'] <- 'Almora'
levels(icrisat$district)[levels(icrisat$district)=='Amethi C.S.M.Nagar'] <- 'Amethi'
levels(icrisat$district)[levels(icrisat$district)=='Amarawati'] <- 'Amravati'
levels(icrisat$district)[levels(icrisat$district)=='Amroha J.B.Fulenagar'] <- 'Amroha'
levels(icrisat$district)[levels(icrisat$district)=='Anantapur'] <- 'Anantapuramu'
levels(icrisat$district)[levels(icrisat$district)=='Angul'] <- 'Anugul'
levels(icrisat$district)[levels(icrisat$district)=='Bagalkote'] <- 'Bagalkot'
levels(icrisat$district)[levels(icrisat$district)=='Bagpat'] <- 'Baghpat'
levels(icrisat$district)[levels(icrisat$district)=='Barabanki'] <- 'Bara Banki'
levels(icrisat$district)[levels(icrisat$district)=='Boudh'] <- 'Baudh'
levels(icrisat$district)[levels(icrisat$district)=='Beed'] <- 'Bid'
levels(icrisat$district)[levels(icrisat$district)=='Bemetra'] <- 'Bemetara'
levels(icrisat$district)[levels(icrisat$district)=='Bangalore(Rural)'] <- 'Bengalore Rural'
levels(icrisat$district)[levels(icrisat$district)=='Bangalore(Urban)'] <- 'Bengalore'
levels(icrisat$district)[levels(icrisat$district)=='Bhadradri Kothagudam'] <- 'Bhadradri'
levels(icrisat$district)[levels(icrisat$district)=='Buland Shahar'] <- 'Bulandshahr'
levels(icrisat$district)[levels(icrisat$district)=='Buldhana'] <- 'Buldana'
levels(icrisat$district)[levels(icrisat$district)=='Chamaraja Nagar'] <- 'ChamarajNagar'
levels(icrisat$district)[levels(icrisat$district)=='Champavat'] <- 'Champawat'
levels(icrisat$district)[levels(icrisat$district)=='Chengalpattu MGR Kancheepuram'] <- 'Chengalpattu'
levels(icrisat$district)[levels(icrisat$district)=='Chickmagalur'] <- 'Chikkamagaluru'
levels(icrisat$district)[levels(icrisat$district)=='Chittorgarh'] <- 'Chittaurgarh'
levels(icrisat$district)[levels(icrisat$district)=='Dantewara'] <- 'Dakshin Bastar Dantewada'
levels(icrisat$district)[levels(icrisat$district)=='Davanagere'] <- 'Davangere'
levels(icrisat$district)[levels(icrisat$district)=='Deogarh'] <- 'Debagarh'
levels(icrisat$district)[levels(icrisat$district)=='Devghar Deogarh'] <- 'Deoghar'
levels(icrisat$district)[levels(icrisat$district)=='Dholpur'] <- 'Dhaulpur'
levels(icrisat$district)[levels(icrisat$district)=='North Cachar Hil'] <- 'Dima Hasao'
levels(icrisat$district)[levels(icrisat$district)=='Dindigul Anna'] <- 'Dindigul'
levels(icrisat$district)[levels(icrisat$district)=='Khandwa'] <- 'Khandwa (East Nimar)'
levels(icrisat$district)[levels(icrisat$district)=='Eranakulam'] <- 'Ernakulam'
levels(icrisat$district)[levels(icrisat$district)=='Gariaband'] <- 'Gariyaband'
levels(icrisat$district)[levels(icrisat$district)=='G.B.Nagar'] <- 'Gautam Buddha Nagar'
levels(icrisat$district)[levels(icrisat$district)=='Gondia'] <- 'Gondiya'
levels(icrisat$district)[levels(icrisat$district)=='Howrah'] <- 'Haora'
levels(icrisat$district)[levels(icrisat$district)=='Haridwar'] <- 'Hardwar'
levels(icrisat$district)[levels(icrisat$district)=='Hassan'] <- 'Hasan'
levels(icrisat$district)[levels(icrisat$district)=='Hissar'] <- 'Hisar'
levels(icrisat$district)[levels(icrisat$district)=='Hooghly'] <- 'Hugli'
levels(icrisat$district)[levels(icrisat$district)=='Jagatsinghapur'] <- 'Jagatsinghpur'
levels(icrisat$district)[levels(icrisat$district)=='Jagityal'] <- 'Jagtial'
levels(icrisat$district)[levels(icrisat$district)=='Jalore'] <- 'Jalor'
levels(icrisat$district)[levels(icrisat$district)=='Janagaon'] <- 'Jangaon'
levels(icrisat$district)[levels(icrisat$district)=='Janjgir'] <- 'Janjgir-Champa'
levels(icrisat$district)[levels(icrisat$district)=='ayashankar Bhuppaly'] <- 'Jayashankar'
levels(icrisat$district)[levels(icrisat$district)=='Jogulamba Gadwal'] <- 'Jogulamba'
levels(icrisat$district)[levels(icrisat$district)=='Kawardha'] <- 'Kabeerdham'
levels(icrisat$district)[levels(icrisat$district)=='Gulbarga'] <- 'Kalaburagi'
levels(icrisat$district)[levels(icrisat$district)=='Kamrup(Metro)'] <- 'Kamrup Metropolitan'
levels(icrisat$district)[levels(icrisat$district)=='Phulbani(Kandhamal)'] <- 'Kandhamal'
levels(icrisat$district)[levels(icrisat$district)=='Kanker'] <- 'Uttar Bastar Kanker'
levels(icrisat$district)[levels(icrisat$district)=='Kanyakumari'] <- 'Kanniyakumari'
levels(icrisat$district)[levels(icrisat$district)=='Karoli'] <- 'Karauli'
levels(icrisat$district)[levels(icrisat$district)=='Kasganj Khansi Ram Nagar'] <- 'Kasganj'
levels(icrisat$district)[levels(icrisat$district)=='Khargone'] <- 'Khargone (West Nimar)'
levels(icrisat$district)[levels(icrisat$district)=='Khurda'] <- 'Khordha'
levels(icrisat$district)[levels(icrisat$district)=='Khodrama Koderma'] <- 'Kodarma'
levels(icrisat$district)[levels(icrisat$district)=='Kumurambheem Asifabad'] <- 'Komaram Bheem'
levels(icrisat$district)[levels(icrisat$district)=='Kushi Nagar Padrauna'] <- 'Kushinagar'
levels(icrisat$district)[levels(icrisat$district)=='Lohardagga'] <- 'Lohardaga'
levels(icrisat$district)[levels(icrisat$district)=='Mahrajgani'] <- 'Mahrajganj'
levels(icrisat$district)[levels(icrisat$district)=='Mahasmund'] <- 'Mahasamund'
levels(icrisat$district)[levels(icrisat$district)=='Mahabubnagar'] <- 'Mahbubnagar'
levels(icrisat$district)[levels(icrisat$district)=='Marigaon'] <- 'Morigaon'
levels(icrisat$district)[levels(icrisat$district)=='Mayurbhanja'] <- 'Mayurbhanj'
levels(icrisat$district)[levels(icrisat$district)=='Malkangiri'] <- 'Medchal-Malkajgiri'
levels(icrisat$district)[levels(icrisat$district)=='Mirzpur'] <- 'Mirzapur'
levels(icrisat$district)[levels(icrisat$district)=='Mungli'] <- 'Mungeli'
levels(icrisat$district)[levels(icrisat$district)=='Mysore'] <- 'Mysuru'
levels(icrisat$district)[levels(icrisat$district)=='Nawarangpur'] <- 'Nabarangapur'
levels(icrisat$district)[levels(icrisat$district)=='Narsinghpur'] <- 'Narsimhapur'
levels(icrisat$district)[levels(icrisat$district)=='Nasik'] <- 'Nashik'
levels(icrisat$district)[levels(icrisat$district)=='24 - Paraganas North'] <- 'North 24 Paraganas'
levels(icrisat$district)[levels(icrisat$district)=='Palamau'] <- 'Palamu'
levels(icrisat$district)[levels(icrisat$district)=='West Midnapore'] <- 'Paschim Medinipur'
levels(icrisat$district)[levels(icrisat$district)=='Singhbhum West'] <- 'Paschimi Singhbhum'
levels(icrisat$district)[levels(icrisat$district)=='Peddapally'] <- 'Peddapalli'
levels(icrisat$district)[levels(icrisat$district)=='Perambular'] <- 'Perambalur'
levels(icrisat$district)[levels(icrisat$district)=='Pithorgarh'] <- 'Pithoragarh'
levels(icrisat$district)[levels(icrisat$district)=='East Midnapore Purba Midnapore'] <- 'Purba Medinipur'
levels(icrisat$district)[levels(icrisat$district)=='Singhbhum East'] <- 'Purbi Singhbhum'
levels(icrisat$district)[levels(icrisat$district)=='Purulia'] <- 'Puruliya'
levels(icrisat$district)[levels(icrisat$district)=='Rae - Bareily'] <- 'Rae Bareli'
levels(icrisat$district)[levels(icrisat$district)=='Rajanna Siricilla'] <- 'Rajanna'
levels(icrisat$district)[levels(icrisat$district)=='Ramanagaram'] <- 'Ramanagara'
levels(icrisat$district)[levels(icrisat$district)=='Ramananthapuram'] <- 'Ramanathapuram'
levels(icrisat$district)[levels(icrisat$district)=='Ramgadh'] <- 'Ramgarh'
levels(icrisat$district)[levels(icrisat$district)=='Rangareddy'] <- 'Ranga Reddy'
levels(icrisat$district)[levels(icrisat$district)=='Sahebganj'] <- 'Sahibganj'
levels(icrisat$district)[levels(icrisat$district)=='Santh Kabir Nagar'] <- 'Sant Kabir Nagar'
levels(icrisat$district)[levels(icrisat$district)=='Sariakela Kharsawan'] <- 'Saraikela-Kharsawan'
levels(icrisat$district)[levels(icrisat$district)=='Sheopur Kalan'] <- 'Sheopur'
levels(icrisat$district)[levels(icrisat$district)=='Shimoge'] <- 'Shivamogga'
levels(icrisat$district)[levels(icrisat$district)=='Shravasti'] <- 'Shrawasti'
levels(icrisat$district)[levels(icrisat$district)=='Sidharthnagar'] <- 'Siddharthnagar'
levels(icrisat$district)[levels(icrisat$district)=='Sivagangai Pasumpon'] <- 'Sivaganga'
levels(icrisat$district)[levels(icrisat$district)=='Sibsagar'] <- 'Sivasagar'
levels(icrisat$district)[levels(icrisat$district)=='Sonepur'] <- 'Subarnapur'
levels(icrisat$district)[levels(icrisat$district)=='Sonepat'] <- 'Sonipat'
levels(icrisat$district)[levels(icrisat$district)=='24 - Paraganas South'] <- 'South 24 Parganas'
levels(icrisat$district)[levels(icrisat$district)=='S.P.S.Nellore'] <- 'Spsr Nellore'
levels(icrisat$district)[levels(icrisat$district)=='Ganganagar'] <- 'Sri Ganganagar'
levels(icrisat$district)[levels(icrisat$district)=='Tiruvarur'] <- 'Thiruvarur'
levels(icrisat$district)[levels(icrisat$district)=='Thirunelveli'] <- 'Tirunelveli'
levels(icrisat$district)[levels(icrisat$district)=='Tiruchirapalli Trichy'] <- 'Tiruchirappalli'
levels(icrisat$district)[levels(icrisat$district)=='Thiruvallur'] <- 'Thiruvallur'
levels(icrisat$district)[levels(icrisat$district)=='Thiruppur'] <- 'Tiruppur'
levels(icrisat$district)[levels(icrisat$district)=='Thiruvannamalai'] <- 'Tiruvannamalai'
levels(icrisat$district)[levels(icrisat$district)=='Tumkur'] <- 'Tumakuru'
levels(icrisat$district)[levels(icrisat$district)=='Uttar Kashi"'] <- 'Uttarkashi'
levels(icrisat$district)[levels(icrisat$district)=='Bijapur'] <- 'Vijayapura'
levels(icrisat$district)[levels(icrisat$district)=='Villupuram'] <- 'Viluppuram'
levels(icrisat$district)[levels(icrisat$district)=='Virudhunagar Kamarajar'] <- 'Virudhunagar'
levels(icrisat$district)[levels(icrisat$district)=='Yadadri Bhuvanagiri'] <- 'Yadadri'
levels(icrisat$district)[levels(icrisat$district)=='Yadagiri'] <- 'Yadgiri'
levels(icrisat$district)[levels(icrisat$district)=='Yeotmal'] <- 'Yavatmal'


# Find the max area by district from 2018-2021. 74 f.it > 1
total <- total %>%
  group_by(district) %>%
  mutate(area_max_4 = max(area)) %>%
  ungroup()

# Find the max area by district from 2018-2021. 139 f.it > 1
prod1997 <- prod1997 %>%
  group_by(district) %>%
  mutate(area_max97 = max(area)) %>%
  ungroup()

# Create ins/rain/prod dataframe
ins_rain <- merge(ins, rain, by=c("district", "state", "year"))
total <- merge(ins_rain, prod1997, by=c("district", "state", "year"))

# The below code is used to write a CSV for the dataset with the matched names in ICRISAT
total <- merge(total, icrisat, by=c("district", "state"))
total$f.it <- total$area.ins/(total$icr2017_area)

# The code below is to replace NAs with . for STATA
total[, 4:54][is.na(total[, 4:54])] <- '.'
# ICRISAT 2017 total merge
full <- merge(total, icrisat, by=c("district", "state"))
full$f.it <- full$area.ins/(full$icr2017_area)

## Using this f.it delivers all values lower than zero. However,
## area us likely overestimated because it is annual, not seasonal.

# ICRISAT 2015 total merge
icrisat_2015 <- icrisat_2015 %>%
  filter(year == 2015)
total_2015 <- merge(total, icrisat_2015, by=c("district", "state"))
total_2015$f.it15 <- total_2015$area.ins/(total_2015$icr2015_area)

total_2015$icr2015_area[total_2015$f.it15==Inf] <- 0.43
## Using this f.it15 gives 133 f.it15 > 1 (better to at least use max from 2018-2021).
## I think primarily, should use the full one.

## Using this is from the original prod1997 area, 159 f.it > 1
total$f.it <- total$area.ins/(total$area)
#total$f.it[total$f.it > 1] <- 1.00 # only if no other solution

# GLMs
rfbins.glm <- glm(log(prod) ~ factor(jun.rain_type) + factor(jul.rain_type) +  
                    f.it + I(jun.rf*f.it) + I(jul.rf*f.it) + factor(year.x) 
                  + factor(district), data = full)

rf.glm <- glm(log(prod) ~ jun.rf + jul.rf + I(jun.rf**2) + I(jul.rf**2) +
                f.it + I(jun.rf*f.it) + I(jul.rf*f.it) + factor(year.x) + factor(district), data = full)
# Rainfall categories 

full$jun.rain_type <- as.factor(ifelse(full$jun.ptdef == -1, 'No Rain',
                                 ifelse(full$jun.ptdef >= 0.20, 'Excess',
                                 ifelse(full$jun.ptdef >= -.19 & full$jun.ptdef <= .19, 'Normal',
                                 ifelse(full$jun.ptdef <= -.20, 'Deficient', 'Other')))))
full$jul.rain_type <- as.factor(ifelse(full$jul.ptdef == -1, 'No Rain',
                                 ifelse(full$jul.ptdef >= 0.20, 'Excess',
                                 ifelse(full$jul.ptdef >= -.19 & full$jul.ptdef <= .19, 'Normal',
                                 ifelse(full$jul.ptdef <= -.20, 'Deficient', 'Other')))))
total$aug.rain_type <- as.factor(ifelse(total$aug.ptdef == -1, 'No Rain',
                                 ifelse(total$aug.ptdef >= 0.20, 'Excess',
                                 ifelse(total$aug.ptdef >= -.19 & total$aug.ptdef <= .19, 'Normal',
                                 ifelse(total$aug.ptdef <= -.20, 'Deficient', 'Other')))))
total$sep.rain_type <- as.factor(ifelse(total$sep.ptdef == -1, 'No Rain',
                                 ifelse(total$sep.ptdef >= 0.20, 'Excess',
                                 ifelse(total$sep.ptdef >= -.19 & total$sep.ptdef <= .19, 'Normal',
                                 ifelse(total$sep.ptdef <= -.20, 'Deficient', 'Other')))))

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

# Create variable labels
total = apply_labels(total,
                     district = "District name",
                     state = "State name",
                     year = "Year from 2018-2022",
                     ins.units = "Insurance units (Village/Village Panchayat 
                     level for major crops and a size above the level of 
                     Village/Village Panchayat for other crops)",
                     farmers = "Individual insured farmers",
                     loanee = "Loanee applications",
                     nonloanee = "Non-loanee applications",
                     area.ins = "Area (thousand hect.) insured",
                     farmers.premium = "Farmers' premium in Lac.",
                     state.premium = "State premium in lac.",
                     goi.premium = "Government of India premium in lac.",
                     gross.premium = "Total premium in lac.",
                     sum.insured = "Sum insured in lac.",
                     male.pt = "% male",
                     female.pt = "% female",
                     othergen.pt = "% other gender",
                     sc.pt = "% scheduled caste",
                     st.pt = "% scheduled tribe",
                     obc.pt = "% other backward class",
                     gen.pt = "% forward/general class",
                     marginal.pt = "% marginal (below 1.00 hectare) farmers",
                     small.pt = "% small (1.00-2.00 hectares) farmers",
                     othertype.pt = "% other size farmers",
                     jan.rf = "January rainfall (in mm)",
                     jan.ptdef = "% departures of observed rainfall from January district 
                     normals",
                     feb.rf = "Februraru rainfall (in mm)",
                     feb.ptdef = "% departures of observed rainfall from February district 
                     normals",
                     mar.rf = "March rainfall (in mm)",
                     mar.ptdef = "% departures of observed rainfall from March district 
                     normals",
                     apr.rf = "April rainfall (in mm)",
                     apr.ptdef = "% departures of observed rainfall from April district 
                    normals",
                     may.rf = "May rainfall (in mm)",
                     may.ptdef = "% departures of observed rainfall from May district 
                    normals",
                     jun.rf = "June rainfall (in mm)",
                     jun.ptdef = "% departures of observed rainfall from June district 
                    normals",
                     jul.rf = "July rainfall (in mm)",
                     jul.ptdef = "% departures of observed rainfall from July district 
                    normals",
                     aug.rf = "August rainfall (in mm)",
                     aug.ptdef = "% departures of observed rainfall from August district 
                    normals",
                     sep.rf = "September rainfall (in mm)",
                     sep.ptdef = "% departures of observed rainfall from September district 
                    normals",
                     oct.rf = "October rainfall (in mm)",
                     oct.ptdef = "% departures of observed rainfall from October district 
                    normals",
                     nov.rf = "November rainfall (in mm)",
                     nov.ptdef = "% departures of observed rainfall from November district 
                    normals",
                     dec.rf = "December rainfall (in mm)",
                     dec.ptdef = "% departures of observed rainfall from December district 
                    normals",
                     area = "Production Area (Thousand hect.)",
                     prod = "Production (Tonne)",
                     yield = "Yield (Tonne/Hectare)",
                     f.it = "Fraction insured (Thousand Hect./Thousand Hect."
)
