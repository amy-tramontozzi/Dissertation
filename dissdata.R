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
#yield <- select(prod1997, contains("Yield"))
#prod1997$yield <- rowSums(yield, na.rm = TRUE)

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
prod1997$prod <- prod1997$prod/1000

prod2021 <- filter(prod1997, year > 2017)
prod1997 <- filter(prod1997, year < 2018)

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

rain <- select(rain, !contains(".ptdef"))
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

# Load and tidy ICRISAT data
## to use to fix area
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

icrisat$state <- as.factor(icrisat$state)
icrisat$district <- as.factor(icrisat$district)


# Load and tidy ICRISAT full precip data 1958-2015
## TDO NOT USE FOR PRECIP NORMALS, INSTEAD USE BIG RAIN
icrisat_rain <- read_csv("icrisat-normal-rain.csv")
names(icrisat_rain)[names(icrisat_rain) == "JANUARY NORMAL RAINFALL (Millimeters)"] <- "jan.normal"
names(icrisat_rain)[names(icrisat_rain) == "FEBRUARY NORMAL RAINFALL (Millimeters)"] <- "feb.normal"
names(icrisat_rain)[names(icrisat_rain) == "MARCH NORMAL RAINFALL (Millimeters)"] <- "mar.normal"
names(icrisat_rain)[names(icrisat_rain) == "APRIL NORMAL RAINFALL (Millimeters)"] <- "apr.normal"
names(icrisat_rain)[names(icrisat_rain) == "MAY NORMAL RAINFALL (Millimeters)"] <- "may.normal"
names(icrisat_rain)[names(icrisat_rain) == "JUNE NORMAL RAINFALL (Millimeters)"] <- "jun.normal"
names(icrisat_rain)[names(icrisat_rain) == "JULY NORMAL RAINFALL (Millimeters)"] <- "jul.normal"
names(icrisat_rain)[names(icrisat_rain) == "AUGUST NORMAL RAINFALL (Millimeters)"] <- "aug.normal"
names(icrisat_rain)[names(icrisat_rain) == "SEPTEMBER NORMAL RAINFALL (Millimeters)"] <- "sep.normal"
names(icrisat_rain)[names(icrisat_rain) == "OCTOBER NORMAL RAINFALL (Millimeters)"] <- "oct.normal"
names(icrisat_rain)[names(icrisat_rain) == "NOVEMBER NORMAL RAINFALL (Millimeters)"] <- "nov.normal"
names(icrisat_rain)[names(icrisat_rain) == "DECEMBER NORMAL RAINFALL (Millimeters)"] <- "dec.normal"
names(icrisat_rain)[names(icrisat_rain) == "ANNUAL NORMAL RAINFALL (Millimeters)"] <- "annual.normal"
names(icrisat_rain)[names(icrisat_rain) == "State Name"] <- "state"
names(icrisat_rain)[names(icrisat_rain) == "Dist Name"] <- "district"
icrisat_rain$district <- as.factor(icrisat_rain$district)

icrisat_rain <- merge(icrisat, icrisat_rain, by=c("Dist Code"))
icrisat_rain <- icrisat_rain[,-1:-3]
icrisat_rain <- icrisat_rain[,-6:-8]
names(icrisat_rain)[names(icrisat_rain) == "state.x"] <- "state"
names(icrisat_rain)[names(icrisat_rain) == "district.x"] <- "district"

# change from mm to meters of rf per month normals
icrisat_rain$jan.normal <- icrisat_rain$jan.normal/1000
icrisat_rain$feb.normal <- icrisat_rain$feb.normal/1000
icrisat_rain$mar.normal <- icrisat_rain$mar.normal/1000
icrisat_rain$apr.normal <- icrisat_rain$apr.normal/1000
icrisat_rain$may.normal <- icrisat_rain$may.normal/1000
icrisat_rain$jun.normal <- icrisat_rain$jun.normal/1000
icrisat_rain$jul.normal <- icrisat_rain$jul.normal/1000
icrisat_rain$aug.normal <- icrisat_rain$aug.normal/1000
icrisat_rain$sep.normal <- icrisat_rain$sep.normal/1000
icrisat_rain$oct.normal <- icrisat_rain$oct.normal/1000
icrisat_rain$nov.normal <- icrisat_rain$nov.normal/1000
icrisat_rain$dec.normal <- icrisat_rain$dec.normal/1000
icrisat_rain$annual.normal <- icrisat_rain$annual.normal/1000

# Import big rain to make original rain normals

big_rain <- read_csv("for_unique_rain_normals.csv")
names(big_rain)[names(big_rain) == "JANUARY PERCIPITATION (Millimeters)"] <- "jan.rf"
names(big_rain)[names(big_rain) == "FEBRUARY PERCIPITATION (Millimeters)"] <- "feb.rf"
names(big_rain)[names(big_rain) == "MARCH PERCIPITATION (Millimeters)"] <- "mar.rf"
names(big_rain)[names(big_rain) == "APRIL PERCIPITATION (Millimeters)"] <- "apr.rf"
names(big_rain)[names(big_rain) == "MAY PERCIPITATION (Millimeters)"] <- "may.rf"
names(big_rain)[names(big_rain) == "JUNE PERCIPITATION (Millimeters)"] <- "jun.rf"
names(big_rain)[names(big_rain) == "JULY PERCIPITATION (Millimeters)"] <- "jul.rf"
names(big_rain)[names(big_rain) == "AUGUST PERCIPITATION (Millimeters)"] <- "aug.rf"
names(big_rain)[names(big_rain) == "SEPTEMBER PERCIPITATION (Millimeters)"] <- "sep.rf"
names(big_rain)[names(big_rain) == "OCTOBER PERCIPITATION (Millimeters)"] <- "oct.rf"
names(big_rain)[names(big_rain) == "NOVEMBER PERCIPITATION (Millimeters)"] <- "nov.rf"
names(big_rain)[names(big_rain) == "DECEMBER PERCIPITATION (Millimeters)"] <- "dec.rf"
names(big_rain)[names(big_rain) == "ANNUAL PERCIPITATION (Millimeters)"] <- "annual.rf"
names(big_rain)[names(big_rain) == "State Name"] <- "state"
names(big_rain)[names(big_rain) == "Dist Name"] <- "district"
big_rain$district <- as.factor(big_rain$district)

big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(jan_mean = mean(jan.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(feb_mean = mean(feb.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(mar_mean = mean(mar.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(apr_mean = mean(apr.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(may_mean = mean(may.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(jun_mean = mean(jun.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(jul_mean = mean(jul.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(aug_mean = mean(aug.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(sep_mean = mean(sep.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(oct_mean = mean(oct.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(nov_mean = mean(nov.rf, na.rm = TRUE))
big_rain <- big_rain %>% filter(Year > 1985) %>% group_by(district) %>%mutate(dec_mean = mean(dec.rf, na.rm = TRUE))

names(big_rain)[names(big_rain) == "Year"] <- "year"

big_rain$jan.rf <- big_rain$jan.rf/1000
big_rain$feb.rf <- big_rain$feb.rf/1000
big_rain$mar.rf <- big_rain$mar.rf/1000
big_rain$apr.rf <- big_rain$apr.rf/1000
big_rain$may.rf <- big_rain$may.rf/1000
big_rain$jun.rf <- big_rain$jun.rf/1000
big_rain$jul.rf <- big_rain$jul.rf/1000
big_rain$aug.rf <- big_rain$aug.rf/1000
big_rain$sep.rf <- big_rain$sep.rf/1000
big_rain$oct.rf <- big_rain$oct.rf/1000
big_rain$nov.rf <- big_rain$nov.rf/1000
big_rain$dec.rf <- big_rain$dec.rf/1000
big_rain$jan_mean <- big_rain$jan_mean/1000
big_rain$feb_mean <- big_rain$feb_mean/1000
big_rain$mar_mean <- big_rain$mar_mean/1000
big_rain$apr_mean <- big_rain$apr_mean/1000
big_rain$may_mean <- big_rain$may_mean/1000
big_rain$jun_mean <- big_rain$jun_mean/1000
big_rain$jul_mean <- big_rain$jul_mean/1000
big_rain$aug_mean <- big_rain$aug_mean/1000
big_rain$sep_mean <- big_rain$sep_mean/1000
big_rain$oct_mean <- big_rain$oct_mean/1000
big_rain$nov_mean <- big_rain$nov_mean/1000
big_rain$dec_mean <- big_rain$dec_mean/1000

# Need to fix this so that it is columns not just year
icrisat_normals <- merge(big_rain, icrisat, by=c("Dist Code"))
icrisat_normals <- icrisat_normals[,-30:-33]
names(icrisat_normals)[names(icrisat_normals) == "state.x"] <- "state"
names(icrisat_normals)[names(icrisat_normals) == "district.x"] <- "district"
names(icrisat_normals)[names(icrisat_normals) == "State Code.x"] <- "State Code"
names(icrisat_normals)[names(icrisat_normals) == "year.x"] <- "year"

common_for_all <- icrisat_normals[c(1,3,18:32)]
common_for_all <- distinct(common_for_all)
dist_code <- icrisat_normals[c(1,4,5)]
dist_code <- distinct(dist_code)
icrisat_normals <- icrisat_normals[-c(3,18:32)]


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
levels(ins$district)[levels(ins$district)=='Thiruvarur II'] <- 'Thiruvarur'
levels(ins$district)[levels(ins$district)=='Visakhapatanam'] <- 'Visakhapatnam'
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
icrmissing.dis <- levels(ins$district)[!(levels(ins$district) %in% levels(icrisat_normals$district))]

levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Ahmednagar'] <- 'Ahmadnagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Almorah'] <- 'Almora'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Amethi C.S.M.Nagar'] <- 'Amethi'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Amarawati'] <- 'Amravati'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Amroha J.B.Fulenagar'] <- 'Amroha'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Anantapur'] <- 'Anantapuramu'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Angul'] <- 'Anugul'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bagalkote'] <- 'Bagalkot'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bagpat'] <- 'Baghpat'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bolangir'] <- 'Balangir'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Barabanki'] <- 'Bara Banki'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Boudh'] <- 'Baudh'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bellary'] <- 'Ballari'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Belgaum'] <- 'Belagavi'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Santh Ravi Das Nagar Bhadoi'] <- 'Bhadohi'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Beed'] <- 'Bid'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bilashpur'] <- 'Bilaspur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bemetra'] <- 'Bemetara'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bangalore(Rural)'] <- 'Bengalore Rural'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bangalore(Urban)'] <- 'Bengalore'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Bhadradri Kothagudam'] <- 'Bhadradri'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Buland Shahar'] <- 'Bulandshahr'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Buldhana'] <- 'Buldana'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Chamaraja Nagar'] <- 'ChamarajNagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Champavat'] <- 'Champawat'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Chengalpattu MGR Kancheepuram'] <- 'Chengalpattu'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Chickmagalur'] <- 'Chikkamagaluru'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Chittorgarh'] <- 'Chittaurgarh'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Dantewara'] <- 'Dakshin Bastar Dantewada'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Davanagere'] <- 'Davangere'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Deogarh'] <- 'Debagarh'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Devghar Deogarh'] <- 'Deoghar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Dholpur'] <- 'Dhaulpur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='North Cachar Hil'] <- 'Dima Hasao'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Dindigul Anna'] <- 'Dindigul'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Khandwa'] <- 'Khandwa (East Nimar)'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Eranakulam'] <- 'Ernakulam'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Gariaband'] <- 'Gariyaband'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='G.B.Nagar'] <- 'Gautam Buddha Nagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Gondia'] <- 'Gondiya'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Howrah'] <- 'Haora'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Haridwar'] <- 'Hardwar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Hassan'] <- 'Hasan'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Hissar'] <- 'Hisar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Hooghly'] <- 'Hugli'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Jagatsinghapur'] <- 'Jagatsinghpur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Jagityal'] <- 'Jagtial'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Jalore'] <- 'Jalor'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Janagaon'] <- 'Jangaon'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Janjgir'] <- 'Janjgir-Champa'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='ayashankar Bhuppaly'] <- 'Jayashankar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Jogulamba Gadwal'] <- 'Jogulamba'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Kawardha'] <- 'Kabeerdham'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Gulbarga'] <- 'Kalaburagi'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Kamrup(Metro)'] <- 'Kamrup Metropolitan'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Phulbani(Kandhamal)'] <- 'Kandhamal'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Kanker'] <- 'Uttar Bastar Kanker'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Kanyakumari'] <- 'Kanniyakumari'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Karoli'] <- 'Karauli'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Kasganj Khansi Ram Nagar'] <- 'Kasganj'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Khargone'] <- 'Khargone (West Nimar)'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Khurda'] <- 'Khordha'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Khodrama Koderma'] <- 'Kodarma'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Kumurambheem Asifabad'] <- 'Komaram Bheem'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Kushi Nagar Padrauna'] <- 'Kushinagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Lohardagga'] <- 'Lohardaga'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Mahrajgani'] <- 'Mahrajganj'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Mahasmund'] <- 'Mahasamund'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Mahabubnagar'] <- 'Mahbubnagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Marigaon'] <- 'Morigaon'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Mayurbhanja'] <- 'Mayurbhanj'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Malkangiri'] <- 'Medchal-Malkajgiri'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Mirzpur'] <- 'Mirzapur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Mungli'] <- 'Mungeli'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Mysore'] <- 'Mysuru'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Nawarangpur'] <- 'Nabarangapur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Narsinghpur'] <- 'Narsimhapur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Nasik'] <- 'Nashik'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='24 - Paraganas North'] <- 'North 24 Paraganas'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Palamau'] <- 'Palamu'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Singhbhum West'] <- 'Pashchimi Singhbhum'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='West Midnapore'] <- 'Paschim Medinipur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Peddapally'] <- 'Peddapalli'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Perambular'] <- 'Perambalur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Pithorgarh'] <- 'Pithoragarh'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='East Midnapore Purba Midnapore'] <- 'Purba Medinipur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Singhbhum East'] <- 'Purbi Singhbhum'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Purulia'] <- 'Puruliya'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Rae - Bareily'] <- 'Rae Bareli'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Rajanna Siricilla'] <- 'Rajanna'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Ramanagaram'] <- 'Ramanagara'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Ramananthapuram'] <- 'Ramanathapuram'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Ramgadh'] <- 'Ramgarh'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Rangareddy'] <- 'Ranga Reddy'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Sahebganj'] <- 'Sahibganj'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Santh Kabir Nagar'] <- 'Sant Kabir Nagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Sariakela Kharsawan'] <- 'Saraikela-Kharsawan'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Sheopur Kalan'] <- 'Sheopur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Shimoge'] <- 'Shivamogga'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Shravasti'] <- 'Shrawasti'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Sidharthnagar'] <- 'Siddharthnagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Sivagangai Pasumpon'] <- 'Sivaganga'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Sibsagar'] <- 'Sivasagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Sonepur'] <- 'Subarnapur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Sonepat'] <- 'Sonipat'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='24 - Paraganas South'] <- 'South 24 Parganas'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='S.P.S.Nellore'] <- 'Spsr Nellore'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Ganganagar'] <- 'Sri Ganganagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Tiruvarur'] <- 'Thiruvarur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Thirunelveli'] <- 'Tirunelveli'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Tiruchirapalli Trichy'] <- 'Tiruchirappalli'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Thiruvallur'] <- 'Thiruvallur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Thiruppur'] <- 'Tiruppur'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Thiruvannamalai'] <- 'Tiruvannamalai'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Tumkur'] <- 'Tumakuru'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Uttar Kashi'] <- 'Uttarkashi'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Villupuram'] <- 'Viluppuram'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Virudhunagar Kamarajar'] <- 'Virudhunagar'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Yadadri Bhuvanagiri'] <- 'Yadadri'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Yadagiri'] <- 'Yadgiri'
levels(icrisat_normals$district)[levels(icrisat_normals$district)=='Yeotmal'] <- 'Yavatmal'


# Rename temperature districts (also at top but these are extras)
tempmissing.dis <- levels(climate_data$district)[!(levels(climate_data$district) %in% levels(ins$district))]

levels(climate_data$district)[levels(climate_data$district)=='East Singhbhum'] <- 'Purbi Singhbhum'
levels(climate_data$district)[levels(climate_data$district)=='Gangtok'] <- 'East District'
levels(climate_data$district)[levels(climate_data$district)=='Gondia'] <- 'Gondiya'
levels(climate_data$district)[levels(climate_data$district)=='Nellore'] <- 'Spsr Nellore'
levels(climate_data$district)[levels(climate_data$district)=='Prayagraj'] <- 'Allahabad'
levels(climate_data$district)[levels(climate_data$district)=='Ri Bhoi'] <- 'Ribhoi'


# Create ins/rain/prod dataframe
prod_icrisat_normals <- merge(icrisat_normals, prod1997, by=c("district", "state", "year"))

ins_rain <- merge(ins, rain, by=c("district", "state", "year"))
ins_rain_prod <- merge(ins_rain, prod2021, by=c("district","state", "year"))
ins_rain_prod <- merge(ins_rain_prod, dist_code, by=c("district","state"))

combined_data <- bind_rows(prod_icrisat_normals, ins_rain_prod)

prod_icrisat_all <- merge(combined_data, common_for_all, by ="Dist Code")
prod_icrisat_all[,20:39][is.na(prod_icrisat_all[,20:39])] <- 0

prod_icrisat_all$f.it <- prod_icrisat_all$area.ins/(prod_icrisat_all$icr2017_area)
prod_icrisat_all$f.it2 <- prod_icrisat_all$area.ins/prod_icrisat_all$area
prod_icrisat_all$f.it2[prod_icrisat_all$f.it2 > 1] <- 1
prod_icrisat_all$yield <- prod_icrisat_all$prod/(prod_icrisat_all$area)

prod_icrisat_all$jan.rfdev <- ((prod_icrisat_all$jan.rf - prod_icrisat_all$jan_mean)/(prod_icrisat_all$jan_mean+0.1))
prod_icrisat_all$feb.rfdev <- ((prod_icrisat_all$feb.rf - prod_icrisat_all$feb_mean)/(prod_icrisat_all$feb_mean+0.1))
prod_icrisat_all$mar.rfdev <- ((prod_icrisat_all$mar.rf - prod_icrisat_all$mar_mean)/(prod_icrisat_all$mar_mean+0.1))
prod_icrisat_all$apr.rfdev <- ((prod_icrisat_all$apr.rf - prod_icrisat_all$apr_mean)/(prod_icrisat_all$apr_mean+0.1))
prod_icrisat_all$may.rfdev <- ((prod_icrisat_all$may.rf - prod_icrisat_all$may_mean)/(prod_icrisat_all$may_mean+0.1))
prod_icrisat_all$jun.rfdev <- ((prod_icrisat_all$jun.rf - prod_icrisat_all$jun_mean)/(prod_icrisat_all$jun_mean+0.1))
prod_icrisat_all$jul.rfdev <- ((prod_icrisat_all$jul.rf - prod_icrisat_all$jul_mean)/(prod_icrisat_all$jul_mean+0.1))
prod_icrisat_all$aug.rfdev <- ((prod_icrisat_all$aug.rf - prod_icrisat_all$aug_mean)/(prod_icrisat_all$aug_mean+0.1))
prod_icrisat_all$sep.rfdev <- ((prod_icrisat_all$sep.rf - prod_icrisat_all$sep_mean)/(prod_icrisat_all$sep_mean+0.1))
prod_icrisat_all$oct.rfdev <- ((prod_icrisat_all$oct.rf - prod_icrisat_all$oct_mean)/(prod_icrisat_all$oct_mean+0.1))
prod_icrisat_all$nov.rfdev <- ((prod_icrisat_all$nov.rf - prod_icrisat_all$nov_mean)/(prod_icrisat_all$nov_mean+0.1))
prod_icrisat_all$dec.rfdev <- ((prod_icrisat_all$dec.rf - prod_icrisat_all$dec_mean)/(prod_icrisat_all$dec_mean+0.1))

prod_icrisat_all[is.na(prod_icrisat_all)] <- '.'

library(rstatix)

sums_rf <- rowSums(prod_icrisat_all[5:16])
output_df <- data.frame(Row = 1:nrow(prod_icrisat_all), rf = sums_rf)
output_df$mean_dev <- rowMeans(prod_icrisat_all[57:68])
output_df$mean_norm <- rowMeans(prod_icrisat_all[41:52])

output_df %>% 
  get_summary_stats(
    rf, mean_dev, mean_norm,
    type = "common") 

prod_icrisat_all %>% 
  get_summary_stats(
    jan.rf, jan.rfdev, feb.rf, feb.rfdev, mar.rf, mar.rfdev, apr.rf, apr.rfdev,
    may.rf, may.rfdev, jun.rf, jun.rfdev, jul.rf, jul.rfdev, aug.rf, aug.rfdev,
    sep.rf, sep.rfdev, oct.rf, oct.rfdev, nov.rf, nov.rfdev, dec.rf, dec.rfdev,
    type = "common") 

prod_icrisat_all %>% 
  get_summary_stats(
    area, prod, yield,
    type = "common") 

prod_icrisat_all %>% 
  filter(year > 2017) %>%
  get_summary_stats(
    area.ins,
type = "common") 


average_rainfall <- prod_icrisat_all %>%
  group_by(year) %>%
  summarize(
    jun_rf_avg = sum(jun.rf, na.rm = TRUE),  # Calculate average rainfall for June
    jul_rf_avg = mean(jul.rf, na.rm = TRUE),  # Calculate average rainfall for July
    aug_rf_avg = mean(aug.rf, na.rm = TRUE)   # Calculate average rainfall for August
  )

average_rainfall_melted <- melt(average_rainfall, id.vars = "year", variable.name = "month", value.name = "avg_rainfall")



# Plot the average monthly rainfall across districts by year
ggplot(average_rainfall_melted, aes(x = year, y = avg_rainfall, color = month)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear trend lines
  labs(x = "", y = "Rainfall (Meters)", title = "Average District Monthly Rainfall, 1997-2021") +
  scale_color_viridis_d(labels = c("June", "July", "August"), name = "") +
  scale_x_continuous(breaks = unique(average_rainfall_melted$year)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Merge ins/rain/prod with ICRISAT only for area use
rf_only <- merge(ins, icrisat_normals, by=c("district", "state"))
rf_only$f.it <- rf_only$area.ins/(rf_only$icr2017_area)
# The below code is used to write a CSV for the dataset with the matched names in ICRISAT
total <- merge(total, icrisat_rain, by=c("district", "state"))
total$f.it <- total$area.ins/(total$icr2017_area)
total$jan.rfdev <- ((total$jan.rf - total$jan.normal)/total$jan.normal)
total$feb.rfdev <- ((total$feb.rf - total$feb.normal)/total$feb.normal)
total$mar.rfdev <- ((total$mar.rf - total$mar.normal)/total$mar.normal)
total$apr.rfdev <- ((total$apr.rf - total$apr.normal)/total$apr.normal)
total$may.rfdev <- ((total$may.rf - total$may.normal)/total$may.normal)
total$jun.rfdev <- ((total$jun.rf - total$jun.normal)/total$jun.normal)
total$jul.rfdev <- ((total$jul.rf - total$jul.normal)/total$jul.normal)
total$aug.rfdev <- ((total$aug.rf - total$aug.normal)/total$aug.normal)
total$sep.rfdev <- ((total$sep.rf - total$sep.normal)/total$sep.normal)
total$oct.rfdev <- ((total$oct.rf - total$oct.normal)/total$oct.normal)
total$nov.rfdev <- ((total$nov.rf - total$nov.normal)/total$nov.normal)
total$dec.rfdev <- ((total$dec.rf - total$dec.normal)/total$dec.normal)

# Use CDSP data to create state-wise temp values. Limited to 2020 with 50 state obs
climate_data_2018 <- filter(climate_data, year > 2017) # All climate data w NAs from 2018
climate_data_agg <- aggregate(. ~ district + year, FUN = mean, data=climate_data_2018, na.rm = TRUE) # Aggregate stations to districts
climate_ins_2018 <- inner_join(ins, climate_data_agg, by=c("district" = "district", "year" = "year")) # Merge ins and 2018 climate
climate_ins_2018 <- climate_ins_2018[,-4:-25]
climate_ins_2018 <- aggregate(. ~ state + year, data = climate_ins_2018, FUN = mean, na.rm = TRUE)
climate_ins_2018[, 4:39][climate_ins_2018[, 4:39] == 0] <- NA # Seems like scraping did not work well

rf_remove <- select(climate_ins_2018, contains("rf"))
climate_ins_2018 <- climate_ins_2018[, !names(climate_ins_2018) %in% names(rf_remove)] # Get rid of rainfall categories
climate_ins_2018 <- climate_ins_2018[,-3]

temp_rf_only <- inner_join(rf_only, climate_ins_2018, by=c("state" = "state", "year.x" = "year")) # Merge rf only and 2018 climate
temp_rfdev <- inner_join(total, climate_ins_2018, by=c("state" = "state", "year" = "year")) # Merge rf devs and 2018 climate

# The code below is to replace NAs with . for STATA
rf_only[, 4:57][is.na(rf_only[, 4:57])] <- '.'
total[, 4:80][is.na(total[, 4:80])] <- '.'
temp_rf_only[, 4:81][is.na(temp_rf_only[, 4:81])] <- '.'
temp_rfdev[, 4:104][is.na(temp_rfdev[, 4:104])] <- '.'

# GLMs
model_bins <- glm(log(prod) ~ factor(jun.rain_type) + factor(jul.rain_type) +  
                    f.it + I(jun.rf*f.it) + I(jul.rf*f.it) + factor(year.x) 
                  + factor(district), data = full)

model <- glm(log(prod) ~ aug.rfdev + f.it + I(aug.rfdev*f.it) + factor(year) + factor(district), data = temp_rfdev)
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
