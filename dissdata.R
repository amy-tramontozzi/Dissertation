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
ins$year <- as.factor(ins$year)
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
climate_data$year <- as.factor(climate_data$year)
climate_data$station <- as.factor(climate_data$station)
climate_data$stationID <- as.factor(climate_data$stationID)

### RENAMING STATIONS TO DISTRICTS
stat_dis <- read_excel("station_district.xlsx")
stat_dis$station <- str_to_title(stat_dis$station)
stat_dis$station <- as.factor(stat_dis$station)
climate_data <- merge(climate_data, stat_dis, by = "station")
### RENAMING DISTRICTS TO MERGE

#Rename rainfall states
levels(rain$state)[levels(rain$state)=='A and N Island (Ut)'] <- 'Andaman and Nicobar Islands'
levels(rain$state)[levels(rain$state)=='Puducherry (Ut)'] <- 'Puducherry'
levels(rain$state)[levels(rain$state)=='Tamilnadu'] <- 'Tamil Nadu'
levels(ins$state)[!(levels(ins$state) %in% levels(rain$state))]

#Rename insurance districts
levels(ins$district)[levels(ins$district)=='Ahmadabad'] <- 'Ahmedabad'
levels(ins$district)[levels(ins$district)=='Ahmadnagar'] <- 'Ahmednagar'
levels(ins$district)[levels(ins$district)=='Allahabad'] <- 'Prayagraj'
levels(ins$district)[levels(ins$district)=='Anugul'] <- 'Angul'
levels(ins$district)[levels(ins$district)=='Bagalkot'] <- 'Bagalkote'
levels(ins$district)[levels(ins$district)=='Banas Kantha'] <- 'Banaskantha'
levels(ins$district)[levels(ins$district)=='Baleshwar'] <- 'Balasore'
levels(ins$district)[levels(ins$district)=='Bara Banki'] <- 'Barabanki'
levels(ins$district)[levels(ins$district)=='Baudh'] <- 'Boudh'
levels(ins$district)[levels(ins$district)=='Bhadradri'] <- 'Bhadradri Kothagudem'
levels(ins$district)[levels(ins$district)=='Bid'] <- 'Beed'
levels(ins$district)[levels(ins$district)=='Buldana'] <- 'Buldhana'
levels(ins$district)[levels(ins$district)=='ChamarajNagar'] <- 'Chamarajanagar'
levels(ins$district)[levels(ins$district)=='Chittaurgarh'] <- 'Chittorgarh'
levels(ins$district)[levels(ins$district)=='Dakshin Bastar Dantewada'] <- 'Dantewada'
levels(ins$district)[levels(ins$district)=='DakshinaKannada'] <- 'Dakshina Kannada'
levels(ins$district)[levels(ins$district)=='Darjiling'] <- 'Darjeeling'
levels(ins$district)[levels(ins$district)=='Debagarh'] <- 'Deoghar'
levels(ins$district)[levels(ins$district)=='Dhaulpur'] <- 'Dholpur'
levels(ins$district)[levels(ins$district)=='East District'] <- 'Gangtok'
levels(ins$district)[levels(ins$district)=='East Nimar'] <- 'Khandwa'
levels(ins$district)[levels(ins$district)=='Faizabad'] <- 'Ayodhya'
levels(ins$district)[levels(ins$district)=='Garhwal'] <- 'Pauri Garhwal'
levels(ins$district)[levels(ins$district)=='Gariyaband'] <- 'Gariaband'
levels(ins$district)[levels(ins$district)=='Gorella-Pendra-Marwahi'] <- 'Gaurella Pendra Marwahi'
levels(ins$district)[levels(ins$district)=='Gondiya'] <- 'Gondia'
levels(ins$district)[levels(ins$district)=='Gurugram'] <- 'Gurgaon'
levels(ins$district)[levels(ins$district)=='Haora'] <- 'Howrah'
levels(ins$district)[levels(ins$district)=='Hasan'] <- 'Hassan'
levels(ins$district)[levels(ins$district)=='Hoshangabad'] <- 'Narmadapuram'
levels(ins$district)[levels(ins$district)=='Hugli'] <- 'Hooghly'
levels(ins$district)[levels(ins$district)=='Jajapur'] <- 'Jajpur'
levels(ins$district)[levels(ins$district)=='Jalor'] <- 'Jalore'
levels(ins$district)[levels(ins$district)=='Janjgir - Champa'] <- 'Janjgir-Champa'
levels(ins$district)[levels(ins$district)=='Jayashankar'] <- 'Jayashankar Bhupalpally'
levels(ins$district)[levels(ins$district)=='Jogulamba'] <- 'Jogulamba Gadwal'
levels(ins$district)[levels(ins$district)=='Kabeerdham'] <- 'Kabirdham'
levels(ins$district)[levels(ins$district)=='Kamrup'] <- 'Kamrup (Rural)'
levels(ins$district)[levels(ins$district)=='Kamrup Metropolitan'] <- 'Kamrup (Metro)'
levels(ins$district)[levels(ins$district)=='Kanniyakumari'] <- 'Kanyakumari'
levels(ins$district)[levels(ins$district)=='Khandwa (East Nimar)'] <- 'Khandwa'
levels(ins$district)[levels(ins$district)=='Khargone (West Nimar)'] <- 'Khargone'
levels(ins$district)[levels(ins$district)=='Koch Bihar'] <- 'Cooch Behar'
levels(ins$district)[levels(ins$district)=='Kodarma'] <- 'Koderma'
levels(ins$district)[levels(ins$district)=='Komaram Bheem'] <- 'Kumaram Bheem'
levels(ins$district)[levels(ins$district)=='Maldah'] <- 'Malda'
levels(ins$district)[levels(ins$district)=='Marigaon'] <- 'Morigaon'
levels(ins$district)[levels(ins$district)=='Nabarangapur'] <- 'Nabarangpur'
levels(ins$district)[levels(ins$district)=='Narsimhapur'] <- 'Narsinghpur'
levels(ins$district)[levels(ins$district)=='Nicobars'] <- 'Nicobar'
levels(ins$district)[levels(ins$district)=='North District'] <- 'North Sikkim'
levels(ins$district)[levels(ins$district)=='Osmanabad'] <- 'Dharashiv'
levels(ins$district)[levels(ins$district)=='Pondicherry'] <- 'Puducherry'
levels(ins$district)[levels(ins$district)=='Punch'] <- 'Poonch'
levels(ins$district)[levels(ins$district)=='Puruliya'] <- 'Purulia'
levels(ins$district)[levels(ins$district)=='Rae Bareli'] <- 'Raebareli'
levels(ins$district)[levels(ins$district)=='Rajanna'] <- 'Rajanna Sircilla'
levels(ins$district)[levels(ins$district)=='Ribhoi'] <- 'Ri Bhoi'
levels(ins$district)[levels(ins$district)=='Saraikela-Kharsawan'] <- 'Seraikela-Kharsawan'
levels(ins$district)[levels(ins$district)=='Shrawasti'] <- 'Shravasti'
levels(ins$district)[levels(ins$district)=='South District'] <- 'Namchi'
levels(ins$district)[levels(ins$district)=='Thanjavur I'] <- 'Thanjavur'
levels(ins$district)[levels(ins$district)=='Thanjavur II'] <- 'Thanjavur'
levels(ins$district)[levels(ins$district)=='The Nilgiris'] <- 'Nilgiris'
levels(ins$district)[levels(ins$district)=='Thiruvallur'] <- 'Tiruvallur'
levels(ins$district)[levels(ins$district)=='Thiruvarur'] <- 'Tiruvarur'
levels(ins$district)[levels(ins$district)=='Thiruvarur II'] <- 'Tiruvarur'
levels(ins$district)[levels(ins$district)=='Thoothukkudi'] <- 'Toothukudi'
levels(ins$district)[levels(ins$district)=='Uttar Bastar Kanker'] <- 'Kanker'
levels(ins$district)[levels(ins$district)=='UttarKannada'] <- 'Uttara Kannada'
#levels(ins$district)[levels(ins$district)=='Warangal Rural'] <- 'Warangal'
#levels(ins$district)[levels(ins$district)=='Warangal Urban'] <- 'Warangal'
levels(ins$district)[levels(ins$district)=='West District'] <- 'Gyalshing'
levels(ins$district)[levels(ins$district)=='Bajali'] <- 'Barpeta'
levels(ins$district)[levels(ins$district)=='Mahrajganj'] <- 'Maharajganj'
levels(ins$district)[levels(ins$district)=='Mewat'] <- 'Nuh'
levels(ins$district)[levels(ins$district)=='Sonepur'] <- 'Subarnapur'
levels(ins$district)[levels(ins$district)=='Spsr Nellore'] <- 'Sri Potti Sriramulu Nellore"'

# Rename rainfall districts
levels(rain$district)[levels(rain$district)=='Agar-Malwa'] <- 'Agar Malwa'
levels(rain$district)[levels(rain$district)=='Alapuzha'] <- 'Alappuzha'
levels(rain$district)[levels(rain$district)=='Amraoti'] <- 'Amravati'
levels(rain$district)[levels(rain$district)=='Faizabad'] <- 'Ayodhya'
levels(rain$district)[levels(rain$district)=='Bolangir'] <- 'Balangir'
levels(rain$district)[levels(rain$district)=='Boudhgarh'] <- 'Boudh'
levels(rain$district)[levels(rain$district)=='B. Kothagudem'] <- 'Bhadradri Kothagudem'
levels(rain$district)[levels(rain$district)=='Bulandshahar'] <- 'Bulandshahr'
levels(rain$district)[levels(rain$district)=='Chikaballapura'] <- 'Chikaballapur'
levels(rain$district)[levels(rain$district)=='South Dinajpur'] <- 'Dakshin Dinajpur'
levels(rain$district)[levels(rain$district)=='East Sikkim'] <- 'Gangtok'
levels(rain$district)[levels(rain$district)=='Garhwal Pauri'] <- 'Pauri Garhwal'
levels(rain$district)[levels(rain$district)=='West Sikkim'] <- 'Gyalshing'
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
levels(rain$district)[levels(rain$district)=='Kanpur City'] <-[ 'Kanpur Nagar'
levels(rain$district)[levels(rain$district)=='West Midnapore'] <- 'Paschim Medinipur'
levels(rain$district)[levels(rain$district)=='West Singbhum'] <- 'Paschim Singhbhum'
levels(rain$district)[levels(rain$district)=='East Midnapore'] <- 'Purba Medinipur'
levels(rain$district)[levels(rain$district)=='East Singbhum'] <- 'Purbi Singhbhum'
levels(rain$district)[levels(rain$district)=='Sholapur'] <- 'Solapur'
levels(rain$district)[levels(rain$district)=='Garhwal Tehri'] <- 'Tehri Garhwal']
levels(rain$district)[levels(rain$district)=='Trichy'] <- 'Tiruchirappalli '

# Missing districts
rainmissing.dis <- levels(ins$district)[!(levels(ins$district) %in% levels(rain$district))]

# Create rainfall + insurance df
total <- inner_join(ins, rain, by=c("district" = "district", "state" = "state", "year" = "year"))
summary(total$farmers)

# Lit review stats
total %>%
  group_by(year, state) %>%
  summarise(mean(area.ins))

total %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  ggplot(aes(state, area.ins)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Area insured by State, 2018", x ="", y = "Area (in thousand hectares)")

total %>%
  filter(year == 2022) %>%
  group_by(state) %>%
  ggplot(aes(state, area.ins)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title = "Area insured by State, 2022", x ="", y = "Area (in thousand hectares)")

# Read and tidy production data
lev

# Take sum of all areas, prod, and yields for each yearly district
prod <- prod[,-43:-45]
area.col <- select(prod, contains("Area"))
prod$area <- rowSums(area.col)
prod.col <- select(prod, contains("Production"))
prod$prod <- rowSums(prod.col)
yield.col <- select(prod, contains("Yield"))
prod$yield <- rowSums(yield.col)
prod <- prod[,-4:-153]
prod$state <- str_to_title(prod$state)
prod$district <- str_to_title(prod$district)
prod$state <- as.factor(prod$state)
prod$year <- as.factor(prod$year)
prod$district <- as.factor(prod$district)

# Find states/ districts without corresponding ins name
prodmissing.st <- levels(total$state)[!(levels(total$state) %in% levels(prod$state))]
prodmissing.dis <- levels(total$district)[!(levels(total$district) %in% levels(prod$district))]
# Rename prod states
levels(prod$state)[levels(prod$state)=='Jammu And Kashmir'] <- 'Jammu and Kashmir'

# Rename prod districts
levels(prod$district)[levels(prod$district)=='Ahmadabad'] <- 'Ahmedabad'
levels(prod$district)[levels(prod$district)=='Allahabad'] <- 'Prayagraj'
levels(prod$district)[levels(prod$district)=='Anugul'] <- 'Angul'
levels(prod$district)[levels(prod$district)=='Faizabad'] <- 'Ayodhya'
levels(prod$district)[levels(prod$district)=='Bagalkot'] <- 'Bagalkote'
levels(prod$district)[levels(prod$district)=='Baleshwar'] <- 'Balasorele'
levels(prod$district)[levels(prod$district)=='Bellary'] <- 'Ballari'
levels(prod$district)[levels(prod$district)=='Banas Kantha'] <- 'Banaskantha'
levels(prod$district)[levels(prod$district)=='Belgaum'] <- 'Belagavi'
levels(prod$district)[levels(prod$district)=='Sant Ravidas Nagar'] <- 'Bhadohi'
levels(prod$district)[levels(prod$district)=='Charki Dadri'] <- 'Charkhi Dadri'
levels(prod$district)[levels(prod$district)=='Chikballapur'] <- 'Chikkaballapur'
levels(prod$district)[levels(prod$district)=='Chikmagalur'] <- 'Chikkamagaluru'
levels(prod$district)[levels(prod$district)=='Dinajpur Dakshin'] <- 'Dakshin Dinajpur'
levels(prod$district)[levels(prod$district)=='Dakshin Kannad'] <- 'Dakshina Kannada'
levels(prod$district)[levels(prod$district)=='Gangtok'] <- 'East Sikkim'
levels(prod$district)[levels(prod$district)=='Gariyaband'] <- 'Gariaband'
levels(prod$district)[levels(prod$district)=='Gaurella-Pendra-Marwahi'] <- 'Gaurella Pendra Marwahi'
levels(prod$district)[levels(prod$district)=='Haridwar'] <- 'Hardwar'
levels(prod$district)[levels(prod$district)=='Hoshangabad'] <- 'Narmadapuram'
levels(prod$district)[levels(prod$district)=='Jagitial'] <- 'Jagtial'
levels(prod$district)[levels(prod$district)=='Jajapur'] <- 'Jajpur'
levels(prod$district)[levels(prod$district)=='Jangoan'] <- 'Jangaon'



reg.vars <- total %>%
  select(state, district, year, area.ins, apr.rf, apr.ptdef, may.rf, may.ptdef, jun.rf, jun.ptdef)

