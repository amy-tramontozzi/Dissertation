# Indian Crop Insurance and Moral Hazard: Empirical Analysis

## Overview
This repository contains the dataset and scripts used for my senior-year dissertation, in which I scraped historical Indian meteorological data in Python and constructed an original empirical framework to evaluate the existence of moral hazard within the Indian crop insurance system. The modeling and analysis were conducted in STATA, and data preprocessing was performed using R.

## Contents
- **Data Files**: Includes all compiled datasets used in the analysis.
- **R Scripts**: Scripts to clean, merge, and preprocess the datasets.
- **STATA Files**: .dta file containing the empirical modeling framework and analysis.
- **`scrape_rainfall.py`**: Scrapes historical rainfall data from CRIS - India Meteorological Department (https://hydro.imd.gov.in/hydrometweb/DistrictRaifall.aspx)
  
## Requirements
To run the provided scripts, you will need the following packages:
- R Packages
tidyverse
readxl
stringr
dplyr
stringdist
expss

- Python Packages
requests
lxml
json
oandas
openpyxl

Data Sources
- Indian Meteorological Department (IMD) – Historical rainfall and temperature data.
- ICRISAT District-Level Data – Agricultural production statistics.
- PMFBY Indian Crop Insurance Data – Insurance coverage and claim information.
