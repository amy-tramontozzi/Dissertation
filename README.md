# Indian Crop Insurance and Moral Hazard: Empirical Analysis

## Overview
This repository contains the dataset and scripts used for my senior-year dissertation, in which I scraped historical Indian meteorological data in Python and constructed an original empirical framework to evaluate the existence of moral hazard within the Indian crop insurance system. The modeling and analysis were conducted in STATA, and data preprocessing was performed using R.

Contents
- **Data Files: Includes all compiled datasets used in the analysis.

R Scripts: Scripts to clean, merge, and preprocess the datasets.

STATA Files: Files containing the empirical modeling framework and analysis.

Python Scripts: Used for scraping historical meteorological data.

Requirements

To run the provided scripts, you will need the following packages:

R Packages

Ensure you have R installed along with the following libraries:

tidyverse

readxl

stringr

dplyr

stringdist

expss

Python Packages

For web scraping and data processing, you will need:

pandas

requests

beautifulsoup4

numpy

Data Sources

The data used in this project is compiled from multiple sources:

Indian Meteorological Department (IMD) – Historical rainfall and temperature data.

ICRISAT District-Level Data – Agricultural production statistics.

Indian Crop Insurance Data – Insurance coverage and claim information.

Data Processing

Meteorological Data Scraping (Python)

Scraped historical rainfall and temperature data from official sources.

Data Cleaning and Merging (R)

Standardized variable names and formats.

Removed missing or inconsistent data.

Merged meteorological, agricultural, and insurance datasets by district and year.

Empirical Modeling (STATA)

Used a panel data framework to evaluate the impact of insurance on farmer behavior.

Estimated models to detect potential moral hazard effects.

How to Use

Clone this repository:

git clone https://github.com/yourusername/yourrepository.git

Install the required packages.

Run the R scripts to clean and preprocess the data.

Load the cleaned data into STATA and run the empirical analysis.

Results and Findings

The results of this study contribute to the understanding of how crop insurance affects farmer incentives in India. Findings suggest that certain patterns in the data are consistent with moral hazard behavior, particularly among insured farmers with repeated claims.

Contact

For questions or collaboration, please contact Amy Tramontozzi at your.email@example.com.

