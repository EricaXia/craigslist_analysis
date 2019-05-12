# Exploratory Data Analysis (EDA) of Craigslist Housing Data 

![apartments](https://github.com/EricaXia/craigslist_analysis/blob/main_code/apartment.jpg)

### I) Introduction

Craigslist is a website for posting local classified advertisements with the purpose of allowing easy postings of various subjects including housing rentals. Postings are grouped by location for easy access. This report cleans, organizes, and analyzes a messy dataset of Craigslist apartment postings in California. The aim of this report is to extract meaningful features from the initially unorganized data and analyze to draw conclusions about the apartments. The data was downloaded from the Craigslist website in a .txt file format. Data extraction and analysis is performed in R and Rstudio.

### II) Overview of the Dataset

#### 1. Initial Observations

There are 21,948 initial observations and 20 features. Each unit of observation is a row corresponding to an apartment posting on Craigslist. Each column corresponds with features listed for title, text, date, price, etc of the apartment. The features are various data types such as numeric, integer, logical, character, and factor. There is information for the price of the apartment, size in square feet, location, and other factors significant for potential renters.

#### 2. Data Quality

Quality of data is important for running accurate analyses. After taking a preliminary glance at the data using head() and str(), I found various errors in the form of outliers too extreme to be realistic. For instance, it’s impossible an apartment will have a price of $0 or a size of only 1.0 sq ft. These errors were cleaned from the data set prior to analysis and are addressed in detail in part 5 “Limitations”. After the data was cleaned, there are 21,718 observations left for analysis.

#### 3. Data and Location Span

The postings span from 2018-09-08 to 2018-10-15, about one month. The time series bar plot shows most ads were posted near the end of the time period in mid-October. Locations of postings are mostly in California, from larger cities to smaller towns. The plot shows the five locations with the most postings are San Francicso, Los Angeles, San Diego, San Jose, and Sacramento.
