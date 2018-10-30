#Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

#Packages
require(tidyverse)
require(lazyeval)

#Read in MSA to Country crosswalk
crosswalk <- read_csv('inputs/county_to_msa_crosswalk.csv')

#Set of MSAs 
msas <- c('Cincinnati, OH-KY-IN',
          'Denver-Aurora-Lakewood, CO',
          'Austin-Round Rock, TX',
          'Columbus, OH',
          'Kansas City, MO-KS',
          'Cleveland-Elyria, OH',
          'Pittsburgh, PA',
          'Indianapolis-Carmel-Anderson, IN',
          'St. Louis, MO-IL',
          'Charlotte-Concord-Gastonia, NC-SC',
          'Louisville/Jefferson County, KY-IN',
          'Nashville-Davidson--Murfreesboro--Franklin, TN',
          'Memphis, TN-MS-AR')

#Function to find counties
#The main info we need is the fips state and fips county codes. We can use use these codes in to census api to pull migration data
find_counties <- function(msa_name) {
    counties <- crosswalk %>% 
    filter_('cbsatitle == msa_name') %>% 
    distinct(cbsatitle, cbsacode, countycountyequivalent, statename, fipsstatecode, fipscountycode)
    
    return(counties)}


#Run function for MSAs with for loop
#Results are appended to empty data frame

#Initilize emplty data frame
msa_counties <- data_frame()

#For loop to get all relevent counties
for (msa in msas){
  #users defined function
  counties <- find_counties(msa)
  #Save results to date frame
  msa_counties <- rbind(msa_counties, counties)}

#Save results
save(msa_counties, file = "programs/msa_counties.rda")





