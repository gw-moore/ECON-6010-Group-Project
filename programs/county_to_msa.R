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

# Concatatate county and name together
msa_counties <- msa_counties %>% 
  mutate(county_state = paste0(countycountyequivalent, ', ', statename))

#Save results
save(msa_counties, file = "programs/msa_counties.rda")


#############################
## Clean up migration data ##
#############################

# MSA Migration data
msa_migration <- migration_data %>% 
  inner_join(msa_counties, by = c('county2' = 'county_state')) %>% 
  select(cbsatitle, county1, moved_in, moved_out, moved_net, year)

#Rewrite find_counties function
find_counties <- function(msa_name) {
  counties <- msa_counties %>% 
    filter(cbsatitle == msa_name) %>% 
    distinct(county_state)
  
  return(counties)}

#Create empty dataframe
clean_mig_data <- data.frame()

#Distinct counties list
msas <-  as.vector(unique(msa_counties$cbsatitle))
msas

# Loop through each msa and filter out counties
for (msa in msas) {
  counties <- find_counties(msa)
  counties <- as.vector(counties$county_state)
  
  data <- msa_migration %>% 
    filter(cbsatitle == msa) 
  
  data <- data %>% 
    filter(!county1 %in% counties)
  
  clean_mig_data <- rbind(clean_mig_data, data)
}

# Check datq
counties <- find_counties('Cincinnati, OH-KY-IN')
counties <- as.vector(counties$county_state)
clean_mig_data %>% filter(cbsatitle == 'Cincinnati, OH-KY-IN' & county1 %in% counties)

counties <- find_counties('Kansas City, MO-KS')
counties <- as.vector(counties$county_state)
clean_mig_data %>% filter(cbsatitle == 'Kansas City, MO-KS' & county1 %in% counties)


# Save cleaned data
save(clean_mig_data, file ='programs/clean_mig_data.rda')

