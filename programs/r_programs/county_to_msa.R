#Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

#Packages
require(tidyverse)
require(lazyeval)

#Read in MSA to Country crosswalk
crosswalk <- read_csv('inputs/county_to_msa_crosswalk.csv')
county_lat_long <- read.delim("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project/inputs/2017_Gaz_counties_national.txt", stringsAsFactors=FALSE)
state_abb <- read.csv("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project/inputs/state_abb.csv", stringsAsFactors=FALSE)
load('programs/prepped_data/migration_data.rda')


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

# Function to find counties
# The main info we need is the fips state and fips county codes. We can use use these codes in to census api to pull migration data
find_counties <- function(msa_name) {
    counties <- crosswalk %>% 
    filter_('cbsatitle == msa_name') %>% 
    distinct(cbsatitle, cbsacode, countycountyequivalent, statename, fipsstatecode, fipscountycode)
    
    return(counties)}

############################################
# Run function for MSAs with for loop
# Results are appended to empty data frame
############################################

# Initilize emplty data frame
msa_counties <- data_frame()

#For loop to get all relevent counties
for (msa in msas){
  #users defined function
  counties <- find_counties(msa)
  #Save results to date frame
  msa_counties <- rbind(msa_counties, counties)}

# Concatatate county and state name together
msa_counties <- msa_counties %>% 
  mutate(county_state = paste0(countycountyequivalent, ', ', statename))

# Save results
save(msa_counties, file = "programs/prepped_data/msa_counties.rda")


#############################
## Clean up migration data ##
#############################

########################################################
# First we will go through the clean_mig_data to removed 
# rows that represent migration within an MSA
########################################################

# Aggergate Migration data to the MSA level
msa_migration <- migration_data %>% 
  inner_join(msa_counties, by = c('county2' = 'county_state')) %>% 
  select(cbsatitle, county1, moved_in, moved_out, moved_net, year) %>% 
  group_by(cbsatitle, county1, year) %>% 
  summarise(moved_in = sum(moved_in, na.rm = T),
            moved_out = sum(moved_out,  na.rm = T), 
            moved_net = sum(moved_net, na.rm = T))

# Rewrite find_counties function so that it works for this part of the code
find_counties <- function(msa_name) {
  counties <- msa_counties %>% 
    filter(cbsatitle == msa_name) %>% 
    distinct(county_state)
  
  return(counties)}

# Create empty dataframe
clean_mig_data <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(clean_mig_data) <- c('cbsatitle', 'county1', 'year', 'moved_in', 'moved_out', 'moved_net')

# Distinct counties list
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
  
  clean_mig_data <- bind_rows(clean_mig_data, data)
}

# Check clean_mig_data. Should return zero rows
counties <- find_counties('Cincinnati, OH-KY-IN')
counties <- as.vector(counties$county_state)
clean_mig_data %>% filter(cbsatitle == 'Cincinnati, OH-KY-IN' & county1 %in% counties)

counties <- find_counties('Kansas City, MO-KS')
counties <- as.vector(counties$county_state)
clean_mig_data %>% filter(cbsatitle == 'Kansas City, MO-KS' & county1 %in% counties)

##########################################################################
# Now we are going to join the created clean_mig_data to county_lat_long
# This is will give us more flexibilty to joining clean_mig_data to other
# datasets b/c it will have more ways to ID the counties
# It will also allow us to easily id counties w/ bad names so that 
# we can correct them
###########################################################################

# Join county_lat_long onto state_abb to get full state name
county_lat_long <- county_lat_long %>% 
  left_join(state_abb, by = c('USPS' = 'Abbreviation'))

# mutate county_lat_long so that we have field that matches county1 on clean mig data
county_lat_long <- county_lat_long %>% mutate(county_state = paste0(NAME, ', ', State))

# Join county_lat_long onto clean_mig_data to add county_lat_long data to clean_mig_data
clean_mig_data <- clean_mig_data %>% left_join(county_lat_long, by = c("county1" = "county_state"))
clean_mig_data <- clean_mig_data %>% select(cbsatitle, county1, NAME, State, year, moved_in, moved_out, moved_net, GEOID, INTPTLAT, INTPTLONG)
colnames(clean_mig_data) <- c('cbsatitle', 'county1', 'county_name', 'county_state', 'year', 'moved_in', 'moved_out', 'moved_net', 'geoid', 'county_lat', 'county_long')

## Remove puerto rico & south america from clean_mig_data
clean_mig_data <- clean_mig_data %>% filter(!grepl('Puerto Rico', county1)) 
clean_mig_data <- clean_mig_data %>% filter(!grepl('South America', county1))

# Remove city from some county names

# start by subsetting out bad county names
bad_county_name <- clean_mig_data %>%
  filter(is.na(county_name)) 
# 36171 rows with bad county names

# edit bad name to before all character before city, %>% 
bad_county_name$county1 <- gsub(".*city, ", "", bad_county_name$county1)
bad_county_name$county1 <- gsub(".*town, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".*township, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".*borough, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".*village, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".*municipality, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".*Princeton, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".*Census Area, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".*Parish, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".* Area, ", "", bad_county_name$county1) 
bad_county_name$county1 <- gsub(".*Reservation,", "", bad_county_name$county1)  
bad_county_name$county1 <- gsub(".*UT,", "", bad_county_name$county1)

# remove leading of trailing spaces from edited county names
bad_county_name$county1 <- trimws(bad_county_name$county1)

# remove all bad county names from clean_mig_data
clean_mig_data <- clean_mig_data %>%
  filter(!is.na(county_name)) 

# add back fixed county names
clean_mig_data <- clean_mig_data %>% 
  full_join(bad_county_name)

# rejoin to county_geoid to clean_mig_data to get county_name for orginally messed up county names 
clean_mig_data <- clean_mig_data %>% left_join(county_lat_long, by = c("county1" = "county_state"))
clean_mig_data <- clean_mig_data %>% select(cbsatitle, county1, NAME, State, year, moved_in, moved_out, moved_net, GEOID, INTPTLAT, INTPTLONG)
colnames(clean_mig_data) <- c('cbsatitle', 'county1', 'county_name', 'county_state', 'year', 'moved_in', 'moved_out', 'moved_net', 'geoid', 'county_lat', 'county_long')

# qa check - number of rows should be much smaller
bad_county_name <- clean_mig_data %>%
  filter(is.na(county_name))
# down to 50 rows. good work

# Save cleaned data
save(clean_mig_data, file ='programs/prepped_data/clean_mig_data.rda')

