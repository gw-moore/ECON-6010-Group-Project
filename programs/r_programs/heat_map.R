setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(rgdal)
library(tigris)
library(plotly)
library(sf)

## Look at data
load('programs/clean_mig_data.rda')
clean_mig_data <- as.tibble(clean_mig_data)
clean_mig_data
state_abb <- read.csv("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project/inputs/state_abb.csv", stringsAsFactors=FALSE)

## Remove puerto rico
clean_mig_data <- clean_mig_data %>% filter(!grepl('Puerto Rico', county1)) 
clean_mig_data <- clean_mig_data %>% filter(!grepl('South America', county1))

## Import county lat long
county_geoid <- read.delim("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project/inputs/2017_Gaz_counties_national.txt", stringsAsFactors=FALSE)

## Join onto state_abb to get state name
county_geoid <- county_geoid %>% 
  left_join(state_abb, by = c('USPS' = 'Abbreviation'))

# mutate geoid to match clean mig data
county_geoid <- county_geoid %>% mutate(county_state = paste0(NAME, ', ', State))


# Add county geoid to clean_mig_data
clean_mig_data <- clean_mig_data %>% left_join(county_geoid, by = c("county1" = "county_state"))
clean_mig_data <- clean_mig_data %>% select(cbsatitle, county1, NAME, State, year, moved_in, moved_out, moved_net, GEOID)
colnames(clean_mig_data) <- c('cbsatitle', 'county1', 'county_name', 'county_state', 'year', 'moved_in', 'moved_out', 'moved_net', 'geoid')

# Remove city from some county names

# start by subsetting out bad county names
bad_county_name <- clean_mig_data %>%
  filter(is.na(county_name)) 

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


bad_county_name$county1 <- trimws(bad_county_name$county1)

# remove all bad county names from clean_mig_data
clean_mig_data <- clean_mig_data %>%
  filter(!is.na(county_name)) 

# add back fixed county names
clean_mig_data <- clean_mig_data %>% 
  full_join(bad_county_name)

# rejoin to county_geoid to clean_mig_data to get county_name for messed 
clean_mig_data <- clean_mig_data %>% left_join(county_geoid, by = c("county1" = "county_state"))
clean_mig_data <- clean_mig_data %>% select(cbsatitle, county1, NAME, State, year, moved_in, moved_out, moved_net, GEOID)
colnames(clean_mig_data) <- c('cbsatitle', 'county1', 'county_name', 'county_state', 'year', 'moved_in', 'moved_out', 'moved_net', 'geoid')

# Limit to only cincinnati msa data and year 2015
cincy_clean_mig_data <- clean_mig_data %>% filter(cbsatitle == 'Cincinnati, OH-KY-IN' & year == 2015)

# qa check
bad_counties <- clean_mig_data %>%
  filter(is.na(county_name))

# Read the shape file
# shape_sf <- read_sf(dsn = "tl_2018_us_county", layer = "tl_2018_us_county")
# 
# 
# # Convert geoid to character to shape file
# cincy_clean_mig_data$GEOID <- as.character(cincy_clean_mig_data$GEOID)
# 
# # Join shape file to cincy clean data
# cincy_clean_mig_data <- cincy_clean_mig_data %>% 
#   left_join(shape_sf, by = c('GEOID' = 'GEOID'))
# 
# # Convert cincy clean data to spatial file type
# as(cincy_clean_mig_data, "Spatial") %>% summary
# 
# # Create heatmap
# ggplot(shape_sf) + 
#   geom_sf(aes(fill = AWATER))


# Create csv with mapping data for Tableau
county_mapping_data <- clean_mig_data %>% filter(cbsatitle == 'Cincinnati, OH-KY-IN')

# write out csv
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project/outputs")

write.csv(x = county_mapping_data, file = 'mapping_data.csv')
