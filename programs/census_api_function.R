#Set working directory
setwd("~/School/Fall 2018/Economic Data Analysis/Group Project")
#Load packages
library(censusapi)
library(tidycensus)
library(jsonlite)
library(tidyverse)

#Stuff to set census bureau API key
Sys.setenv(CENSUS_KEY='4406314b19dc50883b841ee1476ba327f14ef275')
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

#Create data frames to see available variables and georaphies from acs/flows
avaiable_variables <- listCensusMetadata('acs/flows', vintage = 2014, type = "variables")
avaiable_geographies <- listCensusMetadata('acs/flows', vintage = 2014, type = "geographies")

#Function to pull data via census burea API
get_data <- function(year, vars) {
  #Creating the URL to pull data from census bureau
  resURL <- paste0('https://api.census.gov/data/',year,'/acs/flows?get=',vars,'&for=county:061&in=state:39&key=4406314b19dc50883b841ee1476ba327f14ef275')

  #Pull in JSON data and storing in json_list
  json_list <- fromJSON(resURL)
  #Convert json_list to data frame
  df <- as_data_frame(json_list)
  #Change column name of df
  colnames(df) <- c('county1', 'county2', 'moved_in', 'moved_out', 'moved_net')
  #From the first 9 row and columns 6-7
  df <- df[-c(1:9), -c(6:7)]
  #Add year variable as numeric
  df$year <- as.numeric(year)
  #Convert moved columns to numeric
  df$moved_in <- as.numeric(df$moved_in)
  df$moved_out <- as.numeric(df$moved_out)
  df$moved_net <- as.numeric(df$moved_net)
  #Return df from the function
  return(df)
}

#Pull in data by year
ham_county_flows_2009 <- get_data('2009', 'FULL2_NAME,FULL1_NAME,MOVEDIN,MOVEDOUT,MOVEDNET')
ham_county_flows_2010 <- get_data('2010', 'FULL2_NAME,FULL1_NAME,MOVEDIN,MOVEDOUT,MOVEDNET')
ham_county_flows_2011 <- get_data('2011', 'FULL2_NAME,FULL1_NAME,MOVEDIN,MOVEDOUT,MOVEDNET')
ham_county_flows_2012 <- get_data('2012', 'FULL2_NAME,FULL1_NAME,MOVEDIN,MOVEDOUT,MOVEDNET')
ham_county_flows_2013 <- get_data('2013', 'FULL2_NAME,FULL1_NAME,MOVEDIN,MOVEDOUT,MOVEDNET')
ham_county_flows_2014 <- get_data('2014', 'FULL2_NAME,FULL1_NAME,MOVEDIN,MOVEDOUT,MOVEDNET')
ham_county_flows_2015 <- get_data('2015', 'FULL2_NAME,FULL1_NAME,MOVEDIN,MOVEDOUT,MOVEDNET')

#Stack the data frame together to make hamilton country data frame
ham_county <- rbind(ham_county_flows_2009, ham_county_flows_2010, ham_county_flows_2011, ham_county_flows_2012, ham_county_flows_2013, ham_county_flows_2014, ham_county_flows_2015)

#Arrange ham_county by county1 name and year
ham_county <- ham_county %>% arrange(county1, year)
