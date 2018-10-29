#Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project/programs")
#Load packages
library(jsonlite)
library(tidyverse)

##Function to view type of data from DataUSA
fromJSON( "http://api.datausa.io/attrs/list/" )

viewDataLevels <- function( attribute )
{
  url <- paste( "http://api.datausa.io/attrs/", attribute, sep="" )
  temp <- fromJSON( url )
  df <- as.data.frame( temp[[1]] )
  names(df) <- temp[[2]]
  print( paste( "NUMBER OF ROWS:", nrow(df) ) )
  return(df)
}

viewDataLevels('ipeds_expense')

##Data at the MSA level
api <- paste0('http://api.datausa.io/api/csv/?show=geo&sumlevel=msa&year=latest&required=pop,income,income_rank,num_emp,mean_commute_minutes') 
#Pull in CSV data and storing in df
df <- read_csv(api)

##Homicide data at the county level
api <- paste0('https://api.datausa.io/api/csv/?sort=desc&show=geo&required=homicide_rate&sumlevel=county&year=all') 
#Pull in CSV data and storing in df
df_homicide <- read_csv(api)

##Violent crime data at the county level
api <- paste0('https://api.datausa.io/api/csv/?sort=desc&show=geo&required=violent_crime&sumlevel=county&year=all') 
#Pull in CSV data and storing in df
df_violent_crime <- read_csv(api)

##Number of Graduates from bacholors programs for Cincinnati MSA
api <- paste0('https://api.datausa.io/api/csv/?sort=desc&sumlevel=6&degree=5&show=cip&year=all&geo=31000US17140') 
#Pull in CSV data and storing in df
df_college_grads <- read_csv(api)

##Housing values by MSA
api <- paste0('https://api.datausa.io/api/csv/?sort=desc&force=acs.yg_property_value&show=geo&sumlevel=msa&year=all') 
#Pull in CSV data and storing in df
df_housing_value <- read_csv(api)

##Property taxes
api <- paste0('https://api.datausa.io/api/csv/?sort=desc&force=acs.yg_property_tax&show=geo&sumlevel=msa&year=all')
#Pull in CSV data and storing in df
df_prop_tax <- read_csv(api)

