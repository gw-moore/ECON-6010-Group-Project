############################################################################
# Creating a modelling dataframe composed of all the Independent Variables

require(tidyverse)

# Manipulated and Cleaned Migration Data
# CBSAtitle = "departing from" county, county1 = arriving to MSA
load("clean_mig_data.rda")

# Generic Demographic Information
load("acs_data.rda")

# More Specific Candidate Indepedent Variables
load("gdp_data.rda")
load("property_tax_data.rda")

# Decided not to use crime dataset because there's only three years of data (only two of which overlap with the migration data).


######################################################################################
# Modelling Data:
# all of which is already aggregated to MSA level: ACS, clean migration data, property tax, GDP
# All that must be done is to standardize the ACS column names to act as a join key

#MSA for __ year: migration in, migration out, net migration, then all the indpendent variables tied back to MSA


#################################
#MSA labels and content

#Standardize the name of the msa column in each dataset to match the column name in clean_mig
# Standardized column name = cbsatitle

acs_cols <- colnames(acs_data)
acs_cols_revised <- sub("geo_name", "cbsatitle", acs_cols)
colnames(acs_data) <- acs_cols_revised

prop_cols <- colnames(df_prop_tax)
prop_cols_revised <- sub("geo_name", "cbsatitle", prop_cols)
colnames(df_prop_tax) <- prop_cols_revised

gpd_cols <- colnames(gpd_data)
gpd_cols_revised <- sub("msa_title", "cbsatitle", gpd_cols)
colnames(gpd_data) <- gpd_cols_revised


#This is to standardize the *content* of the data itself in the msa column
require(gsubfn)
cbsatitle <- gsub(" Metro Area", "", acs_data$cbsatitle)
cbsatitle <- gsub(" Micro Area", "", cbsatitle)
acs_data$cbsatitle <- cbsatitle

cbsatitle <- gsub(" Metro Area", "", df_prop_tax$cbsatitle)
cbsatitle <- gsub(" Micro Area", "", cbsatitle)
df_prop_tax$cbsatitle <- cbsatitle

# the content of the GDP cbsatitle column is already properly formatted



#########################################################################
############# Now to actually join ######################################

# Join together clean_mig_data and acs
cleanmig_acs <- dplyr::left_join(clean_mig_data, acs_data, by = "cbsatitle")

# Add GDP data
cleanmig_acs_GDP <- left_join(cleanmig_acs, gpd_data, by = "cbsatitle")

# Finally, add property tax
cleanmig_acs_GDP_pt <- left_join(cleanmig_acs_GDP, df_prop_tax, by = "cbsatitle")



########

Final_Modelling_DF <- cleanmig_acs_GDP_pt

# Contains all modelling data, with standardized content for MSA data, which is a column named "cbsatitle", upon which it has been joined.

