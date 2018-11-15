#Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

#Load migration data
load('programs/migration_data.rda')
load('programs/msa_counties.rda')
crosswalk <- read_csv('inputs/county_to_msa_crosswalk.csv')

#Packages
require(tidyverse)

#Join migration date to crosswalk to get MSA
migration_data_w_msa <- migration_data %>% 
  inner_join(msa_counties, by = c('state_code' = 'fipsstatecode', 'county_code' = 'fipscountycode')) %>% 
  select(county1, county2, moved_in, moved_out, moved_net, year, cbsatitle)
  
colnames(migration_data_w_msa) <- c('county1', 'county2', 'moved_in', 'moved_out', 'moved_net', 'year', 'msa')
  
#Migration at the total MSA level
migration_agg <- migration_data_w_msa %>% 
  group_by(msa, year) %>% 
  summarise(total_mig_in = sum(moved_in, na.rm=TRUE),
            total_mig_out = sum(moved_out, na.rm=TRUE),
            total_mig_net = sum(moved_net, na.rm=TRUE))

#Migration in/out of Cincinnati MSA
cincy_msa_counties <- msa_counties %>% filter(cbsatitle == 'Cincinnati, OH-KY-IN') %>% select(countycountyequivalent, statename) %>% unite(county, countycountyequivalent, statename, sep = ', ')

cin
  
cincy_msa_mig <- migration_data_w_msa %>% 
  filter(msa == 'Cincinnati, OH-KY-IN') %>%
  filter(!county1 %in% cincy_msa_counties) %>% 
  group_by(msa, county1, year) %>%
  summarise(total_moved_in = sum(moved_in),
            total_moved_out = sum(moved_out),
            total_moved_net = sum(moved_net))

cincy_msa_mig %>% arrange(desc(total_moved_net))


migration_data_w_msa %>% filter
