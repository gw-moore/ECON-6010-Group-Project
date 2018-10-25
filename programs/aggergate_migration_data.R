#Set working directory
setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

#Packages
require(tidyverse)

#Join migration date to crosswalk to get MSA
migration_date_w_msa <- migration_data %>% 
  inner_join(msa_counties, by = c('state_code' = 'fipsstatecode', 'county_code' = 'fipscountycode')) %>% 
  select(county1, county2, moved_in, moved_out, moved_net, year, cbsatitle)
  
colnames(migration_date_w_msa) <- c('county1', 'county2', 'moved_in', 'moved_out', 'moved_net', 'year', 'msa')
  
#Migration at the total MSA level
migration_agg <- migration_date_w_msa %>% 
  group_by(msa, year) %>% 
  summarise(total_mig_in = sum(moved_in, na.rm=TRUE),
            total_mig_out = sum(moved_out, na.rm=TRUE),
            total_mig_net = sum(moved_net, na.rm=TRUE))
