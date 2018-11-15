setwd("~/School/Fall_2018/Economic_Data_Analysis/ECON-6010-Group-Project")

# Load libraries and data
library(tidyverse)
library(ggrepel)
load('programs/gdp_data.Rda')
load('programs/migration_data.Rda')
load('programs/acs_data.Rda')
load('programs/crime_data.Rda')
load('programs/property_tax_data.Rda')
load('programs/msa_counties.Rda')
load('programs/clean_mig_data.Rda')
crosswalk <- read_csv('inputs/county_to_msa_crosswalk.csv')

# Migration trends

# Aggergate to total year migration
yearly_mig <- clean_mig_data %>% 
  group_by(cbsatitle, year) %>% 
  summarise(total_moved_in = sum(moved_in, na.rm = T),  
            total_moved_out = sum(moved_out, na.rm = T), 
            total_moved_net = sum(moved_net, na.rm = T))

# ggplot of time tread

# Convert year to date type
yearly_mig$year <- as.Date(as.character(yearly_mig$year), format = "%Y")

# Cincinnati ggplot time plot
yearly_mig %>% 
  filter(cbsatitle == 'Cincinnati, OH-KY-IN') %>% 
  mutate(label = if_else(year == max(year), as.character(cbsatitle), NA_character_)) %>%
  ggplot(aes(x = year, y=total_moved_net, color = cbsatitle)) + geom_line(size = 2) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE) +
  ggtitle('Net Migration Trend by MSA') +
  ylab('Total Net Migration') +
  xlab('Year') +
  labs(color = 'MSA') +
  theme(legend.position="none")

# ggplot time plot
yearly_mig %>% 
  mutate(label = if_else(year == max(year), as.character(cbsatitle), NA_character_)) %>%
  ggplot(aes(x = year, y=total_moved_net, color = cbsatitle)) + geom_line(size = 2) +
  geom_label_repel(aes(label = label),
                   nudge_x = 1,
                   na.rm = TRUE,
                   show.legend = F) +
  ggtitle('Net Migration Trend by MSA') +
  ylab('Total Net Migration') +
  xlab('Year') +
  labs(color = 'MSA')  +
  theme(legend.position="none")