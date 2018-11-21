library(tidyverse)
install.packages("gsubfn")
library(gsubfn)

modeling_df %>% modeling_df

gdp_2013 <- read_csv("msa_gdp_2013.csv")
gdp_2014 <- read_csv("msa_gdp_2014.csv")
gdp_2015 <- read_csv("msa_gdp_2015.csv")

msa_gdp <- bind_rows(gdp_2013,gdp_2014,gdp_2015)

x <- c(msa_gdp$cbsatitle)
y <- gsub(" \\(Metropolitan Statistical Area)", "", x)

msa_gdp$cbsatitle <- y

modeling_df <- msa_gdp %>% 
  right_join(modeling_df, by = c("cbsatitle", "year"))
  
head(modeling_df)  

modeling_df %>% 
  filter('cbsatitle', 'gdp.x', 'gdp.y')