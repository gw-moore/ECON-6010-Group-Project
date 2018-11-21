load("modeling_df.rda")
# We're trying to model total migration 

# Correlation plot to avoid multicollinearity (package: corrplot)
# Then star gazer on all these variables

require(corrplot)

#This vector contains all the variables that will not be considered for the correlation matrix (some dropped for multicollinearity, some dropped because they aren't numeric)
correlation_kosher <- c("cbsatitle", "year", "total_mig_in", "total_mig_out", "pop_rank", "non_us_citizens", "us_citizens", "income_rank", "msa_code", "geo.y")
modeling_num_corr <- modeling_df[ , !(names(modeling_df) %in% correlation_kosher)]
modeling_matrix <- data.matrix(modeling_num_corr, rownames.force = NA)
correlation_matrix <- cor(modeling_matrix)
corrplot(correlation_matrix, method="circle")

#This vector contains all the variables that will not be considered for the regression calculation (some dropped for multicollinearity, some dropped because they aren't numeric)
regression_kosher <- c("cbsatitle", "year", "total_mig_in", "total_mig_out", "pop_rank", "non_us_citizens", "us_citizens", "income_rank", "msa_code", "geo.y")
modeling_reg <- modeling_df[ , !(names(modeling_df) %in% regression_kosher)]
everything_model <- lm(data = modeling_reg, total_mig_net ~ .)
summary(everything_model)

AIC(everything_model)
BIC(everything_model)

rm(list=ls())
