####################################################
##                                                ##
##   Comprehensive Assessment of Theory of mind   ##
##                                                ##
##                   MJ Heise                     ##
##                  April 2023                    ##
##                                                ##
####################################################

## CODE DESCRIPTION: This script is part of the pre-registration for CAT: https://osf.io/4d9ar/
# 1. BIVARIATE CORRELATIONS:
# This script saves correlation coefficients (R), p-values (p), and sample size (N) in each cell.
#
# 2. PARTIAL CORRELATIONS:
# This script writes a loop to conduct partial correlations controlling for age and saves
# correlation coefficients (partialCorrelation) and p-values (pValues) in a dataframe
# (partial_df). 


# Libraries
library(Hmisc) # v.5.1-1, correlation matrix
library(ppcor) # v.1.1, partial correlations
library(corrplot) # v.0.92, visualize correlations

#### 1. BIVARIATE CORRELATIONS ####
# Read in data
dat <- read.csv('C:/Users/mjheise/Box Sync/Research/BowmanLab/CAT_ContextUpdatingToM/Data/CATdata.csv')

# Create list of variables to correlate
vars <- c('ageYrs', 'vocab_sum', 'divDesAvg', 'divBelAvg', 'knowAvg', 'vptAvg', 
          'falseBelAvg', 'falseSignAvg', 'grassSky_sum', 'upDown_sum')

# Subset variables
corr.dat <- dat[vars]

# Create correlation matrix
corrMtx = rcorr(as.matrix(corr.dat))

# Save correlation coefficients (R), p-values (p), and sample size within each 
# pairwise correlation (N)
R = round(corrMtx$r, digits=2)
p = corrMtx$P
N = corrMtx$n

# Visualize correlations
corrplot(R, p.mat = p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.5, tl.cex = 0.7, cl.cex = 0.5,
         order = 'AOE', diag=F)


#### 2. PARTIAL CORRELATIONS ####
# Define variables to be correlated, controlling for age
variable_combinations <- list(
  c("ageYrs", "vocab_sum", "divDesAvg"),
  c("ageYrs", "vocab_sum", "divBelAvg"),
  c("ageYrs", "vocab_sum", "knowAvg"),
  c("ageYrs", "vocab_sum", "vptAvg"),
  c("ageYrs", "vocab_sum", "falseBelAvg"),
  c("ageYrs", "vocab_sum", "falseSignAvg"),
  c("ageYrs", "vocab_sum", "grassSky_sum"),
  c("ageYrs", "vocab_sum", "upDown_sum"),
  c("ageYrs", "divDesAvg", "divBelAvg"),
  c("ageYrs", "divDesAvg", "knowAvg"),
  c("ageYrs", "divDesAvg", "vptAvg"),
  c("ageYrs", "divDesAvg", "falseBelAvg"),
  c("ageYrs", "divDesAvg", "falseSignAvg"),
  c("ageYrs", "divDesAvg", "grassSky_sum"),
  c("ageYrs", "divDesAvg", "upDown_sum"),
  c("ageYrs", "divBelAvg", "knowAvg"),
  c("ageYrs", "divBelAvg", "vptAvg"),
  c("ageYrs", "divBelAvg", "falseBelAvg"),
  c("ageYrs", "divBelAvg", "falseSignAvg"),
  c("ageYrs", "divBelAvg", "grassSky_sum"),
  c("ageYrs", "divBelAvg", "upDown_sum"),
  c("ageYrs", "knowAvg", "vptAvg"),
  c("ageYrs", "knowAvg", "falseBelAvg"),
  c("ageYrs", "knowAvg", "falseSignAvg"),
  c("ageYrs", "knowAvg", "grassSky_sum"),
  c("ageYrs", "knowAvg", "upDown_sum"),
  c("ageYrs", "vptAvg", "falseBelAvg"),
  c("ageYrs", "vptAvg", "falseSignAvg"),
  c("ageYrs", "vptAvg", "grassSky_sum"),
  c("ageYrs", "vptAvg", "upDown_sum"),
  c("ageYrs", "falseBelAvg", "falseSignAvg"),
  c("ageYrs", "falseBelAvg", "grassSky_sum"),
  c("ageYrs", "falseBelAvg", "upDown_sum"),
  c("ageYrs", "falseSignAvg", "grassSky_sum"),
  c("ageYrs", "falseSignAvg", "upDown_sum"),
  c("ageYrs", "grassSky_sum", "upDown_sum")
)

# Initialize lists to store partial correlation results
partial_correlation_results <- list()
p_values <- list()

# Loop through each variable combination
for (i in 1:length(variable_combinations)) {
  # Subset your data based on the variables in the current combination
  partial_data <- dat[complete.cases(dat[, variable_combinations[[i]]]), ]
  
  # Extract variables from the combination
  vars <- variable_combinations[[i]]
  
  # Compute partial correlation using pcor.test, controlling for ageYrs
  partial_test_result <- pcor.test(partial_data[[vars[3]]], partial_data[[vars[2]]], partial_data[[vars[1]]])
  
  # Extract partial correlation coefficient and p-value
  partial_corr <- partial_test_result$estimate
  p_val <- partial_test_result$p.value
  
  # Store the results
  partial_correlation_results[[i]] <- partial_corr
  p_values[[i]] <- p_val
}

# Combine partial correlation coefficients and p-values into a dataframe
partial_df <- data.frame(
  variables = sapply(variable_combinations, paste, collapse = ", "),
  partialCorrelation = unlist(partial_correlation_results),
  pValue = unlist(p_values)
)
