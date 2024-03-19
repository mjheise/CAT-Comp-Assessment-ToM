####################################################
##                                                ##
##   Comprehensive Assessment of Theory of mind   ##
##                CFAs for TOM                    ##      
##                                                ##
##                   MJ Heise                     ##
##                February 2024                   ##
##                                                ##
####################################################

## CODE DESCRIPTION: This script is part of the pre-registration for CAT: https://osf.io/4d9ar/
# 1. RELATIONS WITH AGE:
# This script pivots data from wide to long, and fits a linear mixed effects model to examine
# the interaction between age and mental-state category/false sign. In the final model,
# the outcome (score on an individual item) is predicted by fixed effects of an interaction
# between age and mental state category/false sign, vocabulary, grass/sky, and up/down, and random
# intercepts for participant and item. 
#
# 2. ALPHA:
# This code calculates alpha for mental-state categories and false sign,
# and for the CAT measure as a whole. 

# Libraries
library(tidyverse) #v.2.0.0
library(lme4) #v.1.1-34, linear mixed effects models (LMEs)
library(lmerTest) #v.3.1-3, p-values for LMEs
library(emmeans) #v.1.8.9, compare marginal means from LMEs
library(lattice) #v.0.22-5, qq visual inspection
library(car) #v.3.1-2, levene's test of homogeneity of variance
library(sjPlot) #v2.8.15, alpha function


#### 1. RELATIONS WITH AGE ####
# Read data
dat <- read.csv('C:/Users/mheise/Box/Research/BowmanLab/CAT_ContextUpdatingToM/Data/CATdata.csv',
                na = 'NA')

## Subset item-level variables
selectVars <- c('subNo', 'ageYrs', 'vocab_sum', 'grassSky_sum', 'upDown_sum',
                'divDes_a1_peWcontrol',
                'divDes_b12_peWcontrol',
                'divDes_c5_peWcontrol',
                'divDes_a9_peWcontrol',
                'divDes_c1_peWcontrol',
                'divDes_b10_peWcontrol',
                'divBel_a3_peWcontrol',
                'divBel_b7_peWcontrol',
                'divBel_a6_peWcontrol',
                'divBel_c8_peWcontrol',
                'trueBel_a7_peWcontrol',
                'trueBel_b1_peWcontrol',
                'trueBel_c12_peWcontrol',
                'falseBelContents_a8_peWcontrol',
                'falseBelContents_b3_peWcontrol',
                'falseBelContents_c4_peWcontrol',
                'falseBelLocation_a10_peWcontrol',
                'falseBelLocation_b11_peWcontrol',
                'falseBelLocation_c6_peWcontrol',
                'falseSign_a5_peWcontrol',
                'falseSign_a13_peWcontrol',
                'falseSign_b4_peWcontrol',
                'falseSign_b8_peWcontrol',
                'falseSign_c2_peWcontrol',
                'falseSign_c9_peWcontrol',
                'vpt1_a11_peWcontrol',
                'vpt1_a2_peWcontrol',
                'vpt1_b2_peWcontrol',
                'vpt1_b5_peWcontrol',
                'vpt2_c7_peWcontrol',
                'vpt2_c11_peWcontrol',
                'knowAcc_a12_peWcontrol',
                'knowAcc_b6_peWcontrol',
                'knowAcc_c3_peWcontrol',
                'knowExpert_a4_peWcontrol',
                'knowExpert_b9_peWcontrol',
                'knowExpert_c10_peWcontrol'
)

dat <- dat[selectVars]

# Pivot data long, create a new variable that designates the mental state category/
# false sign, two items are about 'cookies,' so relabel the vpt cookie item.
dat %>%
  pivot_longer(-c(subNo, ageYrs, vocab_sum, grassSky_sum, upDown_sum),
               names_to = c('name', 'itemDescription'),
               values_to = 'score',
               names_sep = '_') %>%
  mutate(mentalState = case_when(grepl(x = name, 'divDes') ~ 'divDes',
                                 grepl(x = name, 'divBel') ~ 'divBel',
                                 grepl(x = name, 'know') ~ 'know',
                                 grepl(x = name, 'falseBel') ~ 'falseBel',
                                 grepl(x = name, 'falseSign') ~ 'falseSign',
                                 grepl(x = name, 'vpt') ~ 'vpt',
                                 .default = NA),
         itemDescription = case_when(itemDescription == 'cookie' & 
                                       mentalState == 'vpt' ~ 'vptCookie',
                                 .default = itemDescription)) %>%
  filter(mentalState != 'trueBel') -> datL

# Fit linear mixed effects model predicting item score by an interaction between
# age and mental state category/false sign with covariates
fit <- lmer(score ~ ageYrs*mentalState + vocab_sum + grassSky_sum + upDown_sum + (1|subNo) + (1|itemDescription), data = datL)
summary(fit)

# Test model assumptions:
# - Linearity (visual inspection)
plot(resid(fit), dat$score) 

# - Normal distribution of residuals (visual inspection)
qqmath(fit) 

# - Homoscedasticity (test for equal variance across each mental state)
leveneTest(residuals(fit) ~ dat$mentalState) 


#### 2. ALPHA ####
## Calculate alpha within individual mental-state categories and false sign
# Diverse desires
divDes = c('divDes_a1_peWcontrol', 'divDes_b12_peWcontrol', 'divDes_c5_peWcontrol',
    'divDes_a9_peWcontrol', 'divDes_c1_peWcontrol', 'divDes_b10_peWcontrol')
sjt.itemanalysis(dat[divDes])

# Diverse beliefs
divBel = c('divBel_a3_peWcontrol', 'divBel_b7_peWcontrol', 'divBel_a6_peWcontrol', 'divBel_c8_peWcontrol')
sjt.itemanalysis(dat[divBel])

# Knowledge
know = c('knowAcc_a12_peWcontrol', 'knowAcc_b6_peWcontrol', 'knowAcc_c3_peWcontrol',
    'knowExpert_a4_peWcontrol', 'knowExpert_b9_peWcontrol', 'knowExpert_c10_peWcontrol')
sjt.itemanalysis(dat[know])

# False belief
falseBel = c('falseBelContents_a8_peWcontrol', 'falseBelContents_b3_peWcontrol', 'falseBelContents_c4_peWcontrol',
    'falseBelLocation_a10_peWcontrol', 'falseBelLocation_b11_peWcontrol', 'falseBelLocation_c6_peWcontrol')
sjt.itemanalysis(dat[falseBel])

# Visual perspective taking
vpt = c('vpt1_a11_peWcontrol', 'vpt1_a2_peWcontrol', 'vpt1_b2_peWcontrol', 
    'vpt1_b5_peWcontrol', 'vpt2_c7_peWcontrol', 'vpt2_c11_peWcontrol')
sjt.itemanalysis(dat[vpt])

# False sign
falseSign = c('falseSign_a5_peWcontrol', 'falseSign_a13_peWcontrol', 'falseSign_b4_peWcontrol',
    'falseSign_b8_peWcontrol', 'falseSign_c2_peWcontrol', 'falseSign_c9_peWcontrol')
sjt.itemanalysis(dat[falseSign])

# True belief
trueBel = c('trueBel_a7_peWcontrol', 'trueBel_b1_peWcontrol', 'trueBel_c12_peWcontrol')
sjt.itemanalysis(dat[trueBel])

# All items excluding true belief
allItems = c('divDes_a1_peWcontrol',
                'divDes_b12_peWcontrol',
                'divDes_c5_peWcontrol',
                'divDes_a9_peWcontrol',
                'divDes_c1_peWcontrol',
                'divDes_b10_peWcontrol',
                'divBel_a3_peWcontrol',
                'divBel_b7_peWcontrol',
                'divBel_a6_peWcontrol',
                'divBel_c8_peWcontrol',
                'falseBelContents_a8_peWcontrol',
                'falseBelContents_b3_peWcontrol',
                'falseBelContents_c4_peWcontrol',
                'falseBelLocation_a10_peWcontrol',
                'falseBelLocation_b11_peWcontrol',
                'falseBelLocation_c6_peWcontrol',
                'falseSign_a5_peWcontrol',
                'falseSign_a13_peWcontrol',
                'falseSign_b4_peWcontrol',
                'falseSign_b8_peWcontrol',
                'falseSign_c2_peWcontrol',
                'falseSign_c9_peWcontrol',
                'vpt1_a11_peWcontrol',
                'vpt1_a2_peWcontrol',
                'vpt1_b2_peWcontrol',
                'vpt1_b5_peWcontrol',
                'vpt2_c7_peWcontrol',
                'vpt2_c11_peWcontrol',
                'knowAcc_a12_peWcontrol',
                'knowAcc_b6_peWcontrol',
                'knowAcc_c3_peWcontrol',
                'knowExpert_a4_peWcontrol',
                'knowExpert_b9_peWcontrol',
                'knowExpert_c10_peWcontrol'
)
sjt.itemanalysis(dat[allItems])

# All mental-state items (excluding false sign)
allMental = c('divDes_a1_peWcontrol',
             'divDes_b12_peWcontrol',
             'divDes_c5_peWcontrol',
             'divDes_a9_peWcontrol',
             'divDes_c1_peWcontrol',
             'divDes_b10_peWcontrol',
             'divBel_a3_peWcontrol',
             'divBel_b7_peWcontrol',
             'divBel_a6_peWcontrol',
             'divBel_c8_peWcontrol',
             'falseBelContents_a8_peWcontrol',
             'falseBelContents_b3_peWcontrol',
             'falseBelContents_c4_peWcontrol',
             'falseBelLocation_a10_peWcontrol',
             'falseBelLocation_b11_peWcontrol',
             'falseBelLocation_c6_peWcontrol',
             'vpt1_a11_peWcontrol',
             'vpt1_a2_peWcontrol',
             'vpt1_b2_peWcontrol',
             'vpt1_b5_peWcontrol',
             'vpt2_c7_peWcontrol',
             'vpt2_c11_peWcontrol',
             'knowAcc_a12_peWcontrol',
             'knowAcc_b6_peWcontrol',
             'knowAcc_c3_peWcontrol',
             'knowExpert_a4_peWcontrol',
             'knowExpert_b9_peWcontrol',
             'knowExpert_c10_peWcontrol'
)
sjt.itemanalysis(dat[allMental])
