####################################################
##                                                ##
##   Comprehensive Assessment of Theory of mind   ##
##                                                ##
##                   MJ Heise                     ##
##                  April 2023                    ##
##                                                ##
####################################################

# CODE DESCRIPTION: This script is part of the pre-registration for CAT: https://osf.io/4d9ar/
# 1. SUMMARY OF SAMPLE DEMOGRAPHICS:
# This code outputs mean and SDs of participants' ages, number of participants
# in each age grouping and gender category, and summary of participant race and ethnicity,
# parent education and family income. 
#
# 2. CREATE AVERAGE SCORES FOR EACH MENTAL-STATE CATEGORY AND FALSE SIGN:
# This code creates averages of performance on CAT items within each mental-state 
# category and false sign. 
#
# 3. EXAMINE AGE-RELATED CHANGES IN MENTAL-STATE AND FALSE SIGN UNDERSTANDING:
# Create scatterplots and histograms of age-related improvements in mental-state
# and false sign understanding. 
#
# 4. ITEM-LEVEL MEANS AND SDS:
# Create tables of means, sds, and number of participants who completed each item
# grouped by age (e.g., 3-year-olds, 4-year-olds, etc.)


# Libraries
library(psych) # v.2.3.9, describe function
library(tidyverse) # v.2.0.0, piping and data wrangling
library(readxl) #v.1.4.3, read in and write excel files
library(officer) #v.0.6.3, create_pptx function
library(stringr) #v.1.5.0, create_pptx function
library(rvg) #v.0.3.3, create_pptx function

# Function to save ggplot object in powerpoint slide
# -Input: The ggplot object that you want to save in a powerpoint.
# -Optional inputs: Specified width and height of the outputted graph.
#  If no arguments are specified, the graph will encompass the entire
#  powerpoint slide.
# -Notes: After running the function, a window will open that 
#  allows you to select the powerpoint file. The graph will
#  save on a new slide at the end of the powerpoint.
create_pptx <- function(plt = last_plot(), path = file.choose(), width = 0, height = 0){
  if(!file.exists(path)) {
    out <- read_pptx()
  } else {
    out <- read_pptx(path)
  }
  
  if (width != 0 & height != 0) {
    out %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = dml(ggobj = plt), location = ph_location(left = 0, top = 0,
                                                               width = width, height = height)) %>%
      print(target = path)
  } else {
    out %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = dml(ggobj = plt), location = ph_location_fullsize()) %>%
      print(target = path)
    
  }
  
}

# Function to create average scores (out of items completed) for mental-state categories
# and false sign
# -Input: list of items within the mental-state/false sign category
calculate_avg_scores <- function(dat, items) {
  # Calculate the number of items each child completed
  completed <- rowSums(!is.na(dat[items]))
  
  # Calculate the sum of correct items
  correct <- rowSums(dat[items], na.rm = TRUE)
  
  # Calculate the proportion of correct items
  avg_scores <- correct / completed
  
  return(avg_scores)
}


#### 1. SUMMARY OF SAMPLE DEMOGRAPHICS ####
# Read in data
dat <- read.csv('C:/Users/mheise/Box/Research/BowmanLab/CAT_ContextUpdatingToM/Data/CATdata.csv')

# Means, SDs and ranges for variables
describe(dat)

# Summarize mean age of participants by age group
dat %>%
  group_by(ageYrsRound) %>%
  summarise_at(vars(ageYrs), c(mean, sd))

# Count number of participants in each gender and age cell
dat %>% count(ageYrsRound, sex)

# Demographic summary
table(dat$HispRace)
table(dat$parent1_Ed)
table(dat$parent2_Ed)
table(dat$familyIncome)
table(dat$responseGorilla)
table(dat$device_summary)

#### 2. CREATE AVERAGE SCORES FOR EACH MENTAL-STATE CATEGORY AND FALSE SIGN ####
## Diverse Desires
ddItems <- c('divDes1_hat', 'divDes2_picnic', 'divDes3_puzzle', 'divDes4_book', 'divDes5_ball', 'divDes6_grape')

# Calculate average out of items completed
dat$divDesAvg <- calculate_avg_scores(dat, ddItems)

## Diverse Beliefs
dbItems <- c('divBel1_snack', 'divBel2_bunny', 'divBel3_gift', 'divBel4_cat')

# Calculate average out of items completed
dat$divBelAvg <- calculate_avg_scores(dat, dbItems)

## Knowledge
kItems <- c('knowAcc1_alligator', 'knowAcc2_flower', 'knowAcc3_spoon', 
              'knowExpert1_plane', 'knowExpert2_cooking', 'knowExpert3_car')

# Calculate average out of items completed
dat$knowAvg <- calculate_avg_scores(dat, kItems)

## Visual perspective taking
vptItems <- c('vpt1_mountain', 'vpt2_bird', 'vpt3_giraffe','vpt4_raisin', 'vpt5_apple', 'vpt6_cookie')

# Calculate average out of items completed
dat$vptAvg <- calculate_avg_scores(dat, kItems)

## False belief
fbItems <- c('falseBelContents1_bandAid', 'falseBelContents2_crayon', 'falseBelContents3_cheerios', 
              'falseBelLocation1_peach', 'falseBelLocation2_cookie', 'falseBelLocation3_bag')

dat$falseBelAvg <- calculate_avg_scores(dat, fbItems)

## False sign
fsItems <- c('falseSign1_bakery', 'falseSign2_carrot', 'falseSign3_airport', 
              'falseSign4_cupboard', 'falseSign5_horse', 'falseSign6_iceCream')

dat$falseSignAvg <- calculate_avg_scores(dat, fsItems)

## True belief
tbItems <- c('trueBel1_snack', 'trueBel2_cracker', 'trueBel3_shoes')

dat$trueBelAvg <- calculate_avg_scores(dat, fsItems)


#### 3. EXAMINE AGE-RELATED CHANGES IN MENTAL-STATE AND FALSE SIGN UNDERSTANDING ####
# Variables to examine
vars <- c('ageYrs', 'vocab_sum', 'divDesAvg', 'divBelAvg', 'knowAvg', 'vptAvg', 'falseBelAvg', 'falseSignAvg')

# Examine mean ToM and false sign performance across age categories
describeBy(dat[vars], group=dat$ageYrsRound)

## Create scatterplots of average scores by age
gg1 <- ggplot(dat, aes(x=ageYrs, y=divDesAvg)) + 
  geom_jitter(
    color="#ff5c9a",
    alpha=0.5,
    size=6) + 
  theme_minimal()

gg2 <- ggplot(dat, aes(x=ageYrs, y=divBelAvg)) + 
  geom_jitter(
    color="#63a6f7",
    alpha=0.5,
    size=6) + 
  theme_minimal()

gg3 <- ggplot(dat, aes(x=ageYrs, y=knowAvg)) + 
  geom_jitter(
    color="#f8c931",
    alpha=0.5,
    size=6) + 
  theme_minimal()

gg4 <- ggplot(dat, aes(x=ageYrs, y=vptAvg)) + 
  geom_jitter(
    color="#f17a0d",
    alpha=0.5,
    size=6) + 
  theme_minimal()

gg5 <- ggplot(dat, aes(x=ageYrs, y=falseSignAvg)) + 
  geom_jitter(
    color="#a88ae6",
    alpha=0.5,
    size=6) + 
  theme_minimal()

gg6 <- ggplot(dat, aes(x=ageYrs, y=falseBelAvg)) + 
  geom_jitter(
    color="#65ccaf",
    alpha=0.5,
    size=6) + 
  theme_minimal()

create_pptx(gg1)
create_pptx(gg2)
create_pptx(gg3)
create_pptx(gg4)
create_pptx(gg5)
create_pptx(gg6)

## Create histograms of mental-state and false sign performance across age groupings
# Save means across mental state categories for each age group
dat %>% select(ageYrsRound, divDesAvg, divBelAvg, knowAvg, vptAvg, falseBelAvg, falseSignAvg) %>%
  group_by(ageYrsRound) %>%
  summarise(ddAvg = mean(divDesAvg),
            dbAvg = mean(divBelAvg),
            kaAvg = mean(knowAvg),
            vpAvg = mean(vptAvg),
            fbAvg = mean(falseBelAvg),
            fsAvg = mean(falseSignAvg)) %>%
  pivot_longer(!ageYrsRound, names_to = "mental", values_to = "mean") -> datMeans

# Save SEs across mental state categories for each age group
dat %>% select(ageYrsRound, divDesAvg, divBelAvg, knowAvg, vptAvg, falseBelAvg, falseSignAvg) %>%
  group_by(ageYrsRound) %>%
  summarise(ddAvg = std.error(divDesAvg),
            dbAvg = std.error(divBelAvg),
            kaAvg = std.error(knowAvg),
            vpAvg = std.error(vptAvg),
            fbAvg = std.error(falseBelAvg),
            fsAvg = std.error(falseSignAvg)) %>%
  pivot_longer(!ageYrsRound, names_to = "mental", values_to = "se") -> datError

# Create df of means and SEs
datHist = merge(datMeans, datError, by = c('ageYrsRound', 'mental'), all = T)
datHist$mental <- factor(datHist$mental, levels = c("ddAvg", "dbAvg", "kaAvg",
                                                    "vpAvg", "fbAvg", 'fsAvg'))

# Histogram of mental-state and false sign performance across ages
gghist1 <- ggplot(datHist, aes(x=ageYrsRound, y=mean, fill = mental)) + 
  geom_bar(stat = "identity",
           position = "dodge2") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.9)) + 
  theme_minimal()

create_pptx(gghist1)


#### 4. ITEM-LEVEL MEANS AND SDS ####
## Subset item-level variables
selectVars <- c('subNo', 'ageYrsRound',
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

# Create dataframe of item means and SEs for each age group
dat %>%
  group_by(ageYrsRound) %>%
  summarise(across(.cols = everything(), .fns = list(mean = ~mean(., na.rm = TRUE), 
                                                     sd = ~sd(., na.rm = TRUE)), .names = "{col}_{fn}")) %>%
  pivot_longer(cols = -ageYrsRound, names_to = "variable", values_to = "value") -> ageItemMeans

# Create dataframe of the number of participants with each item for each age group
dat %>%
  group_by(ageYrsRound) %>%
  summarise(across(.cols = everything(), .fns = list(n_obs = ~sum(!is.na(.)))), .names = "{col}_{fn}") -> numObs

# Means for grass/sky and up/down for each age group
dat %>%
  group_by(ageYrsRound) %>%
  summarise(gsMean = mean(grassSky_sum, na.rm = T),
            gsSD = sd(grassSky_sum, na.rm = T),
            upDownMean = mean(upDown_sum, na.rm = T),
            upDownSD = sd(upDown_sum, na.rm = T))

