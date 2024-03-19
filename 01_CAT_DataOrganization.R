####################################################
##                                                ##
##   Comprehensive Assessment of Theory of mind   ##
##                                                ##
##                   MJ Heise                     ##
##                  April 2023                    ##
##                                                ##
####################################################

# CODE DESCRIPTION: This script is part of the pre-registration for CAT: https://osf.io/4d9ar/
# 1. GRASS SKY DATA CLEANING: 
# As part of the CAT study, data were collected through the Gorilla web-based platform.
# This code reads in and processes Grass/Sky data to produce sum scores (out of 16 
# total possible points). 
#
# 2. UP DOWN DATA CLEANING:
# Up/down data were coded in datavyu, and data from 2 independent coders were checked in
# excel. Up/down data were scored as the total number of points earned on 20 trials 
# (out of 60 possible points) and participants with partial data were marked as missing data 
# for this task.
#
# 3. TOM DATA CLEANING:
# ToM data were entered as prediction, explanation, and control responses. This code reads in
# and creates composites (referred to in Heise & Bowman as 'items') of prediction & 
# explanations, and prediction items only accounting for participants' performance
# on control questions.
#
# 4. MERGE DATA:
# Read in data from the subject data log and KBIT-2 (Kaufman Brief Intelligence Test-2),
# and merge with grass/sky, up/down, and the CAT ToM measure. 


# Libraries
library(psych) # v.2.3.9, describe function
library(tidyverse) # v.2.0.0, piping and data wrangling
library(readxl) #v.1.4.3, read in and write excel files

#### 1. GRASS SKY DATA CLEANING ####
direct <- 'C:/Users/mjheise/Box Sync/Research/BowmanLab/CAT_ContextUpdatingToM/Data'
setwd(direct)

# Read in data
gsOrig <- read.csv('GS_20220903.csv')

# Subset data by test trials
gsOrig %>%
  filter(display == 'Trials' & (Response == 'greensquare.png' | Response == 'bluesquare.png')) -> gs

# Check that all subjects have 16 trials
gs %>%
  count(Participant.Private.ID) -> table

summary(table)

# Sum of correct trials per subject
gs %>%
  group_by(Participant.Private.ID) %>%
  summarize(gsCorrectTotal = sum(Correct)) -> gs

# Read in subject ID list
names <- read.csv('Gorilla_LinkingIDs_20220903.csv')

names <- names[which(names$Question.Key == 'subNo'), ]

vars <- c('Participant.Private.ID', 'Response')
names <- names[vars]

# Merge subject IDs with data
GS <- merge(names, gs, by = 'Participant.Private.ID', all = T)


#### 2. UP DOWN DATA CLEANING
upOrig <- read_excel('C:/Users/mjheise/Box Sync/Research/BowmanLab/CAT_ContextUpdatingToM/Data/upDown/CAT_UpDown_Final_SingleRows_20230206.xlsx', na = c('n/c', 'NA'))

# Select variable names containing "Score", "totalPractice", and "subNo"
vars <- names(upOrig) %>%
  grep(pattern = "subNo|exclude|Score|totalPractice", x = ., value = TRUE)

# Subset the data to include only selected variables
upOrig %>%
  select(all_of(vars)) -> upSubset

# Exclude any participants with issues in task administration
upSubset %>%
  subset(exclude == 'N') -> up

up %>%
  mutate_at(vars(4:31), as.numeric) %>%
  mutate(upDown_sum = rowSums(select(., 12:30))) -> up

# Calculate mean and SD of the number of practice trials in the final sample included in analyses
describe(up)


#### 3. TOM DATA CLEANING ####
tom <- read_excel('C:/Users/mjheise/Box Sync/Research/BowmanLab/CAT_ContextUpdatingToM/Data/theoryOfMind/CAT_ItemScoring_20230126.xlsx', skip=2, na = 'NA')

tom <- read_excel('C:/Users/mheise/Box/Research/BowmanLab/CAT_ContextUpdatingToM/Data/theoryOfMind/CAT_ItemScoring_20230126.xlsx', skip=2, na = 'NA')

# ITEMS AS A SUM OF PREDICTION AND EXPLANATION QUESTIONS
# Create composites for items as a sum of prediction and explanation questions, where
# if participants get the control question incorrect (i.e., score 0) then they 
# also receive a score of 0 on that item. 
tom %>%
  mutate(divDes_a1_peWcontrol = ifelse(divDes_2choice_A_hat_cntrl == 1, 
                                  divDes_2choice_A_hat_expl + divDes_2choice_A_hat_pred, 
                                  0),
    divDes_b12_peWcontrol = ifelse(divDes_2choice_B_picnic_cntrl == 1, 
                                   divDes_2choice_B_picnic_expl + divDes_2choice_B_picnic_pred, 
                                   0),
    divDes_c5_peWcontrol = ifelse(divDes_2choice_C_toys_cntrl == 1, 
                                  divDes_2choice_C_toys_expl + divDes_2choice_C_toys_pred, 
                                  0),
    divDes_a9_peWcontrol = ifelse(divDes_boyGirl_A_book_cntrl == 1, 
                                  divDes_boyGirl_A_book_expl + divDes_boyGirl_A_book_pred, 
                                  0),
    divDes_c1_peWcontrol = ifelse(divDes_boyGirl_C_ball_cntrl == 1, 
                                  divDes_boyGirl_C_ball_expl + divDes_boyGirl_C_ball_pred, 
                                  0),
    divDes_b10_peWcontrol = ifelse(divDes_boyGirl_B_grapes_cntrl == 1, 
                                   divDes_boyGirl_B_grapes_expl + divDes_boyGirl_B_grapes_pred, 
                                   0),
    divBel_a3_peWcontrol = ifelse(divBel_2choice_A_snack_cntrl == 1, 
                                  divBel_2choice_A_snack_expl + divBel_2choice_A_snack_pred, 
                                  0),
    divBel_b7_peWcontrol = ifelse(divBel_2choice_B_bunny_cntrl == 1, 
                                  divBel_2choice_B_bunny_expl + divBel_2choice_B_bunny_pred, 
                                  0),
    divBel_a6_peWcontrol = ifelse(divBel_boyGirl_A_gift_cntrl == 1, 
                                  divBel_boyGirl_A_gift_expl + divBel_boyGirl_A_gift_pred, 
                                  0),
    divBel_c8_peWcontrol = ifelse(divBel_boyGirl_C_cat_cntrl == 1, 
                                  divBel_boyGirl_C_cat_expl + divBel_boyGirl_C_cat_pred, 
                                  0),
    trueBel_a7_peWcontrol = ifelse(trueBel_na_A_snack_cntrl == 1, 
                                   trueBel_na_A_snack_expl + trueBel_na_A_snack_pred, 
                                   0),
    trueBel_b1_peWcontrol = ifelse(trueBel_na_B_cracker_cntrl == 1, 
                                   trueBel_na_B_cracker_expl + trueBel_na_B_cracker_pred, 
                                   0),
    trueBel_c12_peWcontrol = ifelse(trueBel_na_C_shoes_cntrl == 1, 
                                    trueBel_na_C_shoes_expl + trueBel_na_C_shoes_pred, 
                                    0),
    falseBelContents_a8_peWcontrol = ifelse(falseBel_contents_A_bandAid_cntrl == 1, 
                                            falseBel_contents_A_bandAid_expl + falseBel_contents_A_bandAid_pred, 
                                            0),
    falseBelContents_b3_peWcontrol = ifelse(falseBel_contents_B_crayon_cntrl == 1, 
                                            falseBel_contents_B_crayon_expl + falseBel_contents_B_crayon_pred, 
                                            0),
    falseBelContents_c4_peWcontrol = ifelse(falseBel_contents_C_cheerios_cntrl == 1, 
                                            falseBel_contents_C_cheerios_expl + falseBel_contents_C_cheerios_pred, 
                                            0),
    falseBelLocation_a10_peWcontrol = ifelse(falseBel_location_A_peach_cntrl == 1, 
                                             falseBel_location_A_peach_expl + falseBel_location_A_peach_pred, 
                                             0),
    falseBelLocation_b11_peWcontrol = ifelse(falseBel_location_B_cookie_cntrl == 1, 
                                             falseBel_location_B_cookie_expl + falseBel_location_B_cookie_pred, 
                                             0),
    falseBelLocation_c6_peWcontrol = ifelse(falseBel_location_C_allieSnack_cntrl == 1, 
                                            falseBel_location_C_allieSnack_expl + falseBel_location_C_allieSnack_pred, 
                                            0),
    falseSign_a5_peWcontrol = ifelse(falseSign_na_A_bakery_cntrl == 1, 
                                     falseSign_na_A_bakery_expl + falseSign_na_A_bakery_pred, 
                                     0),
    falseSign_a13_peWcontrol = ifelse(falseSign_na_A_carrotStand_cntrl == 1, 
                                      falseSign_na_A_carrotStand_expl + falseSign_na_A_carrotStand_pred, 
                                      0),
    falseSign_b4_peWcontrol = ifelse(falseSign_na_B_airport_cntrl == 1, 
                                     falseSign_na_B_airport_expl + falseSign_na_B_airport_pred, 
                                     0),
    falseSign_b8_peWcontrol = ifelse(falseSign_na_B_cupboards_cntrl == 1, 
                                     falseSign_na_B_cupboards_expl + falseSign_na_B_cupboards_pred, 
                                     0),
    falseSign_c2_peWcontrol = ifelse(falseSign_na_C_horse_cntrl == 1, 
                                     falseSign_na_C_horse_expl + falseSign_na_C_horse_pred, 
                                     0),
    falseSign_c9_peWcontrol = ifelse(falseSign_na_C_iceCream_cntrl == 1, 
                                     falseSign_na_C_iceCream_expl + falseSign_na_C_iceCream_pred, 
                                     0),
    vpt1_a11_peWcontrol = ifelse(vpt_na_A_mountains_cntrl == 1, 
                                 vpt_na_A_mountains_expl + vpt_na_A_mountains_pred, 
                                 0),
    vpt1_a2_peWcontrol = ifelse(vpt_na_A_bird_cntrl == 1, 
                                vpt_na_A_bird_expl + vpt_na_A_bird_pred, 
                                0),
    vpt1_b2_peWcontrol = ifelse(vpt_na_B_giraffe_cntrl == 1, 
                                vpt_na_B_giraffe_expl + vpt_na_B_giraffe_pred, 
                                0),
    vpt1_b5_peWcontrol = ifelse(vpt_na_B_raisins_cntrl == 1, 
                                vpt_na_B_raisins_expl + vpt_na_B_raisins_pred, 
                                0),
    vpt2_c7_peWcontrol = ifelse(vpt_na_C_apple_cntrl == 1, 
                                vpt_na_C_apple_expl + vpt_na_C_apple_pred, 
                                0),
    vpt2_c11_peWcontrol = ifelse(vpt_na_C_cookie_cntrl == 1, 
                                 vpt_na_C_cookie_expl + vpt_na_C_cookie_pred, 
                                 0),
    knowAcc_a12_peWcontrol = ifelse(knowAcc_na_A_alligator_cntrl == 1, 
                                    knowAcc_na_A_alligator_expl + knowAcc_na_A_alligator_pred, 
                                    0),
    knowAcc_b6_peWcontrol = ifelse(knowAcc_na_B_flower_cntrl == 1, 
                                   knowAcc_na_B_flower_expl + knowAcc_na_B_flower_pred, 
                                   0),
    knowAcc_c3_peWcontrol = ifelse(knowAcc_na_C_spoon_cntrl == 1, 
                                   knowAcc_na_C_spoon_expl + knowAcc_na_C_spoon_pred, 
                                   0),
    knowExpert_a4_peWcontrol = ifelse(knowExpert_na_A_plane_cntrl == 1, 
                                      knowExpert_na_A_plane_expl + knowExpert_na_A_plane_pred, 
                                      0),
    knowExpert_b9_peWcontrol = ifelse(knowExpert_na_B_cooking_cntrl == 1, 
                                      knowExpert_na_B_cooking_expl + knowExpert_na_B_cooking_pred, 
                                      0),
    knowExpert_c10_peWcontrol = ifelse(knowExpert_na_C_car_cntrl == 1, 
                                       knowExpert_na_C_car_expl + knowExpert_na_C_car_pred, 
                                       0)) -> tom

# PREDICTION ONLY ITEMS
# Create scores for prediction only items, where if participants get the control 
# question incorrect (i.e., score 0) then they also receive a score of 0 on that item. 
tom %>%
  mutate(
    divDes_a1_pWcontrol = ifelse(divDes_2choice_A_hat_cntrl == 1, divDes_2choice_A_hat_pred, 0),
    divDes_b12_pWcontrol = ifelse(divDes_2choice_B_picnic_cntrl == 1, divDes_2choice_B_picnic_pred, 0),
    divDes_c5_pWcontrol = ifelse(divDes_2choice_C_toys_cntrl == 1, divDes_2choice_C_toys_pred, 0),
    divDes_a9_pWcontrol = ifelse(divDes_boyGirl_A_book_cntrl == 1, divDes_boyGirl_A_book_pred, 0),
    divDes_c1_pWcontrol = ifelse(divDes_boyGirl_C_ball_cntrl == 1, divDes_boyGirl_C_ball_pred, 0),
    divDes_b10_pWcontrol = ifelse(divDes_boyGirl_B_grapes_cntrl == 1, divDes_boyGirl_B_grapes_pred, 0),
    divBel_a3_pWcontrol = ifelse(divBel_2choice_A_snack_cntrl == 1, divBel_2choice_A_snack_pred, 0),
    divBel_b7_pWcontrol = ifelse(divBel_2choice_B_bunny_cntrl == 1, divBel_2choice_B_bunny_pred, 0),
    divBel_a6_pWcontrol = ifelse(divBel_boyGirl_A_gift_cntrl == 1, divBel_boyGirl_A_gift_pred, 0),
    divBel_c8_pWcontrol = ifelse(divBel_boyGirl_C_cat_cntrl == 1, divBel_boyGirl_C_cat_pred, 0),
    trueBel_a7_pWcontrol = ifelse(trueBel_na_A_snack_cntrl == 1, trueBel_na_A_snack_pred, 0),
    trueBel_b1_pWcontrol = ifelse(trueBel_na_B_cracker_cntrl == 1, trueBel_na_B_cracker_pred, 0),
    trueBel_c12_pWcontrol = ifelse(trueBel_na_C_shoes_cntrl == 1, trueBel_na_C_shoes_pred, 0),
    falseBelContents_a8_pWcontrol = ifelse(falseBel_contents_A_bandAid_cntrl == 1, falseBel_contents_A_bandAid_pred, 0),
    falseBelContents_b3_pWcontrol = ifelse(falseBel_contents_B_crayon_cntrl == 1, falseBel_contents_B_crayon_pred, 0),
    falseBelContents_c4_pWcontrol = ifelse(falseBel_contents_C_cheerios_cntrl == 1, falseBel_contents_C_cheerios_pred, 0),
    falseBelLocation_a10_pWcontrol = ifelse(falseBel_location_A_peach_cntrl == 1, falseBel_location_A_peach_pred, 0),
    falseBelLocation_b11_pWcontrol = ifelse(falseBel_location_B_cookie_cntrl == 1, falseBel_location_B_cookie_pred, 0),
    falseBelLocation_c6_pWcontrol = ifelse(falseBel_location_C_allieSnack_cntrl == 1, falseBel_location_C_allieSnack_pred, 0),
    falseSign_a5_pWcontrol = ifelse(falseSign_na_A_bakery_cntrl == 1, falseSign_na_A_bakery_pred, 0),
    falseSign_a13_pWcontrol = ifelse(falseSign_na_A_carrotStand_cntrl == 1, falseSign_na_A_carrotStand_pred, 0),
    falseSign_b4_pWcontrol = ifelse(falseSign_na_B_airport_cntrl == 1, falseSign_na_B_airport_pred, 0),
    falseSign_b8_pWcontrol = ifelse(falseSign_na_B_cupboards_cntrl == 1, falseSign_na_B_cupboards_pred, 0),
    falseSign_c2_pWcontrol = ifelse(falseSign_na_C_horse_cntrl == 1, falseSign_na_C_horse_pred, 0),
    falseSign_c9_pWcontrol = ifelse(falseSign_na_C_iceCream_cntrl == 1, falseSign_na_C_iceCream_pred, 0),
    vpt1_a11_pWcontrol = ifelse(vpt_na_A_mountains_cntrl == 1, vpt_na_A_mountains_pred, 0),
    vpt1_a2_pWcontrol = ifelse(vpt_na_A_bird_cntrl == 1, vpt_na_A_bird_pred, 0),
    vpt1_b2_pWcontrol = ifelse(vpt_na_B_giraffe_cntrl == 1, vpt_na_B_giraffe_pred, 0),
    vpt1_b5_pWcontrol = ifelse(vpt_na_B_raisins_cntrl == 1, vpt_na_B_raisins_pred, 0),
    vpt2_c7_pWcontrol = ifelse(vpt_na_C_apple_cntrl == 1, vpt_na_C_apple_pred, 0),
    vpt2_c11_pWcontrol = ifelse(vpt_na_C_cookie_cntrl == 1, vpt_na_C_cookie_pred, 0),
    knowAcc_a12_pWcontrol = ifelse(knowAcc_na_A_alligator_cntrl == 1, knowAcc_na_A_alligator_pred, 0),
    knowAcc_b6_pWcontrol = ifelse(knowAcc_na_B_flower_cntrl == 1, knowAcc_na_B_flower_pred, 0),
    knowAcc_c3_pWcontrol = ifelse(knowAcc_na_C_spoon_cntrl == 1, knowAcc_na_C_spoon_pred, 0),
    knowExpert_a4_pWcontrol = ifelse(knowExpert_na_A_plane_cntrl == 1, knowExpert_na_A_plane_pred, 0),
    knowExpert_b9_pWcontrol = ifelse(knowExpert_na_B_cooking_cntrl == 1, knowExpert_na_B_cooking_pred, 0),
    knowExpert_c10_pWcontrol = ifelse(knowExpert_na_C_car_cntrl == 1, knowExpert_na_C_car_pred, 0)) -> tom

#### 4. MERGE DATA ####
# Read in subject data log
sub <- read_excel('C:/Users/mjheise/Box Sync/Research/BowmanLab/CAT_ContextUpdatingToM/Data/subjectDataLog/CAT_SubjectDataLog_20220810.xlsx', skip=2, na = 'NA')

# Read in Kaufman Brief Intelligence Test-2 data
kbit <- read_excel('C:/Users/mjheise/Box Sync/Research/BowmanLab/CAT_ContextUpdatingToM/Data/kbit/KBIT_20230206.xlsx')

# Merge dataframes to create final dataframe
dat <- left_join(sub, tom, by = 'subNo') %>%
  left_join(kbit, by = 'subNo') %>%
  left_join(gs, by = 'subNo') %>%
  left_join(up, by = 'subNo')
