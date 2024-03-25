####################################################
##                                                ##
##   Comprehensive Assessment of Theory of mind   ##
##                                                ##
##                   MJ Heise                     ##
##                  June 2023                     ##
##                                                ##
####################################################

## CODE DESCRIPTION: This script is part of the pre-registration for CAT: https://osf.io/4d9ar/
# 1. CHECK MODEL ASSUMPTIONS:
# This script checks for the assumption of unidimensionality using EFA and CFA. 
# Data are independent because each datapoint comes from a unique story. 
#
# 2. GRADED RESPONSE MODEL:
# This script fits a GRM, prints model summary containing coefficients
# (difficulty, general difficulty by Ali et al. [2015] & discrimination), 
# creates plots for item characteristic curves and item information curves, 
# and extracts theta parameter for each participant. 

# Libraries
library(tidyverse) # v.2.0.0, data tidying
library(lavaan) # v.0.6-16, fit single-factor CFA model
library(psych) # v.2.3.9, exploratory factor analysis
library(ltm) # v.1-2.0, item response theory model
library(stringi) # v.1.7.12, strings
library(mirt) # v.1.41, theta parameter from 2PL
library(ggplot2) # v.3.4.3, data visualization
library(officer) # V.0.3.15; powerpoint ggplot
library(rvg) # V.0.2.5; powerpoint ggplot

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


#### 1. CHECK MODEL ASSUMPTIONS ####
# Read in data
dat <- read.csv('C:/Users/mheise/Box/Research/BowmanLab/CAT_ContextUpdatingToM/Data/CATdata.csv',
                na = 'NA')

selectVars <- c(
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

datIRT <- dat[selectVars]

## Exploratory factory analysis
psych::scree(datIRT)
# EFA suggests a single unitary factor structure using principal components (PC)
# and factor analysis (FA)

## Confirmatory factor analysis
singleFactor <- 'tom =~ 
    divDes_a1_peWcontrol + divDes_b12_peWcontrol + divDes_c5_peWcontrol + 
    divDes_a9_peWcontrol + divDes_c1_peWcontrol + divDes_b10_peWcontrol + 
    knowAcc_a12_peWcontrol + knowAcc_b6_peWcontrol + knowAcc_c3_peWcontrol + 
    knowExpert_a4_peWcontrol + knowExpert_b9_peWcontrol + knowExpert_c10_peWcontrol + 
    vpt1_a11_peWcontrol + vpt1_a2_peWcontrol + vpt1_b2_peWcontrol + 
    vpt1_b5_peWcontrol + vpt2_c7_peWcontrol + vpt2_c11_peWcontrol + 
    divBel_a3_peWcontrol + divBel_b7_peWcontrol + divBel_a6_peWcontrol + 
    divBel_c8_peWcontrol + falseBelContents_a8_peWcontrol + 
    falseBelContents_b3_peWcontrol + falseBelContents_c4_peWcontrol +
    falseBelLocation_a10_peWcontrol + falseBelLocation_b11_peWcontrol + 
    falseBelLocation_c6_peWcontrol + falseSign_a5_peWcontrol + 
    falseSign_a13_peWcontrol + falseSign_b4_peWcontrol + 
    falseSign_b8_peWcontrol + falseSign_c2_peWcontrol + 
    falseSign_c9_peWcontrol'

singleFactorFit <- cfa(model = singleFactor, data = dat, estimator = "WLSMV", missing = "pairwise", std.lv = TRUE, ordered = TRUE)
summary(singleFactorFit, std = TRUE, fit = TRUE)

# Fit indices
fitmeasures(singleFactorFit)
# Fit indices are excellent, meeting assumptions of unidimensionality


#### 2. GRADED RESPONSE MODEL ####
## 2 PL model coefficients
pIRT = grm(datIRT)
coef(pIRT) 

## Item characteristic curves
ICCplot <- as.data.frame(plot(pIRT,type=c("ICC"))) 

# Pivot data to long
df <- ICCplot %>% 
  pivot_longer(2:103) %>%
  mutate(response = stri_sub(name, -1)) %>%
  mutate(task = sub("_peWcontrol.*", "", name)) %>%
  mutate(task = sub(".*pr.", "", task))

# Create facet-wrapped plot of item characteristic curves (for all items)
ICCgg <- ggplot(df, aes(x=z, y=value, color=response)) +
  geom_line() + ylim(0, 1) +
  ggtitle('Item Characteristic Curves for CAT') +
  scale_color_manual(values=c("#d55e00", "#ffc04c", "#009e73")) + 
  theme_minimal() +
  facet_wrap(~task)

# Save to pptx
create_pptx(ICCgg)

## Item information curves
IICplot <- as.data.frame(plot(pIRT,type=c("IIC"))) 

IICplot %>% pivot_longer(2:35) -> IICplot

# Subset data by each mental-state or false sign
db <- IICplot %>% filter(grepl("divBel", name))
dd <- IICplot %>% filter(grepl("divDes", name))
fb <- IICplot %>% filter(grepl("falseBel", name))
fs <- IICplot %>% filter(grepl("falseSign", name))
know <- IICplot %>% filter(grepl("know", name))
vp <- IICplot %>% filter(grepl("vpt", name))

# Plot item information curve for each mental-state or false sign category
IICgg <- ggplot(vp, aes(x=z, y=value, color = name)) +
  geom_line() + ylim(0,1.6) + 
  ggtitle('Item information Curves for CAT') +
  scale_color_manual(values=c("#f9993c", "#dd5c50", "#b95461", "#26465f", "#7985a5", '#b3c2e8')) + 
  theme_minimal()

# Save to pptx
create_pptx(IICgg)

## Test information function
TIFplot <- as.data.frame(plot(pIRT,type=c("IIC"),items=c(0)))

# Plot TIF
TIFgg <- ggplot(TIFplot, aes(x=z, y=test.info)) +
  geom_line() + ylim(0, 30) +
  ggtitle('Test Information Function for CAT') +
  theme_minimal()

# Save to pptx
create_pptx(TIFgg)

## Extract theta from the model
# Fit GRM in MIRT package
mirt.grm <- mirt(datIRT, itemtype = 'graded')

# Save theta in with each participant
dat$theta <- fscores(mirt(datIRT, 1))

# Subset data for participants with theta within the range for highest information
# in the GRM
datTheta <- dat[which(dat$theta <= 1 & dat$theta >= -2.75), ]

# Summary stats for participants in the data subset
describe(datTheta)

# Histogram of ages of participants in data subset (who the measure had high 
# information for)
histGG <- ggplot(datTheta, aes(x=ageYrs)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'gray') + 
  ggtitle('Test Information Function Ages') + ylim(0,40) + 
  theme_minimal()

# Save pptx
create_pptx(histGG)

## Single difficulty score, Ali, Chang & Anderson (2015)
diff <- as.data.frame(gen.difficulty(mirt.grm))
