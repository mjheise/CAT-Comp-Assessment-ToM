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
# 2. 2-PARAMETER LOGISTIC MODEL:
# This script fits a 2PL model, prints model summary containing coefficients
# (difficulty & discrimination), creates plots for item characteristic curves and
# item information curves, and extracts theta parameter for each participant. 

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
  'divDes_a1_pWcontrol',
  'divDes_b12_pWcontrol',
  'divDes_c5_pWcontrol',
  'divDes_a9_pWcontrol',
  'divDes_c1_pWcontrol',
  'divDes_b10_pWcontrol',
  'divBel_a3_pWcontrol',
  'divBel_b7_pWcontrol',
  'divBel_a6_pWcontrol',
  'divBel_c8_pWcontrol',
  'trueBel_a7_pWcontrol',
  'trueBel_b1_pWcontrol',
  'trueBel_c12_pWcontrol',
  'falseBelContents_a8_pWcontrol',
  'falseBelContents_b3_pWcontrol',
  'falseBelContents_c4_pWcontrol',
  'falseBelLocation_a10_pWcontrol',
  'falseBelLocation_b11_pWcontrol',
  'falseBelLocation_c6_pWcontrol',
  'falseSign_a5_pWcontrol',
  'falseSign_a13_pWcontrol',
  'falseSign_b4_pWcontrol',
  'falseSign_b8_pWcontrol',
  'falseSign_c2_pWcontrol',
  'falseSign_c9_pWcontrol',
  'vpt1_a11_pWcontrol',
  'vpt1_a2_pWcontrol',
  'vpt1_b2_pWcontrol',
  'vpt1_b5_pWcontrol',
  'vpt2_c7_pWcontrol',
  'vpt2_c11_pWcontrol',
  'knowAcc_a12_pWcontrol',
  'knowAcc_b6_pWcontrol',
  'knowAcc_c3_pWcontrol',
  'knowExpert_a4_pWcontrol',
  'knowExpert_b9_pWcontrol',
  'knowExpert_c10_pWcontrol'
)

datIRT <- dat[selectVars]

## Exploratory factory analysis
psych::scree(datIRT)
# EFA suggests a single unitary factor structure using principal components (PC)
# and factor analysis (FA)

## Confirmatory factor analysis
singleFactor <- 'tom =~ 
    divDes_a1_pWcontrol + divDes_b12_pWcontrol + divDes_c5_pWcontrol + 
    divDes_a9_pWcontrol + divDes_c1_pWcontrol + divDes_b10_pWcontrol + 
    knowAcc_a12_pWcontrol + knowAcc_b6_pWcontrol + knowAcc_c3_pWcontrol + 
    knowExpert_a4_pWcontrol + knowExpert_b9_pWcontrol + knowExpert_c10_pWcontrol + 
    vpt1_a11_pWcontrol + vpt1_a2_pWcontrol + vpt1_b2_pWcontrol + 
    vpt1_b5_pWcontrol + vpt2_c7_pWcontrol + vpt2_c11_pWcontrol + 
    divBel_a3_pWcontrol + divBel_b7_pWcontrol + divBel_a6_pWcontrol + 
    divBel_c8_pWcontrol + falseBelContents_a8_pWcontrol + 
    falseBelContents_b3_pWcontrol + falseBelContents_c4_pWcontrol +
    falseBelLocation_a10_pWcontrol + falseBelLocation_b11_pWcontrol + 
    falseBelLocation_c6_pWcontrol + falseSign_a5_pWcontrol + 
    falseSign_a13_pWcontrol + falseSign_b4_pWcontrol + 
    falseSign_b8_pWcontrol + falseSign_c2_pWcontrol + 
    falseSign_c9_pWcontrol'

singleFactorFit <- cfa(model = singleFactor, data = dat, estimator = "WLSMV", missing = "pairwise", std.lv = TRUE, ordered = TRUE)
summary(singleFactorFit, std = TRUE, fit = TRUE)

# Fit indices
fitmeasures(singleFactorFit)
# Fit indices are excellent, meeting assumptions of unidimensionality


#### 2. 2-PARAMETER LOGISTIC MODEL ####
## 2 PL model coefficients
pIRT = ltm(datIRT ~ z1, IRT.param = TRUE)
coef(pIRT) 

## Item characteristic curves
ICCplot <- as.data.frame(plot(pIRT,type=c("ICC"))) 

# Pivot data to long
df <- ICCplot %>% 
  pivot_longer(2:38) %>%
  mutate(response = stri_sub(name, -1)) %>%
  mutate(task = sub("_pWcontrol.*", "", name)) %>%
  mutate(task = sub(".*pr.", "", task))

# Create facet-wrapped plot of item characteristic curves (for all items)
ICCgg <- ggplot(df, aes(x=z, y=value, color=response)) +
  geom_line() + ylim(0, 1) +
  ggtitle('Item Characteristic Curves for CAT') +
  scale_color_manual(values=c("#d55e00", "#ffc04c")) + 
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
  geom_line() + ylim(0,7) + 
  ggtitle('Item information Curves for CAT') +
  scale_color_manual(values=c("#f9993c", "#dd5c50", "#b95461", "#26465f", "#7985a5", '#b3c2e8')) + 
  theme_minimal()

# Save to pptx
create_pptx(IICgg)

## Test information function
TIFplot <- as.data.frame(plot(pIRT,type=c("IIC"),items=c(0)))

# Plot TIF
TIFgg <- ggplot(TIFplot, aes(x=z, y=info)) +
  geom_line() + ylim(0, 30) +
  ggtitle('Test Information Function for CAT') +
  theme_minimal()

# Save to pptx
create_pptx(TIFgg)

## Extract theta from the model
# Fit 2PL model in MIRT package
mirt.2PL <- mirt(datIRT, 1,itemtype = '2PL')

# Save theta in with each participant
dat$theta <- fscores(mirt(datIRT, 1))

# Subset data for participants with theta within the range for highest information
# in the 2 PL model
datTheta <- dat[which(dat$theta <= -.5 & dat$theta >= -2.5), ]

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