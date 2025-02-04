#############################################################################
######          IMPORT AND CONSOLIDATE DATA                     #############
#############################################################################

# 2-3-2025
# written for CEMML Fire Program by Annie Kellner (annie.kellner@colostate.edu)


library(tidyverse)
library(conflicted)


## Settings




##  ---   IMPORT DATA   --------- ##

kma <- read.csv('./Data/KMA_Wx_Data_2015_2024.csv')
ptaE <- read.csv('./Data/PTA_East_Wx_Data_2015_2024.csv')
kipuka <- read.csv('Data/PTA_Kipuka_Alala_Wx_Data_2015_2025.csv')
range17 <- read.csv('./Data/PTA_Range_17_WxData_2014_2024.csv')

##  ----  MANIPULATE DATA ------  ##

# Add location to dataframes before combining

kma$Location <- "KMA"
ptaE$Location <- "PTA_East"
kipuka$Location <- "Kipuka"
range17$Location <- "Range17"

# Join Dataframes

join1 <- full_join(kma, ptaE)
join2 <- full_join(join1, kipuka)
all <- full_join(join2, range17)

# Remove variables

all <- select(all, !c(SFlag, WAZI, WFlag, WDIR))







