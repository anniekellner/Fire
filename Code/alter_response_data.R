###########################################################################
###               PRELIMINARY MODELING                                  ###
###########################################################################

# 2-28-2025

# Recursive Feature Elimination (backwards selection)



library(caret)
library(tidyverse)



##  ---   LOAD DATA   -----   ##

kma <- read.csv('./Data/KMA_Wx_Data_2015_2024.csv') # variables

kmaFires <- read.csv("C:/Users/akellner/OneDrive - Colostate/Documents/Fire/KMA_Wx_Fire_Data/KMA_FDRS_Wildfires.csv")

## Manipulate Fire DF

kmaFires$Date <- as.Date(kmaFires$Date, format = "%m/%d/%Y")

kmaFires$Presence <- 1

# Create df with all dates

allDates <- seq(
  from = first(kmaFires$Date),
  to = last(kmaFires$Date),
  by = "day"
)

allDates <- data.frame(Date = allDates) 

allDates <- allDates %>%
  rename(Date = allDates)

kmaComplete <- kmaFires %>%
  full_join(allDates) %>%
  arrange(Date) %>% 
  replace_na(list(Presence = 0)) 


# Use 'dredge' to create global model from high-priority vars

