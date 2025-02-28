###########################################################################
###               DATA PREP                                             ###
###########################################################################

# 2-28-2025

# Recursive Feature Elimination (backwards selection)



library(caret)
library(tidyverse)



##  ---   LOAD AND PREP DATA   -----   ##

kma <- read.csv('./Data/KMA_Wx_Data_2015_2024.csv') # variables

## Presence-Absence Fire Data

kmaFires <- read.csv("C:/Users/akellner/OneDrive - Colostate/Documents/Fire/KMA_Wx_Fire_Data/KMA_FDRS_Wildfires.csv")

kmaFires$Date <- as.Date(kmaFires$Date, format = "%m/%d/%Y")

# Find duplicates (multiple fires on a single day)

dups <- kmaFires %>%
  group_by(Date) %>%
  filter(n() > 1) %>%
  arrange(Date) 

dups <- dups %>%
  add_count(Date) %>%
  arrange(Date) %>%
  rename(count = n)

# remove duplicate rows

dups <- dups %>% # reduce df to only date and count
  select(Date, count) %>%
  distinct()


# Add all dates to response df

# Create df with all dates represented

allDates <- seq(
  from = first(kmaFires$Date),
  to = last(kmaFires$Date),
  by = "day"
)

allDates <- data.frame(Date = allDates) 

allDates <- allDates %>%
  rename(Date = allDates)

# Combine with presence data

kmaFires$Presence <- 1

kmaFires <- kmaFires %>%
  select(Date, Presence)

kmaComplete <- kmaFires %>%
  full_join(allDates) %>%
  arrange(Date) %>% 
  replace_na(list(Presence = 0)) 

kmaComplete <- kmaComplete %>%
  select(Date, Presence)

kmaAll <- kmaComplete %>%
  left_join(dups)

kmaAll <- kmaAll %>%
  mutate(count = coalesce(count, Presence)) 

saveRDS(kmaAll, file = "./Data/KMA/ResponseDF_02282025.Rds")





