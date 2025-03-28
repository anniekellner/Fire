##############################################################################
###             MODELS          ##############################################
##############################################################################

library(tidyverse)
library(MuMIn)
library(kableExtra)


##    ------    LOAD DATA     ------------------------  ##

kma <- readRDS(file = "./Data/CompleteDFs/KMA_20250327.Rds")
ptaE <- readRDS(file = "./Data/CompleteDFs/ptaE_20250327.Rds")
r17 <- readRDS(file = "./Data/CompleteDFs/r17_20250327.Rds")


##  ---------   CREATE GLOBAL MODELs   ----------------  ##


## KMA

# Dredging

globalModel <- glm(Presence ~ Temp + RH + FM10 + FMH + RnDr)


globalModel <- glm(Presence ~ Temp + RH + MnRH + Rain + RnDr + BI + KBDI + FM1 + F100 + FMW + HrRa, 
                   data = kmaHi, 
                   family = binomial)

options(na.action = "na.fail")

QandD <- dredge(globalModel,
                evaluate = TRUE, 
                rank = "AICc")

head(QandD)
tail(QandD)