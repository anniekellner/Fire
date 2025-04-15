################################################################################
###                   CREATE DATAFRAME WITH MU, PI, AND PROBS                ###
################################################################################


library(tidyverse)
library(kableExtra)
library(pscl) # for zero-inflated model
library(MASS) # for negative binomial distribution

## -----    LOAD AND PREP DATA   ----    ##

# Dataframes with Presence/Absence and Weather Station Data

kma <- readRDS(file = "./Data/CompleteDFs/KMA_20250327.Rds")
ptaE <- readRDS(file = "./Data/CompleteDFs/ptaE_20250327.Rds")
r17 <- readRDS(file = "./Data/CompleteDFs/r17_20250327.Rds")

# Make Presence/Absence columns factor class

kma$Presence <- as.factor(kma$Presence)
ptaE$Presence <- as.factor(ptaE$Presence)
r17$Presence <- as.factor(r17$Presence)


##  ----    FINAL MODELS    ----------  ##

# Get coefficients from final models

# PTA EAST

ptaE_ZINB <- zeroinfl(
  Count ~ KBDI | Temp,
  data = ptaE,
  dist = "negbin"
)

coef_ptaE <- coef(ptaE_ZINB)


# Range 17

r17_ZINB <- zeroinfl(
  Count ~ KBDI + BI + RnDr | KBDI + BI,
  data = r17,
  dist = "negbin"
)

coef_r17 <- coef(r17_ZINB)

# KMA

kma_ZINB <- zeroinfl(
  Count ~ F100 + FM1 | FM1,
  data = kma,
  dist = "negbin"
)

coef_kma <- coef(kma_ZINB)

# Calculate mu 

ptaE <- ptaE %>%
  mutate(mu = exp(coef(ptaE_ZINB)["count_(Intercept)"] + 
                                      coef(ptaE_ZINB)["count_KBDI"]*KBDI)) %>%
  mutate(pi = exp(-(coef(ptaE_ZINB)["zero_(Intercept)"] + 
                                               coef(ptaE_ZINB)["zero_Temp"] * Temp)))%>%
  mutate(prob0 = ifelse(
    Count == 0, pi + (1-pi)*((ptaE_r/(ptaE_r+mu))^ptaE_r), NA)) %>%
  mutate(probCount = ifelse(
    Count > 0, (1-pi)*(factorial(Count+ptaE_r-1)/(factorial(ptaE_r-1)*factorial(ptaE_r)))*
      ((ptaE_r/(ptaE_r+mu))^ptaE_r)*((mu/(ptaE_r+mu))^Count), NA))
  





