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


## PTA EAST

# Get coefficients 

ptaE_count_intercept <- ptaE_ZINB$coefficients$count[1]
ptaE_count_coef <- ptaE_ZINB$coefficients$count[2]

ptaE_zero_intercept <- ptaE_ZINB$coefficients$zero[1]
ptaE_zero_coef <- ptaE_ZINB$coefficients$zero[2]

ptaE_r <- ptaE_ZINB$theta


ptaE_withProbs <- ptaE %>%
  mutate(mu = exp(coef(ptaE_ZINB)["count_(Intercept)"] + 
                    coef(ptaE_ZINB)["count_KBDI"] * KBDI)) %>%
  mutate(P = 1/(1 + exp(-(coef(ptaE_ZINB)["zero_(Intercept)"] + 
                            coef(ptaE_ZINB)["zero_Temp"] *Temp)))) %>%
  
  mutate(prob_allZero = ifelse(
    Count == 0, 
    P + (1-P)*((ptaE_r/(ptaE_r+mu))^ptaE_r), 
    NA
  )) %>%
  
  mutate(prob_fire = 1 - prob_allZero) %>%
  
  mutate(probCount = ifelse(
    Count > 0, (1-P)*(factorial(Count+ptaE_r-1)/(factorial(ptaE_r-1)*factorial(Count)))*
      ((ptaE_r/(ptaE_r+mu))^ptaE_r)*((mu/(ptaE_r+mu))^Count), NA))

write_csv(ptaE_withProbs, file = "C:/Users/akellner/OneDrive - Colostate/Documents/Fire/Data/ptaE_with_mu_pi_probs.csv")
  

head(ptaE_withProbs)

ptaE_count_intercept + ptaE_count_coef*ptaE$KBDI[1]


