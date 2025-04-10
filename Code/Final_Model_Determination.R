################################################################################
###             RANGE 17: VARIABLES FOR ZINB MODEL                           ###
################################################################################

# Important Vars from Logistic Regression:
  # 1. KBDI
  # 2. BI
  # 3. RnDr


# KBDI must be in the PA component


library(tidyverse)
library(MuMIn)

rm(list = ls())


# Look at Presence Data

presence <- r17 %>%
  filter(Presence == 1)


# All vars in both model components


globalModel <- zeroinfl(
  Count ~ KBDI + BI + RnDr | KBDI + BI + RnDr,
  data = r17,
  dist = "negbin"
)

options(na.action = "na.omit")

summary(globalModel)

## Dredging isn't working bc model is not converging


# Dredge the Count part (without RnDr by itself in any model)

#count_dredge <- dredge(
#  update(globalModel, . ~ . | 1),  # Keep zero part constant
#  subset = !(RnDr && !(KBDI || BI)),  # Exclude RnDr-only models
#  fixed = ~1,                     # Keep intercept
#  rank = "AICc"
#)

#zero_dredge <- dredge(
#  update(globalModel, . ~ 1 | .),  # Keep count part constant
#  subset = !(RnDr && !(KBDI || BI)),  # Exclude RnDr-only models
#  fixed = ~1,                     # Keep intercept
#  rank = "AICc"
#)

# PA = KBDI + BI; Count = AllVars

CKBDI_BI_PAall <- zeroinfl(
  Count ~ KBDI + BI + RnDr | KBDI + BI,
  data = r17,
  dist = "negbin"
)

summary(CKBDI_BI_PAall)

# PA = KBDI; Count = BI + RnDr

CBI_RnDr_PA_KBDI <- zeroinfl(
  Count ~ BI + RnDr | KBDI,
  data = r17,
  dist = "negbin"
)

summary(CBI_RnDr_PA_KBDI)

# !! KBDI on its own does not seem like a good predictor of PA


# PA = KBDI + BI; Count = BI + RnDr

CBI_RnDr_PA_KBDI_BI <- zeroinfl(
  Count ~ BI + RnDr | KBDI + BI,
  data = r17, 
  dist = "negbin"
)

summary(CBI_RnDr_PA_KBDI_BI)

# PA = allVars; Count = BI + RnDr

CBI_RnDr_PA_allVars <- zeroinfl(
  Count ~ BI + RnDr | KBDI + BI + RnDr,
  data = r17,
  dist = "negbin"
)

summary(CBI_RnDr_PA_allVars)

# Count = BI only; PA = allVars

CBI_PA_allVars <- zeroinfl(
  Count ~ BI | KBDI + BI + RnDr,
  data = r17,
  dist = "negbin"
)

summary(CBI_PA_allVars)

# Count = BI; PA = KBDI + BI

NoRnDr <- zeroinfl(
  Count ~ BI | KBDI + BI,
  data = r17,
  dist = "negbin"
)

summary(NoRnDr)

## Compare Models using AICc

# Create a list of all models
model_list <- list(
  "CKBDI_BI_PAall" = CKBDI_BI_PAall,
  "CBI_RnDr_PA_KBDI" = CBI_RnDr_PA_KBDI,
  "CBI_RnDr_PA_KBDI_BI" = CBI_RnDr_PA_KBDI_BI,
  "CBI_RnDr_PA_allVars" = CBI_RnDr_PA_allVars,
  "CBI_PA_allVars" = CBI_PA_allVars,
  "NoRnDr" = NoRnDr,
  "globalModel" = globalModel
  
)

# Calculate AICc for each model
aicc_values <- sapply(model_list, function(x) {
  AICc(x)
})

# Create comparison table
model_comparison <- data.frame(
  Model = names(model_list),
  AICc = aicc_values,
  Delta_AICc = aicc_values - min(aicc_values)
)

# Sort by AICc
model_comparison <- model_comparison[order(model_comparison$AICc),]

# Print results
print(model_comparison)

# Optional: Add Akaike weights
model_comparison$Weight <- exp(-0.5 * model_comparison$Delta_AICc) / 
  sum(exp(-0.5 * model_comparison$Delta_AICc))

# Print final comparison
print(model_comparison)
