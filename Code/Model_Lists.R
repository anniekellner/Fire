################################################################################
###           MODEL LISTS                                                     ###
################################################################################


### MODEL LIST 1  ###

model_list1 <- list(
  "globalModel" = globalModel, 
  
  # Remove RnDr 
  
  "KBDIBI_all" = KBDIBI_all, # remove RnDr from Intensity only
  "NoRnDr" = NoRnDr, # Remove RnDr from both sides
  "BI_KBDIBI" = BI_KBDIBI, # BI alone explains Intensity
  "KBDIBI_KBDI" = KBDIBI_KBDI, # KBDI alone explains Occurrence
  
  # Put RnDr back in model
  
  "all_KBDI" = KBDIBIRnDr_KBDI, # KBDI alone explains Occurence
  
  # Remove KBDI from Intensity
  
  "BIRnDr_all" = CBI_RnDr_PA_allVars, # All three explain occurrence
  "BI_all" = CBI_PA_allVars, # BI alone explains Intensity; all three explain occurrence
  "BIRnDr_KBDIBI" = CBI_RnDr_PA_KBDI_BI, # RnDr does not explain Occurrence
  "BIRnDr_KBDI" = CBI_RnDr_PA_KBDI # KBDI alone explains occurrence but does not explain Intensity
)


# Global model (all variables in both components)

globalModel <- zeroinfl(
  Count ~ KBDI + BI + RnDr | KBDI + BI + RnDr,
  data = r17,
  dist = "negbin"
)


## Remove RnDr


# Remove RnDr from "Intensity" only

KBDIBI_all <- zeroinfl(
  Count ~ KBDI + BI | KBDI + BI + RnDr,
  data = r17,
  dist = "negbin"
)


# Remove RnDr from both sides; KBDI & BI explain both components

NoRnDr <- zeroinfl(
  Count ~ KBDI + BI | KBDI + BI,
  data = r17,
  dist = "negbin"
)


## REMOVE KBDI FROM INTENSITY ##

# BI alone explains intensity

BI_KBDIBI <- zeroinfl(
  Count ~ BI | KBDI + BI,
  data = r17,
  dist = "negbin"
)


# KBDI alone explains Occurrence

KBDIBI_KBDI <- zeroinfl(
  Count ~ KBDI + BI | KBDI,
  data = r17,
  dist = "negbin"
)


## Put RnDr back in ##


# KBDI alone explains Occurrence

all_KBDI <- zeroinfl(
  Count ~ KBDI + BI + RnDr | KBDI,
  data = r17,
  dist = "negbin"
)


## Remove KBDI from Intensity ##


# All three explain occurrence

BIRnDr_all <- zeroinfl( 
  Count ~ BI + RnDr | KBDI + BI + RnDr,
  data = r17,
  dist = "negbin"
)


# BI alone explains Intensity; all three explain occurrence (BI | KBDI + BI + RnDr)

BI_all <- zeroinfl(
  Count ~ BI | KBDI + BI + RnDr,
  data = r17,
  dist = "negbin"
)


# RnDr does NOT explain Occurrence

BIRnDr_KBDIBI <- zeroinfl(
  Count ~ BI + RnDr | KBDI + BI, 
  data = r17,
  dist = "negbin"
)

# KBDI alone explains occurrence but does NOT explain Intensity

BIRnDr_KBDI <- zeroinfl(
  Count ~ BI + RnDr | KBDI,
  data = r17,
  dist = "negbin"
)

