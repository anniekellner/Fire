##############################################################################
###             MODELS          ##############################################
##############################################################################

library(tidyverse)
library(MuMIn)
library(kableExtra)
library(caret)
library(pROC)


##    ------    LOAD DATA     ------------------------  ##

kma <- readRDS(file = "./Data/CompleteDFs/KMA_20250327.Rds")
ptaE <- readRDS(file = "./Data/CompleteDFs/ptaE_20250327.Rds")
r17 <- readRDS(file = "./Data/CompleteDFs/r17_20250327.Rds")


##  ---------   CREATE GLOBAL MODELS   ----------------  ##


## KMA

# Dredging

globalModel1 <- glm(Presence ~ F100 + RnDr + FM1, data = kma, family = binomial)
globalModel2 <- glm(Presence ~ F100 + RnDr + RH + FM10, data = kma, family = binomial)

options(na.action = "na.fail")

model_dredge <- dredge(globalModel2,
                       evaluate = TRUE,
                       rank = "AICc")

# View top models

top_models_kma2 <- model.sel(model_dredge, rank = "AICc")
subset(top_models_kma2, delta < 2)
avg_model <- model.avg(model_dredge, subset = delta < 2)
summary(avg_model)
imp <- sw(model_dredge)

var_importance_df <- data.frame(
  Variable = names(imp),
  Sum_of_Weights = imp
) %>%
  arrange(desc(Sum_of_Weights)) 


## PTA EAST

globalModel1_ptaE <- glm(Presence ~ Temp + RnDr + KBDI + FM1 + RH + F100,
                         data = ptaE,
                         family = "binomial")

globalModel2_ptaE <- glm(Presence ~ Temp + RnDr + KBDI + FM1 + BI + F100,
                         data = ptaE,
                         family = "binomial")

globalModel3_ptaE <- glm(Presence ~ Temp + RnDr + KBDI + FM10 + RH,
                         data = ptaE,
                         family = "binomial")

globalModel4_ptaE <- glm(Presence ~ Temp + RnDr + KBDI + FM10 + BI,
                         data = ptaE,
                         family = "binomial")


# Dredge

options(na.action = "na.fail")

model_dredge <- dredge(globalModel4_ptaE,
                       evaluate = TRUE,
                       rank = "AICc")

# View top models

top_models_pta4 <- model.sel(model_dredge, rank = "AICc")
subset(top_models_pta4, delta < 2)
avg_model <- model.avg(model_dredge, subset = delta < 2)
summary(avg_model)
imp <- sw(model_dredge)

var_importance_df <- data.frame(
  Variable = names(imp),
  Sum_of_Weights = imp
) %>%
  arrange(desc(Sum_of_Weights)) 


## Range 17

globalModel1_r17 <- glm(Presence ~ KBDI + BI + FMH + RnDr + Temp + FM10,
                        data = r17,
                        family = "binomial")

globalModel2_r17 <- glm(Presence ~ KBDI + BI + FMH + RnDr + Temp + FM1 + F100,
                        data = r17,
                        family = "binomial")


# Dredge

options(na.action = "na.fail")

model_dredge <- dredge(globalModel2_r17,
                       evaluate = TRUE,
                       rank = "AICc")

# View top models

top_models_r17_2 <- model.sel(model_dredge, rank = "AICc")
subset(top_models_r17_2, delta < 2)
avg_model <- model.avg(model_dredge, subset = delta < 2)
summary(avg_model)
imp <- sw(model_dredge)

var_importance_df <- data.frame(
  Variable = names(imp),
  Sum_of_Weights = imp
) %>%
  arrange(desc(Sum_of_Weights)) 


##  -------   K-FOLD CROSS-VALIDATION --------  #

# I am using a K-fold CV for the top model in each group


# ptaE

compare_k_folds <- function(input_data, k_values = c(5, 10, 15, 20)) {
  # Convert Presence to factor with proper levels
  ptaE$Presence <- factor(ptaE$Presence,
                                levels = c(0, 1),
                                labels = c("Absent", "Present"))
  
  results <- list()
  
  for(k in k_values) {
    set.seed(123)
    
    ctrl <- trainControl(
      method = "cv",
      number = k,
      savePredictions = TRUE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
    
    model <- train(
      Presence ~ KBDI + Temp,
      data = ptaE,
      method = "glm",
      trControl = ctrl,
      metric = "ROC",
      preProcess = c("center", "scale")
    )
    
    results[[paste0("k=", k)]] <- model$results
  }
  
  comparison <- data.frame(
    k = k_values,
    ROC = sapply(results, function(x) x$ROC),
    Sens = sapply(results, function(x) x$Sens),
    Spec = sapply(results, function(x) x$Spec)
  )
  
  return(comparison)
}


# Run comparison
k_comparison <- compare_k_folds(ptaE)
print(k_comparison)

# Look at Means and SD's from one iteration

ptaE$Presence <- factor(ptaE$Presence,
                        levels = c(0, 1),
                        labels = c("Absent", "Present"))

# Set up cross-validation control for k=5
ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = TRUE,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Create the cv_model using your variables from ptaE
cv_model <- train(
  Presence ~ Temp + RnDr + KBDI + FM1 + RH + F100,  # using variables from your ptaE model
  data = ptaE,
  method = "glm",
  trControl = ctrl,
  metric = "ROC",
  preProcess = c("center", "scale")
)

# Now we can examine the results
results_df <- cv_model$results

# Create summary
cv_summary <- data.frame(
  Metric = c("ROC", "Sensitivity", "Specificity"),
  Mean = c(results_df$ROC, results_df$Sens, results_df$Spec),
  SD = c(results_df$ROCSD, results_df$SensSD, results_df$SpecSD)
)

# View and export results
print("Cross-validation results:")
print(cv_summary)


###  Range 17


compare_k_folds <- function(input_data, k_values = c(5, 10, 15, 20)) {
  # Convert Presence to factor with proper levels
  r17$Presence <- factor(r17$Presence,
                          levels = c(0, 1),
                          labels = c("Absent", "Present"))
  
  results <- list()
  
  for(k in k_values) {
    set.seed(123)
    
    ctrl <- trainControl(
      method = "cv",
      number = k,
      savePredictions = TRUE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
    
    model <- train(
      Presence ~ KBDI + BI + RnDr,
      data = r17,
      method = "glm",
      trControl = ctrl,
      metric = "ROC",
      preProcess = c("center", "scale")
    )
    
    results[[paste0("k=", k)]] <- model$results
  }
  
  comparison <- data.frame(
    k = k_values,
    ROC = sapply(results, function(x) x$ROC),
    Sens = sapply(results, function(x) x$Sens),
    Spec = sapply(results, function(x) x$Spec)
  )
  
  return(comparison)
}


# Run comparison
k_comparison <- compare_k_folds(r17)
print(k_comparison)


## KMA


compare_k_folds <- function(input_data, k_values = c(5, 10, 15, 20)) {
  # Convert Presence to factor with proper levels
  kma$Presence <- factor(kma$Presence,
                         levels = c(0, 1),
                         labels = c("Absent", "Present"))
  
  results <- list()
  
  for(k in k_values) {
    set.seed(123)
    
    ctrl <- trainControl(
      method = "cv",
      number = k,
      savePredictions = TRUE,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
    
    model <- train(
      Presence ~ F100 + FM1,
      data = kma,
      method = "glm",
      trControl = ctrl,
      metric = "ROC",
      preProcess = c("center", "scale")
    )
    
    results[[paste0("k=", k)]] <- model$results
  }
  
  comparison <- data.frame(
    k = k_values,
    ROC = sapply(results, function(x) x$ROC),
    Sens = sapply(results, function(x) x$Sens),
    Spec = sapply(results, function(x) x$Spec)
  )
  
  return(comparison)
}


# Run comparison
k_comparison <- compare_k_folds(kma)
print(k_comparison)

