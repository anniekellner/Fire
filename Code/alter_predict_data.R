################################################################################
###               COMBINE RESPONSE AND PREDICTOR VARIABLES                  ###
################################################################################

# 2-28-2025


# Manipulate Predictor DF

kmaEnv <- read.csv("./Data/KMA_Wx_Data_2015_2024.csv")

kmaEnv$DATE <- as.Date(kmaEnv$DATE, format = "%m/%d/%Y")

kmaEnv %>%
  distinct(DATE) %>%
  nrow()

kmaPred <- kmaEnv %>%
  filter(Time == 1300)

which(is.na(kmaHi))

kmaHi %>%
  distinct(Date) %>%
  nrow()


