################################################################################
#########       CORRELATIONS                                    ################
###############################################################################

fire <- readRDS("./Data/fire.Rds")


## Let's try a better way

f2 <- select(fire, Temp, MaxT, RH, MnRH, FM1, FMH, FMW, LHerb, LWood, SolR, SOW, DPT)

cor(f2, method = "spearman", use = "pairwise.complete.obs")
