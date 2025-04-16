###############################################################################
###         EXCEL EXAMPLE DATA                                              ###
################################################################################

# Creating example data for Andy to use in Excel 


ptaNoFire <- ptaE %>%
  filter(Presence == 0) %>%
  slice_head() %>%
  mutate(Location = "PTA_East")

ptaFire <- ptaE %>%
  filter(Presence == 1) %>%
  slice_head() %>%
  mutate(Location = "PTA East")

kmaNoFire <- kma %>%
  filter(Presence == 0) %>%
  slice_head() %>%
  mutate(Location = "KMA")

kmaFire <- kma %>%
  filter(Presence == 1) %>%
  slice_head() %>%
  mutate(Location = "KMA")

r17noFire <- r17 %>%
  filter(Presence == 0) %>%
  slice_head() %>%
  mutate(Location = "Range17")

r17fire <- r17 %>%
  filter(Presence == 1) %>%
  slice_head() %>%
  mutate(Location = "Range17")

exampleData <- bind_rows(ptaFire, ptaNoFire, kmaFire, kmaNoFire, r17fire, r17noFire)

exampleData <- exampleData %>%
  dplyr::select(Location, Date, Temp, RnDr, BI, KBDI, FM1, F100, Presence, Count)

write_csv(exampleData, file = "C:/Users/akellner/OneDrive - Colostate/Documents/Fire/Data/exampleData.csv")


