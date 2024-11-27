library(RODBC)
data <- odbcConnectAccess2007("C:/Users/dan91/OneDrive/桌面/Indo-Pacific coral spawning/Coral_Spawning_Database.accdb")
species <- sqlFetch(data, "tblCoralSpecies")
