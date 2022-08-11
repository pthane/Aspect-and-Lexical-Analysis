library(tidyverse)


HS <- read.csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>% 
  filter(Item == "40P")

SDC <- read.csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv") %>% 
  filter(Item == "40P")

# HS statistics
Prof_Stats_HS <- HS %>% 
  summarize(Average = mean(DELE, na.rm = T), SD = sd(DELE, na.rm = T), Max = max(DELE, na.rm = T), Min = min(DELE, na.rm = T))

FofU_Stats_HS <- HS %>% 
  summarize(Average = mean(FofA, na.rm = T), SD = sd(FofA, na.rm = T), Max = max(FofA, na.rm = T), Min = min(FofA, na.rm = T))

FofU_Prod_Stats_HS <- HS %>% 
  summarize(Average = mean(FofA_Prod, na.rm = T), SD = sd(FofA_Prod, na.rm = T), Max = max(FofA_Prod, na.rm = T), Min = min(FofA_Prod, na.rm = T))

FofU_Comp_Stats_HS <- HS %>% 
  summarize(Average = mean(FofA_Comp, na.rm = T), SD = sd(FofA_Comp, na.rm = T), Max = max(FofA_Comp, na.rm = T), Min = min(FofA_Comp, na.rm = T))

AoA_ENG_Stats_HS <- HS %>% 
  summarize(Average = mean(AoA_ENG, na.rm = T), SD = sd(AoA_ENG, na.rm = T), Max = max(AoA_ENG, na.rm = T), Min = min(AoA_ENG, na.rm = T))

AoA_SPA_Stats_HS <- HS %>% 
  summarize(Average = mean(AoA_SPA, na.rm = T), SD = sd(AoA_SPA, na.rm = T), Max = max(AoA_SPA, na.rm = T), Min = min(AoA_SPA, na.rm = T))

Age_Stats_HS <- HS %>% 
  summarize(Average = mean(Age, na.rm = T), SD = sd(Age, na.rm = T), Max = max(Age, na.rm = T), Min = min(Age, na.rm = T))


# SDC stats
Prof_Stats_SDC <- SDC %>% 
  summarize(Average = mean(DELE, na.rm = T), SD = sd(DELE, na.rm = T), Max = max(DELE, na.rm = T), Min = min(DELE, na.rm = T))

FofU_Stats_SDC <- SDC %>% 
  summarize(Average = mean(FofA, na.rm = T), SD = sd(FofA, na.rm = T), Max = max(FofA, na.rm = T), Min = min(FofA, na.rm = T))

FofU_Prod_Stats_SDC <- SDC %>% 
  summarize(Average = mean(FofA_Prod, na.rm = T), SD = sd(FofA_Prod, na.rm = T), Max = max(FofA_Prod, na.rm = T), Min = min(FofA_Prod, na.rm = T))

FofU_Comp_Stats_SDC <- SDC %>% 
  summarize(Average = mean(FofA_Comp, na.rm = T), SD = sd(FofA_Comp, na.rm = T), Max = max(FofA_Comp, na.rm = T), Min = min(FofA_Comp, na.rm = T))

AoA_ENG_Stats_SDC <- SDC %>% 
  summarize(Average = mean(AoA_ENG, na.rm = T), SD = sd(AoA_ENG, na.rm = T), Max = max(AoA_ENG, na.rm = T), Min = min(AoA_ENG, na.rm = T))

AoA_SPA_Stats_SDC <- SDC %>% 
  summarize(Average = mean(AoA_SPA, na.rm = T), SD = sd(AoA_SPA, na.rm = T), Max = max(AoA_SPA, na.rm = T), Min = min(AoA_SPA, na.rm = T))

Age_Stats_SDC <- SDC %>% 
  summarize(Average = mean(Age, na.rm = T), SD = sd(Age, na.rm = T), Max = max(Age, na.rm = T), Min = min(Age, na.rm = T))


# Join stats
FofU <- rbind(FofU_Stats_HS, FofU_Stats_SDC)
FofU_Prod <- rbind(FofU_Prod_Stats_HS, FofU_Prod_Stats_SDC)
FofU_Comp <- rbind(FofU_Comp_Stats_HS, FofU_Comp_Stats_SDC)
Prof <- rbind(Prof_Stats_HS, Prof_Stats_SDC)
AoA_ENG <- rbind(AoA_ENG_Stats_HS, AoA_ENG_Stats_SDC)
AoA_SPA <- rbind(AoA_SPA_Stats_HS, AoA_SPA_Stats_SDC)
Age <- rbind(Age_Stats_HS, Age_Stats_SDC)

