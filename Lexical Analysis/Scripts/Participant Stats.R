library(tidyverse)

# Load data
Lexical_Item_Report <- read.csv("./CSV Files/Lexical Analysis Master.csv") %>% 
  na.omit %>% 
  mutate(Davies_Std = (Davies_Rating - mean(Davies_Rating))/sd(Davies_Rating),
         CORPES_Std = (CORPES_Rating - mean(CORPES_Rating))/sd(CORPES_Rating),
         WordList_Std = (WordList_Rating - mean(WordList_Rating))/sd(WordList_Rating),
         SRLF_Std = (Part_Rating - mean(Part_Rating))/sd(Part_Rating),
         FofU = (FofU_Prod + FofU_Comp),
         FofU_Std = (FofU - mean(FofU))/sd(FofU),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG))


# Participant statistics
Participant_Stats <- Lexical_Item_Report %>%
  filter(Verb == "amar")

FofU_Stats <- Participant_Stats %>% 
  summarize(Average = mean(FofU, na.rm = T), SD = sd(FofU, na.rm = T), Max = max(FofU, na.rm = T), Min = min(FofU, na.rm = T))

Prof_Stats <- Participant_Stats %>% 
  summarize(Average = mean(DELE, na.rm = T), SD = sd(DELE, na.rm = T), Max = max(DELE, na.rm = T), Min = min(DELE, na.rm = T))

AoA_ENG_Stats <- Participant_Stats %>% 
  summarize(Average = mean(AoA_ENG, na.rm = T), SD = sd(AoA_ENG, na.rm = T), Max = max(AoA_ENG, na.rm = T), Min = min(AoA_ENG, na.rm = T))