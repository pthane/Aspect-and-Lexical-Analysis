# Load packages
library(tidyverse)
library(lme4)
library(ggpubr)
library(corrplot)


# Prepare data
## Load items
Lexical_Item_Report <- read.csv("./CSV Files/Lexical Item Analysis/Lexical Item Report.csv")


## Generate participant averages
Average_by_Participant <- aggregate(Lexical_Item_Report$Part_Rating, list(Lexical_Item_Report$Participant_ID), FUN = mean)
Average_by_Participant <- Average_by_Participant %>% rename(Participant_ID = Group.1)
Average_by_Participant <- Average_by_Participant %>% rename(Part_Avg = x)
Average_by_Participant <- left_join(Lexical_Item_Report, Average_by_Participant, by = c("Participant_ID" = "Participant_ID")) %>% 
  mutate(Part_Avg_Std = (Part_Avg - mean(Part_Avg))/sd(Part_Avg))

Average_Single_Verb <- Average_by_Participant %>% 
  filter(Verb == "estar")


## Gather Verb Averages
Average_by_Verb <- aggregate(Lexical_Item_Report$Part_Rating, list(Lexical_Item_Report$Verb), FUN = mean)
Average_by_Verb <- Average_by_Verb %>% rename(Verb = Group.1)
Average_by_Verb <- Average_by_Verb %>% rename(Verb_Avg = x)
Average_by_Verb <- left_join(Lexical_Item_Report, Average_by_Verb, by = c("Verb" = "Verb")) %>% 
  mutate(Verb_Avg_Std = (Verb_Avg - mean(Verb_Avg))/sd(Verb_Avg))

Average_Single_Participant <- Average_by_Verb %>% 
  filter(Participant_ID == "HSP01")


## Create correlation data
Frequency_Data <- Average_Single_Verb %>%
  select(FofU_Std, Part_Avg_Std, DELE_Std, AoA_ENG_Std)

Lexical_Data <- Average_Single_Participant %>%
  select(Verb_Avg_Std, Davies_Std, WordList_Std, CORPES_Std) %>% 
  na.omit


# Correlations for frequency of use
## Generate correlation plots (corrplot)
cor.test(Frequency_Data$FofU_Std, Frequency_Data$Part_Avg_Std)
cor.test(Frequency_Data$FofU_Std, Frequency_Data$DELE_Std)
cor.test(Frequency_Data$FofU_Std, Frequency_Data$AoA_ENG_Std)


## Correlation plot
cor.test(Lexical_Data$Verb_Avg_Std, Lexical_Data$Davies_Std)
cor.test(Lexical_Data$Verb_Avg_Std, Lexical_Data$WordList_Std)
cor.test(Lexical_Data$Verb_Avg_Std, Lexical_Data$CORPES_Std)
