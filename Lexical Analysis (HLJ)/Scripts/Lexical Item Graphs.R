# Run packages
library(tidyverse)
library(effects)
library(dplyr)
library(ggeffects)
library(sjPlot)

options(scipen = 999)

# Preparation of data
## Create standard scores
Lexical_Item_Report = read.csv("./CSV Files/Lexical Item Analysis/Lexical Item Report.csv")


## Create lexical item averages
Average_by_LI <- aggregate(Lexical_Item_Report$Part_Rating, list(Lexical_Item_Report$Verb), FUN = mean)
Average_by_LI <- Average_by_LI %>% rename(Verb = Group.1)
Average_by_LI <- Average_by_LI %>% rename(SRLF_Avg = x)
Average_by_LI <- left_join(Lexical_Item_Report, Average_by_LI, by = c("Verb" = "Verb"))


## Create participant averages
Average_by_Participant <- aggregate(Lexical_Item_Report$Part_Rating, list(Lexical_Item_Report$Participant_ID), FUN = mean)
Average_by_Participant <- Average_by_Participant %>% rename(Participant_ID = Group.1)
Average_by_Participant <- Average_by_Participant %>% rename(Part_Avg = x)
Average_by_Participant <- left_join(Lexical_Item_Report, Average_by_Participant, by = c("Participant_ID" = "Participant_ID"))


# Graphs
## Davies-SRLF graphs
Average_by_LI %>% 
  ggplot(., aes(x = log(Davies_Rating), y = SRLF_Avg)) + 
  geom_point() + 
  geom_smooth(method = lm) +
  scale_y_continuous(breaks = seq (0, 9, 2),
                     limits = c(0, 9)) +
  labs(x = "Log-transformed lemma frequency in Corpus del español", y = "Average of verb ratings across participants", title = "Correlation between Corpus del español and self-ratings") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## WordList-SRLF graphs
Average_by_LI %>% 
  ggplot(., aes(x = log(WordList_Rating), y = SRLF_Avg)) + 
  geom_point() + 
  geom_smooth(method = lm) +
  scale_y_continuous(breaks = seq (0, 9, 2),
                     limits = c(0, 9)) +
  labs(x = "Log-transformed lemma frequency in esTenTen18", y = "Average of verb ratings across participants", title = "Correlation between esTenTen18 and self-ratings") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
