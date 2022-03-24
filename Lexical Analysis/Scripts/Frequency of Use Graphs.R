# Load packages
library(tidyverse)
library(base)
library(lme4)
library(knitr)
library(jtools)
library(sjPlot)
library(sjlabelled)
library(sjmisc)

# Load data
Lexical_Item_Report <- read.csv("./CSV Files/Lexical Item Analysis/Lexical Item Report.csv")


# Prepare averages and sums
## Participant item averages
Average_by_Participant <- aggregate(Lexical_Item_Report$Part_Rating, list(Lexical_Item_Report$Participant_ID), FUN = mean)
Average_by_Participant <- Average_by_Participant %>% rename(Participant_ID = Group.1)
Average_by_Participant <- Average_by_Participant %>% rename(Part_Avg = x)
Average_by_Participant <- left_join(Lexical_Item_Report, Average_by_Participant, by = c("Participant_ID" = "Participant_ID"))

Average_by_Participant <- Average_by_Participant %>%
  mutate(Part_Avg_Std = (Part_Avg - mean(Part_Avg))/sd(Part_Avg))


# Graphs by variable
## Lexical item average and frequency of use
Average_by_Participant %>% 
  ggplot(., aes(x = FofU, y = Part_Avg)) + 
  geom_point() + 
  geom_smooth(method = lm) +
  scale_x_continuous(breaks = seq(10, 45, 5),
                     limits = c(10, 45)) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Average of participant ratings across verbs", title = "Frequency of Use and Lexical Ratings") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Frequency of use and DELE
Lexical_Item_Report %>%
  ggplot(., aes(x = FofU, y = DELE)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_x_continuous(breaks = seq(0, 45, 5),
                     limits = c(10, 45)) +
  scale_y_continuous(breaks = seq(25, 50, 5),
                     limits = c(25, 50)) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Proficiency score", title = "Frequency of Use and Proficiency") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))


## Frequency of use and age of acquisition
Lexical_Item_Report %>%
  ggplot(., aes(x = FofU, y = AoA_ENG)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_x_continuous(breaks = seq(0, 45, 5),
                     limits = c(10, 45)) +
  scale_y_continuous(breaks = seq(0, 8, 1),
                     limits = c(0, 8)) +
  labs(x = "Self-reported frequency of use of Spanish", y = "Age at beginning of acquisition of English", title = "Frequency of Use and Age of Acquisition") +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
