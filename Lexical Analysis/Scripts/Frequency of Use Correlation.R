# Libraries
library(tidyverse)
library(lme4)
library(lmerTest)

options(scipen = 99)

# Data preparation
## Load data
Lexical_Item_Report <- read.csv("./CSV Files/Lexical Item Analysis/Lexical Item Report.csv")


## Generate participant averages
Average_by_Participant <- aggregate(Lexical_Item_Report$Part_Rating, list(Lexical_Item_Report$Participant_ID), FUN = mean)
Average_by_Participant <- Average_by_Participant %>% rename(Participant_ID = Group.1)
Average_by_Participant <- Average_by_Participant %>% rename(Part_Avg = x)
Average_by_Participant <- left_join(Lexical_Item_Report, Average_by_Participant, by = c("Participant_ID" = "Participant_ID"))

Average_by_Participant <- Average_by_Participant %>%
  mutate(Part_Avg_Std = (Part_Avg - mean(Part_Avg))/sd(Part_Avg))


## Generate participant sums
Sum_by_Participant <- aggregate(Lexical_Item_Report$Part_Rating, list(Lexical_Item_Report$Participant_ID), FUN = sum)
Sum_by_Participant <- Sum_by_Participant %>% rename(Participant_ID = Group.1)
Sum_by_Participant <- Sum_by_Participant %>% rename(Part_Sum = x)
Sum_by_Participant <- left_join(Lexical_Item_Report, Sum_by_Participant, by = c("Participant_ID" = "Participant_ID"))

Sum_by_Participant <- Sum_by_Participant %>%
  mutate(Part_Sum_Std = (Part_Sum - mean(Part_Sum))/sd(Part_Sum))


# Frequency of use correlation
## Generate correlations
Frequency_Model <- lmer(
  FofU_Std ~ Part_Avg_Std + DELE_Std + AoA_ENG_Std +
    (1 | Verb),
  data = Average_by_Participant)

summary(Frequency_Model)

plot_model(Frequency_Model, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_x_discrete(labels = c("Age of L2 acquisition", "Proficiency", "Average participant rating", "Intercept")) +
  scale_y_continuous(breaks = seq (-0.5, 0.5, 0.25),
                     limits = c(-0.5, 0.5)) +
  labs(title = "Participant Ratings and Frequency of Use", y = "Parameter Estimates") +
  theme(axis.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"))


## Nested model comparisons
Frequency_Null <- lmer(
  FofU_Std ~ 1 +
    (1 | Verb),
  data = Average_by_Participant)

Frequency_Average <- lmer(
  FofU_Std ~ 1 + Part_Avg_Std +
    (1 | Verb),
  data = Average_by_Participant)

Frequency_Proficiency <- lmer(
  FofU_Std ~ 1 + Part_Avg_Std + DELE_Std +
    (1 | Verb),
  data = Average_by_Participant)

Frequency_Full <- lmer(
  FofU_Std ~ 1 + Part_Avg_Std + DELE_Std + AoA_ENG +
    (1 | Verb),
  data = Average_by_Participant)

anova(Frequency_Null, Frequency_Average, Frequency_Proficiency, Frequency_Full, test = "Chisq")
