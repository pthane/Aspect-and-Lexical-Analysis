# Packages
library(tidyverse)
library(lme4)
library(lmerTest)

options(sciphen = 999)

# Preparation of data
## Load database
HS_EPT = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>% 
  mutate(NewTask = "EPT")

HS_FCT = read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv") %>% 
  mutate(NewTask = "FCT")

SDC_EPT = read.csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv") %>% 
  mutate(NewTask = "EPT")

SDC_FCT = read.csv("./CSV Files/Comparison/Comparison FCT Preterit Data.csv") %>% 
  mutate(NewTask = "FCT")


## Create combined datasets
Preterit_EPT = rbind(HS_EPT, SDC_EPT)
Preterit_FCT = rbind(HS_FCT, SDC_FCT)
Preterit_Aggregate = rbind(Preterit_EPT, Preterit_FCT)


# Prepare participant averages for graphs
## HS production
HS_EPT_Modified = aggregate(HS_EPT$Response, list(HS_EPT$Participant_ID), FUN = mean, na.rm = TRUE)
HS_EPT_Modified = HS_EPT_Modified %>% rename(Part_Avg = x)
HS_EPT_Modified = left_join(HS_EPT, HS_EPT_Modified, by = c("Participant_ID" = "Group.1")) %>% 
  mutate(Part_Avg = (Part_Avg * 100))

## HS FCT
HS_FCT_Modified = aggregate(HS_FCT$Response, list(HS_FCT$Participant_ID), FUN = mean, na.rm = TRUE)
HS_FCT_Modified = HS_FCT_Modified %>% rename(Part_Avg = x)
HS_FCT_Modified = left_join(HS_FCT, HS_FCT_Modified, by = c("Participant_ID" = "Group.1")) %>% 
  mutate(Part_Avg = (Part_Avg * 100))


## Aggregate EPT
Preterit_EPT_Aggregate = aggregate(Preterit_EPT$Response, list(Preterit_EPT$Participant_ID), FUN = mean, na.rm = TRUE)
Preterit_EPT_Aggregate = Preterit_EPT_Aggregate %>% rename(Part_Avg = x)
Preterit_EPT_Aggregate = left_join(Preterit_EPT, Preterit_EPT_Aggregate, by = c("Participant_ID" = "Group.1")) %>% 
  mutate(Part_Avg = (Part_Avg * 100))


## Aggregate FCT
Preterit_FCT_Aggregate = aggregate(Preterit_FCT$Response, list(Preterit_FCT$Participant_ID), FUN = mean, na.rm = TRUE)
Preterit_FCT_Aggregate = Preterit_FCT_Aggregate %>% rename(Part_Avg = x)
Preterit_FCT_Aggregate = left_join(Preterit_FCT, Preterit_FCT_Aggregate, by = c("Participant_ID" = "Group.1")) %>% 
  mutate(Part_Avg = (Part_Avg * 100))


## Data from both tasks and groups
Aggregate_Data = aggregate(Preterit_Aggregate$Response, list(Preterit_Aggregate$Participant_ID), FUN = mean, na.rm = TRUE)
Aggregate_Data = Aggregate_Data %>% rename(Part_Avg = x)
Aggregate_Data = left_join(Preterit_Aggregate, Aggregate_Data, by = c("Participant_ID" = "Group.1")) %>% 
  mutate(Part_Avg_Pct = (Part_Avg * 100))


# Prepare verb averages
## HS only
HS_EPT_VerbAvg = aggregate(HS_EPT$Response, list(HS_EPT$MainVerb), FUN = mean, na.rm = TRUE)
HS_EPT_VerbAvg = HS_EPT_VerbAvg %>% rename(Verb_Avg = x)
HS_EPT_VerbAvg = left_join(HS_EPT, HS_EPT_VerbAvg, by = c("MainVerb" = "Group.1")) %>% 
  mutate(Verb_Avg = (Verb_Avg * 100))

HS_FCT_VerbAvg = aggregate(HS_FCT$Response, list(HS_FCT$MainVerb), FUN = mean, na.rm = TRUE)
HS_FCT_VerbAvg = HS_FCT_VerbAvg %>% rename(Verb_Avg = x)
HS_FCT_VerbAvg = left_join(HS_FCT, HS_FCT_VerbAvg, by = c("MainVerb" = "Group.1")) %>% 
  mutate(Verb_Avg = (Verb_Avg * 100))


## SDC only
SDC_EPT_VerbAvg = aggregate(SDC_EPT$Response, list(SDC_EPT$MainVerb), FUN = mean, na.rm = TRUE)
SDC_EPT_VerbAvg = SDC_EPT_VerbAvg %>% rename(Verb_Avg = x)
SDC_EPT_VerbAvg = left_join(SDC_EPT, SDC_EPT_VerbAvg, by = c("MainVerb" = "Group.1")) %>% 
  mutate(Verb_Avg = (Verb_Avg * 100))

SDC_FCT_VerbAvg = aggregate(SDC_FCT$Response, list(SDC_FCT$MainVerb), FUN = mean, na.rm = TRUE)
SDC_FCT_VerbAvg = SDC_FCT_VerbAvg %>% rename(Verb_Avg = x)
SDC_FCT_VerbAvg = left_join(SDC_FCT, SDC_FCT_VerbAvg, by = c("MainVerb" = "Group.1")) %>% 
  mutate(Verb_Avg = (Verb_Avg * 100))


## Aggregate
HS_Verb_Avg_Master = rbind(HS_EPT_VerbAvg, HS_FCT_VerbAvg)
Verb_Avg_Master = rbind(HS_EPT_VerbAvg, HS_FCT_VerbAvg, SDC_EPT_VerbAvg, SDC_FCT_VerbAvg)


# Plots
## Bar graph of preterit use
Aggregate_Data %>%
  ggplot(aes(x = ExpGroup, y = (Response)*100, fill = NewTask)) + 
  stat_summary(fun.data = mean_se,
               geom = "bar", size = 1,
               position = position_dodge(width = 0.90)) +
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 100, 10),
                     limits = c(0, 100)) +
  labs(x = "Participant group", y = "Percentage of preterit responses by group", fill = "Task", 
       title = "Percentages of Preterit Use by Group and Task") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Regularity and task
Aggregate_Data %>%
  ggplot(aes(x = ExpGroup, y = (Response)*100, fill = NewTask)) + 
  facet_grid(cols = vars(Reg_Main)) +
  stat_summary(fun.data = mean_se,
               geom = "bar", size = 1,
               position = position_dodge(width = 0.90)) +
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Participant group", y = "Percentage of preterit responses by group", fill = "Task", 
       title = "Preterit Use by Morphological Regularity, Group, and Task") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Lexical frequency and task
Verb_Avg_Master %>%
  ggplot(aes(x = log(Token_Main_Lemma), y = Verb_Avg, color = NewTask)) + 
  geom_point() +
  facet_grid(cols = vars(ExpGroup)) +
  geom_smooth(method = glm) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Log-transformed lexical frequency in Davies (2016)", y = "Percentage of preterit responses by verb", color = "Task", 
       title = "Use of Preterit by Lexical Frequency, Group, and Task") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Proficiency and task
Preterit_Proficiency <- rbind(HS_EPT_Modified, HS_FCT_Modified)

Preterit_Proficiency %>% 
  ggplot(aes(x = DELE, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = glm) +
  facet_grid(cols = vars(NewTask)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Proficiency score", y = "Percentage of preterit responses by participant", color = "Token Frequency", 
       title = "HS' Use of Preterit by Proficiency across Tasks") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Frequency of use and task
Preterit_FofA <- rbind(HS_EPT_Modified, HS_FCT_Modified)

Preterit_FofA %>% 
  ggplot(aes(x = FofA, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = glm) +
  facet_grid(cols = vars(NewTask)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Self-reported frequency of use", y = "Percentage of preterit responses by participant", color = "Token Frequency", 
       title = "HS' Use of Preterit and Frequency of Use across Tasks") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Age of L2 acquisition and task
Preterit_AoA <- rbind(HS_EPT_Modified, HS_FCT_Modified)

Preterit_AoA %>% 
  ggplot(aes(x = AoA_ENG, y = Part_Avg)) + 
  geom_point() +
  geom_smooth(method = glm) +
  facet_grid(cols = vars(NewTask)) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Age at the beginning of acquisition of English", y = "Percentage of preterit responses by participant", color = "Token Frequency", 
       title = "HS' Use of Preterit Age of Acquisition of English across Tasks") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
