# Packages
library(tidyverse)
library(lme4)
library(lmerTest)

options(sciphen = 999)

# Preparation of data
## Load database
HS = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "HS",
         LongGroup = "Heritage Speakers")

L2 = read_csv("./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "L2L",
         LongGroup = "L2 Learners")

SDC = read.csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "SDC",
         LongGroup = "Comparison Participants")


## Create combined dataset
Preterit = rbind(HS, L2, SDC)


# Prepare participant averages for graphs
## HS
HS_Modified = aggregate(HS$Response, list(HS$Participant_ID), FUN = mean, na.rm = TRUE)
HS_Modified = HS_Modified %>% rename(Part_Avg = x)
HS_Modified = left_join(HS, HS_Modified, by = c("Participant_ID" = "Group.1"))


## L2L
L2_Modified = aggregate(L2$Response, list(L2$Participant_ID), FUN = mean, na.rm = TRUE)
L2_Modified = L2_Modified %>% rename(Part_Avg = x)
L2_Modified = left_join(L2, L2_Modified, by = c("Participant_ID" = "Group.1"))


## SDC
SDC_Modified = aggregate(SDC$Response, list(SDC$Participant_ID), FUN = mean, na.rm = TRUE)
SDC_Modified = SDC_Modified %>% rename(Part_Avg = x)
SDC_Modified = left_join(SDC, SDC_Modified, by = c("Participant_ID" = "Group.1"))


## Aggregate EPT
Aggregate = aggregate(Preterit$Response, list(Preterit$Participant_ID), FUN = mean, na.rm = TRUE)
Aggregate = Aggregate %>% rename(Part_Avg = x)
Aggregate = left_join(Preterit, Aggregate, by = c("Participant_ID" = "Group.1"))

Aggregate$ShortGroup <- factor(Aggregate$ShortGroup,
                                        levels = c("SDC", "HS", "L2L"))


# Prepare verb averages
## HS only
HS_VerbAvg = aggregate(HS$Response, list(HS$MainVerb), FUN = mean, na.rm = TRUE)
HS_VerbAvg = HS_VerbAvg %>% rename(Verb_Avg = x)
HS_VerbAvg = left_join(HS, HS_VerbAvg, by = c("MainVerb" = "Group.1"))


## L2L
L2_VerbAvg = aggregate(L2$Response, list(L2$MainVerb), FUN = mean, na.rm = TRUE)
L2_VerbAvg = L2_VerbAvg %>% rename(Verb_Avg = x)
L2_VerbAvg = left_join(L2, L2_VerbAvg, by = c("MainVerb" = "Group.1"))


## SDC
SDC_VerbAvg = aggregate(SDC$Response, list(SDC$MainVerb), FUN = mean, na.rm = TRUE)
SDC_VerbAvg = SDC_VerbAvg %>% rename(Verb_Avg = x)
SDC_VerbAvg = left_join(SDC, SDC_VerbAvg, by = c("MainVerb" = "Group.1"))


## Aggregate
Verb_Avg_Master = rbind(HS_VerbAvg, L2_VerbAvg, SDC_VerbAvg)

Verb_Avg_Master$ShortGroup <- factor(Verb_Avg_Master$ShortGroup,
                             levels = c("SDC", "HS", "L2L"))


# Plots
## Individual lexical items
Aggregate %>%
  ggplot(aes(x = MainVerb, y = Response, color = ShortGroup)) + 
  geom_hline(yintercept = 0.8, color = "white", size = 2) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  scale_x_discrete(labels = c("estar", "tener", "haber", "vivir", "gustar", "creer", "faltar", "amar", "doler", "excluir")) +
  labs(x = "State verb (from most to least frequent)", y = "Proportion of preterit responses",
       color = "Group", title = "Average Use of Preterit by Verb and Group") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Regularity and task
Aggregate %>%
  ggplot(aes(x = Reg_Main, y = Response, color = Reg_Main)) + 
  facet_grid(cols = vars(LongGroup)) +
  geom_hline(yintercept = 0.8, color = "white", size = 2) + 
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Morphological regularity", y = "Proportion of preterit responses", color = "Group", 
       title = "Morphological Regularity and Preterit Use") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Lexical frequency
Verb_Avg_Master %>%
  ggplot(aes(x = log(Token_Main_Lemma), y = Verb_Avg, color = ShortGroup)) + 
  geom_hline(yintercept = 0.5, color = "white", size = 1.5) + 
  geom_point() +
  geom_smooth(method = glm) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "Log-transformed lexical frequency in Davies (2016)", y = "Proportion of preterit responses", color = "Group", 
       title = "Preterit Use by Lexical Frequency and Group") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.title = element_text(face="bold"))


## Proficiency and task
Preterit_Proficiency <- rbind(HS_Modified, L2_Modified)

Preterit_Proficiency %>% 
  ggplot(aes(x = DELE, y = Part_Avg)) + 
  geom_hline(yintercept = 0.5, color = "white", size = 1) + 
  geom_point() +
  geom_smooth(method = glm) +
  facet_grid(cols = vars(LongGroup)) +
  scale_y_continuous(breaks = seq (0, 1, 0.2),
                     limits = c(0, 1)) +
  labs(x = "DELE proficiency score", y = "Proportion of preterit responses by participant",
       title = "Proportion of Preterit by Group and Proficiency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))