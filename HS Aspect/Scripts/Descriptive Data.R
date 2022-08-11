# Packages
library(tidyverse)


# Prepare data
## Load preterit data
HS_Preterit = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "HS",
         LongGroup = "Heritage Speakers")

L2_Preterit = read_csv("./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "L2L",
         LongGroup = "L2 Learners")

SDC_Preterit = read.csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "SDC",
         LongGroup = "Comparison Participants")


## Load imperfect data
HS_Imperfect = read_csv("./CSV Files/Heritage/Heritage EPT Imperfect Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "HS",
         LongGroup = "Heritage Speakers")

L2_Imperfect = read_csv("./CSV Files/L2 Learners/L2 Learners EPT Imperfect Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "L2L",
         LongGroup = "L2 Learners")

SDC_Imperfect = read.csv("./CSV Files/Comparison/Comparison EPT Imperfect Data.csv") %>% 
  mutate(NewTask = "EPT",
         ShortGroup = "SDC",
         LongGroup = "Comparison Participants")


## Join preterit and imperfect data
Aggregate <- rbind(HS_Preterit, HS_Imperfect, L2_Preterit, L2_Imperfect, SDC_Preterit, SDC_Imperfect)


# Generate conditions
## Descriptive statistics by condition
Durante_Irregular <- Aggregate %>% 
  filter(Verb_Condition == "Irregular stative with durante") %>% 
  mutate(GraphCondition = "Irregular, durante")

Durante_Regular <- Aggregate %>% 
  filter(Verb_Condition == "Regular stative with durante") %>% 
  mutate(GraphCondition = "Regular, durante")

Durante_Combined <- rbind(Durante_Regular, Durante_Irregular) %>% 
  mutate(GraphCondition = "Durante")

Entre_Irregular <- Aggregate %>% 
  filter(Verb_Condition == "Irregular stative with entre") %>% 
  mutate(GraphCondition = "Irregular, entre")

Entre_Regular <- Aggregate %>% 
  filter(Verb_Condition == "Regular stative with entre") %>% 
  mutate(GraphCondition = "Regular, entre")

Entre_Combined <- rbind(Entre_Regular, Entre_Irregular) %>% 
  mutate(GraphCondition = "Entre")

Imperfect <- Aggregate %>% 
  filter(Property == "Imperfect") %>% 
  mutate(GraphCondition = "Imperfect")

Preterit <- Aggregate %>% 
  filter(Property == "Preterit") %>% 
  mutate(GraphCondition = "Preterit")


## Prepare condition averages
Durante_Irregular_Sum <- aggregate(Durante_Irregular$Response, list(Durante_Irregular$Participant_ID), FUN = sum)
Durante_Irregular_Sum <- Durante_Irregular_Sum %>% rename(Participant_ID = Group.1)
Durante_Irregular_Sum <- Durante_Irregular_Sum %>% rename(Part_Sum = x)
Durante_Irregular_Sum <- left_join(Durante_Irregular, Durante_Irregular_Sum, by = c("Participant_ID" = "Participant_ID"))

Durante_Regular_Sum <- aggregate(Durante_Regular$Response, list(Durante_Regular$Participant_ID), FUN = sum)
Durante_Regular_Sum <- Durante_Regular_Sum %>% rename(Participant_ID = Group.1)
Durante_Regular_Sum <- Durante_Regular_Sum %>% rename(Part_Sum = x)
Durante_Regular_Sum <- left_join(Durante_Regular, Durante_Regular_Sum, by = c("Participant_ID" = "Participant_ID"))

Durante_Combined_Sum <- aggregate(Durante_Combined$Response, list(Durante_Combined$Participant_ID), FUN = sum)
Durante_Combined_Sum <- Durante_Combined_Sum %>% rename(Participant_ID = Group.1)
Durante_Combined_Sum <- Durante_Combined_Sum %>% rename(Part_Sum = x)
Durante_Combined_Sum <- left_join(Durante_Combined, Durante_Combined_Sum, by = c("Participant_ID" = "Participant_ID"))

Entre_Irregular_Sum <- aggregate(Entre_Irregular$Response, list(Entre_Irregular$Participant_ID), FUN = sum)
Entre_Irregular_Sum <- Entre_Irregular_Sum %>% rename(Participant_ID = Group.1)
Entre_Irregular_Sum <- Entre_Irregular_Sum %>% rename(Part_Sum = x)
Entre_Irregular_Sum <- left_join(Entre_Irregular, Entre_Irregular_Sum, by = c("Participant_ID" = "Participant_ID"))

Entre_Regular_Sum <- aggregate(Entre_Regular$Response, list(Entre_Regular$Participant_ID), FUN = sum)
Entre_Regular_Sum <- Entre_Regular_Sum %>% rename(Participant_ID = Group.1)
Entre_Regular_Sum <- Entre_Regular_Sum %>% rename(Part_Sum = x)
Entre_Regular_Sum <- left_join(Entre_Regular, Entre_Regular_Sum, by = c("Participant_ID" = "Participant_ID"))

Entre_Combined_Sum <- aggregate(Entre_Combined$Response, list(Entre_Combined$Participant_ID), FUN = sum)
Entre_Combined_Sum <- Entre_Combined_Sum %>% rename(Participant_ID = Group.1)
Entre_Combined_Sum <- Entre_Combined_Sum %>% rename(Part_Sum = x)
Entre_Combined_Sum <- left_join(Entre_Combined, Entre_Combined_Sum, by = c("Participant_ID" = "Participant_ID"))

Imperfect_Sum <- aggregate(Imperfect$Response, list(Imperfect$Participant_ID), FUN = sum)
Imperfect_Sum <- Imperfect_Sum %>% rename(Participant_ID = Group.1)
Imperfect_Sum <- Imperfect_Sum %>% rename(Part_Sum = x)
Imperfect_Sum <- left_join(Imperfect, Imperfect_Sum, by = c("Participant_ID" = "Participant_ID"))

Preterit_Sum <- aggregate(Preterit$Response, list(Preterit$Participant_ID), FUN = sum)
Preterit_Sum <- Preterit_Sum %>% rename(Participant_ID = Group.1)
Preterit_Sum <- Preterit_Sum %>% rename(Part_Sum = x)
Preterit_Sum <- left_join(Preterit, Preterit_Sum, by = c("Participant_ID" = "Participant_ID"))


# Data for 5 conditions
## Create and organize list
LongConditionsList <- rbind(Durante_Irregular_Sum, Durante_Regular_Sum, Entre_Irregular_Sum, Entre_Regular_Sum, Imperfect_Sum)

LongConditionsList$ShortGroup <- factor(LongConditionsList$ShortGroup,
                               levels = c("SDC", "HS", "L2L"))


## Graph
LongConditionsList %>%
  ggplot(aes(x = GraphCondition, y = Part_Sum, color = ShortGroup)) + 
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 5, 1),
                     limits = c(0, 5)) +
  labs(x = "Condition", y = "Proportion of preterit responses", color = "Group", 
       title = "Average Expected Responses by Group and Condition") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Data for 3 conditions
## Create and organize list
ShortConditionsList <- rbind(Durante_Combined_Sum, Entre_Combined_Sum, Imperfect_Sum, Preterit_Sum) %>% 
  mutate(Percentage = (Response)*100)

ShortConditionsList$ShortGroup <- factor(ShortConditionsList$ShortGroup,
                                         levels = c("SDC", "HS", "L2L"))


## Graph
ShortConditionsList %>%
  ggplot(aes(x = GraphCondition, y = Percentage, color = ShortGroup)) + 
  geom_hline(yintercept = 50, color = "white", size = 2) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  scale_y_continuous(breaks = seq (0, 100, 25),
                     limits = c(0, 100)) +
  labs(x = "Condition", y = "Percentage of expected responses", color = "Group", 
       title = "Percentage of Expected Responses by Group and Condition") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Get statistics tables
HS_Table <- ShortConditionsList %>%
  filter(ShortGroup == "HS") %>% 
  group_by(GraphCondition) %>% 
  summarize(Average = mean(Part_Sum, na.rm = T), SD = sd(Part_Sum, na.rm = T), Max = max(Part_Sum, na.rm = T), Min = min(Part_Sum, na.rm = T))

L2_Table <- ShortConditionsList %>%
  filter(ShortGroup == "L2L") %>% 
  group_by(GraphCondition) %>% 
  summarize(Average = mean(Part_Sum, na.rm = T), SD = sd(Part_Sum, na.rm = T), Max = max(Part_Sum, na.rm = T), Min = min(Part_Sum, na.rm = T))

SDC_Table <- ShortConditionsList %>%
  filter(ShortGroup == "SDC") %>% 
  group_by(GraphCondition) %>% 
  summarize(Average = mean(Part_Sum, na.rm = T), SD = sd(Part_Sum, na.rm = T), Max = max(Part_Sum, na.rm = T), Min = min(Part_Sum, na.rm = T))
