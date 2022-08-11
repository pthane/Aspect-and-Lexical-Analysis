library(tidyverse)
library(lme4)
library(lmerTest)

options(sciphen = 999)

# Preparation of data
HS_EPT_Advanced = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>% 
  filter(DELE < 39) %>% 
  mutate(NewTask = "EPT",
         ProfLevel = "Advanced")

HS_EPT_Intermediate = read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv") %>% 
  filter(DELE > 40) %>% 
  mutate(NewTask = "EPT",
         ProfLevel = "Intermediate")


# Creation of verb averages
## Advanced group
HS_EPT_Advanced_VerbAvg = aggregate(HS_EPT_Advanced$Response, list(HS_EPT_Advanced$MainVerb), FUN = mean, na.rm = TRUE)
HS_EPT_Advanced_VerbAvg = HS_EPT_Advanced_VerbAvg %>% rename(Verb_Avg = x)
HS_EPT_Advanced_VerbAvg = left_join(HS_EPT_Advanced, HS_EPT_Advanced_VerbAvg, by = c("MainVerb" = "Group.1")) %>% 
  mutate(Verb_Avg = (Verb_Avg * 100))

## Intermediate group
HS_EPT_Intermediate_VerbAvg = aggregate(HS_EPT_Intermediate$Response, list(HS_EPT_Intermediate$MainVerb), FUN = mean, na.rm = TRUE)
HS_EPT_Intermediate_VerbAvg = HS_EPT_Intermediate_VerbAvg %>% rename(Verb_Avg = x)
HS_EPT_Intermediate_VerbAvg = left_join(HS_EPT_Intermediate, HS_EPT_Intermediate_VerbAvg, by = c("MainVerb" = "Group.1")) %>% 
  mutate(Verb_Avg = (Verb_Avg * 100))

Data_for_Plot <- rbind(HS_EPT_Advanced_VerbAvg, HS_EPT_Intermediate_VerbAvg)

# Generate plot of interaction
Data_for_Plot %>%
  ggplot(aes(x = log(Token_Main_Lemma), y = Verb_Avg, color = ProfGroup)) + 
  geom_point() +
  geom_smooth(method = glm) +
  scale_y_continuous(breaks = seq (0, 100, 20),
                     limits = c(0, 100)) +
  labs(x = "Log-transformed lexical frequency in Davies (2016)", y = "Percentage of preterit responses by verb", color = "Proficiency Level", 
       title = "HS' Production of Preterit by Verb Frequency and Proficiency") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

