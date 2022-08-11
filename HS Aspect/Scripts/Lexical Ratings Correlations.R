# Load packages
library(tidyverse)
library(lme4)

options(scipen = 999)

# Data preparation
## Load data
Lexical_Item_Report <- read.csv("./CSV Files/Lexical Analysis Master.csv")


## Select lexical items
Amar <- Lexical_Item_Report %>% 
  filter(Verb == "amar")

Creer <- Lexical_Item_Report %>% 
  filter(Verb == "creer")

Doler <- Lexical_Item_Report %>% 
  filter(Verb == "doler")

Estar <- Lexical_Item_Report %>% 
  filter(Verb == "estar")

Excluir <- Lexical_Item_Report %>% 
  filter(Verb == "excluir")

Faltar <- Lexical_Item_Report %>% 
  filter(Verb == "faltar")

Gustar <- Lexical_Item_Report %>% 
  filter(Verb == "gustar")

Haber <- Lexical_Item_Report %>% 
  filter(Verb == "haber")

Tener <- Lexical_Item_Report %>% 
  filter(Verb == "tener")

Vivir <- Lexical_Item_Report %>% 
  filter(Verb == "vivir")

State_Verb_Report <- rbind(Amar, Creer, Doler, Estar, Excluir, Faltar, Gustar, Haber, Tener, Vivir)

## Standardize data
State_Verb_Report <- State_Verb_Report %>% 
  na.omit %>% 
  mutate(Davies_Std = (Davies_Rating - mean(Davies_Rating))/sd(Davies_Rating),
         CORPES_Std = (CORPES_Rating - mean(CORPES_Rating))/sd(CORPES_Rating),
         WordList_Std = (WordList_Rating - mean(WordList_Rating))/sd(WordList_Rating),
         SRLF_Std = (Part_Rating - mean(Part_Rating))/sd(Part_Rating),
         FofU = (FofU_Prod + FofU_Comp),
         FofU_Std = (FofU - mean(FofU))/sd(FofU),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG))


## Generate lexical item average
Average_by_LI <- aggregate(State_Verb_Report$Part_Rating, list(State_Verb_Report$Verb), FUN = mean)
Average_by_LI <- Average_by_LI %>% rename(Verb = Group.1)
Average_by_LI <- Average_by_LI %>% rename(SRLF_Avg = x)
Average_by_LI <- left_join(State_Verb_Report, Average_by_LI, by = c("Verb" = "Verb"))

Average_by_LI <- Average_by_LI %>% 
  mutate(SRLF_Avg_Std = (SRLF_Avg - mean(SRLF_Avg))/sd(SRLF_Avg))


# Lexical item graphs
Average_by_LI %>% 
  ggplot(., aes(x = log(Davies_Rating), y = SRLF_Avg)) + 
  geom_point() + 
  geom_smooth(method = lm) +
  scale_y_continuous(breaks = seq (0, 9, 2),
                     limits = c(0, 9)) +
  labs(x = "Log-transformed lemma frequency in Corpus del español", y = "Average of verb ratings across HS participants", title = "Correlation Between Corpus del español and Self Ratings") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Correlation
Davies_SRLF_Correlation <- lm(Davies_Std ~ SRLF_Avg_Std, data = Average_by_LI)
summary(Davies_SRLF_Correlation)
