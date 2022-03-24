# Libraries
library(tidyverse)
library(ggpubr)
library(lme4)

options(scipen = 99)

# Load data
## Load databases
Lexical_Item_Report <- read.csv("./CSV Files/Lexical Item Analysis/Lexical Item Report.csv")


## Generate participant averages
Average_by_LI <- aggregate(Lexical_Item_Report$Part_Rating, list(Lexical_Item_Report$Verb), FUN = mean)
Average_by_LI <- Average_by_LI %>% rename(Verb = Group.1)
Average_by_LI <- Average_by_LI %>% rename(SRLF_Avg = x)
Average_by_LI <- left_join(Lexical_Item_Report, Average_by_LI, by = c("Verb" = "Verb"))

Average_by_LI <- Average_by_LI %>% 
  mutate(SRLF_Avg_Std = (SRLF_Avg - mean(SRLF_Avg))/sd(SRLF_Avg))


# Davies correlation
cor.test(Average_by_LI$SRLF_Avg, Average_by_LI$Davies_Std)

Davies_lm <- lm(SRLF_Avg ~ Davies_Std,
                data = Average_by_LI)

summary(Davies_lm)


## esTenTen18
cor.test(Average_by_LI$SRLF_Avg, Average_by_LI$WordList_Std)

WordList_lm <- lm(SRLF_Avg ~ WordList_Std,
                data = Average_by_LI)

summary(WordList_lm)
