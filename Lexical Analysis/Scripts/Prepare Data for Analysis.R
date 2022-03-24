library(tidyverse)


Lexical_Item_Report <- read.csv("./CSV Files/Lexical Analysis Master.csv") %>% 
  na.omit %>% 
  mutate(Davies_Std = (Davies_Rating - mean(Davies_Rating))/sd(Davies_Rating),
         CORPES_Std = (CORPES_Rating - mean(CORPES_Rating))/sd(CORPES_Rating),
         WordList_Std = (WordList_Rating - mean(WordList_Rating))/sd(WordList_Rating),
         SRLF_Std = (Part_Rating - mean(Part_Rating))/sd(Part_Rating),
         FofU = (FofU_Prod + FofU_Comp),
         FofU_Std = (FofU - mean(FofU))/sd(FofU),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG))

write.csv(Lexical_Item_Report, "./CSV Files/Lexical Item Analysis/Lexical Item Report.csv")
