# Run packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(broomExtra)
library(broom)

options(scipen = 999)

# Load data
HS_EPT <- read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
HS_FCT <- read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv")
SDC_EPT <- read_csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv")
SDC_FCT <- read_csv("./CSV Files/Comparison/Comparison FCT Preterit Data.csv")
HS_Aggregate <- rbind(HS_EPT, HS_FCT)
SDC_Aggregate <- rbind(SDC_EPT, SDC_FCT)
Master_Aggregate <- rbind(HS_Aggregate, SDC_Aggregate)


# Correlations
## Aggregate model
Omnibus <- glmer(Response_Freq ~ ExpGroup * Task * DELE_Std +
                              (1 | Participant_ID) + (1 | Item),
                            data = Master_Aggregate,
                            family = "binomial")


summary(Omnibus)


## HS-specific models
### Master production model
EPT <- glmer(Response_Freq ~ DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main + Token_Main_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
                     (1 | Seq_part) + (1 | Item),
                   data = HS_EPT,
                   family = "binomial")

summary(EPT)


### Master FCT model
FCT <- glmer(Response_Freq ~ DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main + Token_Main_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
               (1 | Seq_part) + (1 | Item),
             data = HS_FCT,
             family = "binomial")

summary(FCT)