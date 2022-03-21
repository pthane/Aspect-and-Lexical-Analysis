# Load packages
library(tidyverse)
library(lme4)
library(broomExtra)
library(broom)

options(scipen = 999)

# Load and prepare data
SDC_EPT <- read_csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv")
SDC_FCT <- read_csv("./CSV Files/Comparison/Comparison FCT Preterit Data.csv")
SDC_Aggregate <- rbind(SDC_EPT, SDC_FCT)


# SDCEPT
NMC_EPT_Null <- glmer(Response ~ 1 +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = SDC_EPT)

NMC_EPT_DELE <- glmer(Response ~ 1 + DELE_Std +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = SDC_EPT)

NMC_EPT_FofU <- glmer(Response ~ 1 + DELE_Std + FofA_Std +
                             (1 | Participant_ID) + (1 | Item),
                           family = binomial,
                           data = SDC_EPT)

NMC_EPT_Regularity <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main +
                              (1 | Participant_ID) + (1 | Item),
                            family = binomial,
                            data = SDC_EPT)

NMC_EPT_Token <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main + Token_Main_Std +
                              (1 | Participant_ID) + (1 | Item),
                            family = binomial,
                            data = SDC_EPT)

NMC_EPT_FofA_Interaction <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std +
                              (1 | Participant_ID) + (1 | Item),
                            family = binomial,
                            data = SDC_EPT)

NMC_EPT_Regularity_Interaction <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std + DELE_Std:Reg_Main +
                                    (1 | Participant_ID) + (1 | Item),
                                  family = binomial,
                                  data = SDC_EPT)

NMC_EPT_Token_Interaction <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
                                          (1 | Participant_ID) + (1 | Item),
                                        family = binomial,
                                        data = SDC_EPT)


# SDC FCT
NMC_FCT_Null <- glmer(Response ~ 1 +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = SDC_FCT)

NMC_FCT_DELE <- glmer(Response ~ 1 + DELE_Std +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = SDC_FCT)

NMC_FCT_FofU <- glmer(Response ~ 1 + DELE_Std + FofA_Std +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = SDC_FCT)

NMC_FCT_Regularity <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main +
                              (1 | Participant_ID) + (1 | Item),
                            family = binomial,
                            data = SDC_FCT)

NMC_FCT_Token <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main + Token_Main_Std +
                         (1 | Participant_ID) + (1 | Item),
                       family = binomial,
                       data = SDC_FCT)

NMC_FCT_FofA_Interaction <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std +
                                    (1 | Participant_ID) + (1 | Item),
                                  family = binomial,
                                  data = SDC_FCT)

NMC_FCT_Regularity_Interaction <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std + DELE_Std:Reg_Main +
                                          (1 | Participant_ID) + (1 | Item),
                                        family = binomial,
                                        data = SDC_FCT)

NMC_FCT_Token_Interaction <- glmer(Response ~ 1 + DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
                                     (1 | Participant_ID) + (1 | Item),
                                   family = binomial,
                                   data = SDC_FCT)


# Results of NMCs
anova(NMC_EPT_Null, NMC_EPT_DELE, NMC_EPT_FofU, NMC_EPT_Regularity, NMC_EPT_Token, NMC_EPT_FofA_Interaction, NMC_EPT_Regularity_Interaction, NMC_EPT_Token_Interaction, test = "Chisq")
anova(NMC_FCT_Null, NMC_FCT_DELE, NMC_FCT_FofU, NMC_FCT_Regularity, NMC_FCT_Token, NMC_FCT_FofA_Interaction, NMC_FCT_Regularity_Interaction, NMC_FCT_Token_Interaction, test = "Chisq")
