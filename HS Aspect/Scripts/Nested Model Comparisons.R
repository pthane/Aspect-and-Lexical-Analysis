# Load packages
library(tidyverse)
library(lme4)
library(broomExtra)
library(broom)

options(scipen = 999)

# Load and prepare data
HS_EPT <- read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
HS_FCT <- read_csv("./CSV Files/Heritage/Heritage FCT Preterit Data.csv")
SDC_EPT <- read_csv("./CSV Files/Comparison/Comparison EPT Preterit Data.csv")
SDC_FCT <- read_csv("./CSV Files/Comparison/Comparison FCT Preterit Data.csv")
HS_Aggregate <- rbind(HS_EPT, HS_FCT)
SDC_Aggregate <- rbind(SDC_EPT, SDC_FCT)
Master_Aggregate <- rbind(HS_Aggregate, SDC_Aggregate)


# Omnibus
NMC_Omnibus_Null <- glmer(Response_Freq ~ 1 +
                            (1 | Participant_ID) + (1 | Item),
                          family = binomial,
                          data = Master_Aggregate)

NMC_Omnibus_Group <- glmer(Response_Freq ~ 1 + ExpGroup +
                            (1 | Participant_ID) + (1 | Item),
                          family = binomial,
                          data = Master_Aggregate)

NMC_Omnibus_Task <- glmer(Response_Freq ~ 1 + ExpGroup + Task +
                            (1 | Participant_ID) + (1 | Item),
                          family = binomial,
                          data = Master_Aggregate)

NMC_Omnibus_DELE <- glmer(Response_Freq ~ 1 + ExpGroup + Task + DELE_Std +
                                   (1 | Participant_ID) + (1 | Item),
                                 family = binomial,
                                 data = Master_Aggregate)

NMC_Omnibus_Group_Task <- glmer(Response_Freq ~ 1 + ExpGroup + Task + DELE_Std + ExpGroup:Task +
                            (1 | Participant_ID) + (1 | Item),
                          family = binomial,
                          data = Master_Aggregate)

NMC_Omnibus_Group_DELE <- glmer(Response_Freq ~ 1 + ExpGroup + Task + DELE_Std + ExpGroup:Task + ExpGroup:DELE_Std +
                                  (1 | Participant_ID) + (1 | Item),
                                family = binomial,
                                data = Master_Aggregate)

NMC_Omnibus_Task_DELE <- glmer(Response_Freq ~ 1 + ExpGroup + Task + DELE_Std + ExpGroup:Task + ExpGroup:DELE_Std + Task:DELE_Std +
                                  (1 | Participant_ID) + (1 | Item),
                                family = binomial,
                                data = Master_Aggregate)

NMC_Omnibus_Interaction <- glmer(Response_Freq ~ 1 + ExpGroup + Task * DELE_Std +
                                 (1 | Participant_ID) + (1 | Item),
                               family = binomial,
                               data = Master_Aggregate)


# Heritage EPT
NMC_EPT_Null <- glmer(Response_Freq ~ 1 +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = HS_EPT)

NMC_EPT_DELE <- glmer(Response_Freq ~ 1 + DELE_Std +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = HS_EPT)

NMC_EPT_FofU <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std +
                             (1 | Participant_ID) + (1 | Item),
                           family = binomial,
                           data = HS_EPT)

NMC_EPT_AoA <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std +
                       (1 | Participant_ID) + (1 | Item),
                     family = binomial,
                     data = HS_EPT)

NMC_EPT_Regularity <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main +
                              (1 | Participant_ID) + (1 | Item),
                            family = binomial,
                            data = HS_EPT)

NMC_EPT_Token <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main + Token_Main_Std +
                              (1 | Participant_ID) + (1 | Item),
                            family = binomial,
                            data = HS_EPT)

NMC_EPT_Regularity_Interaction <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main + Token_Main_Std + DELE_Std:Reg_Main +
                                    (1 | Participant_ID) + (1 | Item),
                                  family = binomial,
                                  data = HS_EPT)

NMC_EPT_Token_Interaction <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main + Token_Main_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
                                          (1 | Participant_ID) + (1 | Item),
                                        family = binomial,
                                        data = HS_EPT)


# Heritage FCT
NMC_FCT_Null <- glmer(Response_Freq ~ 1 +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = HS_FCT)

NMC_FCT_DELE <- glmer(Response_Freq ~ 1 + DELE_Std +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = HS_FCT)

NMC_FCT_FofU <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std +
                        (1 | Participant_ID) + (1 | Item),
                      family = binomial,
                      data = HS_FCT)

NMC_FCT_AoA <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std +
                       (1 | Participant_ID) + (1 | Item),
                     family = binomial,
                     data = HS_FCT)

NMC_FCT_Regularity <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main +
                              (1 | Participant_ID) + (1 | Item),
                            family = binomial,
                            data = HS_FCT)

NMC_FCT_Token <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main + Token_Main_Std +
                         (1 | Participant_ID) + (1 | Item),
                       family = binomial,
                       data = HS_FCT)

NMC_FCT_Regularity_Interaction <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main + Token_Main_Std + DELE_Std:Reg_Main +
                                          (1 | Participant_ID) + (1 | Item),
                                        family = binomial,
                                        data = HS_FCT)

NMC_FCT_Token_Interaction <- glmer(Response_Freq ~ 1 + DELE_Std + FofA_Std + AoA_ENG_Std + Reg_Main + Token_Main_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
                                     (1 | Participant_ID) + (1 | Item),
                                   family = binomial,
                                   data = HS_FCT)


# Results of NMCs
anova(NMC_Omnibus_Null, NMC_Omnibus_Group, NMC_Omnibus_Task, NMC_Omnibus_Group_Task, NMC_Omnibus_Group_DELE, NMC_Omnibus_Task_DELE, NMC_Omnibus_Interaction, test = "Chisq")
anova(NMC_EPT_Null, NMC_EPT_DELE, NMC_EPT_FofU, NMC_EPT_AoA, NMC_EPT_Regularity, NMC_EPT_Token, NMC_EPT_Regularity_Interaction, NMC_EPT_Token_Interaction, test = "Chisq")
anova(NMC_FCT_Null, NMC_FCT_DELE, NMC_FCT_FofU, NMC_FCT_AoA, NMC_FCT_Regularity, NMC_FCT_Token, NMC_FCT_Regularity_Interaction, NMC_FCT_Token_Interaction, test = "Chisq")

