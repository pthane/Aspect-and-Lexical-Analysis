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


# HS Correlations
## Aggregate model
Omnibus_Task_Model <- glmer(Response ~ ExpGroup * Task +
                              (1 | Participant_ID) + (1 | Item),
                            data = Master_Aggregate,
                            family = "binomial")

summary(Omnibus_Task_Model)

plot_model(Omnibus_Task_Model, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "Task and Group Model", y = "β Estimates") +
  scale_x_discrete(labels = c("Task : Group", "Group", "Task", "Intercept")) +
  scale_y_continuous(breaks = seq (-2.5, 2.5, 0.5),
                     limits = c(-2.5, 2.5)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## Production model
HS_EPT_Model <- glmer(Response ~ DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
                     (1 | Seq_part) + (1 | Item),
                   data = HS_EPT,
                  family = "binomial")

summary(HS_EPT_Model)

plot_model(HS_EPT_Model, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "HS Elicited Production Model", y = "β Estimates") +
  scale_x_discrete(labels = c("Proficiency : lexical interaction", "Proficiency : regularity interaction", "Proficiency : use interaction", "Lexical frequency", "Regularity", "Frequency of use", "Proficiency", "Intercept")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## FCT model
HS_FCT_Model <- glmer(Response ~ DELE_Std +
                     (1 | Seq_part) + (1 | Item),
                   data = HS_FCT,
                   family = "binomial")

summary(HS_FCT_Model)

plot_model(HS_FCT_Model, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "HS Forced Choice Model", y = "β Estimates") +
  scale_x_discrete(labels = c("Proficiency", "Intercept")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# L2L Correlations
## Production model
SDC_EPT_Model <- glmer(Response ~ DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
                        (1 | Seq_part) + (1 | Item),
                      data = SDC_EPT,
                      family = "binomial")

summary(SDC_EPT_Model)

plot_model(SDC_EPT_Model, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "SDC Elicited Production Model", y = "β Estimates") +
  scale_x_discrete(labels = c("Proficiency : Lemma interaction", "Lemma frequency", "Frequency of use", "Proficiency", "Intercept")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## FCT model
SDC_FCT_Model <- glmer(Response ~ DELE_Std + FofA_Std + Reg_Main + Token_Main_Std + DELE_Std:FofA_Std + DELE_Std:Reg_Main + DELE_Std:Token_Main_Std +
                        (1 | Seq_part) + (1 | Item),
                      data = SDC_FCT,
                      family = "binomial")

summary(SDC_FCT_Model)

plot_model(SDC_FCT_Model, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  labs(title = "SDC Forced Choice Model", y = "β Estimates") +
  scale_x_discrete(labels = c("Proficiency : Lemma interaction", "Lemma frequency", "Frequency of use", "Proficiency", "Intercept")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
