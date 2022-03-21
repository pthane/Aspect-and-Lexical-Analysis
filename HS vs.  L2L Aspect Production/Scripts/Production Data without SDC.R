# Run packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(broomExtra)
library(broom)

options(scipen = 999)

# Load data
HS <- read_csv("./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
L2 <- read_csv("./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv")
Master_Aggregate <- rbind(HS, L2)


# Model
Aggregate_Model <- glmer(Response ~ ExpGroup * DELE_Std * Token_Main_Std +
                           (1 | Seq_part) + (1 | Item),
                         data = Master_Aggregate,
                         family = "binomial")

summary(Aggregate_Model)

plot_model(Aggregate_Model, show.values = TRUE, show.intercept = TRUE, value.offset = .3, transform = NULL, vline.color = "black") +
  scale_x_discrete(labels = c("L2L : Proficiency : LF", "Proficiency : LF", "L2L : LF", "L2L : Proficiency", "Lexical Frequency (LF)", "Proficiency", "L2L Group", "(Intercept)")) +
  labs(title = "Results of Two-Group Model", y = "Beta Estimates") +
  scale_y_continuous(breaks = seq (-2, 2, 1),
                     limits = c(-2, 2)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Nested model comparisons
NMC_Null <- glmer(Response ~ 1 +
                    (1 | Seq_part) + (1 | Item),
                  data = Master_Aggregate,
                  family = "binomial")

NMC_ExpGroup <- glmer(Response ~ 1 + ExpGroup  +
                        (1 | Seq_part) + (1 | Item),
                      data = Master_Aggregate,
                      family = "binomial")

NMC_DELE <- glmer(Response ~ 1 + ExpGroup + DELE_Std +
                    (1 | Seq_part) + (1 | Item),
                  data = Master_Aggregate,
                  family = "binomial")

NMC_Token <- glmer(Response ~ 1 + ExpGroup + DELE_Std + Token_Main_Std  +
                     (1 | Seq_part) + (1 | Item),
                   data = Master_Aggregate,
                   family = "binomial")

NMC_Group_DELE <- glmer(Response ~ 1 + ExpGroup + DELE_Std + Token_Main_Std + ExpGroup:DELE_Std  +
                          (1 | Seq_part) + (1 | Item),
                        data = Master_Aggregate,
                        family = "binomial")

NMC_Group_Token <- glmer(Response ~ 1 + ExpGroup + DELE_Std + Token_Main_Std + ExpGroup:DELE_Std + ExpGroup:Token_Main_Std  +
                           (1 | Seq_part) + (1 | Item),
                         data = Master_Aggregate,
                         family = "binomial")

NMC_DELE_Token <- glmer(Response ~ 1 + ExpGroup + DELE_Std + Token_Main_Std + ExpGroup:DELE_Std + ExpGroup:Token_Main_Std + DELE_Std:Token_Main_Std  +
                          (1 | Seq_part) + (1 | Item),
                        data = Master_Aggregate,
                        family = "binomial")

NMC_Final <- glmer(Response ~ 1 + ExpGroup * DELE_Std * Token_Main_Std  +
                     (1 | Seq_part) + (1 | Item),
                   data = Master_Aggregate,
                   family = "binomial")

anova(NMC_Null, NMC_ExpGroup, NMC_DELE, NMC_Token, NMC_Group_DELE, NMC_Group_Token, NMC_DELE_Token, NMC_Final, test = "Chisq")