library(tidyverse)

# Load Data
EPT_Master <- read_csv("./CSV Files/EPT Master.csv")
FCT_Master <- read_csv("./CSV Files/FCT Master.csv")

# Separate by task
EPT_Preterit <- EPT_Master %>% 
  filter(Property == "Preterit")

EPT_Mood <- EPT_Master %>% 
  filter(Property == "Subjunctive")

FCT_Preterit <- FCT_Master %>% 
  filter(Property == "Preterit")

FCT_Mood <- FCT_Master %>% 
  filter(Property == "Subjunctive")

EPT_Imperfect <- EPT_Master %>% 
  filter(Property == "Imperfect")

FCT_Imperfect <- EPT_Master %>% 
  filter(Property == "Imperfect")


# Filter out "valorar" and "conseguir" from mood datasets
## Mood EPT (production task)
EPT_Mood_Esperar <- EPT_Mood %>% 
  filter(MainVerb == "#01: esperar")

EPT_Mood_Querer <- EPT_Mood %>% 
  filter(MainVerb == "#02: querer")

EPT_Mood_Permitir <- EPT_Mood %>% 
  filter(MainVerb == "#03: permitir")

EPT_Mood_Necesitar <- EPT_Mood %>% 
  filter(MainVerb == "#04: necesitar")

EPT_Mood_Pedir <- EPT_Mood %>% 
  filter(MainVerb == "#05: pedir")

EPT_Mood_Desear <- EPT_Mood %>% 
  filter(MainVerb == "#06: desear")

EPT_Mood_Sugerir <- EPT_Mood %>% 
  filter(MainVerb == "#07: sugerir")

EPT_Mood_Ordenar <- EPT_Mood %>% 
  filter(MainVerb == "#08: ordenar")

EPT_Mood_Adjusted <- rbind(EPT_Mood_Desear, EPT_Mood_Esperar, EPT_Mood_Necesitar, EPT_Mood_Ordenar, EPT_Mood_Pedir, EPT_Mood_Permitir, EPT_Mood_Querer, EPT_Mood_Sugerir)


## Mood FCT (listed as "MPST" in manuscript)
FCT_Mood_Esperar <- FCT_Mood %>% 
  filter(MainVerb == "#01: esperar")

FCT_Mood_Querer <- FCT_Mood %>% 
  filter(MainVerb == "#02: querer")

FCT_Mood_Permitir <- FCT_Mood %>% 
  filter(MainVerb == "#03: permitir")

FCT_Mood_Necesitar <- FCT_Mood %>% 
  filter(MainVerb == "#04: necesitar")

FCT_Mood_Pedir <- FCT_Mood %>% 
  filter(MainVerb == "#05: pedir")

FCT_Mood_Desear <- FCT_Mood %>% 
  filter(MainVerb == "#06: desear")

FCT_Mood_Sugerir <- FCT_Mood %>% 
  filter(MainVerb == "#07: sugerir")

FCT_Mood_Ordenar <- FCT_Mood %>% 
  filter(MainVerb == "#08: ordenar")

FCT_Mood_Adjusted <- rbind(FCT_Mood_Desear, FCT_Mood_Esperar, FCT_Mood_Necesitar, FCT_Mood_Ordenar, FCT_Mood_Pedir, FCT_Mood_Permitir, FCT_Mood_Querer, FCT_Mood_Sugerir)


# Create aggregate CSVs
Aggregate_Preterit <- rbind(EPT_Preterit, FCT_Preterit)
Aggregate_Mood <- rbind(EPT_Mood_Adjusted, FCT_Mood_Adjusted)
Aggregate_Imperfect <- rbind(EPT_Imperfect, FCT_Imperfect)


# Generate CSVs for comparison group
#### Note: this script removes any participants who arrived in the U.S. before adolescence (age 13).
EPT_Preterit_Comparison <- EPT_Preterit %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

EPT_Mood_Comparison <- EPT_Mood_Adjusted %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

EPT_Imperfect_Comparison <- EPT_Imperfect %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

FCT_Preterit_Comparison <- FCT_Preterit %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

FCT_Mood_Comparison <- FCT_Mood_Adjusted %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

FCT_Imperfect_Comparison <- FCT_Imperfect %>%
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

Aggregate_Preterit_Comparison <- Aggregate_Preterit %>% 
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

Aggregate_Mood_Comparison <- Aggregate_Mood %>% 
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)

Aggregate_Imperfect_Comparison <- Aggregate_Imperfect %>% 
  filter(ExpGroup == "Comparison") %>% 
  filter(LOR > 12)


## Generate CSVs for HS
EPT_Preterit_Heritage <- EPT_Preterit %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

EPT_Mood_Heritage <- EPT_Mood_Adjusted %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

EPT_Imperfect_Heritage <- EPT_Imperfect %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

FCT_Preterit_Heritage <- FCT_Preterit %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

FCT_Mood_Heritage <- FCT_Mood_Adjusted %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

FCT_Imperfect_Heritage <- FCT_Imperfect %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

Aggregate_Preterit_Heritage <- Aggregate_Preterit %>% 
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

Aggregate_Mood_Heritage <- Aggregate_Mood %>% 
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)

Aggregate_Imperfect_Heritage <- Aggregate_Imperfect %>%
  filter(ExpGroup == "Heritage") %>% 
  filter(AoA_ENG < 8)


## Generate CSVs for L2 Learners
EPT_Preterit_L2_Learners <- EPT_Preterit %>%
  filter(ExpGroup == "L2 Learner")

EPT_Mood_L2_Learners <- EPT_Mood_Adjusted %>%
  filter(ExpGroup == "L2 Learner") %>% 
  filter(Langs =="2")

EPT_Imperfect_L2_Learners <- EPT_Imperfect %>%
  filter(ExpGroup == "L2 Learner")

FCT_Preterit_L2_Learners <- FCT_Preterit %>%
  filter(ExpGroup == "L2 Learner")

FCT_Mood_L2_Learners <- FCT_Mood_Adjusted %>%
  filter(ExpGroup == "L2 Learner") %>% 
  filter(Langs =="2")

FCT_Imperfect_L2_Learners <- FCT_Preterit %>%
  filter(ExpGroup == "L2 Learner")

Aggregate_Preterit_L2_Learners <- Aggregate_Preterit %>% 
  filter(ExpGroup == "L2 Learner")

Aggregate_Mood_L2_Learners <- Aggregate_Mood %>% 
  filter(ExpGroup == "L2 Learner") %>% 
  filter(Langs =="2")

Aggregate_Imperfect_L2_Learners <- Aggregate_Imperfect %>%
  filter(ExpGroup == "L2 Learner")


# Create data sets within heritage group
## Sequential bilingual CSVs
EPT_Preterit_Simultaneous <- EPT_Preterit %>%
  filter(HerGroup == "Simultaneous")

EPT_Mood_Simultaneous <- EPT_Mood_Adjusted %>%
  filter(HerGroup == "Simultaneous")

EPT_Imperfect_Simultaneous <- EPT_Imperfect %>%
  filter(HerGroup == "Simultaneous")

FCT_Preterit_Simultaneous <- FCT_Preterit %>%
  filter(HerGroup == "Simultaneous")

FCT_Mood_Simultaneous <- FCT_Mood_Adjusted %>%
  filter(HerGroup == "Simultaneous")

FCT_Imperfect_Simultaneous <- FCT_Imperfect %>%
  filter(HerGroup == "Simultaneous")

Aggregate_Preterit_Simultaneous <- Aggregate_Preterit %>% 
  filter(HerGroup == "Simultaneous")

Aggregate_Mood_Simultaneous <- Aggregate_Mood %>% 
  filter(HerGroup == "Simultaneous")

Aggregate_Imperfect_Simultaneous <- Aggregate_Imperfect %>% 
  filter(HerGroup == "Simultaneous")


## Simultaneous bilingual CSVs
EPT_Preterit_Sequential <- EPT_Preterit %>%
  filter(HerGroup == "Sequential")

EPT_Mood_Sequential <- EPT_Mood_Adjusted %>%
  filter(HerGroup == "Sequential")

EPT_Imperfect_Sequential <- EPT_Imperfect %>%
  filter(HerGroup == "Sequential")

FCT_Preterit_Sequential <- FCT_Preterit %>%
  filter(HerGroup == "Sequential")

FCT_Mood_Sequential <- FCT_Mood_Adjusted %>%
  filter(HerGroup == "Sequential")

FCT_Imperfect_Sequential <- FCT_Imperfect %>%
  filter(HerGroup == "Sequential")

Aggregate_Preterit_Sequential <- Aggregate_Preterit %>% 
  filter(HerGroup == "Sequential")

Aggregate_Mood_Sequential <- Aggregate_Mood %>% 
  filter(HerGroup == "Sequential")

Aggregate_Imperfect_Sequential <- Aggregate_Imperfect %>%
  filter(HerGroup == "Sequential")


# Standardize datasets for comparison group
## Aspect data
### Preterit EPT
EPT_Preterit_Comparison <- EPT_Preterit_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Imperfect EPT
EPT_Imperfect_Comparison <- EPT_Imperfect_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Preterit FCT
FCT_Preterit_Comparison <- FCT_Preterit_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Imperfect FCT
FCT_Imperfect_Comparison <- FCT_Imperfect_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aggregate preterit
Aggregate_Preterit_Comparison <- Aggregate_Preterit_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aggregate imperfect
Aggregate_Imperfect_Comparison <- Aggregate_Imperfect_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Mood data
### Mood EPT
EPT_Mood_Comparison = EPT_Mood_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Mood FCT
FCT_Mood_Comparison = FCT_Mood_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aggregate mood
Aggregate_Mood_Comparison <- Aggregate_Mood_Comparison %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Standardize datasets for Heritage group
## Aspect data
### Preterit EPT
EPT_Preterit_Heritage <- EPT_Preterit_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect EPT
EPT_Imperfect_Heritage <- EPT_Imperfect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Preterit FCT
FCT_Preterit_Heritage <- FCT_Preterit_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### FCT Imperfect
FCT_Imperfect_Heritage <- FCT_Imperfect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aspect aggregate
Aggregate_Preterit_Heritage = Aggregate_Preterit_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aggregate Imperfect
Aggregate_Imperfect_Heritage = Aggregate_Imperfect_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Mood data
### Mood EPT
EPT_Mood_Heritage = EPT_Mood_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Mood FCT
FCT_Mood_Heritage = FCT_Mood_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aggregate Mood
Aggregate_Mood_Heritage = Aggregate_Mood_Heritage %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Standardize datasets for L2 Learners group
## Aspect data
### Preterit EPT
EPT_Preterit_L2_Learners <- EPT_Preterit_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect EPT
EPT_Imperfect_L2_Learners <- EPT_Imperfect_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Preterit FCT
FCT_Preterit_L2_Learners <- FCT_Preterit_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect FCT
FCT_Imperfect_L2_Learners <- FCT_Imperfect_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aggregate preterit
Aggregate_Preterit_L2_Learners = Aggregate_Preterit_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aggregate imperfect
Aggregate_Imperfect_L2_Learners = Aggregate_Imperfect_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Mood data
### Mood EPT
EPT_Mood_L2_Learners = EPT_Mood_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Mood FCT
FCT_Mood_L2_Learners = FCT_Mood_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Mood aggregate
Aggregate_Mood_L2_Learners = Aggregate_Mood_L2_Learners %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Standardize datasets for simultaneous group
## Aspect data
### Preterit EPT
EPT_Preterit_Simultaneous <- EPT_Preterit_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect EPT
EPT_Imperfect_Simultaneous <- EPT_Imperfect_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Preterit FCT
FCT_Preterit_Simultaneous <- FCT_Preterit_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect FCT
FCT_Imperfect_Simultaneous <- FCT_Imperfect_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Preterit aggregate
Aggregate_Preterit_Simultaneous = Aggregate_Preterit_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect aggregate
Aggregate_Imperfect_Simultaneous <- Aggregate_Imperfect_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Mood data
### Mood EPT
EPT_Mood_Simultaneous = EPT_Mood_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Mood FCT
FCT_Mood_Simultaneous = FCT_Mood_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Aggregate Mood
Aggregate_Mood_Simultaneous = Aggregate_Mood_Simultaneous %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Standardize datasets for Sequential group
## Aspect data
### Preterit EPT
EPT_Preterit_Sequential <- EPT_Preterit_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect EPT
EPT_Imperfect_Sequential <- EPT_Imperfect_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Preterit FCT
FCT_Preterit_Sequential <- FCT_Preterit_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect FCT
FCT_Imperfect_Sequential <- FCT_Imperfect_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Preterit aggregate
Aggregate_Preterit_Sequential = Aggregate_Preterit_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


### Imperfect aggregate
Aggregate_Imperfect_Sequential <- Aggregate_Imperfect_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


## Mood data
### Mood EPT
EPT_Mood_Sequential = EPT_Mood_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Mood FCT
FCT_Mood_Sequential = FCT_Mood_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))

### Aggregate Mood
Aggregate_Mood_Sequential = Aggregate_Mood_Sequential %>%
  mutate(FofA = FofA_Prod + FofA_Comp) %>%
  mutate(Token_Differential = Token_Main_Imp - Token_Main_Pret) %>%
  mutate(FofA_Std = (FofA - mean(FofA))/sd(FofA),
         FofA_Prod_Std = (FofA_Prod - mean(FofA_Prod))/sd(FofA_Prod),
         FofA_Comp_Std = (FofA_Comp - mean(FofA_Comp))/sd(FofA_Comp),
         AoA_ENG_Std = (AoA_ENG - mean(AoA_ENG))/sd(AoA_ENG),
         DELE_Std = (DELE - mean(DELE))/sd(DELE),
         Token_Main_Std = (Token_Main_Lemma - mean(Token_Main_Lemma))/sd(Token_Main_Lemma),
         Token_Differential_Std = (Token_Differential - mean(Token_Differential))/sd(Token_Differential),
         Token_Pret_Std = (Token_Main_Pret - mean(Token_Main_Pret))/sd(Token_Main_Pret),
         Token_Sub_Std = (Token_Sub - mean(Token_Sub))/sd(Token_Sub),
         LOR_Std = (LOR - mean(LOR))/sd(LOR))


# Write revised CSV files
## Comparison
write_csv(EPT_Preterit_Comparison, "./CSV Files/Comparison/Comparison EPT Preterit Data.csv")
write_csv(EPT_Imperfect_Comparison, "./CSV Files/Comparison/Comparison EPT Imperfect Data.csv")
write_csv(EPT_Mood_Comparison, "./CSV Files/Comparison/Comparison EPT Subjunctive Data.csv")
write_csv(FCT_Imperfect_Comparison, "./CSV Files/Comparison/Comparison FCT Imperfect Data.csv")
write_csv(FCT_Preterit_Comparison, "./CSV Files/Comparison/Comparison FCT Preterit Data.csv")
write_csv(FCT_Mood_Comparison, "./CSV Files/Comparison/Comparison FCT Subjunctive Data.csv")
write_csv(Aggregate_Imperfect_Comparison, "./CSV Files/Comparison/Comparison Aggregate Imperfect Data.csv")
write_csv(Aggregate_Preterit_Comparison, "./CSV Files/Comparison/Comparison Aggregate Preterit Data.csv")
write_csv(Aggregate_Mood_Comparison, "./CSV Files/Comparison/Comparison Aggregate Subjunctive Data.csv")


## Heritage
write_csv(EPT_Preterit_Heritage, "./CSV Files/Heritage/Heritage EPT Preterit Data.csv")
write_csv(EPT_Imperfect_Heritage, "./CSV Files/Heritage/Heritage EPT Imperfect Data.csv")
write_csv(EPT_Mood_Heritage, "./CSV Files/Heritage/Heritage EPT Subjunctive Data.csv")
write_csv(FCT_Imperfect_Heritage, "./CSV Files/Heritage/Heritage FCT Imperfect Data.csv")
write_csv(FCT_Preterit_Heritage, "./CSV Files/Heritage/Heritage FCT Preterit Data.csv")
write_csv(FCT_Mood_Heritage, "./CSV Files/Heritage/Heritage FCT Subjunctive Data.csv")
write_csv(Aggregate_Imperfect_Heritage, "./CSV Files/Heritage/Heritage Aggregate Imperfect Data.csv")
write_csv(Aggregate_Preterit_Heritage, "./CSV Files/Heritage/Heritage Aggregate Preterit Data.csv")
write_csv(Aggregate_Mood_Heritage, "./CSV Files/Heritage/Heritage Aggregate Subjunctive Data.csv")


## L2 Learners
write_csv(EPT_Preterit_L2_Learners, "./CSV Files/L2 Learners/L2 Learners EPT Preterit Data.csv")
write_csv(EPT_Imperfect_L2_Learners, "./CSV Files/L2 Learners/L2 Learners EPT Imperfect Data.csv")
write_csv(EPT_Mood_L2_Learners, "./CSV Files/L2 Learners/L2 Learners EPT Subjunctive Data.csv")
write_csv(FCT_Imperfect_L2_Learners, "./CSV Files/L2 Learners/L2 Learners FCT Imperfect Data.csv")
write_csv(FCT_Preterit_L2_Learners, "./CSV Files/L2 Learners/L2 Learners FCT Preterit Data.csv")
write_csv(FCT_Mood_L2_Learners, "./CSV Files/L2 Learners/L2 Learners FCT Subjunctive Data.csv")
write_csv(Aggregate_Imperfect_L2_Learners, "./CSV Files/L2 Learners/L2 Learners Aggregate Imperfect Data.csv")
write_csv(Aggregate_Preterit_L2_Learners, "./CSV Files/L2 Learners/L2 Learners Aggregate Preterit Data.csv")
write_csv(Aggregate_Mood_L2_Learners, "./CSV Files/L2 Learners/L2 Learners Aggregate Subjunctive Data.csv")


## Simultaneous Bilinguals
write_csv(EPT_Preterit_Simultaneous, "./CSV Files/Simultaneous/Simultaneous EPT Preterit Data.csv")
write_csv(EPT_Imperfect_Simultaneous, "./CSV Files/Simultaneous/Simultaneous EPT Imperfect Data.csv")
write_csv(EPT_Mood_Simultaneous, "./CSV Files/Simultaneous/Simultaneous EPT Subjunctive Data.csv")
write_csv(FCT_Imperfect_Simultaneous, "./CSV Files/Simultaneous/Simultaneous FCT Imperfect Data.csv")
write_csv(FCT_Preterit_Simultaneous, "./CSV Files/Simultaneous/Simultaneous FCT Preterit Data.csv")
write_csv(FCT_Mood_Simultaneous, "./CSV Files/Simultaneous/Simultaneous FCT Subjunctive Data.csv")
write_csv(Aggregate_Imperfect_Simultaneous, "./CSV Files/Simultaneous/Simultaneous Aggregate Imperfect Data.csv")
write_csv(Aggregate_Preterit_Simultaneous, "./CSV Files/Simultaneous/Simultaneous Aggregate Preterit Data.csv")
write_csv(Aggregate_Mood_Simultaneous, "./CSV Files/Simultaneous/Simultaneous Aggregate Subjunctive Data.csv")


## Sequential Bilinguals
write_csv(EPT_Preterit_Sequential, "./CSV Files/Sequential/Sequential EPT Preterit Data.csv")
write_csv(EPT_Imperfect_Sequential, "./CSV Files/Sequential/Sequential EPT Imperfect Data.csv")
write_csv(EPT_Mood_Sequential, "./CSV Files/Sequential/Sequential EPT Subjunctive Data.csv")
write_csv(FCT_Imperfect_Sequential, "./CSV Files/Sequential/Sequential FCT Imperfect Data.csv")
write_csv(FCT_Preterit_Sequential, "./CSV Files/Sequential/Sequential FCT Preterit Data.csv")
write_csv(FCT_Mood_Sequential, "./CSV Files/Sequential/Sequential FCT Subjunctive Data.csv")
write_csv(Aggregate_Imperfect_Sequential, "./CSV Files/Sequential/Sequential Aggregate Imperfect Data.csv")
write_csv(Aggregate_Preterit_Sequential, "./CSV Files/Sequential/Sequential Aggregate Preterit Data.csv")
write_csv(Aggregate_Mood_Sequential, "./CSV Files/Sequential/Sequential Aggregate Subjunctive Data.csv")