# ======================================================================
# UNIVERSITE DE CAEN NORMANDIE - L3 ECONOMIE - INFORMATIQUE APPLIQUEE (R)
# PROJET GROUPE 4 : LES CREATIONS D'ENTREPRISES EN FRANCE (2012-2023)
# Auteurs : Diane TEIMAN-NAIM et Kossi Jeannot WODAKO
# Les questions sont reperees par leur numero (1a, 1b, 2a, 2b, 3a ...).
# ======================================================================


# ---- 0. PACKAGES ET DOSSIER DE TRAVAIL --------------------------------
# A faire une seule fois (decommenter) :
# install.packages(c("dplyr", "ggplot2", "scales"))
library(dplyr)
library(ggplot2)
library(scales)

# Le dossier de travail doit contenir le dossier "Datain/" avec les 2 CSV.
# Adapter le chemin a son ordinateur si besoin :
# setwd("chemin/vers/le/projet")
getwd()
dir()

dir.create("Figures",   showWarnings = FALSE)   # graphiques exportes
dir.create("Resultats", showWarnings = FALSE)   # tableaux exportes


# ---- 1. IMPORTATION DES BASES ------------------------------------------
# Donnees (BASE_CREA_EI) et metadonnees (BASE_CREA_EI_META).
# GEO est lu en texte pour conserver les zeros initiaux (ex. "01").
BASE_CREA_EI <- read.csv("Datain/DS_SIDE_CREA_EI_2024_data.csv",
                         sep = ";", header = TRUE, stringsAsFactors = FALSE,
                         colClasses = c(GEO = "character"))
str(BASE_CREA_EI)            # 10 variables

BASE_CREA_EI_META <- read.csv("Datain/DS_SIDE_CREA_EI_2024_metadata.csv",
                              sep = ";", header = TRUE, stringsAsFactors = FALSE)
str(BASE_CREA_EI_META)       # 4 variables

# Verification des modalites (utile pour adapter les constantes plus bas)
table(BASE_CREA_EI$GEO_OBJECT)   # niveaux geographiques : France, regions, departements
table(BASE_CREA_EI$AGE)
table(BASE_CREA_EI$SEX)
table(BASE_CREA_EI$LEGAL_FORM)


# ---- 2. RECODAGE DES VARIABLES AVEC LES LIBELLES DES METADONNEES -------
meta_activity <- BASE_CREA_EI_META %>%
  filter(COD_VAR == "ACTIVITY") %>%
  select(COD_MOD, LIB_MOD) %>%
  setNames(c("ACTIVITY", "Libelle_Activite"))

meta_legal_form <- BASE_CREA_EI_META %>%
  filter(COD_VAR == "LEGAL_FORM") %>%
  select(COD_MOD, LIB_MOD) %>%
  setNames(c("LEGAL_FORM", "Libelle_Forme_Juridique"))

meta_age <- BASE_CREA_EI_META %>%
  filter(COD_VAR == "AGE") %>%
  select(COD_MOD, LIB_MOD) %>%
  setNames(c("AGE", "Libelle_Age"))

meta_sex <- BASE_CREA_EI_META %>%
  filter(COD_VAR == "SEX") %>%
  select(COD_MOD, LIB_MOD) %>%
  setNames(c("SEX", "Libelle_Sexe"))

# Jointure des libelles a la base de donnees
BASE_CREA_FINALE <- BASE_CREA_EI %>%
  left_join(meta_activity,   by = "ACTIVITY") %>%
  left_join(meta_age,        by = "AGE") %>%
  left_join(meta_legal_form, by = "LEGAL_FORM") %>%
  left_join(meta_sex,        by = "SEX")


# ---- 3. CREATION DES VARIABLES REGION ET REGION_GROUPE ------------------
# ATTENTION : la base contient trois niveaux geographiques (France, regions,
# departements). Les codes des regions (11, 24, 27, 28, 32, 44, 52, 53, 75,
# 76, 84, 93, 94) sont aussi des codes de departements (ex. 11 = Aude).
# Il faut donc toujours distinguer le niveau avec GEO_OBJECT.

NIVEAU_REGION <- "REG"   # valeur de GEO_OBJECT pour les regions (voir table() ci-dessus)
CODE_FRANCE   <- "F"     # code GEO de la France entiere
stopifnot(
  "NIVEAU_REGION absent de GEO_OBJECT : adapter la constante" =
    NIVEAU_REGION %in% BASE_CREA_EI$GEO_OBJECT,
  "CODE_FRANCE absent de GEO : adapter la constante" =
    CODE_FRANCE %in% BASE_CREA_EI$GEO
)

# Codes officiels des regions (INSEE)
codes_regions <- data.frame(
  GEO = c("11", "24", "27", "28", "32", "44", "52", "53", "75", "76",
          "84", "93", "94", "01", "02", "03", "04", "06"),
  REGION = c("Île-de-France", "Centre-Val de Loire", "Bourgogne-Franche-Comté",
             "Normandie", "Hauts-de-France", "Grand Est", "Pays de la Loire",
             "Bretagne", "Nouvelle-Aquitaine", "Occitanie",
             "Auvergne-Rhône-Alpes", "Provence-Alpes-Côte d'Azur", "Corse",
             "Guadeloupe", "Martinique", "Guyane", "La Réunion", "Mayotte"),
  GEO_OBJECT = NIVEAU_REGION,
  stringsAsFactors = FALSE
)

BASE_CREA_FINALE <- BASE_CREA_FINALE %>%
  left_join(codes_regions, by = c("GEO", "GEO_OBJECT")) %>%
  mutate(REGION = if_else(GEO == CODE_FRANCE, "France Entière", REGION)) %>%
  # REGION vaut NA pour les departements : ils sont exclus des analyses regionales
  mutate(
    REGION_GROUPE = case_when(
      REGION == "Île-de-France" ~ "Île-de-France",
      REGION %in% c("Grand Est", "Hauts-de-France", "Bourgogne-Franche-Comté") ~ "Nord-Est",
      REGION %in% c("Bretagne", "Normandie", "Pays de la Loire", "Centre-Val de Loire") ~ "Nord-Ouest",
      REGION %in% c("Auvergne-Rhône-Alpes", "Provence-Alpes-Côte d'Azur", "Corse") ~ "Sud-Est",
      REGION %in% c("Nouvelle-Aquitaine", "Occitanie") ~ "Sud-Ouest",
      REGION == "France Entière" ~ "France Entière",
      !is.na(REGION) ~ "Outre-mer",
      TRUE ~ NA_character_
    )
  )


# ---- 4. SELECTION ET RENOMMAGE DES VARIABLES ---------------------------
# On conserve GEO et GEO_OBJECT pour que le controle des doublons soit fiable.
BASE_CREA_FINALE <- BASE_CREA_FINALE %>%
  select(GEO, GEO_OBJECT, REGION, REGION_GROUPE,
         Libelle_Activite, Libelle_Forme_Juridique, Libelle_Age, Libelle_Sexe,
         TIME_PERIOD, OBS_VALUE) %>%
  rename(
    ANNEE              = TIME_PERIOD,
    SECTEUR_ACTIVITE   = Libelle_Activite,
    FORME_JURIDIQUE    = Libelle_Forme_Juridique,
    AGE_CREATEUR       = Libelle_Age,
    SEXE_CREATEUR      = Libelle_Sexe,
    NOMBRE_ENTREPRISES = OBS_VALUE
  )

# Verification des libelles "Total" (a adapter si besoin)
unique(BASE_CREA_FINALE$SEXE_CREATEUR)
unique(BASE_CREA_FINALE$AGE_CREATEUR)
unique(BASE_CREA_FINALE$FORME_JURIDIQUE)
unique(BASE_CREA_FINALE$SECTEUR_ACTIVITE)
LIB_TOTAL <- "Total"     # libelle de la modalite "Total" (age, sexe, forme juridique)


# ======================================================================
# PARTIE I - DESCRIPTION DE LA BASE
# ======================================================================

# ---- 2b. Selection de la periode 2012-2023 -----------------------------
BASE_CREA_FINALE <- BASE_CREA_FINALE %>%
  mutate(ANNEE = as.numeric(ANNEE)) %>%
  filter(ANNEE >= 2012 & ANNEE <= 2023)

# ---- 1b. Structure de la base -------------------------------------------
str(BASE_CREA_FINALE)
nrow(BASE_CREA_FINALE)        # nombre d'observations (a reporter dans le rapport)
ncol(BASE_CREA_FINALE)        # nombre de variables
View(head(BASE_CREA_FINALE))

# ---- 2a. Qualite des donnees ----------------------------------------------
sum(is.na(BASE_CREA_FINALE$NOMBRE_ENTREPRISES))   # valeurs manquantes de la variable d'interet
colSums(is.na(BASE_CREA_FINALE))                  # NA par variable (REGION est NA pour les departements : normal)
sum(duplicated(BASE_CREA_FINALE))                 # doublons exacts (attendu : 0, car GEO est conserve)


# ======================================================================
# PARTIE II - ANALYSE DESCRIPTIVE
# ======================================================================

# Secteur "Total" : on le repere comme la modalite qui a le plus de creations
# en 2023 au niveau national (toutes caracteristiques confondues).
SECTEUR_TOTAL <- BASE_CREA_FINALE %>%
  filter(ANNEE == 2023, REGION == "France Entière",
         AGE_CREATEUR == LIB_TOTAL, SEXE_CREATEUR == LIB_TOTAL,
         FORME_JURIDIQUE == LIB_TOTAL) %>%
  slice_max(NOMBRE_ENTREPRISES, n = 1, with_ties = FALSE) %>%
  pull(SECTEUR_ACTIVITE)
stopifnot("Libelle du total introuvable : verifier LIB_TOTAL" = length(SECTEUR_TOTAL) == 1)
SECTEUR_TOTAL


# ---- 3a. Evolution nationale 2012-2023 --------------------------------------
# Une seule ligne par annee : tous les "Total" sont fixes, y compris le secteur.
Evolution_nationale <- BASE_CREA_FINALE %>%
  filter(REGION == "France Entière",
         SECTEUR_ACTIVITE == SECTEUR_TOTAL,
         AGE_CREATEUR == LIB_TOTAL,
         SEXE_CREATEUR == LIB_TOTAL,
         FORME_JURIDIQUE == LIB_TOTAL) %>%
  arrange(ANNEE)

nrow(Evolution_nationale)     # attendu : 12 (une ligne par annee)
Evolution_nationale %>% select(ANNEE, NOMBRE_ENTREPRISES)

creations_2023 <- Evolution_nationale %>% filter(ANNEE == 2023) %>% pull(NOMBRE_ENTREPRISES)
creations_2012 <- Evolution_nationale %>% filter(ANNEE == 2012) %>% pull(NOMBRE_ENTREPRISES)
creations_2023
percent(creations_2023 / creations_2012 - 1, accuracy = 0.1)   # croissance 2012-2023

graph_evolution <- ggplot(Evolution_nationale, aes(x = ANNEE, y = NOMBRE_ENTREPRISES)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 3) +
  labs(
    title    = "Évolution annuelle du nombre de créations d'entreprises en France (2012-2023)",
    subtitle = "Entrepreneurs individuels, tous secteurs, âges, sexes et formes juridiques confondus",
    x = "Année", y = "Nombre de créations"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  scale_x_continuous(breaks = seq(2012, 2023, by = 1)) +
  theme_minimal()
graph_evolution
ggsave("Figures/evolution_creations_2012_2023.png", graph_evolution, width = 9, height = 5, dpi = 200)


# ---- 3b. Caracteristiques des createurs en 2023 -------------------------------
caract_2023 <- BASE_CREA_FINALE %>%
  filter(ANNEE == 2023, REGION == "France Entière",
         SECTEUR_ACTIVITE == SECTEUR_TOTAL,
         FORME_JURIDIQUE == LIB_TOTAL)

# Repartition par sexe
repartition_sexe <- caract_2023 %>%
  filter(AGE_CREATEUR == LIB_TOTAL, SEXE_CREATEUR != LIB_TOTAL) %>%
  mutate(Part = NOMBRE_ENTREPRISES / sum(NOMBRE_ENTREPRISES)) %>%
  select(Caractéristique = SEXE_CREATEUR, NOMBRE_ENTREPRISES, Part)

# Repartition par tranche d'age (toutes les tranches, y compris "Moins de 30 ans")
repartition_age <- caract_2023 %>%
  filter(SEXE_CREATEUR == LIB_TOTAL, AGE_CREATEUR != LIB_TOTAL) %>%
  mutate(Part = NOMBRE_ENTREPRISES / sum(NOMBRE_ENTREPRISES)) %>%
  select(Caractéristique = AGE_CREATEUR, NOMBRE_ENTREPRISES, Part) %>%
  arrange(desc(NOMBRE_ENTREPRISES))

tableau_caract_2023 <- bind_rows(repartition_sexe, repartition_age) %>%
  mutate(`Nombre de créations (2023)` = format(NOMBRE_ENTREPRISES, big.mark = " "),
         `Part (%)` = percent(Part, accuracy = 0.1)) %>%
  select(-NOMBRE_ENTREPRISES, -Part)
tableau_caract_2023

# Graphique : repartition par age
graph_age <- ggplot(repartition_age,
                    aes(x = reorder(Caractéristique, NOMBRE_ENTREPRISES), y = NOMBRE_ENTREPRISES)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(title = "Créateurs d'entreprise en 2023 selon la tranche d'âge",
       x = NULL, y = "Nombre de créations") +
  scale_y_continuous(labels = label_number(big.mark = " ")) +
  theme_minimal()
graph_age
ggsave("Figures/repartition_age_2023.png", graph_age, width = 8, height = 4.5, dpi = 200)


# ---- 3c. Analyse regionale -------------------------------------------------------
# 3c1. Classement des regions en 2023 (niveau "regions" uniquement : REGION non NA)
regional_total <- BASE_CREA_FINALE %>%
  filter(ANNEE == 2023,
         !is.na(REGION), REGION != "France Entière",
         SECTEUR_ACTIVITE == SECTEUR_TOTAL,
         AGE_CREATEUR == LIB_TOTAL, SEXE_CREATEUR == LIB_TOTAL,
         FORME_JURIDIQUE == LIB_TOTAL) %>%
  mutate(Part_Nationale = NOMBRE_ENTREPRISES / creations_2023) %>%
  arrange(desc(NOMBRE_ENTREPRISES))

sum(regional_total$NOMBRE_ENTREPRISES)   # a comparer a creations_2023 (controle)

classement_regions <- regional_total %>%
  transmute(REGION,
            `Créations (2023)`    = format(NOMBRE_ENTREPRISES, big.mark = " "),
            `Part Nationale (%)`  = percent(Part_Nationale, accuracy = 0.1))
classement_regions

# 3c2. Part des micro-entrepreneurs dans chaque region en 2023
forme_reg_2023 <- BASE_CREA_FINALE %>%
  filter(ANNEE == 2023,
         !is.na(REGION), REGION != "France Entière",
         SECTEUR_ACTIVITE == SECTEUR_TOTAL,
         AGE_CREATEUR == LIB_TOTAL, SEXE_CREATEUR == LIB_TOTAL)

micro_vs_total <- forme_reg_2023 %>%
  group_by(REGION) %>%
  summarise(
    Total_Regional = sum(NOMBRE_ENTREPRISES[FORME_JURIDIQUE == LIB_TOTAL]),
    Micro          = sum(NOMBRE_ENTREPRISES[grepl("micro", FORME_JURIDIQUE, ignore.case = TRUE)]),
    .groups = "drop"
  ) %>%
  mutate(Part_Micro = Micro / Total_Regional) %>%
  arrange(desc(Part_Micro))
micro_vs_total

# 3c3. Specialisations par secteur d'activite en 2023 (France entiere)
secteurs_2023 <- BASE_CREA_FINALE %>%
  filter(ANNEE == 2023, REGION == "France Entière",
         SECTEUR_ACTIVITE != SECTEUR_TOTAL,
         AGE_CREATEUR == LIB_TOTAL, SEXE_CREATEUR == LIB_TOTAL,
         FORME_JURIDIQUE == LIB_TOTAL) %>%
  mutate(Part = NOMBRE_ENTREPRISES / sum(NOMBRE_ENTREPRISES)) %>%
  arrange(desc(NOMBRE_ENTREPRISES)) %>%
  transmute(Secteur = SECTEUR_ACTIVITE,
            `Créations (2023)`   = format(NOMBRE_ENTREPRISES, big.mark = " "),
            `Part Nationale (%)` = percent(Part, accuracy = 0.1))
secteurs_2023


# ======================================================================
# BONUS - EFFET DU COVID-19 SUR LES CREATIONS D'ENTREPRISES (2019-2023)
# ======================================================================
Covid_periode <- BASE_CREA_FINALE %>%
  filter(ANNEE >= 2019 & ANNEE <= 2023,
         REGION == "France Entière",
         SECTEUR_ACTIVITE == SECTEUR_TOTAL,
         AGE_CREATEUR == LIB_TOTAL, SEXE_CREATEUR == LIB_TOTAL,
         FORME_JURIDIQUE == LIB_TOTAL) %>%
  arrange(ANNEE)
Covid_periode %>% select(ANNEE, NOMBRE_ENTREPRISES)

graph_covid_impact <- ggplot(Covid_periode, aes(x = ANNEE, y = NOMBRE_ENTREPRISES)) +
  geom_line(color = "#E69F00", linewidth = 1.2) +
  geom_point(color = "#E69F00", size = 4) +
  annotate("rect", xmin = 2019.8, xmax = 2020.2, ymin = 0, ymax = Inf, fill = "red", alpha = 0.1) +
  annotate("text", x = 2020, y = max(Covid_periode$NOMBRE_ENTREPRISES) * 0.95,
           label = "Début crise COVID", color = "darkred", size = 4) +
  geom_text(aes(label = format(NOMBRE_ENTREPRISES, big.mark = " ", scientific = FALSE)),
            vjust = -1, size = 3.5) +
  scale_y_continuous(labels = label_number(big.mark = " "),
                     expand = expansion(mult = c(0.02, 0.15))) +
  scale_x_continuous(breaks = seq(2019, 2023, 1)) +
  labs(title = "Impact de la période COVID sur les créations d'entreprises en France",
       subtitle = "Évolution nationale du nombre de créations (2019-2023)",
       x = "Année", y = "Nombre de créations") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
graph_covid_impact
ggsave("Figures/impact_covid_2019_2023.png", graph_covid_impact, width = 9, height = 5, dpi = 200)


# ---- EXPORT DES TABLEAUX (pour reporter les chiffres dans le rapport) -------------
write.csv2(Evolution_nationale %>% select(ANNEE, NOMBRE_ENTREPRISES),
           "Resultats/evolution_nationale.csv", row.names = FALSE)
write.csv2(tableau_caract_2023,  "Resultats/caracteristiques_createurs_2023.csv", row.names = FALSE)
write.csv2(classement_regions,   "Resultats/classement_regions_2023.csv",         row.names = FALSE)
write.csv2(micro_vs_total,       "Resultats/part_micro_par_region_2023.csv",      row.names = FALSE)
write.csv2(secteurs_2023,        "Resultats/secteurs_2023.csv",                   row.names = FALSE)
write.csv2(Covid_periode %>% select(ANNEE, NOMBRE_ENTREPRISES),
           "Resultats/covid_2019_2023.csv", row.names = FALSE)
