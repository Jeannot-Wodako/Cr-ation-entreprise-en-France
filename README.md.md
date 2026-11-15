# Les créations d'entreprises en France (2012-2023) 🇫🇷

Projet d'**Informatique appliquée (R)** réalisé en **Licence 3 Économie**, Université de Caen Normandie (UFR de Sciences Économiques et de Gestion), Groupe 4. Rendu : 1er décembre 2025.

## Objectif

Analyser statistiquement les créations d'entreprises en France entre 2012 et 2023 à partir des données de l'INSEE (Système d'Information sur la Démographie d'Entreprises, SIDE) :

- évolution nationale du nombre de créations ;
- profil des créateurs en 2023 (sexe, âge) ;
- analyse régionale et par forme juridique ;
- bonus : effet de la crise du COVID-19 (2019-2023).

## Contenu du dépôt

```
.
├── README.md
├── script_creations_entreprises.R    # script R : import, nettoyage, analyses, graphiques
└── Rapport_Creation_entreprise.pdf   # rapport complet
```

## Données

Les fichiers de données ne sont pas inclus. Ils proviennent de l'INSEE (jeu « DS_SIDE_CREA_EI_2024 ») :

- `DS_SIDE_CREA_EI_2024_data.csv` : données (10 variables)
- `DS_SIDE_CREA_EI_2024_metadata.csv` : libellés des modalités (4 variables)

Pour exécuter le script, place-les dans un dossier `Datain/` à côté du script.

## Utilisation

Packages R utilisés : `dplyr`, `plyr`, `readxl`, `writexl`, `tidyverse`, `janitor`, `lubridate`, `scales`.

1. Installe les packages.
2. Adapte la ligne `setwd(...)` du script à ton dossier de travail.
3. Exécute le script étape par étape.

## Auteurs

- Diane Teiman-Naim
- Kossi Jeannot Wodako

## Remarque

Projet universitaire à but pédagogique.
