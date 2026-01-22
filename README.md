# Tarification Automobile - Méthodes d'Apprentissage Statistique
**Projet de Fin d'Études | Master 2 ISIFAR | Université Paris Cité**

Ce dépôt présente une chaîne complète de tarification actuarielle, de la préparation des données à l'estimation de la prime pure technique. Ce travail a été réalisé sur le jeu de données public `freMTPL`, comprenant environ 412 000 contrats.

## Objectif Actuariel
L'enjeu est d'estimer la prime pure $\pi = \mathbb{E}[N] \times \mathbb{E}[Y]$ en modélisant séparément la fréquence et la sévérité des sinistres.

## Méthodologie et Modèles
Le projet compare des approches classiques (GLM) et des méthodes de Machine Learning :
- **Fréquence des sinistres ($N$) :** Modélisation par régression **Binomiale Négative** pour traiter la surdispersion observée par rapport à une loi de Poisson.
- **Sévérité des sinistres ($Y$) :** Modélisation par **loi Gamma** (lien log), structurellement adaptée aux coûts positifs et asymétriques.
- **Machine Learning :** Utilisation de l'algorithme **XGBoost** pour la classification des sinistres.
- **Validation :** Analyse de la calibration globale sur échantillon test.

## Organisation du Projet (RStudio)
Le projet est structuré de manière modulaire pour garantir la reproductibilité et l'automatisation des traitements :
- **`scripts/`** : Découpage par étapes (nettoyage, analyse descriptive, modélisation 1 à 5) pour une lecture fluide du code.
- **`data/`** : Stockage des bases brutes (`.csv`) et des bases nettoyées (`.RData`).
- **`models/`** : Sauvegarde des objets modèles (`.rds`) pour éviter les temps de re-calcul. *Note : Les fichiers de plus de 100 Mo, comme `logit_fit.rds`, sont exclus de ce dépôt GitHub pour des raisons de stockage*.
- **`Rapport_Tarification.Rmd`** : Fichier source permettant de générer automatiquement le rapport final au format PDF.

---
*Livrable principal : [Consulter le Rapport_Tarification.pdf](Rapport_Tarification.pdf)*
