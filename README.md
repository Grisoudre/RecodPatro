# ACADiscrim / RecodPatro

*Accès restreint*

*Recodage de noms et prémons selon leurs origines géographiques*

L'application est faite pour la recherche ACADiscrim. Elle permet d'importer un tableau de données contenant une colonne constituée de noms ou prénoms afin de les recoder en catégories d'origines.
Le tableau initial augmenté des catégories peut être téléchargé de même que le recodage peut se faire en plusieurs fois, via l'option de sauvegarde.
*Note : L'application est faite sur le modèle de l'application RecodPCS.*

## Installation et démarrage

**Installation du package :**
```{r}
if (!require("devtools")) install.packages("devtools", dep=T)
devtools::install_github("Grisoudre/RecodPatro")
```

**Ouverture de l'application :**
```{r}
RecodPatro::Patro()
```
Puis, sur la nouvelle fenêtre, cliquer sur "Open in browser".

## Démonstration

*A FAIRE*


## Guide d'utilisation

**Avant toute chose, cliquer sur "Open in browser"**

**1er onglet - Importation :** 

1. Importation du tableau
2. Choix de la variable à recoder
3. Choix du mode de recodage : 
    + Soit démarrage du recodage
    + Soit reprise d'une table de recodage sauvegardée

**2ème onglet - Recodage :**
- Tableau de recodage
- Champ de recherche d'aire
- **Ou** Cases à cocher (l'information est la même, c'est au choix, pour le moment)
- Sauvegarde de la table de recodage (pour poursuivre ultérieurement)

**3ème onglet - Table finale :**
- Aperçu de la table avec les variables ajoutées
- Téléchargement de la table finale

## Contenu

### Existant

**Fonction :**

Patro()

**Données :**

