# Projet Mainframe COBOL / JCL / DB2  
## Gestion de commandes et génération de factures

Projet réalisé dans un cadre académique sous IBM z/OS  
Projet de fin de formation, réalisé en binôme pour une soutenance.

---

## Présentation générale

Ce projet a été réalisé en fin de formation Mainframe.  
Il met en œuvre une chaîne batch complète sous z/OS, depuis
l’alimentation des données jusqu’à la génération de factures clients.

Le projet couvre l’ensemble du cycle batch :
- intégration de données
- traitement métier
- accès DB2
- génération de fichiers plats
- production de factures

---

## Environnement technique

- IBM z/OS
- COBOL batch
- JCL
- DB2
- TSO / ISPF
- SPUFI pour la consultation et la gestion des tables DB2
- File Transfer TSO/ISPF pour l’extraction des fichiers vers Git

---

## Organisation du projet

### Programmes COBOL

- PARTIE1-AjoutNouveauxProduits
- PARTIE2-ImportVentes  
- PARTIE3-GenerationFactures  

Chaque répertoire correspond à une étape fonctionnelle du projet.

### JCL

- PARTIE1-AjoutNouveauxProduits.jcl  
- PARTIE2-ImportVentes.jcl  
- P3GENFAC.jcl  
- JFACTURE-Compilation.jcl  
- JFACTURE-Execution.jcl  

Les JCL assurent la compilation, le bind DB2 et l’exécution des programmes.

---

## PARTIE 1 – Ajout de nouveaux produits

### Description

Cette étape permet d’intégrer de nouveaux produits dans la base DB2
à partir d’un fichier plat.

Les traitements réalisés sont :
- lecture du fichier d’entrée
- formatage des descriptions
- gestion des prix
- insertion des données en base DB2

Cette partie alimente les données produits utilisées dans les étapes suivantes.

---

## PARTIE 2 – Import des ventes

### Description

Cette étape permet l’import des ventes à partir de fichiers plats.

Les traitements réalisés sont :
- lecture des fichiers de ventes
- contrôle et préparation des données
- mise en cohérence avec les tables DB2 existantes
- préparation des informations nécessaires à la facturation

---

## PARTIE 3 – Génération des factures

La troisième partie correspond à la chaîne de facturation.
Elle est composée de plusieurs programmes batch exécutés séquentiellement.

---

### Étape 3.1 – Extraction des commandes (PEXTRACT)

Objectif :
- extraire les données depuis les tables DB2
- consolider les informations clients, produits et employés
- générer un fichier plat intermédiaire destiné à la facturation

Tables DB2 utilisées :
- ORDERS
- ITEMS
- PRODUCTS
- EMPLOYEES
- CUSTOMERS
- DEPTS

Fichier généré :
- PROJET.EXTRACT.DATA

Programme COBOL :
- PEXTRACT.cbl

JCL :
- JEXTRACT.jcl

---

### Étape 3.2 – Génération des factures (PFACTURE)

Objectif :
- lire le fichier PROJET.EXTRACT.DATA
- générer les factures clients au format batch
- effectuer les calculs financiers

Traitements réalisés :
- calcul des montants
- application de la TVA (paramétrable via SYSIN)
- calcul des commissions
- totalisation des montants
- mise en page des factures
- conversion de la date en toutes lettres via un sous-programme

Fichier généré :
- PROJET.FACTURES.DATA

Programmes COBOL :
- PFACTURE.cbl
- PDATESTR.cbl

JCL :
- JFACTCMP.jcl
- JFACTRUN.jcl

---

## Travail en binôme

Ce projet a été réalisé en binôme dans un cadre pédagogique.

Les travaux ont porté sur :
- l’analyse fonctionnelle
- la conception batch
- le développement COBOL
- l’écriture des JCL
- l’exploitation DB2
- les tests et la validation des traitements

---

## Remarque

Ce projet est fourni à titre pédagogique  
Il illustre des pratiques courantes en environnement Mainframe z/OS.
