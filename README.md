# Projet Mainframe COBOL / JCL / DB2 - Gestion de commandes et génération de factures

Projet réalisé dans un cadre académique sous IBM z/OS  
Projet de fin de formation, réalisé en binôme pour une soutenance.

---

## Présentation générale

Ce projet met en œuvre une chaîne batch complète sous z/OS, depuis l’alimentation des données jusqu’à la génération de factures clients.  
Le projet couvre l’ensemble du cycle batch : intégration de données, traitement métier, accès DB2, génération de fichiers plats et production de factures.

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

## PARTIE 1 – Ajout de nouveaux produits

Objectif :

- Lire un fichier CSV contenant de nouveaux produits
- Formater les descriptions
- Convertir les prix si nécessaire (via un fichier VSAM de taux de change)
- Insérer les données dans la table DB2 correspondante

Fichier d’entrée :
- DDNEWPRO

Table DB2 utilisée :
- PRODUCTS

Programmes COBOL :
- PNEWPROD.cbl  
- PFORMAT.cbl  
- PDEVISE.cbl  

JCL associé :
- JNEWPRO.jcl

---

## PARTIE 2 – Import des ventes

Objectif :

- Importer les ventes depuis des fichiers plats Europe et Asie
- Préparer les données nécessaires à la facturation
- Appliquer des règles de formatage (dates, textes)
- Insérer/mettre à jour les tables DB2 correspondantes

Fichiers d’entrée :
- DVENTEEU → ventes Europe
- DVENTEAS → ventes Asie

Tables DB2 utilisées :
- ORDERS, ITEMS, CUSTOMERS → mises à jour
- PRODUCTS → consultée pour récupérer le prix

Programmes COBOL :
- PORDER.cbl  
- PFDATE.cbl  

JCL associé :
- JIMPORT.jcl

---

## PARTIE 3 – Extraction des commandes et génération des factures

### Étape 3.1 – Extraction des commandes (PEXTRACT)

Objectif :

- Extraire les commandes depuis les tables DB2
- Consolider les informations clients, produits et employés
- Générer un fichier plat intermédiaire destiné à la facturation

Tables DB2 utilisées :
- ORDERS
- ITEMS
- PRODUCTS
- EMPLOYEES
- CUSTOMERS
- DEPTS

Fichier de sortie :
- PROJET.EXTRACT.DATA

Programme COBOL :
- PEXTRACT.cbl

JCL associé :
- JEXTRACT.jcl

---

### Étape 3.2 – Génération des factures (PFACTURE)

Objectif :

- Lire le fichier extrait des commandes (FEXTRACT)
- Générer les factures clients au format batch (80 colonnes)
- Calculer les montants : sous-total, TVA (paramétrable via SYSIN), commission, total
- Mise en page des factures
- Conversion de la date en toutes lettres via un sous-programme

Fichier d’entrée :
- FEXTRACT

Fichier de sortie :
- FFACTURE

Programmes COBOL :
- PFACTURE.cbl
- PDATESTR.cbl

JCL associé :
- JFACTCMP.jcl
- JFACTRUN.jcl

---

## Travail en binôme

Ce projet a été réalisé en binôme dans un cadre pédagogique.

Travaux réalisés :

- Analyse fonctionnelle et conception batch
- Développement COBOL
- Écriture des JCL de compilation, bind et exécution
- Exploitation DB2
- Tests et validation des traitements
---

## Remarques

- Ce projet est fourni à titre pédagogique et illustre des pratiques courantes en environnement Mainframe z/OS.
- Les fichiers de données (CSV, VSAM, résultats DB2) ne sont pas inclus, mais les programmes et JCL fournis permettent de comprendre et de suivre l'ensemble du traitement batch.
