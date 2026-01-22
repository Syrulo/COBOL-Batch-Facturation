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

### Description

Objectif :

- Lire un fichier plat contenant de nouveaux produits
- Formater les descriptions
- Convertir les prix si nécessaire
- Insérer les données dans une table DB2

Programmes COBOL :
- PNEWPROD.cbl  
- PFORMAT.cbl  
- PDEVISE.cbl  

JCL associé :
- JNEWPRO.jcl

---

## PARTIE 2 – Import des ventes

### Description

Objectif :

- Importer les ventes depuis des fichiers plats
- Préparer les données nécessaires à la facturation
- Appliquer des règles de formatage (dates, textes)

Programmes COBOL :
- PORDER.cbl  
- PFDATE.cbl  

JCL associé :
- JIMPORT.jcl

---

## PARTIE 3 – Extraction des commandes et génération des factures

### Étape 3.1 – Extraction des commandes (PEXTRACT)

Objectif :

- Extraire les données depuis les tables DB2 (ORDERS, ITEMS, PRODUCTS, EMPLOYEES, CUSTOMERS, DEPTS)
- Générer un fichier plat intermédiaire destiné à la facturation : PROJET.EXTRACT.DATA
- Consolider les informations clients, produits et employés

Programme COBOL :
- PEXTRACT.cbl

JCL associé :
- JEXTRACT.jcl

---

### Étape 3.2 – Génération des factures (PFACTURE)

Objectif :

- Lire le fichier PROJET.EXTRACT.DATA
- Générer les factures clients au format batch
- Calculer les montants (sous-total, TVA paramétrable via SYSIN, commission, total)
- Mise en page des factures
- Conversion de la date en toutes lettres via un sous-programme

Fichier généré :
- PROJET.FACTURES.DATA

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

## Remarque

Ce projet est fourni à titre pédagogique et illustre des pratiques courantes en environnement Mainframe z/OS.
