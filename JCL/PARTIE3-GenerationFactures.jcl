//* ==========================================================
//* PARTIE 3 - Extraction des commandes et génération des factures
//* Fichier : PARTIE3-GenerationFactures.jcl
//* Objectif : Extraire les commandes depuis DB2, générer un fichier
//*            d’extraction PROJET.EXTRACT.DATA et créer les factures
//*            dans PROJET.FACTURES.DATA via PEXTRACT
//* Entrées : Tables DB2 clients, commandes, produits
//* Sorties : Fichier plat PROJET.EXTRACT.DATA, fichier factures
//* Remarques :
//* - TVA configurable via SYSIN
//* - Une commande par page
//* - Sous-programme utilisé pour mettre la date en toutes lettres
//* ==========================================================

//API12P3 JOB (ACCT#),'THOMAS',MSGCLASS=H,REGION=4M,
//    CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//    COND=(4,LT),TIME=(0,5)
//* Définition de la bibliothèque de procédures
//PROCLIB  JCLLIB ORDER=SDJ.FORM.PROCLIB
//* Variables pour le programme
//         SET SYSUID=API12,
//             NOMPGM=PEXTRACT
//* Étape 1 : Précompile / Compilation COBOL principal
//APPROC   EXEC COMPDB2
//STEPDB2.SYSLIB   DD DSN=&SYSUID..COB.CPY,DISP=SHR
//STEPDB2.SYSIN    DD DSN=&SYSUID..PROJET.SRC(&NOMPGM),DISP=SHR
//STEPDB2.DBRMLIB  DD DSN=&SYSUID..COB.DBRM(&NOMPGM),DISP=SHR
//STEPLNK.SYSLMOD  DD DSN=&SYSUID..COB.LOAD(&NOMPGM),DISP=SHR
//* Étape 2 : BIND Plan DB2
//BIND     EXEC PGM=IKJEFT01,COND=(4,LT)
//DBRMLIB  DD   DSN=&SYSUID..COB.DBRM,DISP=SHR
//SYSTSPRT DD   SYSOUT=*,OUTLIM=25000
//* MODIFIER CI-DESSOUS LE PLAN ET LE MEMBRE
//* AINSI QUE LE QUALIFIER SI NECESSAIRE
//SYSTSIN  DD  *
  DSN SYSTEM (DSN1)
  BIND PLAN      (PEXTRACT) -
       QUALIFIER (API12)    -
       ACTION    (REPLACE) -
       MEMBER    (PEXTRACT) -
       VALIDATE  (BIND)    -
       ISOLATION (CS)      -
       ACQUIRE   (USE)     -
       RELEASE   (COMMIT)  -
       EXPLAIN   (NO)
/*
//DELDEX   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE API12.PROJET.EXTRACT.DATA PURGE
  SET MAXCC=0
/*
//* Étape 3 : Exécution du programme principal
//STEPRUN  EXEC PGM=IKJEFT01,COND=(4,LT)
//STEPLIB  DD  DSN=&SYSUID..COB.LOAD,DISP=SHR
//SYSOUT   DD  SYSOUT=*,OUTLIM=1000
//SYSTSPRT DD  SYSOUT=*,OUTLIM=2500
//DEXTRACT DD  DSN=&SYSUID..PROJET.EXTRACT.DATA,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(CYL,(2,1),RLSE),
//             DCB=(RECFM=FB,LRECL=306,BLKSIZE=3060)
//SYSTSIN  DD  *
  DSN SYSTEM (DSN1)
  RUN PROGRAM(PEXTRACT) PLAN (PEXTRACT)
/*
//
