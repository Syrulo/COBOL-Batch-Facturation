//* ==========================================================
//* PARTIE 2 - Importation des ventes Europe et Asie
//* Fichier : PARTIE2-ImportVentes.jcl
//* Objectif : Compiler, lier et exécuter PORDER pour importer
//*            les ventes et mettre à jour les balances clients
//* Entrées : DVENTEEU (ventes Europe), DVENTEAS (ventes Asie)
//* Sorties : Tables DB2 ORDERS, ITEMS, CUSTOMERS mises à jour
//* Remarques : Tri des fichiers par N° de commande, client et employé
//* ==========================================================

//API12P2 JOB (ACCT#),'THOMAS',MSGCLASS=H,REGION=4M,
//    CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//    COND=(4,LT),TIME=(0,5)
//* ==========================================================
//* Définition de la bibliothèque de procédures
//* ==========================================================
//PROCLIB  JCLLIB ORDER=SDJ.FORM.PROCLIB
//* ==========================================================
//* Variables pour le programme
//* ==========================================================
//         SET SYSUID=API12,
//             NOMPGM=PORDER
//* ==========================================================
//* Étape 1 : Précompile / Compilation COBOL principal
//* ==========================================================
//APPROC   EXEC COMPDB2
//STEPDB2.SYSLIB   DD DSN=&SYSUID..COB.CPY,DISP=SHR
//STEPDB2.SYSIN    DD DSN=&SYSUID..PROJET.SRC(&NOMPGM),DISP=SHR
//STEPDB2.DBRMLIB  DD DSN=&SYSUID..COB.DBRM(&NOMPGM),DISP=SHR
//STEPLNK.SYSLMOD  DD DSN=&SYSUID..COB.LOAD(&NOMPGM),DISP=SHR
//* ==========================================================
//* Étape 2 : BIND Plan DB2
//* ==========================================================
//BIND     EXEC PGM=IKJEFT01,COND=(4,LT)
//DBRMLIB  DD   DSN=&SYSUID..COB.DBRM,DISP=SHR
//SYSTSPRT DD   SYSOUT=*,OUTLIM=25000
//SYSTSIN  DD  *
  DSN SYSTEM (DSN1)
  BIND PLAN      (PORDER) -
       QUALIFIER (API12)    -
       ACTION    (REPLACE) -
       MEMBER    (PORDER) -
       VALIDATE  (BIND)    -
       ISOLATION (CS)      -
       ACQUIRE   (USE)     -
       RELEASE   (COMMIT)  -
       EXPLAIN   (NO)
/*
//* ==========================================================
//* Étape 3 : Exécution du programme principal
//* ==========================================================
//STEPRUN  EXEC PGM=IKJEFT01,COND=(4,LT)
//STEPLIB  DD   DSN=&SYSUID..COB.LOAD,DISP=SHR
//SYSOUT   DD   SYSOUT=*,OUTLIM=1000
//SYSTSPRT DD   SYSOUT=*,OUTLIM=2500
//DVENTEAS DD   DSN=API12.PROJET.VENTEAS.DATA,DISP=SHR
//DVENTEEU DD   DSN=API12.PROJET.VENTEEU.DATA,DISP=SHR
//SYSTSIN  DD   *
  DSN SYSTEM (DSN1)
  RUN PROGRAM(PORDER) PLAN (PORDER)
/*
