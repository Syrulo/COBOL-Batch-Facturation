//*=========================================================
//* PARTIE2-ImportVentes.jcl - Importation des ventes Europe et Asie
//* Projet académique COBOL / Mainframe
//*
//* Objectif :
//* - Compiler et lier le programme COBOL PORDER
//* - BIND du plan DB2 correspondant
//* - Exécuter PORDER pour importer les ventes et mettre à jour
//*   les balances clients
//*
//* Entrées :
//* - DVENTEEU : fichier des ventes Europe
//* - DVENTEAS : fichier des ventes Asie
//*
//* Sorties :
//* - Tables DB2 ORDERS, ITEMS, CUSTOMERS mises à jour
//*
//* Remarques :
//* - Tri des fichiers : N° de commande, N° client, N° employé
//* - Ce JCL est fourni à titre pédagogique
//*=========================================================
//API12P2 JOB (ACCT#),'THOMAS',MSGCLASS=H,REGION=4M,
//    CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//    COND=(4,LT),TIME=(0,5)
//*
//*------------------------------------------------------*
//* ===> CHANGER XX PAR N DU GROUPE   (XX 01 @ 15)       *
//*      CHANGER     APIXXDB$ PAR LE NOM DU PROGRAMME    *
//*------------------------------------------------------*
//*
//*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
//*   CETTE PROCEDURE CONTIENT 5 STEPS :                             *
//*       ======> SI RE-EXECUTION FAIRE RESTART AU "STEPRUN"         *
//*                                                                  *
//*         1/  PRECOMPILE  DB2                                      *
//*         2/  COMPILE COBOL II                                     *
//*         3/  LINKEDIT  (DANS FORM.CICS.LOAD)                      *
//*         4/  BIND PLAN PARTIR DE API12.SOURCE.DBRMLIB             *
//*         5/  EXECUTE DU PROGRAMME                                 *
//*  LES   PROCEDURES  SE TROUVENT DANS SDJ.FORM.PROCLIB             *
//*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
//PROCLIB  JCLLIB ORDER=SDJ.FORM.PROCLIB
//*
//* MODIFIER LE NOMPGM :
//         SET SYSUID=API12,
//             NOMPGM=PORDER
//*
//APPROC   EXEC COMPDB2
//STEPDB2.SYSLIB   DD DSN=&SYSUID..COB.CPY,DISP=SHR
//STEPDB2.SYSIN    DD DSN=&SYSUID..PROJET.SRC(&NOMPGM),DISP=SHR
//STEPDB2.DBRMLIB  DD DSN=&SYSUID..COB.DBRM(&NOMPGM),DISP=SHR
//STEPLNK.SYSLMOD  DD DSN=&SYSUID..COB.LOAD(&NOMPGM),DISP=SHR
//*
//*--- ETAPE DE BIND --------------------------------------
//*
//BIND     EXEC PGM=IKJEFT01,COND=(4,LT)
//DBRMLIB  DD   DSN=&SYSUID..COB.DBRM,DISP=SHR
//SYSTSPRT DD   SYSOUT=*,OUTLIM=25000
//* MODIFIER CI-DESSOUS LE PLAN ET LE MEMBRE
//* AINSI QUE LE QUALIFIER SI NECESSAIRE
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
//
