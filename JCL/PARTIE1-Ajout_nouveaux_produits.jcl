//* ===========================================================
//* PARTIE 1 - Ajout des nouveaux produits
//* ===========================================================
//* Objectif :
//* Traiter un fichier CSV de produits (PROJET.NEWPRODS.DATA)
//* et insérer les données dans la base de données DB2.
//* 
//* Étapes du job :
//* 1. Précompile DB2 pour le programme principal PNEWPROD
//* 2. Compilation COBOL II du programme principal (compilateur standard z/OS)
//* 3. Linkedit / Load du programme principal
//* 4. Bind plan DB2
//* 5. Exécution du programme principal
//* 
//* Remarques :
//* - Les sous-programmes PFORMAT et PDEVISE sont utilisés par PNEWPROD.
//* - Les fichiers d’entrée et de mapping sont utilisés par le programme principal.
//* - Ce JCL est fourni à titre pédagogique et académique.
//* ===========================================================

//* -----------------------------------------------------------
//* Définition du JOB
//* -----------------------------------------------------------
/API12P1 JOB (ACCT#),'THOMAS',MSGCLASS=H,REGION=4M,
/       CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
 /      COND=(4,LT),TIME=(0,5)

//* -----------------------------------------------------------
//* Définition de la bibliothèque de procédures
//* -----------------------------------------------------------
/PROCLIB JCLLIB ORDER=SDJ.FORM.PROCLIB

//* -----------------------------------------------------------
//* Variables pour le programme
//* -----------------------------------------------------------
/* 
   SET SYSUID=API12
   NOMPGM=PNEWPROD
*/

//* -----------------------------------------------------------
//* Étape : Précompile / Compilation COBOL principal
//* -----------------------------------------------------------
/APPROC EXEC COMPDB2
/STEPDB2.SYSLIB   DD DSN=&SYSUID..COB.CPY,DISP=SHR
/STEPDB2.SYSIN    DD DSN=&SYSUID..PROJET.SRC(&NOMPGM),DISP=SHR
/STEPDB2.DBRMLIB  DD DSN=&SYSUID..COB.DBRM(&NOMPGM),DISP=SHR
/STEPLNK.SYSLMOD  DD DSN=&SYSUID..COB.LOAD(&NOMPGM),DISP=SHR

//* -----------------------------------------------------------
//* Étape : BIND Plan DB2
//* -----------------------------------------------------------
/BIND EXEC PGM=IKJEFT01,COND=(4,LT)
/DBRMLIB DD DSN=&SYSUID..COB.DBRM,DISP=SHR
/SYSTSPRT DD SYSOUT=*,OUTLIM=25000
/SYSTSIN  DD *
  DSN SYSTEM (DSN1)
  BIND PLAN      (PNEWPROD) -
       QUALIFIER (API12)    -
       ACTION    (REPLACE) -
       MEMBER    (PNEWPROD) -
       VALIDATE  (BIND)    -
       ISOLATION (CS)      -
       ACQUIRE   (USE)     -
       RELEASE   (COMMIT)  -
       EXPLAIN   (NO)
/*

//* -----------------------------------------------------------
//* Étape : Exécution du programme principal
//* -----------------------------------------------------------
/STEPRUN EXEC PGM=IKJEFT01,COND=(4,LT)
/STEPLIB  DD DSN=&SYSUID..COB.LOAD,DISP=SHR
/SYSOUT   DD SYSOUT=*,OUTLIM=1000
/SYSTSPRT DD SYSOUT=*,OUTLIM=2500
/SYSTSIN  DD *
  DSN SYSTEM (DSN1)
  RUN PROGRAM(PNEWPROD) PLAN(PNEWPROD)
//DDNEWPRO DD DSN=API12.PROJET.NEWPRODS.DATA,DISP=SHR
//DDDEVISE DD DSN=API12.PROJET.VSAM.DEVISE,DISP=SHR
/*
