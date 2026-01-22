*> ==========================================================
*> PORDER.cbl - Programme principal Partie 2
*> Importation des ventes Europe et Asie et mise à jour des balances clients
*> Projet académique COBOL / Mainframe
*> 
*> Objectif :
*> - Lire les fichiers de ventes Europe (DVENTEEU) et Asie (DVENTEAS)
*> - Insérer les ventes dans les tables DB2 ORDERS et ITEMS
*> - Mettre à jour le solde (BALANCE) des clients dans la table CUSTOMERS
*> 
*> Entrées :
*> - FVENTEEU / DVENTEEU : fichier des ventes Europe
*> - FVENTEAS / DVENTEAS : fichier des ventes Asie
*> 
*> Sorties :
*> - Tables DB2 ORDERS, ITEMS et CUSTOMERS
*> 
*> Sous-programmes appelés :
*> - PFDATE : formatage des dates
*> 
*> Remarques :
*> - Les fichiers de ventes sont triés par N° de commande, client et employé
*> - Ce code est fourni à titre pédagogique
*> ==========================================================
       ID DIVISION.                                                     
       PROGRAM-ID. PORDER.                                              
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT FVENTEAS ASSIGN TO DVENTEAS                           
                           ORGANIZATION IS SEQUENTIAL                   
                           FILE STATUS IS ERRCODE1.                     
                                                                        
           SELECT FVENTEEU ASSIGN TO DVENTEEU                           
                           ORGANIZATION IS SEQUENTIAL                   
                           FILE STATUS IS ERRCODE2.                     
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD FVENTEAS                                                      
           RECORDING MODE IS F.                                         
                                                                        
       01 ENRVENTEAS.                                                   
           05 AS-NO        PIC 9(3).                                    
           05 AS-DATE      PIC X(10).                                   
           05 AS-S-NO      PIC 9(2).                                    
           05 AS-C-NO      PIC 9(4).                                    
           05 AS-P-NO      PIC 9(3).                                    
           05 AS-PRICE-RAW PIC X(5).                                    
           05 AS-PRICE     REDEFINES AS-PRICE-RAW PIC 9(3)V99.          
           05 AS-QUANTITY  PIC 9(2).                                    
           05 FILLER       PIC X(6).                                    
                                                                        
       FD FVENTEEU                                                      
           RECORDING MODE IS F.                                         
                                                                        
       01 ENRVENTEEU.                                                   
           05 EU-NO        PIC 9(3).                                    
           05 EU-DATE      PIC X(10).                                   
           05 EU-S-NO      PIC 9(2).                                    
           05 EU-C-NO      PIC 9(4).                                    
           05 EU-P-NO      PIC 9(3).                                    
           05 EU-PRICE-RAW PIC X(5).                                    
           05 EU-PRICE     REDEFINES EU-PRICE-RAW PIC 9(3)V99.          
           05 EU-QUANTITY  PIC 9(2).                                    
           05 FILLER       PIC X(6).                                    
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
                                                                        
           EXEC SQL INCLUDE PRODUCTS END-EXEC.                          
                                                                        
           EXEC SQL INCLUDE ORDERS END-EXEC.                            
                                                                        
           EXEC SQL INCLUDE CUSTOMER END-EXEC.                          
                                                                        
           EXEC SQL INCLUDE ITEMS END-EXEC.                             
                                                                        
       01 EOF-EU           PIC X VALUE 'N'.                             
       01 EOF-AS           PIC X VALUE 'N'.                             
                                                                        
       77 ERRCODE1         PIC 99.                                      
       77 ERRCODE2         PIC 99.                                      
                                                                        
       01 WS-TOTAL         PIC 9(3)V99 VALUE 0.                         
       01 WS-PRICE-X       PIC X(5).                                    
       01 WS-PRICE-N       PIC 9(3)V99.                                 
                                                                        
       01 PFDATE           PIC X(6) VALUE 'PFDATE'.                     
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       PROCEDURE DIVISION.
                                                                       
       1000-DEBUT.                                                      
                                                                        
           OPEN INPUT  FVENTEAS                                         
                       FVENTEEU                                         
                                                                        
           PERFORM 2100-LECTURE-F1                                      
           PERFORM 2200-LECTURE-F2

*> Boucle principale : fusion des ventes Europe/Asie selon le N° de commande                                                                      
           PERFORM UNTIL EOF-EU = 'Y' AND EOF-AS = 'Y'                  
                                                                        
               EVALUATE TRUE                                            
                                                                        
                  WHEN EOF-AS = 'N' AND (EOF-EU = 'Y' OR AS-NO < EU-NO) 
                                                                        
                     MOVE AS-NO        TO O-O-NO                        
                     MOVE AS-DATE      TO O-O-DATE                      
                     MOVE AS-S-NO      TO O-S-NO                        
                     MOVE AS-C-NO      TO O-C-NO                        
                                                                        
                     MOVE AS-NO        TO I-O-NO                        
                     MOVE AS-P-NO      TO I-P-NO                        
                     MOVE AS-PRICE-RAW TO WS-PRICE-X                    
                     MOVE AS-PRICE     TO WS-PRICE-N                    
                     MOVE AS-QUANTITY  TO I-QUANTITY                    
                                                                        
                     PERFORM 3000-TRAITEMENT                            
                     PERFORM 2100-LECTURE-F1                            
                                                                        
                  WHEN EOF-EU = 'N' AND (EOF-AS = 'Y' OR EU-NO < AS-NO) 
                                                                        
                     MOVE EU-NO        TO O-O-NO                        
                     MOVE EU-DATE      TO O-O-DATE                      
                     MOVE EU-S-NO      TO O-S-NO                        
                     MOVE EU-C-NO      TO O-C-NO                        
                                                                        
                     MOVE EU-NO        TO I-O-NO                        
                     MOVE EU-P-NO      TO I-P-NO                        
                     MOVE EU-PRICE-RAW TO WS-PRICE-X                    
                     MOVE EU-PRICE     TO WS-PRICE-N                    
                     MOVE EU-QUANTITY  TO I-QUANTITY                    
                                                                        
                     PERFORM 3000-TRAITEMENT                            
                     PERFORM 2200-LECTURE-F2                            
                                                                        
                  WHEN EU-NO = AS-NO                                    
*> Gestion des commandes identiques dans les deux fichiers                                                                   
                     MOVE AS-NO        TO O-O-NO                        
                     MOVE AS-DATE      TO O-O-DATE                      
                     MOVE AS-S-NO      TO O-S-NO                        
                     MOVE AS-C-NO      TO O-C-NO                        
                                                                        
                     MOVE AS-NO        TO I-O-NO                        
                     MOVE AS-P-NO      TO I-P-NO                        
                     MOVE AS-PRICE-RAW TO WS-PRICE-X                    
                     MOVE AS-PRICE     TO WS-PRICE-N                    
                     MOVE AS-QUANTITY  TO I-QUANTITY                    
                                                                        
                     PERFORM 3000-TRAITEMENT                            
                     PERFORM 4000-BALANCE                               
                                                                        
                     MOVE EU-NO        TO O-O-NO                        
                     MOVE EU-DATE      TO O-O-DATE                      
                     MOVE EU-S-NO      TO O-S-NO                        
                     MOVE EU-C-NO      TO O-C-NO                        
                                                                        
                     MOVE EU-NO        TO I-O-NO                        
                     MOVE EU-P-NO      TO I-P-NO                        
                     MOVE EU-PRICE-RAW TO WS-PRICE-X                    
                     MOVE EU-PRICE     TO WS-PRICE-N                    
                     MOVE EU-QUANTITY  TO I-QUANTITY                    
                                                                        
                     PERFORM 3000-TRAITEMENT                            
                                                                        
                     PERFORM 2100-LECTURE-F1                            
                     PERFORM 2200-LECTURE-F2                            
                                                                        
               END-EVALUATE                                             
                                                                        
               PERFORM 4000-BALANCE                                     
                                                                        
           END-PERFORM                                                  
                                                                        
           PERFORM 5000-FIN                                             
           . 

*> Lecture fichiers AS/EU                                                                        
       2100-LECTURE-F1.                                                 
                                                                        
           READ FVENTEAS                                                
              AT END                                                    
                MOVE 'Y' TO EOF-AS                                      
              END-READ                                                  
           .                                                            
                                                                        
       2200-LECTURE-F2.                                                 
                                                                        
           READ FVENTEEU                                                
              AT END                                                    
                 MOVE 'Y' TO EOF-EU                                     
              END-READ                                                  
              .                                                         
*> Insertion d’une commande et de ses items dans DB2                                                                                                                               
       3000-TRAITEMENT.                                                 
                                                                        
           CALL PFDATE USING O-O-DATE                                   
                                                                        
           EXEC SQL                                                     
              INSERT INTO API12.ORDERS                                  
              (O_NO, S_NO, C_NO, O_DATE)                                
              VALUES                                                    
              (:O-O-NO, :O-S-NO, :O-C-NO, :O-O-DATE)                    
           END-EXEC.                                                    
                                                                        
           IF SQLCODE = 0                                               
              DISPLAY 'INSERTION ' O-O-NO ' OK'                         
           ELSE                                                         
              DISPLAY 'ERREUR : ' SQLCODE                               
           END-IF                                                       
                                                                        
           IF WS-PRICE-X = SPACES                                       
              EXEC SQL                                                  
                 SELECT PRICE                                           
                 INTO :I-PRICE                                          
                 FROM API12.PRODUCTS                                    
                 WHERE P_NO = :I-P-NO                                   
              END-EXEC                                                  
                                                                        
              MOVE I-PRICE TO WS-PRICE-N                                
           ELSE                                                         
              MOVE WS-PRICE-N TO I-PRICE                                
           END-IF                                                       
                                                                        
           EXEC SQL                                                     
              INSERT INTO API12.ITEMS                                   
              (O_NO, P_NO, QUANTITY, PRICE)                             
              VALUES                                                    
              (:I-O-NO, :I-P-NO, :I-QUANTITY, :I-PRICE)                 
           END-EXEC.                                                    
                                                                        
           IF SQLCODE = 0                                               
              DISPLAY 'INSERTION ' I-O-NO ' OK'                         
           ELSE                                                         
              DISPLAY 'ERREUR : ' SQLCODE                               
           END-IF                                                       
           .                                                            
                                                                        
*> Mise à jour du solde du client                                                                                                                                
       4000-BALANCE.                                                    
                                                                        
           COMPUTE WS-TOTAL = WS-PRICE-N * I-QUANTITY                   
                                                                        
           MOVE WS-TOTAL TO C-BALANCE                                   
                                                                        
           IF SQLCODE = 0                                               
           EXEC SQL                                                     
              UPDATE API12.CUSTOMERS                                    
              SET BALANCE = BALANCE + :C-BALANCE                        
              WHERE C_NO = :O-C-NO                                      
           END-EXEC                                                     
           ELSE                                                         
             DISPLAY 'BALANCE DEJA EFFECTUEE'                           
           END-IF                                                       
                                                                        
           IF SQLCODE = 0                                               
              DISPLAY 'BALANCE ' C-BALANCE ' OK'                        
           ELSE                                                         
              DISPLAY 'ERREUR : ' SQLCODE                               
           END-IF                                                       
           .                                                            
                                                                        
       5000-FIN.                                                        
                                                                        
           CLOSE FVENTEAS                                               
                 FVENTEEU                                               
           STOP RUN                                                     
           .                                                            
