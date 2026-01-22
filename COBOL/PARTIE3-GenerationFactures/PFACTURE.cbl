*> ==========================================================
*> PFACTURE.cbl - Génération des factures à partir de FEXTRACT
*> Projet académique COBOL / Mainframe
*> 
*> Objectif :
*> - Lire le fichier FEXTRACT (extrait des commandes)
*> - Générer un fichier plat FFACTURE (80 colonnes)
*> - Calculer sous-total, taxes, commission et total
*> - Appeler PDATESTR pour convertir la date en toutes lettres
*> 
*> Entrées :
*> - FEXTRACT : fichier plat avec les commandes extraites
*> 
*> Sorties :
*> - FFACTURE : fichier plat 80 colonnes pour les factures
*> 
*> Sous-programmes appelés :
*> - PDATESTR : conversion de la date en texte complet
*> 
*> Remarques :
*> - Code fourni à titre pédagogique
*> ==========================================================

       ID DIVISION.                                                     
       PROGRAM-ID. PFACTURE.                                            
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT FEXTRACT ASSIGN TO DEXTRACT                           
                           ORGANIZATION IS SEQUENTIAL                   
                           FILE STATUS IS WS-ERRCODE1.                  
                                                                        
           SELECT FFACTURE ASSIGN TO DFACTURE                           
                           ORGANIZATION IS SEQUENTIAL                   
                           FILE STATUS IS WS-ERRCODE2.                  
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    

*> Fichier des commandes extraites 
       FD FEXTRACT                                                      
           RECORDING MODE IS F.                                         
                                                                        
       01 ENREXTRACT.                                                   
                                                                        
          05 EXT-ONO             PIC 9(3).                              
          05 EXT-SNO             PIC 9(2).                              
          05 EXT-CNO             PIC 9(4).                              
          05 EXT-ODATE           PIC X(10).                             
          05 EXT-PNO             PIC X(3).                              
          05 EXT-QUANTITY        PIC 9(2).                              
          05 EXT-PRICE           PIC 9(3)V99.                           
          05 EXT-DESCRIPTION     PIC X(30).                             
          05 EXT-DESCRIPTION-LEN PIC 9(4).                              
          05 EXT-LNAME           PIC X(20).                             
          05 EXT-LNAME-LEN       PIC 9(4).                              
          05 EXT-FNAME           PIC X(20).                             
          05 EXT-FNAME-LEN       PIC 9(4).                              
          05 EXT-COM             PIC V9(2).                             
          05 EXT-COMPANY         PIC X(30).                             
          05 EXT-COMPANY-LEN     PIC 9(4).                              
          05 EXT-ADDRESS         PIC X(100).                            
          05 EXT-ADDRESS-LEN     PIC 9(4).                              
          05 EXT-CITY            PIC X(20).                             
          05 EXT-CITY-LEN        PIC 9(4).                              
          05 EXT-STATE           PIC X(2).                              
          05 EXT-ZIP             PIC X(5).                              
          05 EXT-DNAME           PIC X(20).                             
          05 EXT-DNAME-LEN       PIC 9(4).                              

*> Fichier de factures final 
       FD FFACTURE                                                      
           RECORDING MODE IS F                                          
           DATA RECORD IS ENRFACTURE.                                   
                                                                        
       01 ENRFACTURE         PIC X(80).                                 
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       77 WS-ERRCODE1        PIC 99 VALUE 0.                            
       77 WS-ERRCODE2        PIC 99 VALUE 0.                            
                                                                        
       01 WS-TOTAL           PIC 9(5)V99 VALUE 0.                       
       77 WS-TOTAL-STR       PIC Z(5),99.                               
       01 WS-LINE-TOTAL      PIC 9(5)V99 VALUE 0.                       
       77 WS-LINE-TOTAL-STR  PIC Z(5),99.                               
       01 WS-SUB-TOTAL       PIC 9(5)V99 VALUE 0.                       
       77 WS-SUB-TOTAL-STR   PIC Z(5),99.                               
       77 WS-COM-AFFI        PIC 9.                                     
       01 WS-COM-TOT         PIC 9(5)V99 VALUE 0.                       
       77 WS-COM-TOT-STR     PIC Z(4)9,99.                              
       01 WS-TVA-SYSIN       PIC X(4).                                  
       01 WS-TVA             PIC 9V99.                                  
       01 WS-TVA-AFFI        PIC Z9.                                    
       01 WS-TVA-TOT         PIC 9(5)V99.                               
       77 WS-TVA-TOT-STR     PIC Z(4)9,99.                              
       77 EXT-PRICE-STR      PIC Z(3),99.                               
                                                                        
       01 WS-ONO             PIC 9(3) VALUE 0.                          
       01 WS-TVA-LINE        PIC X(20).                                 
       01 WS-COM-LINE        PIC X(20).                                 
                                                                        
       01 PDATESTR           PIC X(8) VALUE 'PDATESTR'.                 
       01 PFORMAT            PIC X(7) VALUE 'PFORMAT'.                  
                                                                        
       01 DATEFACT           PIC X(37).                                 
       01 WS-DATE            PIC X(20).                                 
                                                                        
       01 EOF                PIC X  VALUE 'N'.                          
                                                                        
       PROCEDURE DIVISION.

*> ==========================================================
*> Début du programme : ouverture fichiers et lecture TVA
*> ==========================================================
       1000-DEBUT.                                                      
                                                                        
           OPEN INPUT  FEXTRACT                                         
                OUTPUT FFACTURE                                         
                                                                        
           ACCEPT WS-TVA-SYSIN FROM SYSIN

*> ==========================================================
*> Lecture et traitement des commandes
*> ==========================================================                                                                        
           PERFORM UNTIL EOF = 'Y'                                      
               READ FEXTRACT                                            
                  AT END                                                
                     MOVE 'Y' TO EOF                                    
                  NOT AT END
*> Si nouvelle commande, on génère le footer précédent et l’en-tête du nouveau
                     IF EXT-ONO NOT = WS-ONO                            
                        IF WS-ONO NOT = 0
                           PERFORM 3200-CALCUL-TOTAL                    
                           PERFORM 4000-FOOTER                          
                        END-IF
                        PERFORM 2000-HEADER                             
                        MOVE EXT-ONO TO WS-ONO                          
                     END-IF
*> Calcul du total de la ligne et affichage du produit
                     PERFORM 3100-CALCUL-LINE                           
                     PERFORM 3000-PRODUIT                               
               END-READ                                                 
           END-PERFORM

*> Calcul final et footer pour la dernière commande                                                                         
           IF WS-ONO NOT = 0                                            
               PERFORM 3200-CALCUL-TOTAL                                
               PERFORM 4000-FOOTER                                      
           END-IF                                                       
                                                                        
           PERFORM 5000-FIN                                             
           .

*> ==========================================================
*> 2000-HEADER : génération de l’entête de facture
*> ==========================================================                                                                        
       2000-HEADER.                                                     
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           MOVE '****************************************'              
                TO ENRFACTURE(20:40)                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           MOVE EXT-COMPANY(1:EXT-COMPANY-LEN)                          
           TO ENRFACTURE(55:EXT-COMPANY-LEN)                            
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           MOVE EXT-ADDRESS(1:EXT-ADDRESS-LEN)                          
           TO ENRFACTURE(55:EXT-ADDRESS-LEN)                            
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           MOVE EXT-CITY(1:EXT-CITY-LEN)                                
           TO ENRFACTURE(55:EXT-CITY-LEN)                               
                                                                        
           MOVE ',' TO ENRFACTURE(55 + EXT-CITY-LEN:1)                  
                                                                        
           MOVE EXT-ZIP TO ENRFACTURE(55 + EXT-CITY-LEN + 2:5)          
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           MOVE EXT-STATE TO ENRFACTURE(55:2)                           
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
                                                                        
           MOVE 'NEW YORK, ' TO ENRFACTURE(5:10)                        
                                                                        
           CALL PDATESTR USING EXT-ODATE WS-DATE                        
                                                                        
           MOVE WS-DATE TO ENRFACTURE(15:20)                            
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           MOVE 'ORDER NUMBER : ' TO ENRFACTURE(5:15)                   
           MOVE EXT-ONO TO ENRFACTURE(20:3)                             
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO WS-DATE                                       
                                                                        
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           MOVE 'DATE : ' TO ENRFACTURE (5:7)                           
           MOVE EXT-ODATE TO ENRFACTURE(12:10)                          
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           MOVE 'YOUR CONTACT WITHIN THE DEPARTMENT'                    
                TO ENRFACTURE(5:39)                                     
                                                                        
           MOVE EXT-DNAME(1:EXT-DNAME-LEN)                              
                TO ENRFACTURE(40:EXT-DNAME-LEN)                         
                                                                        
           MOVE ':' TO ENRFACTURE(40 + EXT-DNAME-LEN + 1:1)             
                                                                        
           MOVE EXT-LNAME(1:EXT-LNAME-LEN)                              
                TO ENRFACTURE(40 + EXT-DNAME-LEN + 3:EXT-LNAME-LEN)     
                                                                        
           MOVE EXT-FNAME(1:EXT-FNAME-LEN)                              
                TO ENRFACTURE(40 + EXT-DNAME-LEN                        
                + 3 + EXT-LNAME-LEN + 1:EXT-FNAME-LEN)                  
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
                                                                        
           MOVE 'P_NO' TO ENRFACTURE(5:4)                               
                                                                        
           MOVE 'DESCRIPTION' TO ENRFACTURE(11:20)                      
                                                                        
           MOVE 'QUANTITY' TO ENRFACTURE(34:8)                          
                                                                        
           MOVE 'PRICE' TO ENRFACTURE(46:6)                             
                                                                        
           MOVE 'LINE TOTAL' TO ENRFACTURE (56:10)                      
                                                                        
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
           .

*> ==========================================================
*> 3000-PRODUIT : génération d’une ligne produit dans la facture
*> ==========================================================                                                                     
       3000-PRODUIT.                                                    
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
                                                                        
           MOVE EXT-PNO TO ENRFACTURE(5:4)                              
                                                                        
           MOVE EXT-DESCRIPTION TO ENRFACTURE(11:20)                    
                                                                        
           MOVE EXT-QUANTITY TO ENRFACTURE(34:8)

*> Formatage du prix et ligne totale                                                                        
           MOVE EXT-PRICE TO EXT-PRICE-STR                              
                                                                        
           MOVE EXT-PRICE-STR                                           
                TO ENRFACTURE(46:6)                                     
                                                                        
           MOVE WS-LINE-TOTAL TO WS-LINE-TOTAL-STR                      
                                                                        
           MOVE WS-LINE-TOTAL-STR                                       
                TO ENRFACTURE(56:10)                                    
                                                                        
           WRITE ENRFACTURE                                             
           .                                                            
                                                                        
*> ==========================================================
*> 3100-CALCUL-LINE : calcul du total de la ligne et sous-total
*> ==========================================================                                                                        
       3100-CALCUL-LINE.                                                
                                                                        
           COMPUTE WS-LINE-TOTAL = EXT-QUANTITY * EXT-PRICE             
           COMPUTE WS-SUB-TOTAL  = WS-SUB-TOTAL + WS-LINE-TOTAL         
           .

*> ==========================================================
*> 3200-CALCUL-TOTAL : calcul du total final avec taxes et commission
*> ==========================================================                                                                        
       3200-CALCUL-TOTAL.                                               
                                                                        
           COMPUTE WS-TVA = FUNCTION NUMVAL-C(WS-TVA-SYSIN)             
           COMPUTE WS-TVA-AFFI = WS-TVA * 100                           
           COMPUTE WS-TVA-TOT = WS-TVA * WS-SUB-TOTAL                   
           COMPUTE WS-COM-AFFI = EXT-COM * 100                          
           COMPUTE WS-COM-TOT = WS-SUB-TOTAL * EXT-COM                  
           COMPUTE WS-TOTAL = WS-SUB-TOTAL + WS-TVA-TOT                 
           .

*> ==========================================================
*> 4000-FOOTER : génération du pied de facture
*> ==========================================================                                                                        
       4000-FOOTER.                                                     
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE

*> Affichage du sous-total
           MOVE 'SUB TOTAL : ' TO ENRFACTURE(40:12)                     
                                                                        
           MOVE WS-SUB-TOTAL TO WS-SUB-TOTAL-STR                        
                                                                        
           MOVE WS-SUB-TOTAL-STR TO ENRFACTURE(54:9)                    
           MOVE '$' TO ENRFACTURE(63:1)                                 
           WRITE ENRFACTURE                                             
                                                                        
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE
*> Affichage de la TVA
           STRING                                                       
               'SALES TAXES (' DELIMITED BY SIZE                        
               WS-TVA-AFFI     DELIMITED BY SIZE                        
               '%) :'          DELIMITED BY SIZE                        
           INTO                                                         
               WS-TVA-LINE                                              
           END-STRING                                                   
                                                                        
           MOVE WS-TVA-LINE TO ENRFACTURE(32:22)                        
                                                                        
           MOVE WS-TVA-TOT TO WS-TVA-TOT-STR                            
                                                                        
           MOVE WS-TVA-TOT-STR TO ENRFACTURE(54:8)                      
           MOVE '$' TO ENRFACTURE(63:1)                                 
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE
*> Affichage de la commission
           STRING                                                       
               'COMMISSION ('  DELIMITED BY SIZE                        
               WS-COM-AFFI     DELIMITED BY SIZE                        
               '%) :'         DELIMITED BY SIZE                         
           INTO                                                         
               WS-COM-LINE                                              
           END-STRING                                                   
                                                                        
           MOVE WS-COM-LINE TO ENRFACTURE(34:18)                        
                                                                        
           MOVE WS-COM-TOT TO WS-COM-TOT-STR                            
                                                                        
           MOVE WS-COM-TOT-STR TO ENRFACTURE(57:5)                      
           MOVE '$' TO ENRFACTURE(63:1)                                 
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE

*> Affichage du total final
           MOVE 'TOTAL :' TO ENRFACTURE(44:8)                           
                                                                        
           MOVE WS-TOTAL TO WS-TOTAL-STR                                
                                                                        
           MOVE WS-TOTAL-STR TO ENRFACTURE(54:8)                        
           MOVE '$' TO ENRFACTURE(63:1)                                 
           WRITE ENRFACTURE                                             
                                                                        
           MOVE SPACES TO ENRFACTURE                                    
           WRITE ENRFACTURE

*> Réinitialisation des totaux pour la commande suivante                                                                        
           MOVE ZEROS TO WS-TOTAL                                       
           MOVE ZEROS TO WS-SUB-TOTAL                                   
           .                                                            

*> ==========================================================
*> 5000-FIN : fermeture fichiers et fin du programme
*> ==========================================================
       5000-FIN.                                                        
           CLOSE FEXTRACT                                               
                 FFACTURE                                               
           STOP RUN                                                     
           .                                                            
