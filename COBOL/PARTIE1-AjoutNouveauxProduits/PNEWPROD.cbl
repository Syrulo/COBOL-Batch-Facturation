*> ==========================================================
*> PNEWPROD.cbl - Programme principal
*> Traitement des nouveaux produits et insertion en DB2
*> Projet académique COBOL / Mainframe
*> 
*> Objectif :
*> - Lire un fichier CSV contenant les nouveaux produits (DDNEWPRO)
*> - Formater les descriptions via le sous-programme PFORMAT
*> - Convertir les prix selon la devise via le sous-programme PDEVISE
*> - Insérer les données dans la table DB2 API12.PRODUCTS
*> 
*> Sous-programmes appelés :
*> - PFORMAT : formatage des descriptions (majuscules/minuscules)
*> - PDEVISE : conversion des prix selon la devise
*> 
*> Remarques :
*> - Fichier DDDEVISE : mapping des devises pour PDEVISE
*> - Ce code est fourni à titre pédagogique
*> ==========================================================
       ID DIVISION.                                                     
       PROGRAM-ID. PNEWPROD.                                            
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
                                                                        
           SELECT FNEWPRO ASSIGN TO DDNEWPRO                            
                          ORGANIZATION IS SEQUENTIAL                    
                          FILE STATUS IS ERRCODE.                       
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD FNEWPRO                                                       
           RECORDING MODE IS F.                                         
                                                                        
       01 ENRPRO         PIC X(45).                                     
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
                                                                        
           EXEC SQL INCLUDE PRODUCTS END-EXEC.                          
                                                                        
       01 EOF         PIC X VALUE 'N'.                                  
                                                                        
       01 WS-ENRPRO   PIC X(45).                                        
       01 P-NO        PIC X(3).                                         
       01 DESCRIPTION PIC X(30).                                        
       01 PRICE       PIC X(6).                                         
       01 DEVISE      PIC X(2).                                         
                                                                        
       77 ERRCODE     PIC 99.                                           
       77 WS-STATUS   PIC 99.                                           
                                                                        
       01 WS-RESULT   PIC 9(3)V99.                                      
                                                                        
       01 PFORMAT     PIC X(7) VALUE 'PFORMAT'.                         
       01 PDEVISE     PIC X(7) VALUE 'PDEVISE'.                         
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
           PERFORM 1000-DEBUT                                           
           PERFORM 2000-TRAITEMENT UNTIL EOF = 'Y'.                     
           PERFORM 3000-FIN.                                            
           GOBACK.                                                      
                                                                        
       1000-DEBUT.                                                      
*> Ouverture du fichier des nouveaux produits (DDNEWPRO)                                                                        
              OPEN INPUT  FNEWPRO                                       
                                                                        
              READ FNEWPRO INTO WS-ENRPRO                               
                   AT END                                               
                       MOVE 'Y' TO EOF                                  
              END-READ                                                  
              .                                                         
*> Lecture initiale du premier enregistrement pour démarrer la boucle                                                                        
       2000-TRAITEMENT.                                                 
*> Décomposition de l’enregistrement CSV en champs internes                                                                        
              UNSTRING WS-ENRPRO DELIMITED BY ';'                       
                    INTO P-NO DESCRIPTION PRICE DEVISE                  
              END-UNSTRING.                                             
                                                                        
              MOVE P-NO TO P-P-NO                                       
                                                                      
              CALL PFORMAT USING DESCRIPTION                            
                                                                        
              MOVE DESCRIPTION TO P-DESCRIPTION-TEXT                    
              MOVE LENGTH OF P-DESCRIPTION-TEXT TO P-DESCRIPTION-LEN    
*> Conversion du prix selon la devise avec PDEVISE                                                                        
              CALL PDEVISE USING DEVISE PRICE WS-RESULT                 
                                                                        
              MOVE WS-RESULT TO P-PRICE                                 
*> Insertion du produit dans la table DB2 PRODUCTS                                                                       
              EXEC SQL                                                  
                  INSERT INTO API12.PRODUCTS                            
                      (P_NO, DESCRIPTION, PRICE)                        
                  VALUES                                                
                      (:P-P-NO, :P-DESCRIPTION, :P-PRICE)               
              END-EXEC                                                  
*> Vérification du résultat de l’insertion SQL                                                                      
              IF SQLCODE NOT = 0                                        
                DISPLAY 'ERREUR INSERT : ' SQLCODE                      
                DISPLAY 'PRODUIT ' P-NO ' NON INSERE.'                  
              ELSE                                                      
                DISPLAY 'INSERTION REUSSI :' P-NO                       
              END-IF                                                    
                                                                        
              READ FNEWPRO INTO WS-ENRPRO                               
                   AT END                                               
                       MOVE 'Y' TO EOF                                  
              END-READ                                                  
              .                                                         
                                                                        
       3000-FIN.
*> Fermeture du fichier des produits
              CLOSE FNEWPRO                                             
              .                                                         
                                
