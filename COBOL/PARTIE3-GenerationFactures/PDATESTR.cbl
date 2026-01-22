*> ==========================================================
*> PDATESTR.cbl - Conversion d'une date YYYY-MM-DD en texte complet
*> Projet académique COBOL / Mainframe
*> 
*> Objectif :
*> - Transformer une date au format YYYY-MM-DD en texte complet (ex : 2026-01-22 → 22 JANVIER 2026)
*> - Utilisé par le programme PEXTRACT pour générer des factures avec la date en toutes lettres
*> 
*> Entrée :
*> - LS-IN-DATE (X(10)) : date au format YYYY-MM-DD
*> 
*> Sortie :
*> - LS-OUT-DATE-TEXT (X(20)) : date en toutes lettres
*> 
*> Remarques :
*> - Code fourni à titre pédagogique
*> ==========================================================
  
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PDATESTR.                                            
       ENVIRONMENT DIVISION.                                            
                                                                        
       DATA DIVISION.                                                   
                                                                        
       WORKING-STORAGE SECTION.                                         
*> Variables pour découper la date et stocker le mois en texte                                                                       
       01 WS-ANNEE        PIC X(4).                                     
       01 WS-MOIS         PIC X(2).                                     
       01 WS-JOUR         PIC X(2).                                     
       01 WS-MOIS-TEXT    PIC X(12).                                    
       01 WS-MOIS-LEN     PIC 9(2).                                     
       01 WS-DATE-TEXT    PIC X(20).                                    
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01 LS-IN-DATE       PIC X(10).                                   
       01 LS-OUT-DATE-TEXT PIC X(20).                                   
                                                                        
       PROCEDURE DIVISION USING LS-IN-DATE LS-OUT-DATE-TEXT.

*> Découpage de la date en année, mois et jour                                                                                                  
           UNSTRING LS-IN-DATE                                          
               DELIMITED BY '-'                                         
               INTO WS-ANNEE, WS-MOIS, WS-JOUR                          
           END-UNSTRING

*> Conversion du mois numérique en mois en toutes lettres                                                                                                        
           EVALUATE WS-MOIS                                             
               WHEN 01 MOVE "JANVIER" TO WS-MOIS-TEXT                   
                       MOVE 7 TO WS-MOIS-LEN                            
               WHEN 02 MOVE "FEVRIER" TO WS-MOIS-TEXT                   
                       MOVE 7 TO WS-MOIS-LEN                            
               WHEN 03 MOVE "MARS" TO WS-MOIS-TEXT                      
                       MOVE 4 TO WS-MOIS-LEN                            
               WHEN 04 MOVE "AVRIL" TO WS-MOIS-TEXT                     
                       MOVE 5 TO WS-MOIS-LEN                            
               WHEN 05 MOVE "MAI" TO WS-MOIS-TEXT                       
                       MOVE 3 TO WS-MOIS-LEN                            
               WHEN 06 MOVE "JUIN" TO WS-MOIS-TEXT                      
                       MOVE 4 TO WS-MOIS-LEN                            
               WHEN 07 MOVE "JUILLET" TO WS-MOIS-TEXT                   
                       MOVE 7 TO WS-MOIS-LEN                            
               WHEN 08 MOVE "AOUT" TO WS-MOIS-TEXT                      
                       MOVE 4 TO WS-MOIS-LEN                            
               WHEN 09 MOVE "SEPTEMBRE" TO WS-MOIS-TEXT                 
                       MOVE 9 TO WS-MOIS-LEN                            
               WHEN 10 MOVE "OCTOBRE" TO WS-MOIS-TEXT                   
                       MOVE 7 TO WS-MOIS-LEN                            
               WHEN 11 MOVE "NOVEMBRE" TO WS-MOIS-TEXT                  
                       MOVE 8 TO WS-MOIS-LEN                            
               WHEN 12 MOVE "DECEMBRE" TO WS-MOIS-TEXT                  
                       MOVE 8 TO WS-MOIS-LEN                            
               WHEN OTHER MOVE "MOIS INCONNU" TO WS-MOIS-TEXT           
                       MOVE 12 TO WS-MOIS-LEN                           
           END-EVALUATE

*> Assemblage de la date en texte complet                                                                                                        
           STRING WS-JOUR DELIMITED BY SIZE                             
               " " DELIMITED BY SIZE                                    
               WS-MOIS-TEXT(1:WS-MOIS-LEN) DELIMITED BY SIZE            
               " " DELIMITED BY SIZE                                    
               WS-ANNEE DELIMITED BY SIZE                               
               INTO LS-OUT-DATE-TEXT                                    
           END-STRING                                                   
           GOBACK.                                                      
