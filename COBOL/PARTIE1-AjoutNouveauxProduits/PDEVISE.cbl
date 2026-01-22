*> ==========================================================
*> PDEVISE.cbl - Sous-programme de conversion des prix
*> Objectif : convertir un prix selon sa devise en dollars
*> Entrées :
*> - LS-DEVISE : code de la devise (X(2))
*> - LS-PRICE  : prix en devise locale (X(6))
*> Sortie :
*> - LS-RESULT : prix converti en dollars (9(3)V99)
*> Remarques :
*> - Fichier DDDEVISE : mapping des devises
*> - Gestion des devises inconnues : prix conservé sans conversion
*> ==========================================================
       ID DIVISION.                                                     
       PROGRAM-ID. PDEVISE.                                             
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT FDEVISE ASSIGN TO DDDEVISE                            
                          ORGANIZATION IS INDEXED                       
                          ACCESS MODE IS RANDOM                         
                          RECORD KEY IS DEV-DEVI                        
                          FILE STATUS IS WS-STATUS.                     
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD FDEVISE                                                       
           DATA RECORD IS ENRDEVIS.                                     
                                                                        
       01 ENRDEVIS.                                                     
          05 DEV-DEVI     PIC X(2).                                     
          05 DEV-TAUX     PIC 99V999.                                   
          05 FILLER       PIC X(3).                                     
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       77 WS-STATUS       PIC 99 VALUE 0.                               
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01 LS-DEVISE       PIC X(2).                                     
       01 LS-PRICE        PIC X(6).                                     
       01 LS-RESULT       PIC 9(3)V99.                                  
                                                                        
       PROCEDURE DIVISION USING LS-DEVISE LS-PRICE LS-RESULT.           
                                                                        
           PERFORM 1000-DEBUT.                                          
           PERFORM 2000-TRAITEMENT.                                     
           PERFORM 3000-FIN.                                            
           GOBACK                                                       
           .                                                            
                                                                        
       1000-DEBUT.                                                      
                                                                        
           OPEN INPUT FDEVISE                                           
           .                                                            
                                                                        
       2000-TRAITEMENT.                                                 
                                                                        
           MOVE LS-DEVISE TO DEV-DEVI                                   
                                                                        
           READ FDEVISE                                                 
                                                                        
               INVALID KEY                                              
                   COMPUTE LS-RESULT ROUNDED =                          
                       FUNCTION NUMVAL-C(LS-PRICE)                      
               NOT INVALID KEY                                          
                   IF FUNCTION NUMVAL-C(LS-PRICE) * DEV-TAUX > 999.99   
                       MOVE 999.99 TO LS-RESULT                         
                   ELSE                                                 
                       COMPUTE LS-RESULT ROUNDED =                      
                           FUNCTION NUMVAL-C(LS-PRICE) * DEV-TAUX       
                   END-IF                                               
                                                                        
           END-READ                                                     
           .                                                            
                                                                        
       3000-FIN.                                                        
                                                                        
           CLOSE FDEVISE                                                
           .                                                            
