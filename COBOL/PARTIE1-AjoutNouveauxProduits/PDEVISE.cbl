*> ==========================================================
*> PDEVISE.cbl - Sous-programme de conversion des prix
*>
*> Objectif : convertir un prix selon sa devise en dollars
*>
*> Entrées :
*> - LS-DEVISE : code de la devise (X(2))
*> - LS-PRICE  : prix en devise locale (X(6))
*>
*> Sortie :
*> - LS-RESULT : prix converti en dollars (9(3)V99)
*>
*> Remarques :
*> - Fichier DDDEVISE : mapping des devises
*> - Gestion des devises inconnues : prix conservé sans conversion
*> - Code fourni à titre pédagogique
*> ==========================================================

       ID DIVISION.                                                     
       PROGRAM-ID. PDEVISE.                                             
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
*> Définition du fichier DDDEVISE utilisé pour récupérer le taux de conversion
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
*> Variables d’entrée et sortie                                                                      
       01 LS-DEVISE       PIC X(2).                                     
       01 LS-PRICE        PIC X(6).                                     
       01 LS-RESULT       PIC 9(3)V99.                                  
                                                                        
       PROCEDURE DIVISION USING LS-DEVISE LS-PRICE LS-RESULT.           
*> Début du sous-programme : ouverture du fichier des taux                                                                      
           PERFORM 1000-DEBUT.
*> Traitement principal : lecture du fichier, calcul du prix converti
           PERFORM 2000-TRAITEMENT.
*> Fin du sous-programme : fermeture du fichier
           PERFORM 3000-FIN.                                            
           GOBACK                                                       
           .                                                            
                                                                        
       1000-DEBUT.                                                      
                                                                     
           OPEN INPUT FDEVISE                                           
           .                                                            
                                                                        
       2000-TRAITEMENT.                                                 
*> Déplacement du code devise fourni dans la clé du fichier FDEVISE                                                                      
           MOVE LS-DEVISE TO DEV-DEVI
*> Lecture du fichier FDEVISE pour récupérer le taux correspondant                                                                       
           READ FDEVISE                                                 
*> Si devise inconnue, on garde le prix inchangé                                                                        
               INVALID KEY                                              
                   COMPUTE LS-RESULT ROUNDED =                          
                       FUNCTION NUMVAL-C(LS-PRICE)
*> Sinon, on multiplie par le taux de conversion et on limite à 999.99
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
