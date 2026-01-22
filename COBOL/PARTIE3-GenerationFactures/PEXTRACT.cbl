*> ==========================================================
*> PEXTRACT.cbl - Extraction des commandes et génération des fichiers factures
*> Projet académique COBOL / Mainframe
*> 
*> Objectif :
*> - Extraire les commandes depuis les tables DB2 ORDERS, ITEMS, PRODUCTS, EMPLOYEES, CUSTOMERS et DEPTS
*> - Mettre les données dans un fichier plat PROJET.EXTRACT.DATA
*> - Préparer les informations clients, produits et employés pour facturation
*> 
*> Entrées :
*> - Tables DB2 : ORDERS, ITEMS, PRODUCTS, EMPLOYEES, CUSTOMERS, DEPTS
*> 
*> Sorties :
*> - Fichier plat : PROJET.EXTRACT.DATA
*> 
*> Sous-programmes appelés :
*> - Aucun spécifique (SQL FETCH utilisé pour extraire les données)
*> 
*> Remarques :
*> - Les données sont placées dans ENREXTRACT
*> - Un sous-programme gère la date en toutes lettres si nécessaire
*> - code est fourni à titre pédagogique
*> ==========================================================

       ID DIVISION.                                                     
       PROGRAM-ID. PEXTRACT.                                            
                                                                       
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                     
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT FEXTRACT ASSIGN TO DEXTRACT                           
                           ORGANIZATION IS SEQUENTIAL                   
                           FILE STATUS IS WS-STATUS.                    
                                                                     
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                   
       FD FEXTRACT                                                      
           RECORDING MODE F                                             
           DATA RECORD IS ENREXTRACT.                                   
                                                                        
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
                                                                   
       WORKING-STORAGE SECTION.                                         
                                                                        
           EXEC SQL INCLUDE SQLCA END-EXEC.                             
                                                                        
           EXEC SQL INCLUDE ORDERS END-EXEC.                            
                                                                        
           EXEC SQL INCLUDE ITEMS END-EXEC.                             
                                                                        
           EXEC SQL INCLUDE PRODUCTS END-EXEC.                          
                                                                        
           EXEC SQL INCLUDE EMPLOYEE END-EXEC.                          
                                                                        
           EXEC SQL INCLUDE CUSTOMER END-EXEC.                          
                                                                        
           EXEC SQL INCLUDE DEPTS END-EXEC.                             
                                                                    
           EXEC SQL DECLARE C1 CURSOR FOR                               
                SELECT O.O_NO, O.S_NO, O.C_NO, O.O_DATE,                
                       I.P_NO, I.QUANTITY, I.PRICE,                     
                       P.DESCRIPTION,                                   
                       E.LNAME, E.FNAME,E.COM,                          
                       C.COMPANY, C.ADDRESS, C.CITY, C.STATE, C.ZIP,    
                       D.DNAME                                          
                FROM API12.ORDERS O                                     
                JOIN API12.ITEMS I ON O.O_NO = I.O_NO                   
                JOIN API12.PRODUCTS P ON I.P_NO = P.P_NO                
                JOIN API12.EMPLOYEES E ON O.S_NO = E.E_NO               
                JOIN API12.CUSTOMERS C ON O.C_NO = C.C_NO               
                JOIN API12.DEPTS D ON E.DEPT = D.DEPT                   
                ORDER BY O.O_NO                                         
           END-EXEC.                                                    
                                                                        
       77 WS-STATUS  PIC 99 VALUE 0.                                    
                                                                    
       PROCEDURE DIVISION. 

*> Ouverture du fichier de sortie                                                                     
       1000-DEBUT.                                                      
                                                                        
           EXEC SQL SET CURRENT SQLID='API12' END-EXEC                  
                                                                        
           OPEN OUTPUT FEXTRACT                                         
           .

*> Ouverture du curseur DB2                                                                      
       2000-OPEN-C1.                                                    
                                                                        
           EXEC SQL OPEN C1 END-EXEC                                    
           .  

*> Boucle de lecture des commandes et écriture dans le fichier plat                                                                 
       3000-FETCH-LOOP.                                                 
                                                                        
           PERFORM UNTIL SQLCODE = 100                                  
                                                                        
             EXEC SQL FETCH C1                                          
                 INTO :O-O-NO, :O-S-NO, :O-C-NO, :O-O-DATE,             
                      :I-P-NO, :I-QUANTITY, :I-PRICE,                   
                      :P-DESCRIPTION,                                   
                      :E-LNAME, :E-FNAME,:E-COM,                        
                      :C-COMPANY, :C-ADDRESS,                           
                      :C-CITY, :C-STATE, :C-ZIP,                        
                      :D-DNAME                                          
             END-EXEC                                                   
                                                                        
             IF SQLCODE = 0                                             
                                                                        
                 MOVE SPACES TO ENREXTRACT 

*> Transfert des champs DB2 vers l'enregistrement plat                                                                       
                 MOVE O-O-NO TO EXT-ONO                                 
                 MOVE O-S-NO TO EXT-SNO                                 
                 MOVE O-C-NO TO EXT-CNO                                 
                 MOVE O-O-DATE TO EXT-ODATE                             
                 MOVE I-P-NO TO EXT-PNO                                 
                 MOVE I-QUANTITY TO EXT-QUANTITY                        
                 MOVE I-PRICE TO EXT-PRICE                              
                 MOVE P-DESCRIPTION-TEXT(1:P-DESCRIPTION-LEN)           
                      TO EXT-DESCRIPTION                                
                 MOVE P-DESCRIPTION-LEN TO EXT-DESCRIPTION-LEN          
                 MOVE E-LNAME-TEXT(1:E-LNAME-LEN) TO EXT-LNAME          
                 MOVE E-LNAME-LEN TO EXT-LNAME-LEN                      
                 MOVE E-FNAME-TEXT(1:E-FNAME-LEN) TO EXT-FNAME          
                 MOVE E-FNAME-LEN TO EXT-FNAME-LEN                      
                 MOVE E-COM TO EXT-COM                                  
                 MOVE C-COMPANY-TEXT(1:C-COMPANY-LEN)                   
                      TO EXT-COMPANY                                    
                 MOVE C-COMPANY-LEN TO EXT-COMPANY-LEN                  
                 MOVE C-ADDRESS-TEXT(1:C-ADDRESS-LEN) TO EXT-ADDRESS    
                 MOVE C-ADDRESS-LEN TO EXT-ADDRESS-LEN                  
                 MOVE C-CITY-TEXT(1:C-CITY-LEN) TO EXT-CITY             
                 MOVE C-CITY-LEN TO EXT-CITY-LEN                        
                 MOVE C-STATE TO EXT-STATE                              
                 MOVE C-ZIP TO EXT-ZIP                                  
                 MOVE D-DNAME-TEXT(1:D-DNAME-LEN) TO EXT-DNAME          
                 MOVE D-DNAME-LEN TO EXT-DNAME-LEN                      
                                                                        
                 WRITE ENREXTRACT                                       
                                                                        
             ELSE                                                       
                DISPLAY 'ERROR : ' SQLCODE                              
             END-IF                                                     
                                                                        
           END-PERFORM                                                  
                                                                        
           PERFORM 4000-CLOSE-C1                                        
                                                                        
           PERFORM 5000-PROG-FIN                                        
           .

*> Fermeture du curseur DB2 et du fichier de sortie                                                                    
       4000-CLOSE-C1.                                                   
                                                                        
           EXEC SQL CLOSE C1 END-EXEC                                   
           .                                                            
                                                                     
       5000-PROG-FIN.                                                   
                                                                        
           CLOSE FEXTRACT                                               
                                                                        
           DISPLAY 'FIN'                                                
                                                                        
           STOP RUN                                                     
           .                                                            
