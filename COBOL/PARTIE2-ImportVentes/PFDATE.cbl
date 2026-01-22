*> ==========================================================
*> PFDATE.cbl - Sous-programme de formatage des dates
*> Partie 2 : Importation des ventes Europe et Asie
*> 
*> Objectif :
*> - Transformer une date au format JJ/MM/AAAA en MM/JJ/AAAA
*>   (utile pour l’insertion en DB2)
*> 
*> Entrée :
*> - LS-STRING : date au format JJ/MM/AAAA (X(10))
*> Sortie :
*> - LS-STRING : date formatée en MM/JJ/AAAA (X(10))
*> 
*> Remarques :
*> - Appelé depuis PORDER.cbl
*> - Ce code est fourni à titre pédagogique
*> ==========================================================
       ID DIVISION.                                                     
       PROGRAM-ID. PFDATE.                                              
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
       01  JJ             PIC X(2).                                     
       01  MM             PIC X(2).                                     
       01  YYYY           PIC X(4).                                     
                                                                        
       LINKAGE SECTION.                                                 
                                                                        
       01  LS-STRING      PIC X(10).                                    
                                                                        
       PROCEDURE DIVISION USING LS-STRING.                              
                                                                        
           UNSTRING LS-STRING DELIMITED BY '/'                          
             INTO  JJ MM YYYY                                           
           END-UNSTRING                                                 
                                                                        
           STRING MM '/' JJ '/' YYYY DELIMITED BY SIZE                  
             INTO LS-STRING                                             
           END-STRING                                                   
           .                                                            
