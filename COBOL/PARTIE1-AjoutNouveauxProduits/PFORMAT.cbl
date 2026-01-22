*> ==========================================================
*> PFORMAT.cbl - Sous-programme de formatage des descriptions
*>
*> Objectif : mettre une majuscule au début de chaque mot
*> et le reste en minuscules
*>
*> Entrée : DESCRIPTION (PIC X(30))
*>
*> Sortie : DESCRIPTION (modifiée)
*>
*> Remarques :
*> - Code fourni à titre pédagogique
*> ==========================================================

       ID DIVISION.                                                     
       PROGRAM-ID. PFORMAT.                                             
                                                                        
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           DECIMAL-POINT IS COMMA.                                      
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       WORKING-STORAGE SECTION.                                         
       01  CPT           PIC 99 VALUE 1.                                
       01  WS-RESULT     PIC X(30).                                     
       01  WS-SPACE      PIC X VALUE 'Y'.                               
                                                                        
       LINKAGE SECTION.                                                 
       01  LS-STRING     PIC X(30).                                     
                                                                        
       PROCEDURE DIVISION USING LS-STRING.                              
*> Boucle caractère par caractère pour mettre la première lettre en majuscule après un espace                                                                        
           PERFORM VARYING CPT FROM 1 BY 1 UNTIL CPT >                  
                   FUNCTION LENGTH(LS-STRING)                           
              IF WS-SPACE = 'Y'                                         
                 MOVE FUNCTION UPPER-CASE(LS-STRING(CPT:1))             
                 TO WS-RESULT(CPT:1)                                    
              ELSE                                                      
                 MOVE FUNCTION LOWER-CASE(LS-STRING(CPT:1))             
                 TO WS-RESULT(CPT:1)                                    
              END-IF                                                    
              IF LS-STRING(CPT:1) = SPACE                               
                 MOVE 'Y' TO WS-SPACE                                   
              ELSE                                                      
                 MOVE 'N' TO WS-SPACE                                   
              END-IF                                                    
           END-PERFORM                                                  
*> Remise du texte formaté dans la variable d’entrée                                                                        
           MOVE WS-RESULT TO LS-STRING                                  
*> Réinitialisation du flag                                                                    
           MOVE 'Y' TO WS-SPACE                                         
                                                                        
           GOBACK.                                                      
