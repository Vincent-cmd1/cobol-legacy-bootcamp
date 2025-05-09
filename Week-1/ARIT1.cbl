      *> ---------------------------------------------------------------
      *> PROGRAMME : ARIT1
      *> OBJET     : Saisie de deux puis trois chiffres.
      *>             Calcule la somme des deux premiers et la moyenne
      *>             des trois. Affiche les résultats formatés.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Utilise Les pictogrammes d’édition Z(3).
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARIT1.
       AUTHOR. Vincent-cmd1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *> Premier nombre saisi
       01 WS-NUM1            PIC 9(3).                          
      *> Deuxième nombre saisi
       01 WS-NUM2            PIC 9(3).
      *> Troisième nombre saisi
       01 WS-NUM3            PIC 9(3). 

      *> Variable drapeau (non utilisée ici)
       01 WS-FLAG           PIC X.                                
           88 FLAG-ON       VALUE 'Y'.                            
           88 FLAG-OFF      VALUE 'N'.                             
      *> Résultat de la somme des deux premiers
       01 WS-RESULT1         PIC 9(3). 
      *> Résultat de la moyenne des trois                             
       01 WS-RESULT2         PIC 9(3).                              

      *> Résultat 1 formaté, blanc si zéro
       01 WS-RESULT1-ED      PIC Z(3) BLANK WHEN ZEROES.
      *> Résultat 2 formaté, blanc si zéro         
       01 WS-RESULT2-ED      PIC Z(3) BLANK WHEN ZEROES.          

       PROCEDURE DIVISION.

       0000-INITIALIZATION-DEB.  
      *> Demande saisie du premier chiffre                                  
           DISPLAY 'Veuillez saisir le chiffre A entre 1 et 99.'.  
           ACCEPT WS-NUM1.
      *> Demande saisie du second chiffre
           DISPLAY 'Veuillez saisir le chiffre B entre 1 et 99.'.  
           ACCEPT WS-NUM2.
       0000-INITIALIZATION-FIN.
      *> Vérifie validité de la saisie
           IF WS-NUM1 < 1 OR WS-NUM1 > 99 OR                      
              WS-NUM2 < 0 OR WS-NUM2 > 99
      *> Affiche erreur si invalide
               DISPLAY "Erreur dans la saisie du chiffre."
       *> Redemande la saisie         
               PERFORM 0000-INITIALIZATION-DEB                    
           ELSE
       *> Additionne les deux chiffres
               ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT1
      *> Formate le résultat           
               MOVE WS-RESULT1 TO WS-RESULT1-ED
      *> Affiche le résultat (faussement nommé)                   
               DISPLAY 'Le produit de ces deux chiffres est : '  
                       WS-RESULT1-ED
           END-IF.

       0010-MOYENNE-DEB.
      *> Demande troisième chiffre
           DISPLAY "Veuillez saisir en troisième chiffre entre 1 et 99". 
           ACCEPT WS-NUM3.
       0010-MOYENNE-FIN.

           IF WS-NUM3 < 1 OR WS-NUM3 > 99
      *> Vérifie validité du troisième chiffre
               DISPLAY "Erreur dans la saisie du chiffre."
      *> Redemande la saisie        
               PERFORM 0010-MOYENNE-DEB                         
           ELSE
      *> Calcule la moyenne
               COMPUTE WS-RESULT2 =                              
                       (WS-NUM1 + WS-NUM2 + WS-NUM3) / 3
      *> Formate le résultat
               MOVE WS-RESULT2 TO WS-RESULT2-ED 
      *> Affiche la moyenne                 
               DISPLAY 'Voici la moyenne des trois chiffres : '   
                       WS-RESULT2-ED
           END-IF.
      *> Fin du programme
           STOP RUN.                                           
