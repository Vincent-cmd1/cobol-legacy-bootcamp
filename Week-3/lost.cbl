      *> ---------------------------------------------------------------
      *> PROGRAMME : LOST
      *> OBJET     : Affiche les N premiers termes de la suite de Fibonacci.
      *>             L'utilisateur saisit le nombre de termes souhaité.
      *>             La suite commence par 0 et 1, puis chaque terme est
      *>             la somme des deux précédents.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  :
      *>             - Utilise 77 pour les variables : B (n-2), O (n-1), N (courant)
      *>             - Contrôle que l'utilisateur entre une valeur >= 1
      *>             - Affiche directement les termes à la suite
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Nombre de termes à afficher (saisi par l'utilisateur)
       77 WS-F-NBR-AFFICHAGE     PIC 9(03) VALUE 0.

      * Compteur de boucle
       77 I     PIC 9(03) VALUE 1.

      * Variables de calcul : B = terme n-2, O = terme n-1, N = terme courant
       77 B     PIC 9(05) VALUE 0.
       77 O     PIC 9(05) VALUE 1.
       77 N     PIC 9(05).

       PROCEDURE DIVISION.

      * Demande du nombre de termes à générer
           DISPLAY "Entrez le nombre de termes attendu : ".
           ACCEPT WS-F-NBR-AFFICHAGE

      * Vérifie que la valeur saisie est correcte
           IF WS-F-NBR-AFFICHAGE < 1 THEN
               DISPLAY "Valeur invalide. Le nombre doit être >= 1."

      * Si la valeur est correcte, on génère la suite
           ELSE
               DISPLAY "Suite de Fibonacci :"
      *> Premier terme : 0
               DISPLAY B                                     

               IF WS-F-NBR-AFFICHAGE >= 2
      *> Deuxième terme : 1
                   DISPLAY O                                

                   PERFORM UNTIL I > (WS-F-NBR-AFFICHAGE - 2)
      *> Calcul du terme suivant
                       COMPUTE N = B + O                     
      *> Affichage du terme
                       DISPLAY N                            
      *> Décalage des termes
                       MOVE O TO B                           
                       MOVE N TO O
      *> Incrément du compteur
                       ADD 1 TO I                            
                   END-PERFORM
               END-IF
           END-IF.

           STOP RUN.

