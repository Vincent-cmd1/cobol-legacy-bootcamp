      *> ---------------------------------------------------------------
      *> PROGRAMME : ARET10
      *> OBJET     : Demande à l'utilisateur de saisir un nombre
      *>             tant que ce n'est pas 0. Affiche un message
      *>             de moquerie tant que ce n'est pas la "bonne" valeur.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Utilise une boucle PERFORM UNTIL et un message 
      *>             constant en WORKING-STORAGE.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARET10.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> Nombre saisi par l'utilisateur
       01 WS-NUM               PIC 9 VALUE 0.                    
       01 WS-TAUNT             PIC X(40) VALUE 
      *> Message d'erreur moqueur
                               "C EST PAS LE BON NUMERO ! REESSAYEZ !". 

       PROCEDURE DIVISION.

           DISPLAY "VEUILLEZ TROUVER LE NUMERO SECRET :".       
      *> Première saisie
           ACCEPT WS-NUM.                                      
      *> Boucle jusqu'à la valeur 0
           PERFORM UNTIL WS-NUM = 0
      *> Nouvelle saisie                           
               ACCEPT WS-NUM                                    
               IF WS-NUM > 0
      *> Moquerie si pas encore 0
                   DISPLAY WS-TAUNT                             
               END-IF
           END-PERFORM.

           DISPLAY "PAS TROP TOT!".                            

           STOP RUN.                                          
