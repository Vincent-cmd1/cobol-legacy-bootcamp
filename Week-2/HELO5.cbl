      *> ---------------------------------------------------------------
      *> PROGRAMME : HELO5
      *> OBJET     : Afficher 5 fois le message "HELLO-WORLD!".
      *>             Utilise une boucle PERFORM TIMES.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Exemple simple d’utilisation de la structure
      *>             répétitive PERFORM avec variable de contrôle.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELO5.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> Message à afficher
       01 WS-HELLO          PIC X(12) VALUE "HELLO-WORLD!".  
      *> Nombre de répétitions   
       01 WS-REP            PIC 9(1)  VALUE 5.                  

       PROCEDURE DIVISION.
      *> Répète l'affichage 5 fois
           PERFORM WS-REP TIMES 
      *> Affiche le message                                
               DISPLAY WS-HELLO                               
           END-PERFORM

           STOP RUN.                                      
