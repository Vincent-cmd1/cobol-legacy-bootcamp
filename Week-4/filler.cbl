      ******************************************************************
      * PROGRAMME COBOL DE SAISIE DE NOTES AVEC DATES ASSOCIEES        *
      * L'utilisateur entre une série de notes (0-99) accompagnées     *
      * d'une date (JJ/MM/AAAA). Il peut arrêter à tout moment.        *
      * En fin de saisie, les notes et leurs dates sont affichées.     *
      * Auteur : Vincent-Cmd1                                          *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. filler.
       AUTHOR. Vincent-Cmd1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Déclaration d’un tableau de 99 notes, sur 2 chiffres
       01 WS-TB-NOTES.
           05 WS-NOTE OCCURS 99 TIMES.
               10 WS-VALUE PIC 9(02).

      * Déclaration d’un tableau de 99 dates (JJ/MM/AAAA)
       01 WS-TB-DATE.
           05 WS-DATES OCCURS 99 TIMES.
               10 WS-DATE-JJ     PIC X(02) VALUE ZERO.  
               10 FILLER         PIC X(01) VALUE '/'.   
               10 WS-DATE-MM     PIC X(02) VALUE ZERO.  
               10 FILLER         PIC X(01) VALUE '/'.  
               10 WS-DATE-AAAA   PIC X(04) VALUE ZERO. 

      * Index pour le tableau des notes
       01 WS-IDX-NOTE    PIC 9(03).

      * Index pour le tableau des dates
       01 WS-IDX-DATE    PIC 9(03).

      * Compteur du nombre d’éléments saisis
       01 WS-COMPT-TB    PIC 9(03) VALUE ZERO.

      * Contrôle d’arrêt de la saisie
       01 WS-STOP        PIC X.
           88 QUITTER VALUE 'Y' OR 'y'.  

       PROCEDURE DIVISION.

      * Initialisation des index et du drapeau de sortie
           MOVE 0 TO WS-IDX-NOTE
           MOVE 0 TO WS-IDX-DATE
           MOVE 'N' TO WS-STOP

      * Boucle de saisie : répète jusqu'à demande d'arrêt
           PERFORM UNTIL QUITTER

      * Incrément des compteurs et index
               ADD 1 TO WS-IDX-NOTE
               ADD 1 TO WS-IDX-DATE
               ADD 1 TO WS-COMPT-TB   

      * Saisie d'une note
               DISPLAY "Entrez une note : " WITH NO ADVANCING
               ACCEPT WS-NOTE(WS-IDX-NOTE)

      * Saisie du jour
               DISPLAY "Entrez un jour (JJ) : " WITH NO ADVANCING
               ACCEPT WS-DATE-JJ(WS-IDX-DATE)

      * Saisie du mois
               DISPLAY "Entrez un mois (MM) : " WITH NO ADVANCING
               ACCEPT WS-DATE-MM(WS-IDX-DATE)

      * Saisie de l’année
               DISPLAY "Entrez une année (AAAA) : " WITH NO ADVANCING
               ACCEPT WS-DATE-AAAA(WS-IDX-DATE)

      * Demande à l’utilisateur s’il veut arrêter
               DISPLAY "Voulez-vous quitter ? (Y/N) : "WITH NO ADVANCING
               ACCEPT WS-STOP
           END-PERFORM.

      * Affichage des notes et dates saisies
           PERFORM VARYING WS-IDX-NOTE FROM 1 BY 1 
                     UNTIL WS-IDX-NOTE > WS-COMPT-TB

               DISPLAY "Note : " WS-NOTE(WS-IDX-NOTE)
                       " le " WS-DATE-JJ(WS-IDX-NOTE)
                              "/" WS-DATE-MM(WS-IDX-NOTE)
                              "/" WS-DATE-AAAA(WS-IDX-NOTE)

           END-PERFORM.

           STOP RUN.

       