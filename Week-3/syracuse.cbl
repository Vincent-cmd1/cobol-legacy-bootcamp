      *> ---------------------------------------------------------------
      *> PROGRAMME : SYRACUSE
      *> OBJET     : Implémentation de la conjecture de Syracuse.
      *>             L'utilisateur saisit un nombre entier positif non nul.
      *>             Le programme applique les règles suivantes :
      *>             - Si le nombre est pair : le diviser par 2
      *>             - Si le nombre est impair : le multiplier par 3 et ajouter 1
      *>             - Répéter jusqu'à atteindre la valeur 1
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  :
      *>             - Utilise une boucle PERFORM UNTIL WS-NUM <= 1
      *>             - Reste modulo 2 obtenu avec DIVIDE ... REMAINDER
      *>             - WS-POUBELLE stocke le quotient inutile
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYRACUSE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Nombre saisi par l'utilisateur
       01  WS-NUM        PIC 9(3).

      * Reste de la division par 2
       01  WS-NUM-R      PIC 9(1).

      * Quotient jeté (non utilisé)
       01  WS-POUBELLE   PIC 9.

       PROCEDURE DIVISION.

      * Demande de saisie utilisateur
           DISPLAY "Saisie du nombre :".
           ACCEPT WS-NUM.

      * Boucle jusqu'à obtention de 1
           PERFORM UNTIL WS-NUM <= 1

      * Récupération du reste pour test de parité
               DIVIDE WS-NUM BY 2
                   GIVING WS-POUBELLE
                   REMAINDER WS-NUM-R

      * Affichage de l'état actuel
               DISPLAY "Valeur actuelle : " WS-NUM

      * Application des règles de Syracuse
               IF WS-NUM-R = 0 THEN
                   COMPUTE WS-NUM = WS-NUM / 2
               ELSE
                   COMPUTE WS-NUM = WS-NUM * 3 + 1
               END-IF

           END-PERFORM.

      * Fin du programme
           DISPLAY "Conjecture terminée : valeur finale = 1".
           STOP RUN.
