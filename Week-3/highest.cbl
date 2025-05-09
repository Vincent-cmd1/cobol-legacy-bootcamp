      *> ---------------------------------------------------------------
      *> PROGRAMME : HIGHEST
      *> OBJET     : Demander à l'utilisateur de saisir 5 nombres,
      *>             puis déterminer et afficher le plus petit parmi eux.
      *> AUTEUR    : Vincent&Terry&Anais
      *> REMARQUE  :
      *>             - La variable `WS-NUM-PETIT` est initialisée à 999
      *>               pour garantir qu'une valeur inférieure pourra
      *>               être détectée à la première comparaison.
      *>             - Boucle `PERFORM 5 TIMES` pour collecter 5 saisies.
      *>             - Utilisation de `IF` pour mise à jour du minimum.
      *>             - Pas de tableau utilisé ici (stockage temporaire unique).
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. HIGHEST.
       AUTHOR. Vincent&Terry&Anais.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Variable de saisie utilisateur
       01 WS-NUM          PIC 9(02) VALUE ZERO.

      * Variable contenant la plus petite valeur rencontrée
       01 WS-NUM-PETIT    PIC 9(03) VALUE 999.

      * Variable prévue pour un affichage (optionnelle ici)
       01 WS-RESULTAT     PIC 9(03).

       PROCEDURE DIVISION.

      * Boucle exécutée 5 fois
           PERFORM 5 TIMES

      * Demande de saisie
               DISPLAY "ENTREZ UN NOMBRE"
               ACCEPT WS-NUM

      * Mise à jour du plus petit nombre si nécessaire
               IF WS-NUM < WS-NUM-PETIT THEN
                   MOVE WS-NUM TO WS-NUM-PETIT
               END-IF

           END-PERFORM.

      * Affichage du plus petit nombre trouvé
           DISPLAY "LE PLUS PETIT NOMBRE EST : " WS-NUM-PETIT

           STOP RUN.
