       IDENTIFICATION DIVISION.
       PROGRAM-ID. TBL5.
       AUTHOR. Vincent-cmd1.

      * Programme permettant de saisir 5 notes dans un tableau
      * de calculer leur somme, d’en déduire la moyenne
      * et d'afficher chaque note ainsi que la moyenne obtenue.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Déclaration du tableau pour stocker 5 notes avec 2 chiffres avant
      * la virgule et 2 après.
       01 WS-TABLEAU.
           05 WS-NOTE      PIC 9(2)V99 OCCURS 5 TIMES.

      * Index pour itérations
       01 WS-INDEX         PIC 9(2) VALUE ZERO.

      * Accumulateur pour la somme des notes
       01 WS-SOMME         PIC 9(3)V99 VALUE ZERO.

      * Résultat du calcul de la moyenne
       01 WS-MOYENNE       PIC 9(2)V99.

       PROCEDURE DIVISION.

      * Saisie des 5 notes et calcul de la somme
       PERFORM VARYING WS-INDEX  FROM 1 BY 1 UNTIL WS-INDEX > 5
           DISPLAY "Veuillez entrer une note (5)"
           ACCEPT WS-NOTE (WS-INDEX)
           COMPUTE WS-SOMME = WS-SOMME + WS-NOTE(WS-INDEX)
       END-PERFORM.

      * Affichage des notes saisies
       PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
           DISPLAY "NOTE " WS-INDEX ": " WS-NOTE(WS-INDEX)
       END-PERFORM.

      * Calcul et affichage de la moyenne
       COMPUTE WS-MOYENNE = WS-SOMME / 5.
       DISPLAY "MOYENNE : " WS-MOYENNE.

       STOP RUN.
