      *> ---------------------------------------------------------------
      *> PROGRAMME : TBL5
      *> OBJET     : Permet la saisie de 5 notes à virgule fixe (XX.XX),
      *>             calcule leur somme, puis leur moyenne,
      *>             et affiche l'ensemble des notes suivies de la moyenne.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : 
      *>             - Utilise un tableau WS-NOTE OCCURS 5 TIMES
      *>               pour stocker les 5 notes.
      *>             - Boucles contrôlées avec un index (WS-INDEX)
      *>               pour la saisie et l'affichage.
      *>             - La somme est accumulée dans WS-SOMME,
      *>               puis la moyenne calculée via COMPUTE.
      *>             - Format numérique : 2 chiffres entiers, 2 décimaux.
      *>             - Exemple basique mais propre d'utilisation de tableaux
      *>               en COBOL avec affichage séquentiel.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. TBL5.
       AUTHOR. Vincent-cmd1.

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


