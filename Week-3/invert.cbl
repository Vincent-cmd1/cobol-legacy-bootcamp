      *> ---------------------------------------------------------------
      *> PROGRAMME : INVERT
      *> OBJET     : Ce programme demande à l'utilisateur de saisir un mot,
      *>             puis l'inverse à l'aide de la fonction REVERSE.
      *>             Il affiche ensuite le mot d'origine et sa version inversée.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  :
      *>             - Utilise FUNCTION REVERSE pour inverser la chaîne.
      *>             - FUNCTION TRIM est utilisée pour supprimer les
      *>               espaces à gauche du mot inversé.
      *>             - La zone WS-MOT est de taille fixe (20 caractères),
      *>               donc les blancs en fin de mot sont à prévoir.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVERT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Zone de saisie du mot par l'utilisateur
       01 WS-MOT               PIC X(20).

      * Zone pour contenir le mot inversé
       01 WS-MOT-INVERSE       PIC X(20).

       PROCEDURE DIVISION.

      * Saisie du mot original
           DISPLAY "Ecrire un mot".
           ACCEPT WS-MOT.

      * Inversion du mot
           MOVE FUNCTION REVERSE(WS-MOT)
             TO WS-MOT-INVERSE.

      * Affichage du mot initial et du résultat
           DISPLAY "Voici votre mot initial : " WS-MOT.
           DISPLAY "Le voici inverse : "
                   FUNCTION TRIM(WS-MOT-INVERSE, LEADING).

           STOP RUN.
