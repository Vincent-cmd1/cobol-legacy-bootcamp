      *> ---------------------------------------------------------------
      *> PROGRAMME : TBL10
      *> OBJET     : Demande à l'utilisateur un nombre entre 0 et 100,
      *>             puis affiche sa table de multiplication de 1 à 10.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Utilise une boucle PERFORM VARYING pour parcourir
      *>             les multiplicateurs, et une instruction COMPUTE
      *>             pour calculer chaque produit.
      *> ---------------------------------------------------------------

       IDENTIFICATION DIVISION.
           PROGRAM-ID. TBL10.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Nombre saisi par l'utilisateur
       01 WS-NUM1         PIC 9(3).

      * Résultat de la multiplication
       01 WS-NUM2         PIC 9(5).

      * Multiplicateur de 1 à 10
       01 WS-MULTIPLY     PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.

      * Demande de saisie à l'utilisateur
           DISPLAY "VEUILLEZ ENTRER UN NOMBRE ENTRE 0 ET 100 : ".
           ACCEPT WS-NUM1.

      * Affichage de l'en-tête de la table
           DISPLAY "TABLE DE MULTIPLICATION DE " WS-NUM1 " :".

      * Boucle de 1 à 10 pour afficher chaque ligne de la table
           PERFORM VARYING WS-MULTIPLY FROM 1 BY 1
               UNTIL WS-MULTIPLY > 10

               COMPUTE WS-NUM2 = WS-NUM1 * WS-MULTIPLY      
               DISPLAY WS-NUM1 " x " WS-MULTIPLY " = " WS-NUM2 
           END-PERFORM.

      * Fin du programme
           STOP RUN.

           


