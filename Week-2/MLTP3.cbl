      *> ---------------------------------------------------------------
      *> PROGRAMME : MLTP3
      *> OBJET     : Affiche tous les multiples de 3 compris entre 1 et 100.
      *>             Le programme utilise une boucle VARYING pour incrémenter
      *>             un compteur, effectue une division entière par 3,
      *>             et affiche uniquement les valeurs dont le reste est nul.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : Le quotient de la division est stocké dans une variable
      *>             temporaire inutile (WS-TRASH), seule la vérification du
      *>             reste (WS-REMD) est utilisée pour décider de l'affichage.
      *>             Exercice de maîtrise du PERFORM et de DIVIDE ... REMAINDER.
      *> ---------------------------------------------------------------      
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MLTP3.
  
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Variables de travail
      * Compteur principal pour la boucle
       01 WS-CPT           PIC 9(3) VALUE 0.
      * Copie temporaire du compteur pour le calcul
       01 WS-CPT-TEMP      PIC 9(3) VALUE 0.
      * Variable pour stocker le quotient (non utilisé)
       01 WS-TRASH         PIC 9(1) VALUE 0.
      * Variable pour stocker le reste de la division
       01 WS-REMD          PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
      * Point d'entrée du programme
           DISPLAY "MULTIPLES DE 3 ENTRE 1 ET 100 :".

      * Boucle principale - parcourt tous les nombres de 1 à 100
           PERFORM VARYING WS-CPT FROM 1 BY 1 UNTIL WS-CPT > 100
      * Copie du compteur pour préserver sa valeur originale
           MOVE WS-CPT TO WS-CPT-TEMP
      * Division par 3 pour vérifier si le nombre est un multiple de 3
           DIVIDE WS-CPT-TEMP BY 3 GIVING WS-TRASH REMAINDER WS-REMD
      * Si le reste est 0, alors c'est un multiple de 3
               IF WS-REMD = 0
               DISPLAY WS-CPT
               END-IF
           END-PERFORM.

      * Fin du programme
           STOP RUN.
