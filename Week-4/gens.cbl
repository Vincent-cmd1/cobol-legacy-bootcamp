      *****************************************************************
      * PROGRAMME : gens.cbl
      * AUTEUR    : Vincent-Cmd1
      * OBJET     : Lire un fichier texte contenant 10 lignes de 
      *             noms/prénoms et les stocker en mémoire pour 
      *             affichage formaté à l'écran.
      * FORMAT    : Le fichier "gens.txt" est de type ligne séquentielle.
      * STRUCTURE : Chaque ligne contient un prénom (12 caractères)
      *             suivi d’un nom (17 caractères).
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. gens.
       AUTHOR. VIncent.Cmd1

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Déclaration du fichier d’entrée texte
               SELECT FICHIER-GENS ASSIGN TO "gens.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD FICHIER-GENS.
       01 FS-ENR-GENS.
      * Enregistrement d’un agent : prénom + nom
           05 FS-PRENOM     PIC X(12).
           05 FS-NOM        PIC X(17).

       WORKING-STORAGE SECTION.

       01 WS-ENR-GENS.
      * Tableau pour stocker les 10 agents en mémoire
           05 WS-GENS OCCURS 10 TIMES.
               10 WS-PRENOM         PIC X(12).
               10 WS-NOM            PIC X(17).

      * Index de début (lecture et affichage)
       01 WS-IDX-DEB            PIC 9(03)    VALUE 1.
      * Index fixe représentant le nombre d’entrées
       01 WS-IDX-FIN            PIC 9(03)    VALUE 10.

      * Drapeau de fin de lecture
       01 FLAG-STOP             PIC X.
           88 QUITTER                        VALUE 'Y'. 

       PROCEDURE DIVISION.

      * Ouverture du fichier texte
           OPEN INPUT FICHIER-GENS.
       
      * Lecture des 10 lignes du fichier jusqu’à la fin
           PERFORM UNTIL QUITTER
               READ FICHIER-GENS
                   AT END 
                       SET QUITTER TO TRUE
                   NOT AT END
                       IF WS-IDX-DEB <= WS-IDX-FIN
                           MOVE FS-PRENOM TO WS-PRENOM(WS-IDX-DEB)
                           MOVE FS-NOM TO WS-NOM(WS-IDX-DEB)              
                           ADD 1 TO WS-IDX-DEB
                       END-IF
               END-READ
           END-PERFORM.

      * Fermeture du fichier après lecture
           CLOSE FICHIER-GENS.

      * Affichage de l’en-tête du tableau
           DISPLAY "|    Prénom    |  Nom de l'agent  |".
           DISPLAY "***********************************".  

      * Affichage des noms et prénoms formatés
           PERFORM VARYING WS-IDX-DEB FROM 1 BY 1 
                 UNTIL WS-IDX-DEB > WS-IDX-FIN
               DISPLAY "| " WS-PRENOM(WS-IDX-DEB) 
                       SPACE WITH NO ADVANCING
               DISPLAY "|" SPACE WITH NO ADVANCING
               DISPLAY WS-NOM(WS-IDX-DEB) "|"
               DISPLAY "*---------------------------------*"
           END-PERFORM.

      * Fin du programme
           STOP RUN.

