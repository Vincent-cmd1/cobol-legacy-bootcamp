      *****************************************************************
      * PROGRAMME : GENS.CBL                                         *
      * AUTEUR    : Vincent-Cmd1                                     *
      * DATE      : 2025-05-14                                       *
      * OBJET     : Lire un fichier texte contenant 10 lignes de     *
      *             noms/prénoms et les stocker en mémoire pour      *
      *             affichage formaté à l'écran, puis écriture dans  *
      *             un fichier de sortie.                            *
      * FORMAT    : Le fichier "gens.txt" est séquentiel, chaque     *
      *             ligne contient un prénom (12 caractères)         *
      *             suivi d’un nom (17 caractères).                  *
      * REMARQUE  : Traitement linéaire sans contrôle qualité        *
      *             (fichier supposé correct et complet).            *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENS.
       AUTHOR. Vincent.Cmd1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Déclaration du fichier source contenant les noms/prénoms
           SELECT FICHIER-GENS ASSIGN TO "gens.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

      * Déclaration du fichier de sortie texte avec les données copiées
           SELECT FICHIER-GENS-WRITE ASSIGN TO "gens-write-reverse.txt"
               ORGANIZATION IS LINE SEQUENTIAL.     

       DATA DIVISION.
       FILE SECTION.

      * Description du fichier d’entrée
       FD FICHIER-GENS.
       01 FS-ENR-GENS.
           05 FS-PRENOM              PIC X(12).
           05 FS-NOM                 PIC X(17).

      * Description du fichier de sortie
       FD FICHIER-GENS-WRITE.
       01 FS-ENR-GENS-WRITE.
           05 FS-PRENOM-WRITE        PIC X(12).
           05 FS-NOM-WRITE           PIC X(17).   

       WORKING-STORAGE SECTION.

      * Zone de travail contenant les 10 lignes en mémoire
       01 WS-ENR-GENS.
           05 WS-GENS OCCURS 10 TIMES.
               10 WS-PRENOM          PIC X(12).
               10 WS-NOM             PIC X(17).

      * Index utilisé pour lecture et écriture des tableaux
       01 WS-IDX-DEB            PIC 9(03)    VALUE 1.
       01 WS-IDX-FIN            PIC 9(03)    VALUE 10.

      * Drapeau pour sortir de la boucle de lecture
       01 FLAG-STOP             PIC X.
           88 QUITTER                        VALUE 'Y'. 

       PROCEDURE DIVISION.

      * Ouverture du fichier source
           OPEN INPUT FICHIER-GENS.

      * Lecture ligne à ligne du fichier, stockage en mémoire
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

      * Fermeture du fichier après chargement
           CLOSE FICHIER-GENS.

      * Affichage de l’en-tête du tableau (console)
           DISPLAY "***********************************".
           DISPLAY "|    Prénom    |  Nom de l'agent  |".
           DISPLAY "***********************************".

      * Réinitialisation de l’index pour l’affichage
           MOVE 1 TO WS-IDX-DEB.

      * Affichage ligne par ligne formatée à l'écran
           PERFORM VARYING WS-IDX-DEB FROM 1 BY 1 
                 UNTIL WS-IDX-DEB > WS-IDX-FIN
               DISPLAY "| " WS-PRENOM(WS-IDX-DEB) 
                       SPACE WITH NO ADVANCING
               DISPLAY "|" SPACE WITH NO ADVANCING
               DISPLAY WS-NOM(WS-IDX-DEB) "|"
               DISPLAY "*---------------------------------*"
           END-PERFORM.

      * Ouverture du fichier de sortie
           OPEN OUTPUT FICHIER-GENS-WRITE.

      * Copie des données mémorisées vers le fichier de sortie
           PERFORM VARYING WS-IDX-DEB FROM WS-IDX-FIN BY -1 
               UNTIL WS-IDX-DEB < 1
               MOVE WS-PRENOM(WS-IDX-DEB) TO FS-PRENOM-WRITE
               MOVE WS-NOM(WS-IDX-DEB) TO FS-NOM-WRITE
               WRITE FS-ENR-GENS-WRITE
           END-PERFORM. 

      * Fermeture du fichier de sortie
           CLOSE FICHIER-GENS-WRITE.

      * Fin du traitement
           STOP RUN.
