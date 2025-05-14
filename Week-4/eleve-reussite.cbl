      ******************************************************************
      * PROGRAMME : ELEVE-REUSSITE
      * AUTEUR    : Vincent-Cmd1
      * DATE      : 15-05-2025
      * OBJET     : Lire un fichier contenant les prénoms et notes
      *             d'élèves, puis enregistrer dans un fichier
      *             de sortie les élèves ayant obtenu une note > 10.
      *
      *             Affichage en console des élèves retenus.
      *
      * FICHIERS  :
      *   - F-ELEVES    : fichier source (entrée)
      *   - F-REUSSITE  : fichier destination (sortie)
      *
      * COMMENTAIRES :
      *   - Nombre max d'élèves : 15
      *   - Pas de contrôle sur doublons ou caractères non numériques
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. eleve-reussite.
       AUTHOR. VIncent-Cmd1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Déclaration du fichier source contenant les noms et les notes
           SELECT F-ELEVES ASSIGN TO "eleves.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

      * Déclaration du fichier de sortie avec les élèves ayant plus de 10
           SELECT F-REUSSITE ASSIGN TO "reussite.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
      * Description du fichier d'entrée
       FD F-ELEVES.
       01 FS-ENR-ELEVES.
           05 FS-PRENOM              PIC X(10).
           05 FS-NOTE                PIC 9(02).

      * Description du fichier de sortie
       FD F-REUSSITE.
       01 FS-ENR-REUSSITE.
           05 FS-PRENOM-REUSSITE     PIC X(10).
           05 FS-NOTE-REUSSITE       PIC 9(02).

       WORKING-STORAGE SECTION.
       
      * Tableau mémoire pour stocker jusqu'à 15 élèves lus
       01 WS-ENR-ELEVES.
           05 WS-ELEVES OCCURS 15 TIMES.
               10 WS-PRENOM          PIC X(10).
               10 WS-NOTE            PIC 9(02).

      * Index pour boucle de traitement
       01 WS-IDX                     PIC 9(03)    VALUE 1.
       01 WS-IDX-FIN                 PIC 9(03)    VALUE 15.

      * Drapeau de fin de lecture du fichier
       01 FLAG-STOP                  PIC X.
           88 QUITTER                            VALUE 'Y'.

       PROCEDURE DIVISION.

      ******************************************************************
      * COMPOSANT PRINCIPAL : enchaînement des traitements
      ******************************************************************
       0000-TRT-PRINCIPAL-DEB.
      * Ouverture du fichier d'entrée
           PERFORM 6010-OPEN-F-ELEVES-DEB
              THRU 6010-OPEN-F-ELEVES-FIN.

      * Lecture fichier + stockage en mémoire
           PERFORM 6110-READ-F-ELEVES-DEB
              THRU 6110-READ-F-ELEVES-FIN.

      * Fermeture du fichier d'entrée
           PERFORM 6210-CLOSE-F-ELEVES-DEB
              THRU 6210-CLOSE-F-ELEVES-FIN.

      * Affichage des élèves retenus
           PERFORM 8000-AFFICHAGE-DEB
              THRU 8000-AFFICHAGE-FIN.
        
      * Ouverture du fichier de sortie
           PERFORM 6020-OPEN-F-REUSSITE-DEB
              THRU 6020-OPEN-F-REUSSITE-FIN.

      * Écriture des élèves retenus
           PERFORM 6320-WRITE-F-REUSSITE-DEB
              THRU 6320-WRITE-F-REUSSITE-FIN.

      * Fermeture du fichier de sortie
           PERFORM 6220-CLOSE-F-REUSSITE-DEB
              THRU 6220-CLOSE-F-REUSSITE-FIN.

       0000-TRT-PRINCIPAL-FIN.
           EXIT.

           STOP RUN.

      ******************************************************************
      * SOUS-PROGRAMMES DE GESTION FICHIER
      ******************************************************************
       6010-OPEN-F-ELEVES-DEB.
           OPEN INPUT F-ELEVES.
       6010-OPEN-F-ELEVES-FIN.
           EXIT.

       6020-OPEN-F-REUSSITE-DEB.
           OPEN OUTPUT F-REUSSITE.
       6020-OPEN-F-REUSSITE-FIN.
           EXIT.

       6210-CLOSE-F-ELEVES-DEB.
           CLOSE F-ELEVES.
       6210-CLOSE-F-ELEVES-FIN.
           EXIT.

       6220-CLOSE-F-REUSSITE-DEB.
           CLOSE F-REUSSITE.
       6220-CLOSE-F-REUSSITE-FIN.
           EXIT.

      ******************************************************************
      * LECTURE DU FICHIER D'ÉLÈVES ET STOCKAGE EN MÉMOIRE
      ******************************************************************
       6110-READ-F-ELEVES-DEB.
           MOVE 'N' TO FLAG-STOP.
           PERFORM UNTIL QUITTER
               READ F-ELEVES
                   AT END 
                       SET QUITTER TO TRUE
                   NOT AT END
                       IF WS-IDX <= WS-IDX-FIN
                           MOVE FS-PRENOM TO WS-PRENOM(WS-IDX)
                           MOVE FS-NOTE   TO WS-NOTE(WS-IDX)              
                           ADD 1 TO WS-IDX
                       ELSE
                           SET QUITTER TO TRUE
                       END-IF
               END-READ
           END-PERFORM.
       6110-READ-F-ELEVES-FIN.
           EXIT.

      ******************************************************************
      * ÉCRITURE DU FICHIER DE SORTIE POUR LES ÉLÈVES RÉUSSIS
      ******************************************************************
       6320-WRITE-F-REUSSITE-DEB.
           PERFORM VARYING WS-IDX FROM 1 BY 1 
               UNTIL WS-IDX > WS-IDX-FIN
               IF WS-NOTE(WS-IDX) > 10
                   MOVE WS-PRENOM(WS-IDX) TO FS-PRENOM-REUSSITE
                   MOVE WS-NOTE(WS-IDX)   TO FS-NOTE-REUSSITE
                   WRITE FS-ENR-REUSSITE
               END-IF
           END-PERFORM.
       6320-WRITE-F-REUSSITE-FIN.
           EXIT.

      ******************************************************************
      * AFFICHAGE À L'ÉCRAN DES ÉLÈVES RÉUSSIS
      ******************************************************************
       8000-AFFICHAGE-DEB.
           DISPLAY "*******************".
           DISPLAY "|   Prénom   |Note|".
           DISPLAY "*******************".
           PERFORM VARYING WS-IDX FROM 1 BY 1 
               UNTIL WS-IDX > WS-IDX-FIN
               IF WS-NOTE(WS-IDX) > 10  
                   DISPLAY "| " WS-PRENOM(WS-IDX) 
                           SPACE WITH NO ADVANCING
                   DISPLAY "|" SPACE WITH NO ADVANCING
                   DISPLAY WS-NOTE(WS-IDX) " |"
                   DISPLAY "*-----------------*"
               END-IF
           END-PERFORM.
       8000-AFFICHAGE-FIN.
           EXIT.
