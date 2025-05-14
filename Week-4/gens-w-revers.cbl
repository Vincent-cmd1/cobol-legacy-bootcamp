      *****************************************************************
      * PROGRAMME : gens-w-revers.cbl                                         *
      * AUTEUR    : Vincent-Cmd1                                     *
      * DATE      : 2025-05-14                                       *
      * OBJET     : Lire un fichier texte contenant 10 lignes de     *
      *             noms/prénoms et les stocker en mémoire pour      *
      *             affichage formaté à l'écran, puis écriture dans  *
      *             un fichier de sortie en ordre inversé.           *
      * FORMAT    : Le fichier "gens.txt" est séquentiel, chaque     *
      *             ligne contient un prénom (12 caractères)         *
      *             suivi d’un nom (17 caractères).                  *
      * SORTIE    : Un fichier "gens-write-reverse.txt" contenant    *
      *             les lignes dans l’ordre inverse de la lecture.   *
      * REMARQUE  : Fichier supposé propre (pas de contrôle).        *
      *****************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENS.
       AUTHOR. Vincent.Cmd1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Déclaration du fichier d’entrée (prénoms + noms)
           SELECT FICHIER-GENS ASSIGN TO "gens.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

      * Déclaration du fichier de sortie (ordre inversé)
           SELECT FICHIER-GENS-WRITE ASSIGN TO "gens-write-reverse.txt"
               ORGANIZATION IS LINE SEQUENTIAL.     

       DATA DIVISION.
       FILE SECTION.

      * Description de l’enregistrement d’entrée
       FD FICHIER-GENS.
       01 FS-ENR-GENS.
           05 FS-PRENOM              PIC X(12).
           05 FS-NOM                 PIC X(17).

      * Description de l’enregistrement de sortie
       FD FICHIER-GENS-WRITE.
       01 FS-ENR-GENS-WRITE.
           05 FS-PRENOM-WRITE        PIC X(12).
           05 FS-NOM-WRITE           PIC X(17).   

       WORKING-STORAGE SECTION.

      * Table pour stocker les agents en mémoire
       01 WS-ENR-GENS.
           05 WS-GENS OCCURS 10 TIMES.
               10 WS-PRENOM          PIC X(12).
               10 WS-NOM             PIC X(17).

      * Index de lecture / écriture
       01 WS-IDX-DEB            PIC 9(03)    VALUE 1.
       01 WS-IDX-FIN            PIC 9(03)    VALUE 10.

      * Drapeau de fin de fichier
       01 FLAG-STOP             PIC X.
           88 QUITTER                        VALUE 'Y'. 

       PROCEDURE DIVISION.

      * Ouverture du fichier source
           OPEN INPUT FICHIER-GENS.

      * Lecture séquentielle des 10 lignes dans le tableau
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

      * Affichage de l’en-tête en console
           DISPLAY "***********************************".
           DISPLAY "|    Prénom    |  Nom de l'agent  |".
           DISPLAY "***********************************".

      * Réinitialisation de l’index pour affichage normal
           MOVE 1 TO WS-IDX-DEB.

      * Affichage des données dans l’ordre de lecture
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

      * Ecriture des lignes en ordre inversé (du 10 au 1)
           PERFORM VARYING WS-IDX-DEB FROM WS-IDX-FIN BY -1 
               UNTIL WS-IDX-DEB < 1
               MOVE WS-PRENOM(WS-IDX-DEB) TO FS-PRENOM-WRITE
               MOVE WS-NOM(WS-IDX-DEB) TO FS-NOM-WRITE
               WRITE FS-ENR-GENS-WRITE
           END-PERFORM. 

      * Fermeture du fichier de sortie
           CLOSE FICHIER-GENS-WRITE.

      * Fin de programme
           STOP RUN.
