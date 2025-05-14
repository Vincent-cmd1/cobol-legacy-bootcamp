       IDENTIFICATION DIVISION.
       PROGRAM-ID. command-cli.
       AUTHOR. Vincent-Cmd1

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Fichier source contenant les noms clients
           SELECT F-CLIENTS ASSIGN TO "clients.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

      * Fichier de sortie contenant les commandes par client
           SELECT F-COMMANDES ASSIGN TO "commandes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      * Définition du fichier d'entrée des clients
       FD F-CLIENTS.
       01 FS-ENR-CLIENTS.
           05 FS-CLIENTS-ID              PIC X(02).
           05 FS-FILLER                  PIC X(06).
           05 FS-NOM                     PIC X(11).
           05 FS-PRENOM                  PIC X(09).

      * Définition du fichier des commandes
       FD F-COMMANDES.
       01 FS-ENR-COMMANDES.
           05 FS-COMMANDES-CLI-ID        PIC X(02).
           05 FS-COMMANDES-NB            PIC X(04).
           05 FS-COMMANDES-NB            PIC X(05).

       WORKING-STORAGE SECTION.

      * Stockage mémoire des clients (10 max)
       01 WS-ENR-CLIENTS.
           05 WS-CLIENTS OCCURS 10 TIMES.
               10 FS-CLIENTS-ID          PIC X(02).
               10 FS-FILLER              PIC X(06).
               10 FS-NOM                 PIC X(11).
               10 FS-PRENOM              PIC X(09).

      * Stockage mémoire des produits (17 max)
       01 WS-ENR-COMMANDES.
           05 WS-COMMANDES OCCURS 17 TIMES.
               10 WS-COMMANDES-CLI-ID    PIC X(02).
               10 WS-COMMANDES-NB        PIC X(04).
               10 WS-COMMANDES-NB        PIC X(05).

      * Index pour parcours du tableau
       01 WS-IDX                     PIC 9(03) VALUE 1.
       01 WS-IDX-FIN                 PIC 9(03) VALUE 15.

      * Drapeau pour signaler fin de lecture
       01 FLAG-STOP                  PIC X.
           88 QUITTER                            VALUE 'Y'.

       PROCEDURE DIVISION.

      ******************************************************************
      * COMPOSANT PRINCIPAL : enchaînement des traitements
      ******************************************************************
       0000-TRT-PRINCIPAL-DEB.

      * Ouverture du fichier des clients
           PERFORM 6010-OPEN-F-CLIENTS-DEB
              THRU 6010-OPEN-F-CLIENTS-FIN.

      * Ouverture du fichier des commandes
           PERFORM 6020-OPEN-F-COMMANDES-DEB
              THRU 6020-OPEN-F-COMMANDES-FIN.

      * Lecture et stockage des données clients en mémoire
           PERFORM 6110-READ-F-CLIENTS-DEB
              THRU 6110-READ-F-CLIENTS-FIN.

      * Lecture et stockage des données des commandes en mémoire
           PERFORM 6110-READ-F-COMMANDES-DEB
              THRU 6110-READ-F-COMMANDES-FIN.

      * Fermeture du fichier d'entrée
           PERFORM 6210-CLOSE-F-CLIENTS-DEB
              THRU 6210-CLOSE-F-CLIENTS-FIN.

      * Fermeture du fichier de sortie
           PERFORM 6220-CLOSE-F-COMMANDES-DEB
              THRU 6220-CLOSE-F-COMMANDES-FIN.

      * Affichage du résultat à l'écran
           PERFORM 8000-AFFICHAGE-DEB
              THRU 8000-AFFICHAGE-FIN.

       0000-TRT-PRINCIPAL-FIN.
           EXIT.

           STOP RUN.

      ******************************************************************
      * OUVERTURE ET FERMETURE DES FICHIERS
      ******************************************************************
       6010-OPEN-F-CLIENTS-DEB.
           OPEN INPUT F-CLIENTS.
       6010-OPEN-F-CLIENTS-FIN.
           EXIT.

       6020-OPEN-F-COMMANDES-DEB.
           OPEN INPUT F-COMMANDES.
       6020-OPEN-F-COMMANDES-FIN.
           EXIT.

       6210-CLOSE-F-CLIENTS-DEB.
           CLOSE F-CLIENTS.
       6210-CLOSE-F-CLIENTS-FIN.
           EXIT.

       6220-CLOSE-F-COMMANDES-DEB.
           CLOSE F-COMMANDES.
       6220-CLOSE-F-COMMANDES-FIN.
           EXIT. 

      ******************************************************************
      * LECTURE DES FICHIERS ET STOCKAGE EN MÉMOIRE
      ******************************************************************
       6110-READ-F-INVENTAIRE-DEB.
           MOVE 'N' TO FLAG-STOP.
           PERFORM UNTIL QUITTER
               READ F-INVENTAIRE
                   AT END 
                       SET QUITTER TO TRUE
                   NOT AT END
                       IF WS-IDX <= WS-IDX-FIN
                           MOVE FS-PRODUIT TO WS-PRODUIT(WS-IDX)
                           MOVE FS-STOCK   TO WS-STOCK(WS-IDX)              
                           ADD 1 TO WS-IDX
                       ELSE
                           SET QUITTER TO TRUE
                       END-IF
               END-READ
           END-PERFORM.
       6110-READ-F-INVENTAIRE-FIN.
           EXIT.













       