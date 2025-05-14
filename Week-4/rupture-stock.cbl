      ******************************************************************
      * PROGRAMME : RUPTURE-STOCK
      * AUTEUR    : Vincent-Cmd1
      * DATE      : 14-05-2025
      *
      * OBJET     : Ce programme lit un fichier contenant un inventaire
      *             de produits avec leur stock. Il détecte les produits
      *             en rupture (stock à 0) et les écrit dans un fichier
      *             de sortie.
      *
      * FICHIERS  :
      *   - F-INVENTAIRE : fichier source (entrée)
      *   - F-RUPTURE    : fichier destination (sortie)
      *
      * COMMENTAIRES :
      *   - Le programme traite jusqu'à 15 produits maximum.
      *   - L’affichage donne un résumé du nombre de ruptures.
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. rupture-stock.
       AUTHOR. Vincent-Cmd1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Fichier source contenant les noms de produits et leur stock
           SELECT F-INVENTAIRE ASSIGN TO "inventaire.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

      * Fichier de sortie contenant les produits en rupture
           SELECT F-RUPTURE ASSIGN TO "rupture.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      * Définition du fichier d'entrée (structure d’un enregistrement)
       FD F-INVENTAIRE.
       01 FS-ENR-INVENTAIRE.
           05 FS-PRODUIT             PIC X(10).
           05 FS-STOCK               PIC 9(02).

      * Définition du fichier de sortie
       FD F-RUPTURE.
       01 FS-ENR-RUPTURE.
           05 FS-RUPTURE-PRODUIT     PIC X(10).
           05 FS-RUPTURE-STOCK       PIC X(02).

       WORKING-STORAGE SECTION.

      * Stockage mémoire des produits (15 max)
       01 WS-ENR-INVENTAIRE.
           05 WS-INVENTAIRE OCCURS 15 TIMES.
               10 WS-PRODUIT         PIC X(10).
               10 WS-STOCK           PIC X(02).

      * Compteur de produits en rupture
       01 WS-NB-RUPTURE              PIC 9(02) VALUE 0.

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

      * Ouverture du fichier d'entrée
           PERFORM 6010-OPEN-F-INVENTAIRE-DEB
              THRU 6010-OPEN-F-INVENTAIRE-FIN.

      * Lecture et stockage des données en mémoire
           PERFORM 6110-READ-F-INVENTAIRE-DEB
              THRU 6110-READ-F-INVENTAIRE-FIN.

      * Fermeture du fichier d'entrée
           PERFORM 6210-CLOSE-F-INVENTAIRE-DEB
              THRU 6210-CLOSE-F-INVENTAIRE-FIN.

      * Ouverture du fichier de sortie
           PERFORM 6020-OPEN-F-RUPTURE-DEB
              THRU 6020-OPEN-F-RUPTURE-FIN.

      * Écriture des produits en rupture dans le fichier de sortie
           PERFORM 6320-WRITE-F-RUPTURE-DEB
              THRU 6320-WRITE-F-RUPTURE-FIN.

      * Fermeture du fichier de sortie
           PERFORM 6220-CLOSE-F-RUPTURE-DEB
              THRU 6220-CLOSE-F-RUPTURE-FIN.

      * Affichage du résultat à l'écran
           PERFORM 8000-AFFICHAGE-DEB
              THRU 8000-AFFICHAGE-FIN.

       0000-TRT-PRINCIPAL-FIN.
           EXIT.

           STOP RUN.

      ******************************************************************
      * OUVERTURE ET FERMETURE DES FICHIERS
      ******************************************************************
       6010-OPEN-F-INVENTAIRE-DEB.
           OPEN INPUT F-INVENTAIRE.
       6010-OPEN-F-INVENTAIRE-FIN.
           EXIT.

       6020-OPEN-F-RUPTURE-DEB.
           OPEN OUTPUT F-RUPTURE.
       6020-OPEN-F-RUPTURE-FIN.
           EXIT.

       6210-CLOSE-F-INVENTAIRE-DEB.
           CLOSE F-INVENTAIRE.
       6210-CLOSE-F-INVENTAIRE-FIN.
           EXIT.

       6220-CLOSE-F-RUPTURE-DEB.
           CLOSE F-RUPTURE.
       6220-CLOSE-F-RUPTURE-FIN.
           EXIT. 

      ******************************************************************
      * LECTURE DU FICHIER D’INVENTAIRE ET STOCKAGE EN MÉMOIRE
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

      ******************************************************************
      * ÉCRITURE DES PRODUITS EN RUPTURE DANS LE FICHIER DE SORTIE
      ******************************************************************
       6320-WRITE-F-RUPTURE-DEB.
           PERFORM VARYING WS-IDX FROM 1 BY 1 
               UNTIL WS-IDX > WS-IDX-FIN
               IF FUNCTION NUMVAL(WS-STOCK(WS-IDX)) EQUAL 0
                   MOVE WS-PRODUIT(WS-IDX) TO FS-RUPTURE-PRODUIT 
                   MOVE FUNCTION NUMVAL(WS-STOCK(WS-IDX)) 
                                           TO FS-RUPTURE-STOCK
                   WRITE FS-ENR-RUPTURE
                   ADD 1 TO WS-NB-RUPTURE
               END-IF
           END-PERFORM.
       6320-WRITE-F-RUPTURE-FIN.
           EXIT.

      ******************************************************************
      * AFFICHAGE DU RÉSULTAT EN CONSOLE
      ******************************************************************
       8000-AFFICHAGE-DEB.
           DISPLAY "************************************".
           DISPLAY "*        INVENTAIRE GENERAL        *".
           DISPLAY "************************************".
           DISPLAY "* Produits en rupture de stock: "WS-NB-RUPTURE" *".
           DISPLAY "************************************".
       8000-AFFICHAGE-FIN.
           EXIT.

