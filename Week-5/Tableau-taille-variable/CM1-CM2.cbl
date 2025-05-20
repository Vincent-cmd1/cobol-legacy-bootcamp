      ******************************************************************
      * PROGRAMME : CM1-CM2                                           *
      * AUTEUR    : Vincent-Cmd1                                      *
      * OBJET     : Lecture d'un fichier d'élèves classés par niveau  *
      *             (CM1 ou CM2), stockage en mémoire dans un tableau *
      *             à 2 dimensions, et recherche d’un élève par nom.  *
      *                                                               *
      * ENTREE    : Fichier texte ligne par ligne (LINE SEQUENTIAL)   *
      *             Format : CLASSE NOM PRENOM (espacés)              *
      *                                                               *
      * SORTIE    : Affichage des élèves correspondant à un nom donné *
      *             sur la console terminal.                          *
      *                                                               *
      * STRUCTURE :                                                   *
      *   - Ouverture fichier                                         *
      *   - Lecture et stockage mémoire                               *
      *   - Fermeture fichier                                         *
      *   - Recherche par nom (saisie utilisateur)                    *
      *                                                               *
      * DATE     : 20/05/2025                                         *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM1-CM2.
       AUTHOR. Vincent-Cmd1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * Déclaration du fichier source
       SELECT F-ELEVES ASSIGN TO "input-classes.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      * Description du fichier d'entrée
       FD F-ELEVES.
       01 FS-ENR-ELEVES.
           05 FS-CLASSE                     PIC X(03).   *> "CM1 - CM2"
           05 FILLER                        PIC X(02).   *> Séparateur
           05 FS-NOM-ELEVE                  PIC X(09).   *> Nom de famille
           05 FS-PRENOM-ELEVE               PIC X(08).   *> Prénom

       WORKING-STORAGE SECTION.
       
      * Tableau des classes, chaque classe contient jusqu’à 12 élèves
       01 TAB-ELEVES.
           05 WS-CLASSE OCCURS 2 TIMES.        
               10 WS-NOM-CLASSE             PIC X(03).   *> "CM1" ou "CM2"
               10 WS-ELEVE OCCURS 12 TIMES.
                   15 WS-NOM-ELEVE          PIC X(09).   *> Nom de famille
                   15 WS-PRENOM-ELEVE       PIC X(08).   *> Prénom 

      * Nom recherché saisi par l'utilisateur
       01 WS-NOM-ELEVE-SEARCH               PIC X(09). 

      * Index de boucle
       01 WS-I-CLASSE                       PIC 9(03)    VALUE 1.
       01 WS-I-ELEVE                        PIC 9(03)    VALUE 1.

      * Drapeau de fin de lecture
       01 FLAG-STOP                         PIC X.
           88 QUITTER                                    VALUE 'Y'.

       PROCEDURE DIVISION.

      ******************************************************************
      * COMPOSANT PRINCIPAL : chaîne de traitement principale
      ******************************************************************         

       0000-TRT-PRINCIPAL-DEB.

      * Ouverture du fichier d'entrée
           PERFORM 6010-OPEN-F-ELEVES-DEB
              THRU 6010-OPEN-F-ELEVES-FIN.

      * Lecture et stockage des données élèves
           PERFORM 6110-READ-F-ELEVES-DEB
              THRU 6110-READ-F-ELEVES-FIN.

      * Fermeture du fichier
           PERFORM 6210-CLOSE-F-ELEVES-DEB
              THRU 6210-CLOSE-F-ELEVES-FIN.

      * Recherche et affichage d'élève par nom
           PERFORM 8000-AFFICHAGE-ELEVES-DEB
              THRU 8000-AFFICHAGE-ELEVES-FIN.       

       0000-TRT-PRINCIPAL-FIN.
           EXIT.
           STOP RUN.

      ******************************************************************
      * OUVERTURE / FERMETURE DU FICHIER
      ******************************************************************
      
       6010-OPEN-F-ELEVES-DEB.
           OPEN INPUT F-ELEVES.
       6010-OPEN-F-ELEVES-FIN.
           EXIT.

       6210-CLOSE-F-ELEVES-DEB.
           CLOSE F-ELEVES.
       6210-CLOSE-F-ELEVES-FIN.
           EXIT.

      ******************************************************************
      * LECTURE DU FICHIER ET STOCKAGE EN MÉMOIRE
      ******************************************************************
       6110-READ-F-ELEVES-DEB.

           MOVE 'N' TO FLAG-STOP.
           MOVE 1 TO WS-I-CLASSE.
           MOVE 1 TO WS-I-ELEVE.

      * Lecture du premier enregistrement
           READ F-ELEVES
               AT END
                   SET QUITTER TO TRUE
               NOT AT END
                   IF FLAG-STOP = 'N'
                       MOVE FS-CLASSE TO WS-NOM-CLASSE(WS-I-CLASSE)
                       MOVE FS-NOM-ELEVE 
                            TO WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                       MOVE FS-PRENOM-ELEVE 
                            TO WS-PRENOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                       ADD 1 TO WS-I-ELEVE
                   END-IF
           END-READ.

      * Traitement des enregistrements suivants jusqu'à la fin du fichier
           PERFORM UNTIL QUITTER
               READ F-ELEVES
                   AT END
                       SET QUITTER TO TRUE
                   NOT AT END
                       IF FS-CLASSE NOT = WS-NOM-CLASSE(WS-I-CLASSE)
                           ADD 1 TO WS-I-CLASSE
                           MOVE 1 TO WS-I-ELEVE
                           MOVE FS-CLASSE TO WS-NOM-CLASSE(WS-I-CLASSE)
                       END-IF

                       MOVE FS-NOM-ELEVE 
                           TO WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                       MOVE FS-PRENOM-ELEVE 
                           TO WS-PRENOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                       ADD 1 TO WS-I-ELEVE
               END-READ
           END-PERFORM.

       6110-READ-F-ELEVES-FIN.
           EXIT.

      ******************************************************************
      * AFFICHAGE DES ÉLÈVES DONT LE NOM EST RECHERCHÉ
      ******************************************************************

       8000-AFFICHAGE-ELEVES-DEB.

      * Saisie du nom à rechercher (nom exact attendu)
           DISPLAY "Veuillez saisir un nom de famille".
           ACCEPT WS-NOM-ELEVE-SEARCH.

      * Affichage de l'intégralité des éléves
      *    PERFORM VARYING WS-I-CLASSE FROM 1 BY 1 UNTIL WS-I-CLASSE > 2
      *        IF WS-NOM-CLASSE(WS-I-CLASSE) NOT = SPACES    
      *            DISPLAY " "
      *            DISPLAY "Classe : " WS-NOM-CLASSE(WS-I-CLASSE)
      *            DISPLAY "-------------------------"

      *            PERFORM VARYING WS-I-ELEVE FROM 1 BY 1 
      *                    UNTIL WS-I-ELEVE > 12
      *                IF WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE) 
      *                                                     NOT = SPACES
      *                    DISPLAY WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
      *                    " " WS-PRENOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
      *                END-IF
      *             END-PERFORM
      *         END-IF
      *    END-PERFORM.

      * Affichage de la classe et du/des noms des éléves recherchés
           PERFORM VARYING WS-I-CLASSE FROM 1 BY 1 UNTIL WS-I-CLASSE > 2
           
               DISPLAY " "
               DISPLAY "Classe : " WS-NOM-CLASSE(WS-I-CLASSE)
               DISPLAY "-------------------------"
           
               PERFORM VARYING WS-I-ELEVE FROM 1 BY 1 
                                          UNTIL WS-I-ELEVE > 12
                   IF WS-NOM-ELEVE-SEARCH = 
                                   WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                        DISPLAY WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                            " " WS-PRENOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                   END-IF
               END-PERFORM
           END-PERFORM.
       8000-AFFICHAGE-ELEVES-FIN.
           EXIT.

