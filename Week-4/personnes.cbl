      ******************************************************************
      * PROGRAMME : PERSONNES                                         *
      * AUTEUR    : Vincent-CMd1                                      *
      * OBJET     : Lecture d'un fichier de personnes contenant       *
      *             nom, prénom et date de naissance, stockage en     *
      *             tableau, affichage formaté et recherche par nom   *
      *             avec calcul de l'âge.                             *
      *                                                               *
      * FICHIER ENTREE : personnes.txt (séquentiel, LINE SEQUENTIAL) *
      * TABLEAU      : 15 personnes maximum                           *
      * REMARQUE     : Programme non normalisé HN, structure mixte.   *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. personnes.
       AUTHOR. Vincent-CMd1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-PERSONNES ASSIGN TO "personnes.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD F-PERSONNES.
       01 FS-F-PERSONNES.
           05 FS-NOM            PIC X(15).
           05 FS-PRENOM         PIC X(15).
           05 FS-DATE-N         PIC X(08).

       WORKING-STORAGE SECTION.

      * Tableau des personnes (15 max), indexé par WS-IDX-DEB
       01 WS-F-PERSONNES.
           05 WS-TB-PERSONNES OCCURS 15 TIMES INDEXED BY WS-IDX-DEB.
               10 WS-NOM        PIC X(15).
               10 WS-PRENOM     PIC X(15).
               10 WS-DATE-N     PIC 9(08).
           05 WS-DATE-ED      OCCURS 15 TIMES.
               10 WS-DD-ED      PIC 9(02).
               10 FILLER        PIC X(01) VALUE "/".
               10 WS-MM-ED      PIC 9(02).
               10 FILLER        PIC X(01) VALUE "/".
               10 WS-AAAA-ED    PIC 9(04).

      * Zone de saisie pour la recherche
       01 WS-NOM-AGE            PIC X(15).

      * Date système et date temporaire pour le calcul d'âge
       01 WS-DATE-COURANTE      PIC 9(08).
       01 WS-AGE                PIC 9(02).
       01 WS-DATE-TEMP.
           05 WS-DD-TEMP        PIC 9(02).
           05 WS-MM-TEMP        PIC 9(02).
           05 WS-AAAA-TEMP      PIC 9(04).

      * Constantes et indicateurs divers
       01 WS-IDX-FIN            PIC 9(03) VALUE 10.
       01 FLAG-STOP             PIC X.
           88 QUITTER VALUE 'Y'.

       PROCEDURE DIVISION.

      * Initialisation de l’index
           SET WS-IDX-DEB TO 1.

      * Ouverture du fichier
           OPEN INPUT F-PERSONNES.

      * Lecture et stockage des lignes du fichier dans le tableau
           PERFORM UNTIL QUITTER
               READ F-PERSONNES
                   AT END 
                       SET QUITTER TO TRUE
                   NOT AT END
                       IF WS-IDX-DEB <= WS-IDX-FIN
                           MOVE FS-NOM     TO WS-NOM(WS-IDX-DEB)
                           MOVE FS-PRENOM  TO WS-PRENOM(WS-IDX-DEB)
                           MOVE FS-DATE-N  TO WS-DATE-N(WS-IDX-DEB)
      * Extraction des composantes de la date pour affichage formaté
                           MOVE FS-DATE-N(3:2) TO WS-DD-ED(WS-IDX-DEB)
                           MOVE FS-DATE-N(1:2) TO WS-MM-ED(WS-IDX-DEB)
                           MOVE FS-DATE-N(5:4) TO WS-AAAA-ED(WS-IDX-DEB)
                           ADD 1 TO WS-IDX-DEB
                       END-IF
               END-READ
           END-PERFORM.

      * Fermeture du fichier
           CLOSE F-PERSONNES.

      * Affichage du tableau des personnes
           DISPLAY "|    Prénom       |       Nom       | Naissance | "
           DISPLAY "**************************************************"

           PERFORM VARYING WS-IDX-DEB FROM 1 BY 1 
               UNTIL WS-IDX-DEB > WS-IDX-FIN
               DISPLAY "| " WS-PRENOM(WS-IDX-DEB)
                       SPACE WITH NO ADVANCING
               DISPLAY "|" SPACE WITH NO ADVANCING
               DISPLAY WS-NOM(WS-IDX-DEB)
                       SPACE WITH NO ADVANCING
               DISPLAY "|" SPACE WITH NO ADVANCING
               DISPLAY WS-DATE-ED(WS-IDX-DEB) " |"
            DISPLAY "*------------------------------------------------*"
           END-PERFORM.

      * Demande de saisie à l'utilisateur
           DISPLAY "**************************************************"
           DISPLAY "* Saisir un nom pour afficher son âge           *"
           DISPLAY "**************************************************"
           DISPLAY "Nom choisi : " SPACE WITH NO ADVANCING
           ACCEPT WS-NOM-AGE.

      * Recherche séquentielle dans le tableau
           SET WS-IDX-DEB TO 1
           SEARCH WS-TB-PERSONNES
               AT END DISPLAY "/!\ Nom introuvable /!\"
               WHEN WS-NOM(WS-IDX-DEB) = WS-NOM-AGE
                   DISPLAY "Nom    : " WS-NOM(WS-IDX-DEB)   
                   DISPLAY "Prénom : " WS-PRENOM(WS-IDX-DEB)
      * Récupération de la date courante système
                   MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE-COURANTE
                   MOVE WS-DATE-COURANTE(1:4) TO WS-AAAA-TEMP
                   MOVE WS-DATE-COURANTE(5:2) TO WS-MM-TEMP
                   MOVE WS-DATE-COURANTE(7:2) TO WS-DD-TEMP 
      * Calcul approximatif de l'âge
                  COMPUTE WS-AGE = WS-AAAA-TEMP - WS-AAAA-ED(WS-IDX-DEB)
      * Ajustement si la date d’anniversaire n’est pas encore passée
                   IF WS-MM-ED(WS-IDX-DEB) > WS-MM-TEMP
                       IF WS-DD-ED(WS-IDX-DEB) > WS-DD-TEMP
                           SUBTRACT 1 FROM WS-AGE GIVING WS-AGE
                       END-IF
                   END-IF
                   DISPLAY "Âge : " WS-AGE
           END-SEARCH.

      * Fin du programme
           STOP RUN.
