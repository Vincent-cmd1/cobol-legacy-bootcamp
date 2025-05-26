      ******************************************************************
      * PROGRAMME : ASSURANCES.CBL
      * AUTEURS   : Anais & Vincent
      * OBJET     : Ce programme lit un fichier de contrats d'assurance,
      *             stocke les enregistrements en mémoire,
      *             formate les dates et affiche certains contrats.
      *
      * FORMAT    : Le fichier source est en format texte séquentiel,
      *             avec les dates au format AAAAMMJJ.
      *             Ce programme les affiche en format JJ/MM/AAAA.
      *
      * REMARQUE  : Seuls les contrats aux index 3 et 7 sont affichés.
      ******************************************************************    
         
       IDENTIFICATION DIVISION.
       PROGRAM-ID. assurances.
       AUTHOR. Anais & Vincent.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.   

      * Déclaration du fichier source
           SELECT F-ASSURANCES ASSIGN TO "assurances.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

      * Déclaration du fichier de sortie
           SELECT F-RAPPORT ASSIGN TO "rapport-assurances-bis.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      * Description du fichier d'entrée
       FD F-ASSURANCES.
       01 FS-ENR-ASSURANCES.
           05 FS-LIGNE-ASSURANCE          PIC X(121).

       FD F-RAPPORT.
       01 FS-ENR-RAPPORT.
           05 FS-LIGNE-RAPPORT            PIC X(121).

       WORKING-STORAGE SECTION.

      * Tableau mémoire pour stocker jusqu'à 36 contrats d'assurances
      * Chaque ligne est un enregistrement complet du fichier
       01 WS-ENR-ASSURANCES.
           05 WS-ASSURANCES OCCURS 36 TIMES.
             10 WS-LIGNE-ASSURANCE        PIC X(121).

      * Index pour boucle de traitement
       01 WS-IDX                          PIC 9(03)    VALUE 1.
       01 WS-IDX-FIN                      PIC 9(03)    VALUE 36.

      * Drapeau de fin de lecture du fichier
      * La valeur 'Y' de la condition QUITTER permet de sortir duPERFORM

       01 FLAG-STOP                       PIC X.
           88 QUITTER                                  VALUE 'Y'.

      * Varaiable d'affichage
       01 WS-TIRET                        PIC X(123)   VALUE ALL '-'.  

      * Variables pour reformater les dates AAAAMMJJ en JJ/MM/AAAA
       01 WS-ANNEE                        PIC X(4).
       01 WS-MOIS                         PIC X(2).
       01 WS-JOUR                         PIC X(2).
       01 WS-DATE-DEB-FORMATTEE           PIC X(10).
       01 WS-DATE-FIN-FORMATTEE           PIC X(10).
       01 WS-DATE-TEMP                    PIC X(121).


       PROCEDURE DIVISION.

      ******************************************************************
      * COMPOSANT PRINCIPAL : enchaînement des traitements
      ******************************************************************

       0000-TRT-PRINCIPAL-DEB.
       
      * Ouverture du fichier d'entrée
           PERFORM 6010-OPEN-F-ASSURANCES-DEB
              THRU 6010-OPEN-F-ASSURANCES-FIN.
       
      * Lecture fichier + stockage en mémoire
           PERFORM 6110-READ-F-ASSURANCES-DEB
              THRU 6110-READ-F-ASSURANCES-FIN.

      * Fermeture du fichier d'entrée
           PERFORM 6210-CLOSE-F-ASSURANCES-DEB
              THRU 6210-CLOSE-F-ASSURANCES-FIN.

      * Ouverture du fichier de sortie
           PERFORM 6020-OPEN-F-RAPPORT-DEB
              THRU 6020-OPEN-F-RAPPORT-FIN.

      * Écriture des élèves retenus
           PERFORM 6320-WRITE-F-RAPPORT-DEB
              THRU 6320-WRITE-F-RAPPORT-FIN.

      * Fermeture du fichier de sortie
           PERFORM 6220-CLOSE-F-RAPPORT-DEB
              THRU 6220-CLOSE-F-RAPPORT-FIN.

      * Affichage des assurances
           PERFORM 8000-AFFICHAGE-ASSU-DEB
              THRU 8000-AFFICHAGE-ASSU-FIN.       

       0000-TRT-PRINCIPAL-FIN.
           EXIT.

           STOP RUN.

      ******************************************************************
      * ORDRES D'OUVERTURE ET FERMETURE DE FICHIER
      ******************************************************************
      
      * Ouvre le fichier d'entrée contenant les contrats
       6010-OPEN-F-ASSURANCES-DEB.
           OPEN INPUT F-ASSURANCES.
       6010-OPEN-F-ASSURANCES-FIN.
           EXIT.

      * Ouvre le fichier de sortie contenant les contrats 
       6020-OPEN-F-RAPPORT-DEB.
           OPEN OUTPUT F-RAPPORT.
       6020-OPEN-F-RAPPORT-FIN.
           EXIT.

      * Ferme le fichier d'entrée après lecture complète
       6210-CLOSE-F-ASSURANCES-DEB.
           CLOSE F-ASSURANCES.
       6210-CLOSE-F-ASSURANCES-FIN.
           EXIT.

      * Ferme le fichier de sortie après ecriture
       6220-CLOSE-F-RAPPORT-DEB.
           CLOSE F-RAPPORT.
       6220-CLOSE-F-RAPPORT-FIN.
           EXIT. 

      ******************************************************************
      * LECTURE DU FICHIER ASSURANCES ET STOCKAGE EN MÉMOIRE
      ******************************************************************
      
      * Lecture du fichier ligne par ligne
      * Chaque enregistrement est stocké dans le tableau WS-ASSURANCES
      * L'indice WS-IDX contrôle le nombre d'enregistrements à stocker
       6110-READ-F-ASSURANCES-DEB.
           MOVE 'N' TO FLAG-STOP.
           PERFORM UNTIL QUITTER
               READ F-ASSURANCES
                   AT END
                       SET QUITTER TO TRUE
                   NOT AT END
                       IF WS-IDX <= WS-IDX-FIN
                           MOVE FS-LIGNE-ASSURANCE 
                               TO WS-LIGNE-ASSURANCE(WS-IDX)
                           ADD 1 TO WS-IDX     
                       ELSE
                           SET QUITTER TO TRUE
                       END-IF
                END-READ         
           END-PERFORM.
       6110-READ-F-ASSURANCES-FIN.
           EXIT.

      ******************************************************************
      * ÉCRITURE DU FICHIER DE SORTIE POUR LES ÉLÈVES RÉUSSIS
      ******************************************************************
       6320-WRITE-F-RAPPORT-DEB.
           MOVE 1 TO WS-IDX.
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-IDX-FIN
               IF WS-IDX = 3 OR WS-IDX = 7 
                   PERFORM 7000-FORMATER-DATES-DEB
                      THRU 7000-FORMATER-DATES-FIN
                   MOVE WS-LIGNE-ASSURANCE(WS-IDX) TO FS-LIGNE-RAPPORT
                   WRITE FS-ENR-RAPPORT
               END-IF
           END-PERFORM.
       6320-WRITE-F-RAPPORT-FIN.
           EXIT.

      ******************************************************************
      * COMPOSANT INDEPENDANT
      ******************************************************************
      
      * Ce composant reformate les dates de début et de fin d'un contrat
      * Il utilise WS-DATE-TEMP pour extraire AAAA MM JJ
      * Puis construit la date JJ/MM/AAAA dans WS-DATE-*-FORMATTEE
       7000-FORMATER-DATES-DEB.
           MOVE WS-LIGNE-ASSURANCE(WS-IDX) TO WS-DATE-TEMP.
           MOVE WS-DATE-TEMP(91:4) TO WS-ANNEE
           MOVE WS-DATE-TEMP(95:2) TO WS-MOIS
           MOVE WS-DATE-TEMP(97:2) TO WS-JOUR
           STRING WS-JOUR "/" WS-MOIS "/" WS-ANNEE
               INTO WS-DATE-DEB-FORMATTEE
           STRING WS-DATE-DEB-FORMATTEE 
               DELIMITED BY SIZE
               INTO WS-LIGNE-ASSURANCE(WS-IDX)(91:10)

           MOVE WS-LIGNE-ASSURANCE(WS-IDX) TO WS-DATE-TEMP.
           MOVE WS-DATE-TEMP(100:4) TO WS-ANNEE
           MOVE WS-DATE-TEMP(104:2) TO WS-MOIS
           MOVE WS-DATE-TEMP(106:2) TO WS-JOUR
           STRING WS-JOUR "/" WS-MOIS "/" WS-ANNEE
               INTO WS-DATE-FIN-FORMATTEE.
           STRING WS-DATE-FIN-FORMATTEE 
               DELIMITED BY SIZE
               INTO WS-LIGNE-ASSURANCE(WS-IDX)(100:10)
       7000-FORMATER-DATES-FIN.
           EXIT.

      ******************************************************************
      * AFFICHAGE À L'ÉCRAN
      ******************************************************************
      
      * Affiche l'entête de tableau pour l'utilisateur
      * Parcourt les enregistrements stockés
      * Affiche uniquement ceux d'index 3 et 7 avec les dates formatées
       8000-AFFICHAGE-ASSU-DEB. 
           DISPLAY " Code    |" 
                   " Nom contrat    |" 
                   " Nom produit    |" 
                   " Nom client                                |"
                   " Statut   |"
                   "Deb. contrat|"
                   " Fin contrat|"
                   " Montant super".
           DISPLAY WS-TIRET.
               
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-IDX-FIN
               IF WS-IDX = 3 OR WS-IDX = 7 
                   PERFORM 7000-FORMATER-DATES-DEB
                      THRU 7000-FORMATER-DATES-FIN
        
                   DISPLAY WS-LIGNE-ASSURANCE(WS-IDX)
               END-IF
           END-PERFORM.
       8000-AFFICHAGE-ASSU-FIN.
           EXIT.
      

