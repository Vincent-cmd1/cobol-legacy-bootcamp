      ******************************************************************
      * PROGRAMME : CM1-CM2-TRI                                       *
      *                                                               *
      * DESCRIPTION :                                                 *
      *   Ce programme permet la gestion d'élèves de CM1 et CM2.      *
      *   Il réalise les opérations suivantes :                       *
      *    - Saisie des informations des élèves par classe            *
      *    - Tri automatique par ordre alphabétique dans chaque classe*
      *    - Sauvegarde des données dans un fichier texte             *
      *    - Affichage des élèves par classe                          *
      *                                                               *
      * AUTEUR    : Vincent-Cmd1                                      *
      * VERSION   : 1.0                                               *
      * DATE      : 20/05/2025                                        *
      *                                                               *
      * STRUCTURE DE DONNEES :                                        *
      *   - Tableau à 2 dimensions (2 classes x 3 élèves maximum)     *
      *   - Chaque élève est identifié par son nom et prénom          *
      *                                                               *
      * ENTREE    : Saisie utilisateur des élèves par classe          *
      *                                                               *
      * SORTIE    : - Fichier texte "output-classes.txt"              *
      *             - Affichage console des élèves triés par classe   *
      *                                                               *
      * ALGORITHME PRINCIPAL :                                        *
      *   1. Ouverture du fichier de sortie                           *
      *   2. Saisie des données élèves (avec tri automatique)         *
      *   3. Écriture dans le fichier                                 *
      *   4. Fermeture du fichier                                     *
      *   5. Affichage des résultats                                  *
      *                                                               *
      * PARTICULARITÉS :                                              *
      *   - Utilisation de l'instruction SORT native pour le tri      *
      *   - Organisation des données par niveau scolaire              *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CM1-CM2-TRI.
       AUTHOR. Vincent-Cmd1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT F-ELEVES-OUT ASSIGN TO "output-classes.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

      *----------------------------------------------------------------*
      * Structure d'enregistrement du fichier de sortie                *
      *----------------------------------------------------------------*
       FD F-ELEVES-OUT.
       01 FS-ENR-ELEVES-OUT.
           05 FS-CLASSE                     PIC X(03).   *> "CM1 ou CM2"
           05 FS-NOM-ELEVE                  PIC X(09).   *> Nom de famille
           05 FS-PRENOM-ELEVE               PIC X(08).   *> Prénom

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Tableau principal pour stocker les informations des élèves     *
      * Structure hiérarchique : Classe > Élèves > Infos (nom/prénom)  *
      *----------------------------------------------------------------*
       01 TAB-ELEVES.
           05 WS-CLASSE OCCURS 2 TIMES.        
               10 WS-NOM-CLASSE             PIC X(03).   *> "CM1" ou "CM2"
               10 WS-ELEVE OCCURS 3 TIMES
                          ASCENDING KEY IS WS-NOM-ELEVE
                          INDEXED BY IDX-ELEVE.
                   15 WS-NOM-ELEVE          PIC X(09).   *> Nom de famille
                   15 WS-PRENOM-ELEVE       PIC X(08).   *> Prénom 

      *----------------------------------------------------------------*
      * Variables de contrôle et compteurs                             *
      *----------------------------------------------------------------*
       01 WS-I-CLASSE                       PIC 9(03)    VALUE 1.
       01 WS-I-ELEVE                        PIC 9(03)    VALUE 1.

      *----------------------------------------------------------------*
      * Indicateur pour la fin du programme                            *
      *----------------------------------------------------------------*
       01 FLAG-STOP                         PIC X.
           88 QUITTER                                    VALUE 'Y'.

       PROCEDURE DIVISION.

      ******************************************************************
      * COMPOSANT PRINCIPAL : chaîne de traitement principale          *
      * Contrôle le flux d'exécution général du programme              *
      ******************************************************************
       0000-TRT-PRINCIPAL-DEB.

      *----------------------------------------------------------------*
      * 1. Ouverture du fichier de sortie                              *
      *----------------------------------------------------------------*
           PERFORM 6010-OPEN-F-ELEVES-DEB
              THRU 6010-OPEN-F-ELEVES-FIN.

      *----------------------------------------------------------------*
      * 2. Saisie des informations des élèves par classe               *
      *    Le tri est effectué automatiquement pendant la saisie       *
      *----------------------------------------------------------------*
           PERFORM 1000-SAISIE-ELEVE-DEB
              THRU 1000-SAISIE-ELEVE-FIN.

      *----------------------------------------------------------------*
      * 3. Écriture des données triées dans le fichier de sortie       *
      *----------------------------------------------------------------*
           PERFORM 6310-WRITE-F-ELEVES-DEB
              THRU 6310-WRITE-F-ELEVES-FIN.

      *----------------------------------------------------------------*
      * 4. Fermeture du fichier                                        *
      *----------------------------------------------------------------*
           PERFORM 6210-CLOSE-F-ELEVES-DEB
              THRU 6210-CLOSE-F-ELEVES-FIN.

      *----------------------------------------------------------------*
      * 5. Affichage des élèves classés par niveau et triés par nom    *
      *----------------------------------------------------------------*
           PERFORM 8000-AFFICHAGE-ELEVES-DEB
              THRU 8000-AFFICHAGE-ELEVES-FIN.       

       0000-TRT-PRINCIPAL-FIN.
           EXIT.

           STOP RUN.

      ******************************************************************
      * SAISIE DES CLASSES ET DES ÉLÈVES                              *
      * Collecte les informations et applique le tri automatiquement   *
      ******************************************************************
       1000-SAISIE-ELEVE-DEB.
      *----------------------------------------------------------------*
      * Boucle pour chaque classe (CM1 et CM2)                         *
      *----------------------------------------------------------------*
       PERFORM VARYING WS-I-CLASSE FROM 1 BY 1 
                                   UNTIL WS-I-CLASSE > 2
           DISPLAY "Veuillez entrer la classe des élèves (CM1/CM2)."
           ACCEPT WS-NOM-CLASSE(WS-I-CLASSE) 

      *----------------------------------------------------------------*
      * Boucle pour chaque élève de la classe (max 3 par classe)       *
      *----------------------------------------------------------------*
           PERFORM VARYING WS-I-ELEVE FROM 1 BY 1 
                                      UNTIL WS-I-ELEVE > 3

               DISPLAY "Veuillez entrer son nom, puis son prénom."
               DISPLAY "Nom : " SPACE WITH NO ADVANCING 
               ACCEPT WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE) 

               DISPLAY "Prénom : " SPACE WITH NO ADVANCING
               ACCEPT WS-PRENOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE) 
           END-PERFORM

      *----------------------------------------------------------------*
      * Tri des élèves par ordre alphabétique du nom dans la classe    *
      * Utilisation de l'instruction SORT native de COBOL              *
      *----------------------------------------------------------------*
           SORT WS-ELEVE(WS-I-CLASSE) ON ASCENDING KEY WS-NOM-ELEVE

       END-PERFORM.
       1000-SAISIE-ELEVE-FIN.
           EXIT.

      ******************************************************************
      * GESTION DES FICHIERS : OUVERTURE / FERMETURE                  *
      ******************************************************************
       
      *----------------------------------------------------------------*
      * Ouverture du fichier de sortie en mode écriture                *
      *----------------------------------------------------------------*
       6010-OPEN-F-ELEVES-DEB.
           OPEN OUTPUT F-ELEVES-OUT.
       6010-OPEN-F-ELEVES-FIN.
           EXIT.

      *----------------------------------------------------------------*
      * Fermeture du fichier après traitement                          *
      *----------------------------------------------------------------*
       6210-CLOSE-F-ELEVES-DEB.
           CLOSE F-ELEVES-OUT.
       6210-CLOSE-F-ELEVES-FIN.
           EXIT.

      ******************************************************************
      * ECRITURE DES DONNÉES DANS LE FICHIER                          *
      * Transfert des données du tableau en mémoire vers le fichier    *
      ******************************************************************
       6310-WRITE-F-ELEVES-DEB.

      *----------------------------------------------------------------*
      * Double boucle pour parcourir les classes et les élèves         *
      * et écrire chaque enregistrement dans le fichier                *
      *----------------------------------------------------------------*
       PERFORM VARYING WS-I-CLASSE FROM 1 BY 1 UNTIL WS-I-CLASSE > 2
           MOVE WS-NOM-CLASSE(WS-I-CLASSE) TO FS-CLASSE 
           PERFORM VARYING WS-I-ELEVE FROM 1 BY 1 UNTIL WS-I-ELEVE > 3
               MOVE WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE) 
                 TO FS-NOM-ELEVE
               MOVE WS-PRENOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE) 
                 TO FS-PRENOM-ELEVE
               WRITE FS-ENR-ELEVES-OUT          
           END-PERFORM
       END-PERFORM.

       6310-WRITE-F-ELEVES-FIN.
           EXIT.

      ******************************************************************
      * AFFICHAGE DES ÉLÈVES PAR CLASSE                               *
      * Présentation formatée des résultats à l'utilisateur            *
      ******************************************************************
       8000-AFFICHAGE-ELEVES-DEB.

      *----------------------------------------------------------------*
      * Parcours des classes et affichage formaté des élèves           *
      * Les élèves sont déjà triés par ordre alphabétique              *
      *----------------------------------------------------------------*
           PERFORM VARYING WS-I-CLASSE FROM 1 BY 1 UNTIL WS-I-CLASSE > 2
             IF WS-NOM-CLASSE(WS-I-CLASSE) NOT = SPACES    
                   DISPLAY " "
                   DISPLAY "Classe : " WS-NOM-CLASSE(WS-I-CLASSE)
                   DISPLAY "-------------------------" 
                   PERFORM VARYING WS-I-ELEVE FROM 1 BY 1 
                                              UNTIL WS-I-ELEVE > 3
                       IF WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE) 
                                                            NOT = SPACES
                           DISPLAY WS-NOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                           " " WS-PRENOM-ELEVE(WS-I-CLASSE, WS-I-ELEVE)
                       END-IF
                    END-PERFORM
                END-IF
           END-PERFORM.

       8000-AFFICHAGE-ELEVES-FIN.
           EXIT.
           
