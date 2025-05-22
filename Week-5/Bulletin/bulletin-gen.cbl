      ****************************************************************** 
       IDENTIFICATION DIVISION.
      ******************************************************************  
       PROGRAM-ID. bulletin-gen.
       AUTHOR. Vincent-Cmd1.
      *----------------------------------------------------------------*
      * Ce programme utilise une structure de données à X dimensions   *
      * pour stocker et gérer les informations                         *
      *----------------------------------------------------------------*
      
      ****************************************************************** 
       ENVIRONMENT DIVISION.
      ******************************************************************  

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. MSI WITH DEBUGGING MODE.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       
      *----------------------------------------------------------------*
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.txt'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT-STATUS. 

      *     SELECT F-OUTPUT
      *         ASSIGN TO 'output.dat'
      *         ACCESS MODE IS SEQUENTIAL
      *         ORGANIZATION IS LINE SEQUENTIAL
      *         FILE STATUS IS F-OUTPUT-STATUS.  

      *----------------------------------------------------------------*
      * Définition du fichier de sortie qui contiendra les données     *
      *----------------------------------------------------------------*
      
      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
 
       FILE SECTION.

      *----------------------------------------------------------------*
      * Structure d'enregistrement du fichier                          *
      *----------------------------------------------------------------*

       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT.
           05 REC-TYPE-CODE               PIC 9(02).
           05 REC-DATA                    PIC X(998).

      * FD  F-OUTPUT
      *     RECORD CONTAINS 250 CHARACTERS
      *     RECORDING MODE IS F.

      * 01  REC-F-OUTPUT                   PIC X(250).

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Variables pour la gestion des fichiers                         *
      *----------------------------------------------------------------*
       01  WS-FS-INPUT-STATUS              PIC X(02)   VALUE SPACE.
           88 WS-FS-INPUT-STATUS-OK                    VALUE '00'.        
           88 WS-FS-INPUT-STATUS-EOF                   VALUE '10'.

      * 01  F-OUTPUT-STATUS                PIC X(02)   VALUE SPACE.
      *     88 F-OUTPUT-STATUS-OK                      VALUE '00'.        
      *     88 F-OUTPUT-STATUS-EOF                     VALUE '10'.

      *----------------------------------------------------------------*
      * Structure pour stocker les étudiants et leurs notes            *
      *----------------------------------------------------------------*
       01  TAB-DATA-STUDENT.
           05 WS-STUDENT-LGHT              PIC 9(03).
           05 WS-COURSE-LGHT               PIC 9(03).
           05 WS-STUDENT OCCURS 999 TIMES.
               10 WS-S-PRENOM              PIC X(06).      
               10 WS-S-NOM                 PIC X(07).
               10 WS-S-AGE                 PIC 9(02).
               10 WS-S-NB-COURS            PIC 9(02)    VALUE ZEROS.
               10 WS-S-MOYENNE             PIC 99V99    VALUE ZEROS.
               10 WS-C-COURS OCCURS 20 TIMES. 
                   15 WS-C-LIBELLE         PIC X(21).
                   15 WS-C-COEF            PIC 9V9.
                   15 WS-C-NOTE            PIC 99V99.

      *----------------------------------------------------------------*
      * Tableau de stockage des matieres                                         *
      *----------------------------------------------------------------*
       01  TAB-MOYENNES-MATIERES.
           05 WS-NB-MATIERES-UNIQUES       PIC 9(03)    VALUE ZEROS.
           05 WS-MATIERE-INFO OCCURS 10 TIMES.
               10 WS-MAT-LIBELLE           PIC X(21).
               10 WS-MAT-TOTAL-NOTES       PIC 9(05)V99 VALUE ZEROS.
               10 WS-MAT-NB-NOTES          PIC 9(03)    VALUE ZEROS.
               10 WS-MAT-MOYENNE           PIC 99V99    VALUE ZEROS.
       
      *----------------------------------------------------------------*
      * Variables de travail                                           *
      *----------------------------------------------------------------*
       01 WS-WORK-VARIABLES.
           05 WS-IDX                       PIC 9(03)    VALUE ZEROS.
           05 WS-IDX-COURS                 PIC 9(03)    VALUE ZEROS.
           05 WS-IDX-MATIERE               PIC 9(03)    VALUE ZEROS.         
           05 WS-MATIERE-TROUVEE           PIC 9(03)    VALUE ZEROS.
           05 WS-CURRENT-STUDENT           PIC 9(03)    VALUE ZEROS.
           05 WS-TOTAL-COEF                PIC 9(03)V9  VALUE ZEROS.
           05 WS-TOTAL-POND                PIC 9(03)V99 VALUE ZEROS.
           05 WS-TOTAL-TEMP                PIC 9(03)V99 VALUE ZEROS. 

      ****************************************************************** 
       PROCEDURE DIVISION.    
      ****************************************************************** 
      ******************************************************************
      * COMPOSANT PRINCIPAL : chaîne de traitement principale          *
      * Contrôle le flux d'exécution général du programme              *
      ******************************************************************

       0000-PRINCIPAL-DEB.
           PERFORM 1000-INIT-DEB
              THRU 1000-INIT-FIN.
               
           PERFORM 2000-ENRG-DATA-DEB
              THRU 2000-ENRG-DATA-FIN.
               
           PERFORM 3000-TRAITEMENT-DATA-DEB
              THRU 3000-TRAITEMENT-DATA-FIN.

           PERFORM 8000-AFFICHAGE-DATA-DEB
              THRU 8000-AFFICHAGE-DATA-FIN.  
               
           PERFORM 9999-FIN-NORMALE-PROGRAMME-DEB
              THRU 9999-FIN-NORMALE-PROGRAMME-FIN.
       0000-PRINCIPAL-FIN.
           EXIT.

      ******************************************************************
      * ORDRES DE TRAITEMENT DES DONNEES                               *
      ******************************************************************
       1000-INIT-DEB.

           PERFORM 6010-OPEN-F-INPUT-DEB
              THRU 6010-OPEN-F-INPUT-FIN.

           MOVE 0 TO WS-STUDENT-LGHT.
           MOVE 0 TO WS-CURRENT-STUDENT.

       1000-INIT-FIN.
           EXIT.


       2000-ENRG-DATA-DEB.

      * Lecture et stockage des données élèves
           PERFORM 6110-READ-F-INPUT-DEB
              THRU 6110-READ-F-INPUT-FIN.

           PERFORM UNTIL WS-FS-INPUT-STATUS-EOF
      * Ajout de routines de débogage
           DISPLAY "Lecture d'un enregistrement - Type: " REC-TYPE-CODE
           
               EVALUATE REC-TYPE-CODE
      * Nouvel étudiant         
                  WHEN 01 
                      ADD 1 TO WS-STUDENT-LGHT
                      MOVE WS-STUDENT-LGHT 
                        TO WS-CURRENT-STUDENT
                      MOVE REC-DATA(1:7) 
                        TO WS-S-NOM(WS-CURRENT-STUDENT)
                      MOVE REC-DATA(8:6) 
                        TO WS-S-PRENOM(WS-CURRENT-STUDENT)
                      MOVE REC-DATA(14:2) 
                        TO WS-S-AGE(WS-CURRENT-STUDENT)
                      MOVE 0 TO WS-S-NB-COURS(WS-CURRENT-STUDENT)
                      DISPLAY "  Ajout étudiant: " 
                          WS-S-NOM(WS-CURRENT-STUDENT) " " 
                          WS-S-PRENOM(WS-CURRENT-STUDENT) ", "
                          WS-S-AGE(WS-CURRENT-STUDENT) " ans"
                  WHEN 02
      * Nouveau cours pour l'étudiant courant 
                      IF WS-CURRENT-STUDENT > 0
                          ADD 1 TO WS-S-NB-COURS(WS-CURRENT-STUDENT)
                          MOVE REC-DATA(1:21) 
                            TO WS-C-LIBELLE(WS-CURRENT-STUDENT, 
                                    WS-S-NB-COURS(WS-CURRENT-STUDENT))
                          MOVE REC-DATA(22:4)
                            TO WS-C-COEF(WS-CURRENT-STUDENT, 
                                    WS-S-NB-COURS(WS-CURRENT-STUDENT))
                          MOVE REC-DATA(24:6) 
                            TO WS-C-NOTE(WS-CURRENT-STUDENT, 
                                    WS-S-NB-COURS(WS-CURRENT-STUDENT))
      * DISPLAY de debuggage pour enticiper la sortie                   
      D                   DISPLAY "  Ajout cours: " 
      D                       WS-C-LIBELLE(WS-CURRENT-STUDENT,
      D                             WS-S-NB-COURS(WS-CURRENT-STUDENT))
      D                       ", Note: "
      D                       WS-C-NOTE(WS-CURRENT-STUDENT,
      D                             WS-S-NB-COURS(WS-CURRENT-STUDENT))
      D                       ", Coef: "
      D                       WS-C-COEF(WS-CURRENT-STUDENT,
      D                             WS-S-NB-COURS(WS-CURRENT-STUDENT))
                      END-IF
                  WHEN OTHER
                      DISPLAY "Code inconnu: " REC-TYPE-CODE
               END-EVALUATE
      * Lecture et stockage des données élèves
               PERFORM 6110-READ-F-INPUT-DEB
                  THRU 6110-READ-F-INPUT-FIN
           END-PERFORM.

      * Fermeture du fichier
           PERFORM 6210-CLOSE-F-INPUT-DEB
              THRU 6210-CLOSE-F-INPUT-FIN.

       2000-ENRG-DATA-FIN.
           EXIT.


       3000-TRAITEMENT-DATA-DEB.

           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > WS-STUDENT-LGHT
      * Réinitialistation des compteur                            
           MOVE 0 TO WS-TOTAL-COEF
           MOVE 0 TO WS-TOTAL-POND
           MOVE 0 TO WS-TOTAL-TEMP

               IF WS-S-NB-COURS(WS-IDX) > 0
                   PERFORM VARYING WS-IDX-COURS FROM 1 BY 1
                           UNTIL WS-IDX-COURS > WS-S-NB-COURS(WS-IDX)
      * Calcul de la somme pondérée (note × coefficient + ...)
                       COMPUTE WS-TOTAL-POND 
                               = (WS-C-COEF(WS-IDX, WS-IDX-COURS) 
                               * WS-C-NOTE(WS-IDX, WS-IDX-COURS)) 
                               + WS-TOTAL-TEMP
      * Stockage de la somme précédente pour le calcul de la moyenne
                       MOVE WS-TOTAL-POND TO WS-TOTAL-TEMP
      * Calcul de la somme des coefficients matières                 
                       COMPUTE WS-TOTAL-COEF 
                               = WS-C-COEF(WS-IDX, WS-IDX-COURS) 
                               + WS-TOTAL-COEF
                   END-PERFORM
               END-IF
      * Calcul de la moyenne pondérée pour l'éléve en cours de traitement
           COMPUTE WS-S-MOYENNE(WS-IDX) ROUNDED = 
                   WS-TOTAL-POND / WS-TOTAL-COEF
      D    DISPLAY "Moyenne de l'eleve : " WS-S-MOYENNE(WS-IDX)
           END-PERFORM.                               

      *------------------------------------------------------------------------* 
       
















       3000-TRAITEMENT-DATA-FIN.
           EXIT.



      ******************************************************************
      * ORDRES DE MANIPULATION DES FICHIERS                            *
      ******************************************************************
       
      *----------------------------------------------------------------*
      * Ouverture du fichier F-INPUT                                   *
      *----------------------------------------------------------------*
       6010-OPEN-F-INPUT-DEB.
           OPEN INPUT F-INPUT.
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "Probleme ouverture F-INPUT"
               DISPLAY "Code : " WS-FS-INPUT-STATUS
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-F-INPUT-FIN.
           EXIT.

      *----------------------------------------------------------------*
      * Fermeture du fichier F-INPUT                                   *        
      *----------------------------------------------------------------*
       6210-CLOSE-F-INPUT-DEB.
           CLOSE F-INPUT.
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "Probleme fermeture F-INPUT"
               DISPLAY "Code : " WS-FS-INPUT-STATUS
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6210-CLOSE-F-INPUT-FIN.
           EXIT.

      *----------------------------------------------------------------*
      * Lecture du fichier F-INPUT                                     *
      *----------------------------------------------------------------*
       6110-READ-F-INPUT-DEB.
           READ F-INPUT INTO REC-F-INPUT
           AT END 
               SET WS-FS-INPUT-STATUS-EOF TO TRUE
           NOT AT END 
               SET WS-FS-INPUT-STATUS-OK TO TRUE
           END-READ.
           
           IF NOT WS-FS-INPUT-STATUS-OK AND 
              NOT WS-FS-INPUT-STATUS-EOF
               DISPLAY "Erreur lecture F-INPUT"
               DISPLAY "Code : " WS-FS-INPUT-STATUS
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-READ-F-INPUT-FIN.
           EXIT.

      ******************************************************************
      * ORDRES DE MANIPULATION DES EDITIONS                            *
      ******************************************************************

       8000-AFFICHAGE-DATA-DEB.
           DISPLAY "****************************************".
           DISPLAY "*       DONNÉES DES ÉTUDIANTS          *".
           DISPLAY "****************************************".
           DISPLAY "Nombre total d'étudiants: " WS-STUDENT-LGHT.
           
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > WS-STUDENT-LGHT
               DISPLAY "----------------------------------------"
               DISPLAY "Étudiant N°: " WS-IDX
               DISPLAY "Nom: " WS-S-NOM(WS-IDX)
               DISPLAY "Prénom: " WS-S-PRENOM(WS-IDX)
               DISPLAY "Age: " WS-S-AGE(WS-IDX)
               DISPLAY "Nombre de cours: " WS-S-NB-COURS(WS-IDX)
               DISPLAY "Moyenne de l'eleve: " WS-S-MOYENNE(WS-IDX)
               
               IF WS-S-NB-COURS(WS-IDX) > 0
                   DISPLAY "  --- Liste des cours ---"
                   PERFORM VARYING WS-IDX-COURS FROM 1 BY 1
                       UNTIL WS-IDX-COURS > WS-S-NB-COURS(WS-IDX)
                       DISPLAY "  Cours: " 
                           WS-C-LIBELLE(WS-IDX, WS-IDX-COURS)
                       DISPLAY "  Coefficient: " 
                           WS-C-COEF(WS-IDX, WS-IDX-COURS)
                       DISPLAY "  Note: " 
                           WS-C-NOTE(WS-IDX, WS-IDX-COURS)
                       DISPLAY "  ----------------"
                   END-PERFORM
               END-IF
           END-PERFORM.
           
           DISPLAY "****************************************".
           DISPLAY "*     FIN AFFICHAGE DES DONNÉES       *".
           DISPLAY "****************************************".
       8000-AFFICHAGE-DATA-FIN.
           EXIT.

      ******************************************************************
      * ORDRES DE MANIPULATION DES SOUS PROGRAMMES                     *
      ******************************************************************
       
       9999-FIN-NORMALE-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de fin normale du programme                          *
      * - Affiche un bandeau de fin normale                            *
      * - Ferme le fichier d'entrée                                    *
      * - Termine le programme avec un code retour de succès           *
      *----------------------------------------------------------------*
           DISPLAY "****************************************".
           DISPLAY "*        FIN NORMALE PROGRAMME         *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'est terminé           *".
           DISPLAY "* correctement                         *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
           CLOSE F-INPUT.
           STOP RUN.
       9999-FIN-NORMALE-PROGRAMME-FIN.
           EXIT.       
       
       9999-ERREUR-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de traitement des erreurs                            *
      * - Affiche un bandeau d'erreur                                  *
      * - Ferme le fichier d'entrée                                    *
      * - Termine le programme avec un code retour d'erreur            *
      *----------------------------------------------------------------*
           DISPLAY "****************************************".
           DISPLAY "*      FIN ANORMALE DU PROGRAMME       *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'arrête suite à une    *".
           DISPLAY "* erreur détectée dans le traitement   *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
           CLOSE F-INPUT.
           STOP RUN.
       9999-ERREUR-PROGRAMME-FIN.
          EXIT.
