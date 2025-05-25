      ******************************************************************
      * Programme : bulletin-gen                                       *
      * Auteur    : Vincent-Cmd1                                       *
      * Création  : 21/05/2025                                         *
      * Mise à jour : 25/05/2025                                       *
      *                                                                *
      * Objet : Génération de bulletins de notes étudiants             *
      *            à partir d’un fichier brut de données.              *
      *                                                                *
      * Fonction  :                                                    *
      *  - Lit un fichier séquentiel (input.txt) contenant les         *
      *    informations sur les étudiants et leurs cours.              *
      *  - Calcule la moyenne pondérée de chaque étudiant.             *
      *  - Produit un bulletin formaté dans un fichier de sortie.      *
      *                                                                *
      * Limitations :                                                  *
      *  - Ne gère pas les erreurs de format ou de contenu dans les    *
      *    fichiers input-test1.txt et input.txt                       *
      *  - Ne calcule pas les moyennes par matière                     *
      *  - Ne calcule pas la moyenne globale de la classe              *
      *                                                                *
      * Remarques :                                                    *
      *  - Le traitement s’arrête proprement sur EOF ou sur erreur     *
      *    de lecture/écriture fichier.                                *
      ******************************************************************

      
      ****************************************************************** 
       IDENTIFICATION DIVISION.
      ******************************************************************  
       PROGRAM-ID. bulletin-gen.
       AUTHOR. Vincent-Cmd1.
      
      ****************************************************************** 
       ENVIRONMENT DIVISION.
      ******************************************************************  

       CONFIGURATION SECTION.
      *----------------------------------------------------------------*
      * Configuration système et utilisation de la fonction de debuggage
      *----------------------------------------------------------------*
      * SOURCE-COMPUTER. MSI WITH DEBUGGING MODE.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
             
       INPUT-OUTPUT SECTION.
      *----------------------------------------------------------------*
      * Définition des fichiers d'entrée et de sortie                  *
      * - F-INPUT  : Données brutes des étudiants et cours             *
      * - F-OUTPUT : Bulletin formaté pour impression                  *
      *----------------------------------------------------------------*
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.txt'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT-STATUS. 

           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.  

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
 
       FILE SECTION.

      *----------------------------------------------------------------*
      * Définition des structures d'enregistrements pour les fichiers  *
      *----------------------------------------------------------------*

       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT.
           05 REC-TYPE-CODE                 PIC 9(02).
           05 REC-DATA                      PIC X(998).

      * Enregistrement d'entrée à longueur variable (type + données) 
       FD  F-OUTPUT
           RECORD CONTAINS 100 CHARACTERS
           RECORDING MODE IS F.

       01  REC-F-OUTPUT                     PIC X(100).

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * VARIABLES DE CONTRÔLE DES FICHIERS                             *
      * Gestion des statuts d'ouverture, lecture et écriture           *
      *----------------------------------------------------------------*
       01  WS-FS-INPUT-STATUS               PIC X(02)    VALUE SPACE.
           88 WS-FS-INPUT-STATUS-OK                      VALUE '00'.        
           88 WS-FS-INPUT-STATUS-EOF                     VALUE '10'.

       01  F-OUTPUT-STATUS                  PIC X(02)    VALUE SPACE.
           88 F-OUTPUT-STATUS-OK                         VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF                        VALUE '10'.

      *----------------------------------------------------------------*
      * STRUCTURE PRINCIPALE DE DONNÉES ÉTUDIANTS                      *
      * Table multidimensionnelle pour stocker jusqu'à 999 étudiants   *
      * avec un maximum de 20 cours chacun                             *
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
      * VARIABLES DE TRAVAIL POUR LES CALCULS                          *
      * Utilisées dans les boucles et calculs de moyennes              *
      *----------------------------------------------------------------*
       01 WS-WORK-VARIABLES.
           05 WS-IDX                    PIC 9(03)    VALUE ZEROS.
           05 WS-IDX-COURS              PIC 9(03)    VALUE ZEROS.
           05 WS-IDX-MATIERE            PIC 9(03)    VALUE ZEROS.         
           05 WS-MATIERE-TROUVEE        PIC 9(03)    VALUE ZEROS.
           05 WS-CURRENT-STUDENT        PIC 9(02)    VALUE ZEROS.
           05 WS-TOTAL-COEF             PIC 9(03)V9  VALUE ZEROS.
           05 WS-TOTAL-POND             PIC 9(03)V99 VALUE ZEROS.
           05 WS-TOTAL-TEMP             PIC 9(03)V99 VALUE ZEROS.
           05 WS-TOTAL-NOTE             PIC 9(02)    VALUE ZEROS.    

      *----------------------------------------------------------------*
      * SOUS-PROGRAMME DES VARIABLES D'ÉDITION                         *
      * Les variables d'édition et de formatage sont centralisées      *
      *----------------------------------------------------------------*
           COPY bulletin-ed.


      ****************************************************************** 
       PROCEDURE DIVISION.    
      ****************************************************************** 
      ******************************************************************
      * PROGRAMME PRINCIPAL                                             *
      * Orchestration du flux de traitement complet                    *
      * Séquence : Init -> Lecture -> Calculs -> Sortie -> Fin         *
      ******************************************************************

      * 1. Initialisation de l'environnement
           PERFORM 1000-INIT-DEB
              THRU 1000-INIT-FIN.
               
      * 2. Chargement des données depuis le fichier d'entrée
           PERFORM 2000-ENRG-DATA-DEB
              THRU 2000-ENRG-DATA-FIN.
               
      * 3. Calculs mathématiques (moyennes pondérées)
           PERFORM 3000-TRAITEMENT-DATA-DEB
              THRU 3000-TRAITEMENT-DATA-FIN.

      * 4. Génération du bulletin de sortie formaté
           PERFORM 6320-WRITE-F-OUTPUT-DEB
              THRU 6320-WRITE-F-OUTPUT-FIN.

      * 5. Finalisation et nettoyage
           PERFORM 5000-FIN-PROGRAMME-DEB
              THRU 5000-FIN-PROGRAMME-FIN.


      ******************************************************************
      * === 1000 === MODULE D'INITIALISATION                           *
      * Préparation de l'environnement de traitement                   *
      ******************************************************************
          
       1000-INIT-DEB.
      *----------------------------------------------------------------*
      * Ouverture sécurisée des fichiers d'entrée et de sortie         *
      * Initialisation des compteurs et variables de contrôle          *
      *----------------------------------------------------------------*

      * Ouverture du fichier d'entrée pour lecture séquentielle
           PERFORM 6010-OPEN-F-INPUT-DEB
              THRU 6010-OPEN-F-INPUT-FIN.

      * Ouverture du fichier de sortie pour écriture du bulletin
           PERFORM 6020-OPEN-F-OUTPUT-DEB
              THRU 6020-OPEN-F-OUTPUT-FIN.

      * Initialisation des compteurs de données
           MOVE 0 TO WS-STUDENT-LGHT.
           MOVE 0 TO WS-CURRENT-STUDENT.

       1000-INIT-FIN.
           EXIT.


      ******************************************************************
      * === 2000 === MODULE DE LECTURE ET STOCKAGE DES DONNÉES         *
      * Traitement séquentiel du fichier d'entrée                      *
      ******************************************************************

       2000-ENRG-DATA-DEB.
      *----------------------------------------------------------------*
      * Lecture complète du fichier d'entrée avec analyse des codes    *
      * types et stockage structuré des données étudiants/cours        *
      *----------------------------------------------------------------*

      * Lecture du premier enregistrement
           PERFORM 6110-READ-F-INPUT-DEB
              THRU 6110-READ-F-INPUT-FIN.

      * Boucle de traitement jusqu'à fin de fichier
           PERFORM UNTIL WS-FS-INPUT-STATUS-EOF
      * Trace de débogage pour suivi du traitement
      D     DISPLAY "Lecture d'un enregistrement - Type: " REC-TYPE-CODE
           
      * Analyse du code type et traitement adapté
               EVALUATE REC-TYPE-CODE
      * ---- TRAITEMENT TYPE 01 : NOUVEL ÉTUDIANT ----
                  WHEN 01 
                      ADD 1 TO WS-STUDENT-LGHT
                      MOVE WS-STUDENT-LGHT 
                        TO WS-CURRENT-STUDENT
      * Extraction des données étudiant depuis l'enregistrement                
                      MOVE REC-DATA(1:7) 
                        TO WS-S-NOM(WS-CURRENT-STUDENT)
                      MOVE REC-DATA(8:6) 
                        TO WS-S-PRENOM(WS-CURRENT-STUDENT)
                      MOVE REC-DATA(14:2) 
                        TO WS-S-AGE(WS-CURRENT-STUDENT)
      * Initialisation du compteur de cours pour ce nouvel étudiant
                      MOVE 0 TO WS-S-NB-COURS(WS-CURRENT-STUDENT)
      D               DISPLAY "  Ajout étudiant: " 
      D                   WS-S-NOM(WS-CURRENT-STUDENT) " " 
      D                   WS-S-PRENOM(WS-CURRENT-STUDENT) ", "
      D                   WS-S-AGE(WS-CURRENT-STUDENT) " ans"
      
      * ---- TRAITEMENT TYPE 02 : NOUVEAU COURS ----
                  WHEN 02
      * Vérification qu'un étudiant actuel existe
                      IF WS-CURRENT-STUDENT > 0
                          ADD 1 TO WS-S-NB-COURS(WS-CURRENT-STUDENT)
      * Extraction des données cours depuis l'enregistrement                    
                          MOVE REC-DATA(1:21) 
                            TO WS-C-LIBELLE(WS-CURRENT-STUDENT, 
                                    WS-S-NB-COURS(WS-CURRENT-STUDENT))
                          MOVE REC-DATA(22:4)
                            TO WS-C-COEF(WS-CURRENT-STUDENT, 
                                    WS-S-NB-COURS(WS-CURRENT-STUDENT))
                          MOVE REC-DATA(24:6) 
                            TO WS-C-NOTE(WS-CURRENT-STUDENT, 
                                    WS-S-NB-COURS(WS-CURRENT-STUDENT))
      * Traces de débogage pour vérification des données                   
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
      * ---- TRAITEMENT CODES INCONNUS ----                
                  WHEN OTHER
                      DISPLAY "Code inconnu: " REC-TYPE-CODE
               END-EVALUATE
               
      * Lecture de l'enregistrement suivant
               PERFORM 6110-READ-F-INPUT-DEB
                  THRU 6110-READ-F-INPUT-FIN
           END-PERFORM.

       2000-ENRG-DATA-FIN.
           EXIT.


      ******************************************************************
      * === 3000 === MODULE DE CALCUL DES MOYENNES                     *
      * Traitement mathématique des notes avec pondération             *
      ******************************************************************

       3000-TRAITEMENT-DATA-DEB.
      *----------------------------------------------------------------*
      * Calcul des moyennes pondérées pour chaque étudiant             *
      * Formule : S(Note × Coefficient) / S(Coefficients)              *
      *----------------------------------------------------------------*

      * Boucle de traitement pour chaque étudiant
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > WS-STUDENT-LGHT
      * Réinitialisation des accumulateurs pour chaque étudiant                            
           MOVE 0 TO WS-TOTAL-COEF
           MOVE 0 TO WS-TOTAL-POND
           MOVE 0 TO WS-TOTAL-TEMP

      * Vérification que l'étudiant a au moins un cours
               IF WS-S-NB-COURS(WS-IDX) > 0
      * Boucle de calcul pour chaque cours de l'étudiant            
                   PERFORM VARYING WS-IDX-COURS FROM 1 BY 1
                           UNTIL WS-IDX-COURS > WS-S-NB-COURS(WS-IDX)
      * Calcul de la somme pondérée : (Note × Coef) + somme précédente
                       COMPUTE WS-TOTAL-POND 
                               = (WS-C-COEF(WS-IDX, WS-IDX-COURS) 
                               * WS-C-NOTE(WS-IDX, WS-IDX-COURS)) 
                               + WS-TOTAL-TEMP
      * Sauvegarde pour itération suivante
                       MOVE WS-TOTAL-POND TO WS-TOTAL-TEMP
      * Accumulation des coefficients pour le dénominateur                 
                       COMPUTE WS-TOTAL-COEF 
                               = WS-C-COEF(WS-IDX, WS-IDX-COURS) 
                               + WS-TOTAL-COEF
                   END-PERFORM
               END-IF
      * Calcul final de la moyenne avec arrondi automatique
           COMPUTE WS-S-MOYENNE(WS-IDX) ROUNDED = 
                   WS-TOTAL-POND / WS-TOTAL-COEF
      D    DISPLAY "Moyenne de l'eleve : " WS-S-MOYENNE(WS-IDX)
           END-PERFORM.                               

       3000-TRAITEMENT-DATA-FIN.
           EXIT.


      ******************************************************************
      * == 5000 == MODULE DE FINALISATION                              *
      * Affichage, fermeture des fichiers et terminaison               *
      ******************************************************************

       5000-FIN-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Séquence de clôture : affichage -> fermeture -> arrêt normal   *
      *----------------------------------------------------------------*

      * Affichage console des données traitées
           PERFORM 8000-AFFICHAGE-DATA-DEB
              THRU 8000-AFFICHAGE-DATA-FIN. 

      * Fermeture sécurisée du fichier d'entrée
           PERFORM 6210-CLOSE-F-INPUT-DEB
              THRU 6210-CLOSE-F-INPUT-FIN.

      * Fermeture sécurisée du fichier de sortie
           PERFORM 6220-CLOSE-F-OUTPUT-DEB
              THRU 6220-CLOSE-F-OUTPUT-FIN.
               
      * Terminaison normale du programme
           PERFORM 9999-FIN-NORMALE-PROGRAMME-DEB
              THRU 9999-FIN-NORMALE-PROGRAMME-FIN.

       5000-FIN-PROGRAMME-FIN.
           EXIT.


      ******************************************************************
      * == 6000 == MODULES DE GESTION DES FICHIERS                     *
      * Opérations d'E/S avec contrôle d'erreurs intégré               *
      ******************************************************************
       
       6010-OPEN-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Ouverture du fichier d'entrée en mode INPUT pour lecture       *
      * Vérification du status et arrêt en cas d'erreur                *
      *----------------------------------------------------------------*
           OPEN INPUT F-INPUT.
      * Contrôle de l'état d'ouverture du fichier
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "Probleme ouverture F-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-F-INPUT-FIN.
           EXIT.


       6020-OPEN-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Ouverture du fichier de sortie en mode OUTPUT pour écriture    *
      * Vérification du status et arrêt en cas d'erreur                *
      *----------------------------------------------------------------*
           OPEN OUTPUT F-OUTPUT.
      * Contrôle de l'état d'ouverture du fichier
           IF NOT F-OUTPUT-STATUS-OK
               DISPLAY "Probleme ouverture F-OUTPUT"
               DISPLAY "Code F-STATUS : " F-OUTPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-OPEN-F-OUTPUT-FIN.
           EXIT.


       6110-READ-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Lecture d'un enregistrement depuis le fichier d'entrée         *
      * Gestion des conditions AT END et NOT AT END                    *
      * Contrôle d'erreurs sur les opérations de lecture               *
      *----------------------------------------------------------------*
      * Lecture de l'enregistrement suivant dans la structure
           READ F-INPUT INTO REC-F-INPUT
      * Traitement de la condition de fin de fichier
           AT END 
               SET WS-FS-INPUT-STATUS-EOF TO TRUE
      * Traitement de la lecture réussie
           NOT AT END 
               SET WS-FS-INPUT-STATUS-OK TO TRUE
           END-READ.
           
      * Contrôle d'erreur : ni succès ni fin de fichier
           IF NOT WS-FS-INPUT-STATUS-OK AND 
              NOT WS-FS-INPUT-STATUS-EOF
               DISPLAY "Erreur lecture F-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-READ-F-INPUT-FIN.
           EXIT.


       6210-CLOSE-F-INPUT-DEB.
      *----------------------------------------------------------------*
      * Fermeture du fichier d'entrée après traitement complet         *
      * Vérification du status et arrêt en cas d'erreur                *
      *----------------------------------------------------------------*
           CLOSE F-INPUT.
      * Contrôle de l'état de fermeture du fichier
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "Probleme fermeture F-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6210-CLOSE-F-INPUT-FIN.
           EXIT.

   
       6220-CLOSE-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Fermeture du fichier de sortie après écriture complète        *
      * Vérification du status et arrêt en cas d'erreur               *
      *----------------------------------------------------------------*
           CLOSE F-OUTPUT.
      * Contrôle de l'état de fermeture du fichier
           IF NOT F-OUTPUT-STATUS-OK
               DISPLAY "Probleme fermeture F-OUTPUT"
               DISPLAY "Code F-STATUS : " F-OUTPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6220-CLOSE-F-OUTPUT-FIN.
           EXIT.


       6320-WRITE-F-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Construction et écriture du bulletin de notes formaté         *
      * Structure : Entête -> Données étudiants -> Pied de page       *
      * Gestion du positionnement et formatage des colonnes           *
      *----------------------------------------------------------------*
       
      * === SECTION ENTÊTE DU BULLETIN ===
      * Écriture de l'encadrement supérieur avec astérisques
           WRITE REC-F-OUTPUT FROM WS-ASTX-ED           AFTER 1.
      * Écriture du titre principal du bulletin
           WRITE REC-F-OUTPUT FROM WS-LIGNE-ENTETE-1-ED AFTER 1.
      * Écriture de l'encadrement de titre avec astérisques
           WRITE REC-F-OUTPUT FROM WS-ASTX-ED           AFTER 1.
      * Écriture d'un séparateur avec tirets et espacement
           WRITE REC-F-OUTPUT FROM WS-TIRET-ED          AFTER 2.
      * Écriture des en-têtes de colonnes
           WRITE REC-F-OUTPUT FROM WS-LIGNE-ENTETE-2-ED AFTER 1.
      * Écriture du séparateur sous les en-têtes
           WRITE REC-F-OUTPUT FROM WS-TIRET-ED          AFTER 1.    
       
      * === SECTION DONNÉES DES ÉTUDIANTS ===
      * Boucle de traitement pour chaque étudiant enregistré
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > WS-STUDENT-LGHT
       
      * Initialisation de la ligne d'édition pour cet étudiant
               INITIALIZE WS-LIGNE-ELEVE-ED
               MOVE 1 TO WS-POINTER
       
      * Préparation des données formatées pour l'édition
               MOVE WS-S-NOM(WS-IDX)     TO WS-S-NOM-ED
               MOVE WS-S-PRENOM(WS-IDX)  TO WS-S-PRENOM-ED  
               MOVE WS-S-MOYENNE(WS-IDX) TO WS-S-MOYENNE-ED
       
      * === CONSTRUCTION MANUELLE DE LA LIGNE FORMATÉE ===
      * Positionnement du nom (colonne 2, largeur 15)
               MOVE WS-S-NOM-ED          TO WS-LIGNE-ELEVE-ED(2:15)
      * Ajout du séparateur de colonne
               MOVE " | "                TO WS-LIGNE-ELEVE-ED(16:3)
      * Positionnement du prénom (colonne 19, largeur 14)
               MOVE WS-S-PRENOM-ED       TO WS-LIGNE-ELEVE-ED(19:14)
      * Ajout du séparateur de colonne
               MOVE " | "                TO WS-LIGNE-ELEVE-ED(33:3)
      * Positionnement de la moyenne (colonne 38, largeur 7)
               MOVE WS-S-MOYENNE-ED      TO WS-LIGNE-ELEVE-ED(38:7)
      * Ajout du séparateur de colonne
               MOVE " | "                TO WS-LIGNE-ELEVE-ED(43:3)
       
      * Initialisation du pointeur pour les notes (position 46)
               MOVE 46 TO WS-POINTER
       
      * === AJOUT DYNAMIQUE DES NOTES DE COURS ===
      * Boucle pour chaque cours de l'étudiant actuel
               PERFORM VARYING WS-IDX-COURS FROM 1 BY 1
                         UNTIL WS-IDX-COURS > WS-S-NB-COURS(WS-IDX)

      * Formatage de la note pour l'édition
                   MOVE WS-C-NOTE(WS-IDX, WS-IDX-COURS) TO WS-C-NOTE-ED
      * Positionnement de la note dans la ligne
                   MOVE WS-C-NOTE-ED TO WS-LIGNE-ELEVE-ED(WS-POINTER:6)
      * Avancement du pointeur de 6 positions
                   ADD 6 TO WS-POINTER
      * Ajout du séparateur après la note
                   MOVE " | " TO WS-LIGNE-ELEVE-ED(WS-POINTER:3)
      * Avancement du pointeur de 3 positions
                   ADD 3 TO WS-POINTER
               END-PERFORM
       
      * Traces de débogage pour contrôle de la construction
      D        DISPLAY "Étudiant: " WS-S-NOM(WS-IDX) 
      D                  " - Nb cours: " WS-S-NB-COURS(WS-IDX)
      D        DISPLAY "Ligne: [" WS-LIGNE-ELEVE-ED "]"
       
      * Écriture de la ligne complète de l'étudiant
               WRITE REC-F-OUTPUT FROM WS-LIGNE-ELEVE-ED    AFTER 1

      * Contrôle d'erreur sur l'écriture
               IF NOT F-OUTPUT-STATUS-OK
                    DISPLAY 'Erreur écriture F-OUTPUT'
                    DISPLAY "Status: " F-OUTPUT-STATUS
      * Appel de la procédure d'arrêt d'urgence
                    PERFORM 9999-ERREUR-PROGRAMME-DEB
                       THRU 9999-ERREUR-PROGRAMME-FIN
               END-IF
       
           END-PERFORM.

      * === SECTION PIED DE PAGE DU BULLETIN ===
      * Écriture du séparateur de fin de données
           WRITE REC-F-OUTPUT FROM WS-TIRET-ED          AFTER 1. 
      * Écriture de l'encadrement avec astérisques
           WRITE REC-F-OUTPUT FROM WS-ASTX-ED           AFTER 2.
      * Écriture du séparateur avant les statistiques
           WRITE REC-F-OUTPUT FROM WS-TIRET-ED          AFTER 1.
      * Écriture des lignes de commentaires fixes
           WRITE REC-F-OUTPUT FROM WS-LIGNE-C1          AFTER 1.
           WRITE REC-F-OUTPUT FROM WS-LIGNE-C2          AFTER 1.
           WRITE REC-F-OUTPUT FROM WS-LIGNE-C3          AFTER 1.
           WRITE REC-F-OUTPUT FROM WS-LIGNE-C4          AFTER 1.
           WRITE REC-F-OUTPUT FROM WS-LIGNE-C5          AFTER 1.
           WRITE REC-F-OUTPUT FROM WS-LIGNE-C6          AFTER 1.
      * Écriture du séparateur avant les statistiques
           WRITE REC-F-OUTPUT FROM WS-TIRET-ED          AFTER 1.
      * Écriture de l'encadrement des statistiques
           WRITE REC-F-OUTPUT FROM WS-ASTX-ED           AFTER 1.

      * === SECTION STATISTIQUES DYNAMIQUES ===
      * Construction et écriture du nombre total d'étudiants
           STRING WS-NB-ELEVES-ED WS-CURRENT-STUDENT
               INTO WS-NB-ELEVES-TXT-ED
           END-STRING.
           WRITE REC-F-OUTPUT FROM WS-NB-ELEVES-TXT-ED  AFTER 1.

      * Construction et écriture du nombre de cours du dernier étudiant
           STRING WS-NB-COURS-ED WS-S-NB-COURS(WS-CURRENT-STUDENT)
               INTO WS-NB-COURS-TXT-ED
           END-STRING.
           WRITE REC-F-OUTPUT FROM WS-NB-COURS-TXT-ED   AFTER 1. 

      * Calcul et affichage du nombre total de notes
           COMPUTE WS-TOTAL-NOTE = WS-S-NB-COURS(WS-CURRENT-STUDENT)
                                 * WS-CURRENT-STUDENT.
           STRING WS-NB-NOTES-ED WS-TOTAL-NOTE
           INTO WS-NB-NOTES-TXT-ED 
           END-STRING.
           WRITE REC-F-OUTPUT FROM WS-NB-NOTES-TXT-ED   AFTER 1.    
      
      * Écriture de l'encadrement final
           WRITE REC-F-OUTPUT FROM WS-ASTX-ED           AFTER 1.
      * Écriture du message de fin de bulletin
           WRITE REC-F-OUTPUT FROM WS-FIN-BULLETIN      AFTER 1.

       6320-WRITE-F-OUTPUT-FIN.
           EXIT.
      

      ******************************************************************
      * === 8000 === MODULE D'AFFICHAGE CONSOLE                        *
      * Présentation structurée des données pour débogage              *
      ******************************************************************

       8000-AFFICHAGE-DATA-DEB.
      *----------------------------------------------------------------*
      * Affichage formaté de toutes les données traitées               *
      * Structure hiérarchique : Étudiants -> Cours -> Notes           *
      * Utilisé pour contrôle et débogage du traitement                *
      *----------------------------------------------------------------*
      * Affichage de l'en-tête principal
           DISPLAY "****************************************".
           DISPLAY "*       DONNÉES DES ÉTUDIANTS          *".
           DISPLAY "****************************************".
      * Affichage du nombre total d'étudiants traités
           DISPLAY "Nombre total d'étudiants: " WS-STUDENT-LGHT.
           
      * Boucle d'affichage pour chaque étudiant
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                  UNTIL WS-IDX > WS-STUDENT-LGHT
      * Séparateur visuel entre les étudiants
               DISPLAY "----------------------------------------"
      * Affichage des informations principales de l'étudiant
               DISPLAY "Étudiant N°: " WS-IDX
               DISPLAY "Nom: " WS-S-NOM(WS-IDX)
               DISPLAY "Prénom: " WS-S-PRENOM(WS-IDX)
               DISPLAY "Age: " WS-S-AGE(WS-IDX)
               DISPLAY "Nombre de cours: " WS-S-NB-COURS(WS-IDX)
               DISPLAY "Moyenne de l'eleve: " WS-S-MOYENNE(WS-IDX)
               
      * Affichage détaillé des cours si l'étudiant en a
               IF WS-S-NB-COURS(WS-IDX) > 0
                   DISPLAY "  --- Liste des cours ---"
      * Boucle d'affichage pour chaque cours de l'étudiant
                   PERFORM VARYING WS-IDX-COURS FROM 1 BY 1
                       UNTIL WS-IDX-COURS > WS-S-NB-COURS(WS-IDX)
      * Affichage des détails de chaque cours
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
           
      * Affichage du pied de page
           DISPLAY "****************************************".
           DISPLAY "*     FIN AFFICHAGE DES DONNÉES       *".
           DISPLAY "****************************************".
       8000-AFFICHAGE-DATA-FIN.
           EXIT.


      ******************************************************************
      * === 9000 === MODULES DE TERMINAISON DU PROGRAMME               *
      * Gestion des fins normales et anormales d'exécution             *
      ******************************************************************
       
       9999-FIN-NORMALE-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de fin normale du programme                          *
      * - Affiche un bandeau de fin normale                            *
      * - Ferme le fichier d'entrée                                    *
      * - Termine le programme avec un code retour de succès           *
      *----------------------------------------------------------------*
      * Affichage du bandeau de fin normale
           DISPLAY "****************************************".
           DISPLAY "*        FIN NORMALE PROGRAMME         *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'est terminé           *".
           DISPLAY "* correctement                         *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
      * Fermeture de sécurité du fichier d'entrée
           CLOSE F-INPUT.
      * Terminaison normale avec code retour 0
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
      * Affichage du bandeau de fin anormale    
           DISPLAY "****************************************".
           DISPLAY "*      FIN ANORMALE DU PROGRAMME       *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'arrête suite à une    *".
           DISPLAY "* erreur détectée dans le traitement   *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
      * Fermeture de sécurité du fichier d'entrée     
           CLOSE F-INPUT.
      * Terminaison normale avec code retour 0 
           STOP RUN.
       9999-ERREUR-PROGRAMME-FIN.
           EXIT.
