******************************************************************
      *                                                                *
      * PROGRAMME DE VALIDATION DES DONNÉES UTILISATEUR               *
      *                                                                *
      ******************************************************************
      *                                                                *
      * Programme : main.cbl                                          *
      * Version   : 1.0                                               *
      * Auteur    : Vincent-Cmd1                                      *
      * Création  : 11/06/2025                                        *
      * Révision  : 11/06/2025                                        *
      *                                                                *
      * DESCRIPTION :                                                  *
      * Ce programme lit un fichier de données utilisateur contenant  *
      * des informations (ID, nom, email) et effectue une validation  *
      * des adresses email via un sous-programme externe. Les erreurs *
      * de validation sont consignées dans un fichier de log.         *
      *                                                                *
      * FICHIERS UTILISÉS :                                            *
      * - Entrée  : user.dat (données utilisateur à valider)          *
      * - Sortie  : errors.log (log des erreurs de validation)        *
      *                                                                *
      * STRUCTURE DES DONNÉES D'ENTRÉE :                               *
      * Position 1-10  : ID utilisateur (numérique)                   *
      * Position 11-56 : Nom utilisateur (46 caractères)              *
      * Position 57-86 : Email utilisateur (30 caractères)            *
      *                                                                *
      * MODULES EXTERNES :                                             *
      * - validate : Sous-programme de validation des emails          *
      *                                                                *
      * LIMITATIONS :                                                  *
      * - Maximum 999 enregistrements utilisateur                     *
      * - Longueur maximale des enregistrements : 250 caractères      *
      *                                                                *
      * CODES DE RETOUR :                                              *
      * - 0 : Traitement réussi                                       *
      * - Autre : Erreur lors du traitement                           *
      *                                                                *
      ******************************************************************

      ****************************************************************** 
       IDENTIFICATION DIVISION.
      ******************************************************************  
       PROGRAM-ID. MAIN.
       AUTHOR. Vincent-Cmd1.
      
      ****************************************************************** 
       ENVIRONMENT DIVISION.
      ******************************************************************  

       CONFIGURATION SECTION.
      *----------------------------------------------------------------*
      * Configuration système et utilisation de la fonction de debug   *
      * Le mode debug peut être activé en décommentant la ligne        *
      * SOURCE-COMPUTER pour obtenir des traces d'exécution détaillées *
      *----------------------------------------------------------------*
      *SOURCE-COMPUTER. MSI WITH DEBUGGING MODE.

       SPECIAL-NAMES.
      *    Définition du séparateur décimal (virgule pour conformité FR)
           DECIMAL-POINT IS COMMA.
             
       INPUT-OUTPUT SECTION.
      *----------------------------------------------------------------*
      * Définition des fichiers d'entrée et de sortie                  *
      * F-USER-INPUT  : Fichier des données utilisateur à traiter      *
      * F-USER-OUTPUT : Fichier de log des erreurs de validation       *
      *----------------------------------------------------------------*
       FILE-CONTROL.
           SELECT F-USER-INPUT
               ASSIGN TO 'user.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-INPUT-STATUS.

           SELECT F-USER-OUTPUT
               ASSIGN TO 'errors.log'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.  

      ****************************************************************** 
       DATA DIVISION.
      ****************************************************************** 
 
       FILE SECTION.  

      *----------------------------------------------------------------*
      * Définition des structures d'enregistrements pour les fichiers  *
      * Chaque enregistrement fait 250 caractères maximum              *
      *----------------------------------------------------------------*

       FD  F-USER-INPUT
           RECORD CONTAINS 250 CHARACTERS.
       01  FS-ENR-USER-INPUT                PIC X(250).   

       FD  F-USER-OUTPUT
           RECORD CONTAINS 250 CHARACTERS.
       01  FS-ENR-USER-OUTPUT               PIC X(250).    

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * VARIABLES DE CONTRÔLE DES FICHIERS                             *
      * Gestion des statuts d'ouverture, lecture et écriture           *
      * Les conditions 88 simplifient les tests de statut              *
      *----------------------------------------------------------------*
       01  WS-FS-INPUT-STATUS               PIC X(02)    VALUE SPACE.
           88 WS-FS-INPUT-STATUS-OK                      VALUE '00'.        
           88 WS-FS-INPUT-STATUS-EOF                     VALUE '10'.

       01  F-OUTPUT-STATUS                  PIC X(02)    VALUE SPACE.
           88 F-OUTPUT-STATUS-OK                         VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF                        VALUE '10'.

      *----------------------------------------------------------------*
      * STRUCTURE PRINCIPALE DE DONNÉES                                *
      * Table multidimensionnelle pour stocker jusqu'à 999 utilisateurs*
      * Chaque entrée contient : ID (10 chiffres), Nom (46 car.), Email*
      *----------------------------------------------------------------*
       01  TAB-DATA-USER.
           05 WS-USER OCCURS 999 TIMES.
               10 WS-ID                     PIC 9(10).
               10 WS-NOM                    PIC X(46).
               10 WS-EMAIL                  PIC X(30).

      *----------------------------------------------------------------*
      * TABLE DES ERREURS DE VALIDATION                                *
      * Stockage temporaire des enregistrements en erreur avant        *
      * écriture dans le fichier de log                                *
      *----------------------------------------------------------------*  
       01  TAB-DATA-USER-ERREUR.
           05 WS-USER-ERREUR OCCURS 999 TIMES.
               10 WS-NUM-LIGNE-ERREUR       PIC 9(04).
               10 WS-ID-ERREUR              PIC 9(10).
               10 WS-NOM-ERREUR             PIC X(46).
               10 WS-EMAIL-ERREUR           PIC X(30).

      *----------------------------------------------------------------*
      * STRUCTURE DE COMMUNICATION AVEC LE SOUS-PROGRAMME              *
      * Variables utilisées pour passer les données au module          *
      * de validation externe                                           *
      *----------------------------------------------------------------*
       01  WS-USER-DATA.
           05 WS-ID-USER                    PIC X(10).
           05 WS-NOM-USER                   PIC X(46).
           05 WS-EMAIL-USER                 PIC X(30).

      *    Compteur retourné par le sous-programme de validation
       01 WS-COUNT                          PIC 9(02).   

      *----------------------------------------------------------------*
      * VARIABLES DE TRAVAIL POUR LES CALCULS ET INDEXATION            *
      *----------------------------------------------------------------*
       01 WS-WORK-VARIABLES.
           05 WS-IDX                        PIC 9(04)    VALUE ZEROS.
           05 WS-LIGNE-ERREUR               PIC X(100)   VALUE SPACE.
           05 WS-NB-ERREURS                 PIC 9(04)    VALUE ZEROS.

      ****************************************************************** 
       PROCEDURE DIVISION.    
      ****************************************************************** 
      ******************************************************************
      * PROGRAMME PRINCIPAL                                            *
      * Orchestration du flux de traitement complet                    *
      * Séquence : Initialisation -> Lecture des données ->           *
      *           Validation -> Génération du log -> Finalisation      *
      ******************************************************************      

      * 1. Initialisation de l'environnement (ouverture des fichiers)
           PERFORM 1000-INITIALISATION-DEB
              THRU 1000-INITIALISATION-FIN.
               
      * 2. Chargement et validation des données depuis le fichier d'entrée
           PERFORM 2000-ENRG-DATA-DEB
              THRU 2000-ENRG-DATA-FIN.

      * 3. Génération du fichier de log des erreurs
           PERFORM 6320-WRITE-F-USER-OUTPUT-DEB
              THRU 6320-WRITE-F-USER-OUTPUT-FIN.

      * 4. Finalisation et nettoyage (fermeture des fichiers)
           PERFORM 5000-FIN-PROGRAMME-DEB
              THRU 5000-FIN-PROGRAMME-FIN.

      ******************************************************************
      * === 1000 === MODULE D'INITIALISATION                           *
      * Préparation de l'environnement de traitement                   *
      * - Ouverture sécurisée des fichiers                             *
      * - Initialisation des compteurs                                  *
      ******************************************************************
          
       1000-INITIALISATION-DEB.
      *----------------------------------------------------------------*
      * Phase critique : l'échec d'ouverture des fichiers provoque     *
      * l'arrêt immédiat du programme avec message d'erreur            *
      *----------------------------------------------------------------*

      * Ouverture du fichier d'entrée pour lecture séquentielle
           PERFORM 6010-OPEN-F-USER-INPUT-DEB
              THRU 6010-OPEN-F-USER-INPUT-FIN.

      * Ouverture du fichier de sortie pour écriture du log d'erreurs
           PERFORM 6020-OPEN-F-USER-OUTPUT-DEB
              THRU 6020-OPEN-F-USER-OUTPUT-FIN.

      * Initialisation du compteur d'index pour parcours des données
           MOVE 0 TO WS-IDX.

       1000-INITIALISATION-FIN.
           EXIT.

       2000-ENRG-DATA-DEB.
      *----------------------------------------------------------------*
      * Lecture complète du fichier d'entrée avec validation           *
      * - Parsing des données selon le format fixe                     *
      * - Appel du sous-programme de validation pour chaque email      *
      * - Stockage des erreurs pour génération ultérieure du log       *
      *----------------------------------------------------------------*

      * Lecture du premier enregistrement du fichier
           PERFORM 6110-READ-F-USER-INPUT-DEB
              THRU 6110-READ-F-USER-INPUT-FIN.

      * Boucle de traitement principal - continue jusqu'à EOF
           PERFORM UNTIL WS-FS-INPUT-STATUS-EOF
      * Trace de débogage pour suivi du traitement (activable via D)
      D    DISPLAY "Lecture d'un enregistrement - Type: " 
      D                                          FS-ENR-USER-INPUT(1:10) 
      
      * Incrémentation du compteur pour indexation des tableaux
               ADD 1 TO WS-IDX

      * Parsing des données selon le format de fichier défini :
      * - Positions 1-10   : ID utilisateur
      * - Positions 11-56  : Nom complet
      * - Positions 57-86  : Adresse email
               MOVE FS-ENR-USER-INPUT(1:10) TO WS-ID(WS-IDX)
               MOVE FS-ENR-USER-INPUT(11:46) TO WS-NOM(WS-IDX)
               MOVE FS-ENR-USER-INPUT(57:30) TO WS-EMAIL(WS-IDX)

      * Validation des données via sous-programme externe
               PERFORM 7010-VALIDATION-F-USER-INPUT-DEB
                  THRU 7010-VALIDATION-F-USER-INPUT-FIN

      * Affichage console pour vérification (mode debug/développement)
                DISPLAY "Enregistrement " WS-IDX " : " WS-ID(WS-IDX) 
                                                 " - " WS-NOM(WS-IDX) 
                                                " - " WS-EMAIL(WS-IDX)

      * Lecture de l'enregistrement suivant
               PERFORM 6110-READ-F-USER-INPUT-DEB
                  THRU 6110-READ-F-USER-INPUT-FIN
           END-PERFORM.

       2000-ENRG-DATA-FIN.
           EXIT.

      ******************************************************************
      * == 5000 == MODULE DE FINALISATION                              *
      * Séquence de clôture propre du programme                        *
      * - Fermeture sécurisée de tous les fichiers                     *
      * - Affichage du message de fin normale                          *
      ******************************************************************

       5000-FIN-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Séquence de clôture : fermeture des fichiers -> arrêt normal   *
      * La fermeture des fichiers est obligatoire pour éviter          *
      * la corruption des données                                       *
      *----------------------------------------------------------------*

      * Fermeture sécurisée du fichier d'entrée
           PERFORM 6210-CLOSE-F-USER-INPUT-DEB
              THRU 6210-CLOSE-F-USER-INPUT-FIN.
               
      * Fermeture sécurisée du fichier de sortie
           PERFORM 6220-CLOSE-F-USER-OUTPUT-DEB
              THRU 6220-CLOSE-F-USER-OUTPUT-FIN.

      * Terminaison normale du programme avec message
           PERFORM 9999-FIN-NORMALE-PROGRAMME-DEB
              THRU 9999-FIN-NORMALE-PROGRAMME-FIN.

       5000-FIN-PROGRAMME-FIN.
           EXIT.          

      ******************************************************************
      * == 6000 == MODULES DE GESTION DES FICHIERS                     *
      * Opérations d'E/S avec contrôle d'erreurs intégré               *
      * Chaque opération vérifie le FILE-STATUS et déclenche           *
      * une procédure d'arrêt d'urgence en cas d'échec                 *
      ******************************************************************
       
       6010-OPEN-F-USER-INPUT-DEB.
      *----------------------------------------------------------------*
      * Ouverture du fichier d'entrée en mode INPUT pour lecture       *
      * FILE-STATUS '00' = succès, autres valeurs = erreur             *
      *----------------------------------------------------------------*
           OPEN INPUT F-USER-INPUT.
      
      * Vérification critique : échec d'ouverture = arrêt programme
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "Probleme ouverture F-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Déclenchement de la procédure d'arrêt d'urgence
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6010-OPEN-F-USER-INPUT-FIN.
           EXIT.

       6020-OPEN-F-USER-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Ouverture du fichier de log en mode OUTPUT pour écriture       *
      * Le fichier est créé s'il n'existe pas, écrasé sinon            *
      *----------------------------------------------------------------*
           OPEN OUTPUT F-USER-OUTPUT.
      
      * Contrôle d'erreur sur l'ouverture du fichier de sortie
           IF NOT F-OUTPUT-STATUS-OK
               DISPLAY "Probleme ouverture F-USER-OUTPUT"
               DISPLAY "Code F-STATUS : " F-OUTPUT-STATUS
      * Arrêt du programme en cas d'échec
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6020-OPEN-F-USER-OUTPUT-FIN.
           EXIT.

       6110-READ-F-USER-INPUT-DEB.
      *----------------------------------------------------------------*
      * Lecture séquentielle d'un enregistrement                       *
      * Gestion des conditions AT END (fin de fichier) et NOT AT END   *
      * FILE-STATUS '10' indique la fin de fichier                     *
      *----------------------------------------------------------------*
           READ F-USER-INPUT
      * Condition de fin de fichier atteinte
           AT END 
               SET WS-FS-INPUT-STATUS-EOF TO TRUE
      * Lecture réussie d'un enregistrement
           NOT AT END 
               SET WS-FS-INPUT-STATUS-OK TO TRUE
           END-READ.
           
      * Détection d'erreurs de lecture (ni succès ni EOF)
           IF NOT WS-FS-INPUT-STATUS-OK AND 
              NOT WS-FS-INPUT-STATUS-EOF
               DISPLAY "Erreur lecture F-USER-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Arrêt en cas d'erreur de lecture
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6110-READ-F-USER-INPUT-FIN.
           EXIT.

       6210-CLOSE-F-USER-INPUT-DEB.
      *----------------------------------------------------------------*
      * Fermeture du fichier d'entrée après traitement complet         *
      * Libération des ressources système                               *
      *----------------------------------------------------------------*
           CLOSE F-USER-INPUT.
      
      * Vérification de la fermeture (rare mais possible échec)
           IF NOT WS-FS-INPUT-STATUS-OK
               DISPLAY "Probleme fermeture F-USER-INPUT"
               DISPLAY "Code F-STATUS : " WS-FS-INPUT-STATUS
      * Arrêt d'urgence même en fin de traitement
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6210-CLOSE-F-USER-INPUT-FIN.
           EXIT.

       6220-CLOSE-F-USER-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Fermeture du fichier de sortie - CRITIQUE pour la sauvegarde   *
      * Les données ne sont garanties écrites qu'après fermeture       *
      *----------------------------------------------------------------*
           CLOSE F-USER-OUTPUT.
      
      * Contrôle de l'état de fermeture
           IF NOT F-OUTPUT-STATUS-OK
               DISPLAY "Probleme fermeture F-USER-OUTPUT"
               DISPLAY "Code F-STATUS : " F-OUTPUT-STATUS
      * Arrêt d'urgence pour éviter la perte de données
               PERFORM 9999-ERREUR-PROGRAMME-DEB
                  THRU 9999-ERREUR-PROGRAMME-FIN
           END-IF.
       6220-CLOSE-F-USER-OUTPUT-FIN.
           EXIT.

       6320-WRITE-F-USER-OUTPUT-DEB.
      *----------------------------------------------------------------*
      * Génération du fichier de log des erreurs de validation         *
      * Format : [Ligne XXX] Erreur : Email invalide - email@domain    *
      * Parcours de toutes les erreurs stockées en mémoire             *
      *----------------------------------------------------------------*

      * Boucle sur toutes les erreurs détectées pendant la validation
           PERFORM VARYING WS-IDX FROM 1 BY 1 
                                   UNTIL WS-IDX > WS-NB-ERREURS

      * Initialisation de la ligne de log
               MOVE SPACES TO WS-LIGNE-ERREUR

      * Construction du message d'erreur formaté
               STRING "[Ligne " 
                   WS-NUM-LIGNE-ERREUR(WS-IDX) 
                   "] Erreur : Email invalide - " 
                   WS-EMAIL-ERREUR(WS-IDX)
                   INTO WS-LIGNE-ERREUR
               END-STRING

      * Écriture de la ligne dans le fichier de log
               WRITE FS-ENR-USER-OUTPUT FROM WS-LIGNE-ERREUR
               
           END-PERFORM.

       6320-WRITE-F-USER-OUTPUT-FIN.
           EXIT.

      ******************************************************************
      * === 7000 === MODULES DE VALIDATION                             *
      * Interface avec les sous-programmes de validation métier        *
      ******************************************************************

       7010-VALIDATION-F-USER-INPUT-DEB.
      *----------------------------------------------------------------*
      * Validation des données utilisateur via sous-programme externe  *
      * - Préparation des données pour le module de validation         *
      * - Appel du sous-programme 'validate'                           *
      * - Traitement du code de retour et stockage des erreurs         *
      *----------------------------------------------------------------*
      
      * Préparation des données pour le sous-programme
           MOVE WS-ID(WS-IDX)           TO WS-ID-USER.
           MOVE WS-NOM(WS-IDX)          TO WS-NOM-USER.
           MOVE WS-EMAIL(WS-IDX)        TO WS-EMAIL-USER.
       
      * Appel du sous-programme de validation externe
      * WS-COUNT retourne : 1 = valide, autre = invalide
           CALL 'validate' USING WS-USER-DATA WS-COUNT.

      * Traitement du résultat de validation
           IF WS-COUNT NOT EQUAL 1
      * Incrémentation du compteur d'erreurs
               ADD 1 TO WS-NB-ERREURS
      * Stockage des informations d'erreur pour le log
               MOVE WS-IDX         TO WS-NUM-LIGNE-ERREUR(WS-NB-ERREURS)
               MOVE WS-ID-USER     TO WS-ID-ERREUR(WS-NB-ERREURS)
               MOVE WS-NOM-USER    TO WS-NOM-ERREUR(WS-NB-ERREURS)
               MOVE WS-EMAIL-USER  TO WS-EMAIL-ERREUR(WS-NB-ERREURS)
           END-IF.

      * Réinitialisation du compteur pour le prochain appel
              MOVE 0 to WS-COUNT.

       7010-VALIDATION-F-USER-INPUT-FIN.
           EXIT.

      ******************************************************************
      * === 9000 === MODULES DE TERMINAISON DU PROGRAMME               *
      * Gestion des fins normales et anormales d'exécution             *
      * - Affichage de messages informatifs                            *
      * - Fermeture de sécurité des fichiers                           *
      * - Codes de retour appropriés                                    *
      ******************************************************************
       
       9999-FIN-NORMALE-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de fin normale du programme                          *
      * - Affichage du bandeau de fin normale                          *
      * - Fermeture de sécurité des fichiers                           *
      * - Terminaison avec code retour de succès (0)                   *
      *----------------------------------------------------------------*
      
      * Affichage du bandeau de fin normale pour l'utilisateur
           DISPLAY "****************************************".
           DISPLAY "*        FIN NORMALE PROGRAMME         *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'est terminé           *".
           DISPLAY "* correctement                         *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
           
      * Fermeture de sécurité des fichiers (double sécurité)
           CLOSE F-USER-INPUT.
           CLOSE F-USER-OUTPUT.
           
      * Terminaison normale avec code retour 0 (succès)
           STOP RUN.
       9999-FIN-NORMALE-PROGRAMME-FIN.
           EXIT.       

       9999-ERREUR-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procédure de traitement des erreurs critiques                  *
      * - Affichage du bandeau d'erreur                                *
      * - Fermeture de sécurité des fichiers ouverts                   *
      * - Terminaison avec code d'erreur                               *
      *----------------------------------------------------------------*
      
      * Affichage du bandeau d'erreur pour diagnostic    
           DISPLAY "****************************************".
           DISPLAY "*      FIN ANORMALE DU PROGRAMME       *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'arrête suite à une    *".
           DISPLAY "* erreur détectée dans le traitement   *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
           
      * Fermeture de sécurité des fichiers pour éviter corruption     
           CLOSE F-USER-INPUT.
           CLOSE F-USER-OUTPUT.
           
      * Note : En COBOL standard, STOP RUN termine toujours avec 0
      * Pour un vrai code d'erreur, utiliser GOBACK avec RETURN-CODE
           STOP RUN.
       9999-ERREUR-PROGRAMME-FIN.
           EXIT.
           