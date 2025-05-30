      ******************************************************************
      * PROGRAMME : CRUD - GESTION D'INDIVIDUS                         *
      ******************************************************************
      * AUTEUR       : Vincent-cmd1                                    *
      * DATE CREATION: 28/05/2025                                      *
      * MODIFICATION : 30/05/2025                                      *
      *                                                                *
      * RESUME :                                                       *
      * Programme de gestion CRUD (Create, Read, Update, Delete)       *
      * pour une base de donnees d'individus.                          *
      *                                                                *
      * FONCTIONNALITES :                                              *
      * - Ajouter un nouvel individu (nom, prenom, telephone)          *
      * - Modifier les informations d'un individu existant             *
      * - Supprimer un individu de la base                             *
      * - Afficher les details d'un individu                           *
      * - Interface menu interactive                                   *
      *                                                                *
      * BASE DE DONNEES :                                              *
      * - Table : individus                                            *
      * - Champs : id, nom, prenom, telephone                          *
      * - SGBD : Compatible SQL standard                               *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. crud.
       AUTHOR. Vincent-cmd1.
       DATE-WRITTEN. 2025-05-28.

      ****************************************************************** 
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.

      * Variables de controle du menu et navigation
       01 WS-CHOIX                PIC 9(01).
       01 WS-INDIVIDU-ID          PIC 9(04).
       01 WS-STOP                 PIC 9(01).

      * Section SQL - Variables hotes pour communication avec la BD
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-NOM             PIC X(50).
       01  WS-PRENOM          PIC X(50).
       01  WS-TELEPHONE       PIC X(10).
       01  USERNAME           PIC X(30) VALUE "vincent-cmd1".
       01  PASSWD             PIC X(30) VALUE "13051994".
       01  DBNAME             PIC X(10) VALUE "testdb".
       EXEC SQL END DECLARE SECTION END-EXEC.

      * Inclusion de la zone de communication SQL
       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.

      * Programme principal - Sequence d'exécution
           PERFORM 0000-INTITIALISATION-DEB
              THRU 0000-INTITIALISATION-FIN.
           PERFORM 0000-MENU-DEB
              THRU 0000-MENU-FIN.     

      ******************************************************************
      * === 0000 === MODULE D'INITIALISATION                           *
      * Préparation de l'environnement de traitement                   *
      ******************************************************************
       0000-INTITIALISATION-DEB.

      * Établissement de la connexion à la base de données
           DISPLAY "Début de l'initialisation...". 
           DISPLAY "Connexion à la base de données...".
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.

      * Verification du succes de la connexion
           IF SQLCODE NOT = 0
               DISPLAY "Erreur de connexion SQLCODE: " SQLCODE
               STOP RUN
           END-IF.

       0000-INTITIALISATION-FIN.
           EXIT.

      ******************************************************************
      * === 0000 === MODULE MENU PRINCIPAL                             *
      * Gestion de la navigation et dispatch des fonctions             *
      ******************************************************************
       0000-MENU-DEB.

      * Affichage du menu et saisie du choix utilisateur
           PERFORM 8000-AFFICHAGE-MENU-DEB
              THRU 8000-AFFICHAGE-MENU-FIN. 
 
      * Dispatch vers la fonction choisie
           EVALUATE WS-CHOIX
           WHEN 1
                   PERFORM 1010-AJOUTER-DEB
                      THRU 1010-AJOUTER-FIN
           WHEN 2
                   PERFORM 1020-MODIFIER-DEB
                      THRU 1020-MODIFIER-FIN
           WHEN 3
                   PERFORM 1030-SUPPRIMER-DEB
                      THRU 1030-SUPPRIMER-FIN             
           WHEN 4
                   PERFORM 1040-AFFICHER-DEB
                      THRU 1040-AFFICHER-FIN
           WHEN 5
                   PERFORM 9999-FIN-NORMALE-PROGRAMME-DEB
                      THRU 9999-FIN-NORMALE-PROGRAMME-FIN        
           WHEN OTHER
                   DISPLAY "Erreur de saisie"
                   DISPLAY "Veuillez reessayer." 
                   PERFORM 0000-MENU-DEB
                      THRU 0000-MENU-FIN
           END-EVALUATE.

       0000-MENU-FIN.
           EXIT.

      ******************************************************************
      * === 1000 === MODULES DE GESTION CRUD                           *
      * Operations Create, Read, Update, Delete                        *
      ******************************************************************
     
      ******************************************************************
      * CREATE - Ajout d'un nouvel individu                            *
      ******************************************************************
       1010-AJOUTER-DEB.

      * Saisie des informations du nouvel individu
           DISPLAY "Entrez le nom : ".
           ACCEPT WS-NOM.
           DISPLAY "Entrez le prenom : ".
           ACCEPT WS-PRENOM.
           DISPLAY "Entrez le numero de telephone : ".
           ACCEPT WS-TELEPHONE.
       
      * Insertion en base de donnees
           EXEC SQL
               INSERT INTO individus (nom, prenom, telephone)
               VALUES (:WS-NOM, :WS-PRENOM, :WS-TELEPHONE)
           END-EXEC.

      * Verification du resultat et validation
           IF SQLCODE = 0
               DISPLAY "Insertion reussie."
           ELSE
               DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
           END-IF.

           EXEC SQL COMMIT END-EXEC.

      * Retour au menu principal
           PERFORM 0000-MENU-DEB
              THRU 0000-MENU-FIN.

       1010-AJOUTER-FIN.
           EXIT.

      ******************************************************************
      * UPDATE - Modification d'un individu existant                   *
      ******************************************************************
       1020-MODIFIER-DEB.

      * Selection de l'individu à modifier
           DISPLAY "Entrez le numero d'ID de l'individu à selectionner".    
           ACCEPT WS-INDIVIDU-ID.

      * Lecture des donnees actuelles
           EXEC SQL 
               SELECT nom, prenom, telephone
               INTO :WS-NOM, :WS-PRENOM, :WS-TELEPHONE
               FROM individus
               WHERE id = :WS-INDIVIDU-ID
           END-EXEC.

      * Affichage des donnees actuelles si trouvees
           IF SQLCODE = 0
               DISPLAY "------------------------------------"
               DISPLAY "Individu que vous souhaitez modifier"
               DISPLAY "------------------------------------"
               DISPLAY "Nom : " WS-NOM
               DISPLAY "Prenom : " WS-PRENOM
               DISPLAY "Telephone : " WS-TELEPHONE
               DISPLAY "------------------------------------"
           ELSE
               DISPLAY "Erreur de lecture SQLCODE: " SQLCODE
               PERFORM 0000-MENU-DEB
                  THRU 0000-MENU-FIN
           END-IF.

      * Saisie des nouvelles valeurs
           DISPLAY "Entrez un nouveau nom".
           ACCEPT WS-NOM.

           DISPLAY "Entrez un nouveau prénom".
           ACCEPT WS-PRENOM.

           DISPLAY "Entrez un nouveau numéro de telephone".
           ACCEPT WS-TELEPHONE.

      * Mise à jour en base
           EXEC SQL 
               UPDATE individus
               SET nom = :WS-NOM,
                   prenom = :WS-PRENOM,
                   telephone = :WS-TELEPHONE
               WHERE id = :WS-INDIVIDU-ID
           END-EXEC.

      * Verification et validation de la modification
           IF SQLCODE = 0
               DISPLAY "Modification reussie."
               EXEC SQL COMMIT END-EXEC
           ELSE
               DISPLAY "Erreur de modification SQLCODE: " SQLCODE
           END-IF.

      * Relecture et affichage des donnees modifiees
           EXEC SQL 
               SELECT nom, prenom, telephone
               INTO :WS-NOM, :WS-PRENOM, :WS-TELEPHONE
               FROM individus
               WHERE id = :WS-INDIVIDU-ID
           END-EXEC.

           IF SQLCODE = 0
               DISPLAY "------------------------------------"
               DISPLAY "********* Individu modifie *********"
               DISPLAY "------------------------------------"
               DISPLAY "Nom : " WS-NOM
               DISPLAY "Prenom : " WS-PRENOM
               DISPLAY "Telephone : " WS-TELEPHONE
               DISPLAY "------------------------------------"
           ELSE
               DISPLAY "Erreur de lecture SQLCODE: " SQLCODE
           END-IF.

           PERFORM 0000-MENU-DEB
              THRU 0000-MENU-FIN.

       1020-MODIFIER-FIN.
           EXIT.

      ******************************************************************
      * DELETE - Suppression d'un individu                             *
      ******************************************************************
       1030-SUPPRIMER-DEB.

      * Selection de l'individu à supprimer
           DISPLAY "Entrez le numero d'ID de l'individu à supprimer".    
           ACCEPT WS-INDIVIDU-ID.

      * Lecture des donnees avant suppression
           EXEC SQL 
               SELECT nom, prenom, telephone
               INTO :WS-NOM, :WS-PRENOM, :WS-TELEPHONE
               FROM individus
               WHERE id = :WS-INDIVIDU-ID
           END-EXEC.

      * Affichage des donnéees à supprimer
           IF SQLCODE = 0
               DISPLAY "------------------------------------"
               DISPLAY "Individu que vous souhaitez supprimer"
               DISPLAY "------------------------------------"
               DISPLAY "Nom : " WS-NOM
               DISPLAY "Prenom : " WS-PRENOM
               DISPLAY "Telephone : " WS-TELEPHONE
               DISPLAY "------------------------------------"
           ELSE
               DISPLAY "Erreur de lecture SQLCODE: " SQLCODE
               PERFORM 0000-MENU-DEB
                  THRU 0000-MENU-FIN
           END-IF.

      * Demande de confirmation avant suppression
           DISPLAY "Voulez-vous continuer la suppression ?"
           DISPLAY "(Oui = 1 | Non = 2)"
           ACCEPT WS-STOP.
           
      * Exécution de la suppression si confirmee
           IF WS-STOP = 1
               EXEC SQL 
                   DELETE FROM individus
                   WHERE id = :WS-INDIVIDU-ID
               END-EXEC

               IF SQLCODE = 0
                   DISPLAY "------------------------------------"
                   DISPLAY "********* Individu supprime ********"
                   DISPLAY "------------------------------------"
                   EXEC SQL COMMIT END-EXEC
               ELSE
                   DISPLAY "Erreur de suppression SQLCODE: " SQLCODE
               END-IF
           ELSE 
               DISPLAY "Retour au menu"
           END-IF.

           PERFORM 0000-MENU-DEB
              THRU 0000-MENU-FIN.              

       1030-SUPPRIMER-FIN.
           EXIT.

      ******************************************************************
      * READ - Affichage des details d'un individu                     *
      ******************************************************************
       1040-AFFICHER-DEB.

      * Selection de l'individu à afficher
           DISPLAY "Entrez le numero d'ID de l'individu à selectionner".
           ACCEPT WS-INDIVIDU-ID.

      * Lecture des donnees
           EXEC SQL 
               SELECT nom, prenom, telephone
               INTO :WS-NOM, :WS-PRENOM, :WS-TELEPHONE
               FROM individus
               WHERE id = :WS-INDIVIDU-ID
           END-EXEC.

      * Affichage des resultats
           IF SQLCODE = 0
               DISPLAY "Lecture reussie."
               DISPLAY "Nom : " WS-NOM
               DISPLAY "Prenom : " WS-PRENOM
               DISPLAY "Telephone : " WS-TELEPHONE
           ELSE
               DISPLAY "Erreur de lecture SQLCODE: " SQLCODE
           END-IF.

           PERFORM 0000-MENU-DEB
              THRU 0000-MENU-FIN.

       1040-AFFICHER-FIN.
           EXIT.

      ******************************************************************
      * === 8000 === MODULE D'AFFICHAGE CONSOLE                        *
      * Presentation structuree des donnees                            *
      ******************************************************************

       8000-AFFICHAGE-MENU-DEB.
           DISPLAY "****************************************".
           DISPLAY "*        MENU PRINCIPAL CRUD           *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* 1 - Ajouter un individu              *".
           DISPLAY "* 2 - Modifier un individu             *".
           DISPLAY "* 3 - Supprimer un individu            *".
           DISPLAY "* 4 - Afficher un individu             *".
           DISPLAY "* 5 - Quitter le programme             *".
           DISPLAY "*                                      *".
           DISPLAY "****************************************".
           DISPLAY "Entrez un choix : " WITH NO ADVANCING.
           ACCEPT WS-CHOIX.
       8000-AFFICHAGE-MENU-FIN.
           EXIT.

      ******************************************************************
      * === 9000 === MODULES DE TERMINAISON DU PROGRAMME               *
      * Gestion des fins normales et anormales d'exécution             *
      ******************************************************************

       9999-FIN-NORMALE-PROGRAMME-DEB.
      *----------------------------------------------------------------*
      * Procedure de fin normale du programme                          *
      * - Affiche un bandeau de fin normale                            *
      * - Termine le programme avec un code retour de succes           *
      *----------------------------------------------------------------*
      * Affichage du bandeau de fin normale
           DISPLAY "****************************************".
           DISPLAY "*        FIN NORMALE PROGRAMME         *".
           DISPLAY "****************************************".
           DISPLAY "*                                      *".
           DISPLAY "* Le programme s'est termine           *".
           DISPLAY "* correctement                         *".
           DISPLAY "*                                      *".  
           DISPLAY "****************************************".
      * Terminaison normale avec code retour 0
           STOP RUN.
       9999-FIN-NORMALE-PROGRAMME-FIN.
           EXIT.
