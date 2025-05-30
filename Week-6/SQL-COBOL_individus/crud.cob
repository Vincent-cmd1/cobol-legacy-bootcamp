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
OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-NOM             PIC X(50).
       01  WS-PRENOM          PIC X(50).
       01  WS-TELEPHONE       PIC X(10).
       01  USERNAME           PIC X(30) VALUE "vincent-cmd1".
       01  PASSWD             PIC X(30) VALUE "13051994".
       01  DBNAME             PIC X(10) VALUE "testdb".
OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.

      * Inclusion de la zone de communication SQL
OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(068) VALUE "INSERT INTO individus (nom, pr"
OCESQL  &  "enom, telephone) VALUES ( $1, $2, $3 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(058) VALUE "SELECT nom, prenom, telephone "
OCESQL  &  "FROM individus WHERE id = $1".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(072) VALUE "UPDATE individus SET nom = $1,"
OCESQL  &  " prenom = $2, telephone = $3 WHERE id = $4".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(058) VALUE "SELECT nom, prenom, telephone "
OCESQL  &  "FROM individus WHERE id = $1".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0005.
OCESQL     02  FILLER PIC X(058) VALUE "SELECT nom, prenom, telephone "
OCESQL  &  "FROM individus WHERE id = $1".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0006.
OCESQL     02  FILLER PIC X(035) VALUE "DELETE FROM individus WHERE id"
OCESQL  &  " = $1".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0007.
OCESQL     02  FILLER PIC X(058) VALUE "SELECT nom, prenom, telephone "
OCESQL  &  "FROM individus WHERE id = $1".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
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
OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 10
OCESQL     END-CALL.

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
OCESQL*    EXEC SQL
OCESQL*        INSERT INTO individus (nom, prenom, telephone)
OCESQL*        VALUES (:WS-NOM, :WS-PRENOM, :WS-TELEPHONE)
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-PRENOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-TELEPHONE
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0001
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

      * Verification du resultat et validation
           IF SQLCODE = 0
               DISPLAY "Insertion reussie."
           ELSE
               DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
           END-IF.

OCESQL*    EXEC SQL COMMIT END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

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
OCESQL*    EXEC SQL 
OCESQL*        SELECT nom, prenom, telephone
OCESQL*        INTO :WS-NOM, :WS-PRENOM, :WS-TELEPHONE
OCESQL*        FROM individus
OCESQL*        WHERE id = :WS-INDIVIDU-ID
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-PRENOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-TELEPHONE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-INDIVIDU-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

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
OCESQL*    EXEC SQL 
OCESQL*        UPDATE individus
OCESQL*        SET nom = :WS-NOM,
OCESQL*            prenom = :WS-PRENOM,
OCESQL*            telephone = :WS-TELEPHONE
OCESQL*        WHERE id = :WS-INDIVIDU-ID
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-PRENOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-TELEPHONE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-INDIVIDU-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL          BY VALUE 4
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

      * Verification et validation de la modification
           IF SQLCODE = 0
               DISPLAY "Modification reussie."
OCESQL*        EXEC SQL COMMIT END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
           ELSE
               DISPLAY "Erreur de modification SQLCODE: " SQLCODE
           END-IF.

      * Relecture et affichage des donnees modifiees
OCESQL*    EXEC SQL 
OCESQL*        SELECT nom, prenom, telephone
OCESQL*        INTO :WS-NOM, :WS-PRENOM, :WS-TELEPHONE
OCESQL*        FROM individus
OCESQL*        WHERE id = :WS-INDIVIDU-ID
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-PRENOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-TELEPHONE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-INDIVIDU-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0004
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

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
OCESQL*    EXEC SQL 
OCESQL*        SELECT nom, prenom, telephone
OCESQL*        INTO :WS-NOM, :WS-PRENOM, :WS-TELEPHONE
OCESQL*        FROM individus
OCESQL*        WHERE id = :WS-INDIVIDU-ID
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-PRENOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-TELEPHONE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-INDIVIDU-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0005
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

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
OCESQL*        EXEC SQL 
OCESQL*            DELETE FROM individus
OCESQL*            WHERE id = :WS-INDIVIDU-ID
OCESQL*        END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-INDIVIDU-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0006
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

               IF SQLCODE = 0
                   DISPLAY "------------------------------------"
                   DISPLAY "********* Individu supprime ********"
                   DISPLAY "------------------------------------"
OCESQL*            EXEC SQL COMMIT END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "COMMIT" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
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
OCESQL*    EXEC SQL 
OCESQL*        SELECT nom, prenom, telephone
OCESQL*        INTO :WS-NOM, :WS-PRENOM, :WS-TELEPHONE
OCESQL*        FROM individus
OCESQL*        WHERE id = :WS-INDIVIDU-ID
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-NOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 50
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-PRENOM
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-TELEPHONE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 1
OCESQL          BY VALUE 4
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE WS-INDIVIDU-ID
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecSelectIntoOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0007
OCESQL          BY VALUE 1
OCESQL          BY VALUE 3
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL.

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
