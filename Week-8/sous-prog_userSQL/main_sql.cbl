      ******************************************************************
      * PROGRAMME : MAIN_SQL - GESTION D'INDIVIDUS                         *
      ******************************************************************
      * AUTEUR       : Vincent-cmd1                                    *
      * DATE CREATION: 12/06/2025                                      *
      * MODIFICATION : 12/06/2025                                      *
      *                                                                *
      * RESUME :                                                       *
      *                                                                *
      * FONCTIONNALITES :                                              *

      * BASE DE DONNEES :                                              *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. main_sql.
       AUTHOR. Vincent-cmd1.
       DATE-WRITTEN. 2025-06-12.

      ****************************************************************** 
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * Section SQL - Variables hotes pour communication avec la BD
      *----------------------------------------------------------------*
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-USER-DATA.
           05 WS-ID-USER                    PIC X(10).
           05 WS-NOM-USER                   PIC X(50).
           05 WS-PASSWORD-USER-TEMP         PIC X(50).
           05 WS-PASSWORD-USER              PIC X(50).
       01  USERNAME           PIC X(30) VALUE "vincent-cmd1".
       01  PASSWD             PIC X(30) VALUE "13051994".
       01  DBNAME             PIC X(12) VALUE "exercice_sql".
       EXEC SQL END DECLARE SECTION END-EXEC.

      * Inclusion de la zone de communication SQL
       EXEC SQL INCLUDE SQLCA END-EXEC.


       PROCEDURE DIVISION.
    

      * Programme principal - Sequence d'exécution
           PERFORM 0000-INTITIALISATION-DEB
              THRU 0000-INTITIALISATION-FIN.

           PERFORM 1000-INSERTION-DEB
              THRU 1000-INSERTION-FIN.    

           PERFORM 9999-FIN-NORMALE-PROGRAMME-DEB
              THRU 9999-FIN-NORMALE-PROGRAMME-FIN.

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
      * === 1000 === MODULES DE D'INSERTION D'UTILISATEUR              *
      * Appel d'un sous-programme pour executer l'action               *
      ****************************************************************** 

       1000-INSERTION-DEB.

      * Appel du sous-programme d'insertion
           DISPLAY "Début de l'insertion...".
           CALL 'insert1' USING WS-USER-DATA.

       1000-INSERTION-FIN.
           EXIT.


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
    