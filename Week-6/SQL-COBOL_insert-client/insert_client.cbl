       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT-CLIENT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  CLIENT-NOM     PIC X(100).
       01  CLIENT-PRENOM  PIC X(100).
       01  CLIENT-EMAIL   PIC X(100).
       01  USERNAME       PIC X(30) VALUE "vincent-cmd1".
       01  PASSWD         PIC X(30) VALUE "13051994".
       01  DBNAME         PIC X(10) VALUE "testdb".
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.
       DISPLAY "Connexion à la base de données...".
       EXEC SQL
            CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
       END-EXEC.
       IF SQLCODE NOT = 0
           DISPLAY "Erreur de connexion SQLCODE: " SQLCODE
           STOP RUN
       END-IF.

       DISPLAY "Entrez le nom du client : ".
       ACCEPT CLIENT-NOM.
       DISPLAY "Entrez le prénom du client : ".
       ACCEPT CLIENT-PRENOM.
       DISPLAY "Entrez l'email du client : ".
       ACCEPT CLIENT-EMAIL.

       EXEC SQL
            INSERT INTO clients (nom, prenom, email)
            VALUES (:CLIENT-NOM, :CLIENT-PRENOM, :CLIENT-EMAIL)
       END-EXEC.
       IF SQLCODE = 0
           DISPLAY "Insertion réussie."
       ELSE
           DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
       END-IF.

       EXEC SQL COMMIT END-EXEC.
       STOP RUN.


       