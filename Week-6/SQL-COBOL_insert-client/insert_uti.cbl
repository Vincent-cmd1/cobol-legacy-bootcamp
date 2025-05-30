       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT_UTI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  UTI-NOM            PIC X(100).
       01  UTI-PRENOM         PIC X(100).
       01  UTI-AGE            PIC X(03).
       01  UTI-TELEPHONE      PIC X(10).
       01  USERNAME           PIC X(30) VALUE "vincent-cmd1".
       01  PASSWD             PIC X(30) VALUE "13051994".
       01  DBNAME             PIC X(10) VALUE "testdb".
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

       DISPLAY "Entrez le nom de l'utilisateur : ".
       ACCEPT UTI-NOM.
       DISPLAY "Entrez le prénom de l'utilisateur : ".
       ACCEPT UTI-PRENOM.
       DISPLAY "Entrez l'âge de l'utilisateur : ".
       ACCEPT UTI-AGE.
       DISPLAY "Entrez le numéro de telephone (10 chiffres) : ".
       ACCEPT UTI-TELEPHONE.

       EXEC SQL
            INSERT INTO utilisateur (nom, prenom, age, telephone)
            VALUES (:UTI-NOM, :UTI-PRENOM, :UTI-AGE, :UTI-TELEPHONE)
       END-EXEC.

       IF SQLCODE = 0
           DISPLAY "Insertion réussie."
       ELSE
           DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
       END-IF.

       EXEC SQL COMMIT END-EXEC.
       STOP RUN.


       