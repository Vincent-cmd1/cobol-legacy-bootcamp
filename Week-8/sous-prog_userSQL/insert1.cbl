       IDENTIFICATION DIVISION.
       PROGRAM-ID. insert1.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       
       01  WS-USER-DATA.
           05 WS-ID-USER                    PIC X(10).
           05 WS-NOM-USER                   PIC X(50).
           05 WS-PASSWORD-USER-TEMP         PIC X(50).
           05 WS-PASSWORD-USER              PIC X(50).

      * Inclusion de la zone de communication SQL
       EXEC SQL INCLUDE SQLCA END-EXEC.

       LINKAGE SECTION.
       01  LK-USER-DATA.
           05 LK-ID-USER          PIC X(10).
           05 LK-NOM-USER         PIC X(50).
           05 LK-PASSWORD-USER    PIC X(50).

       PROCEDURE DIVISION USING LK-USER-DATA.

      * Saisie des informations du nouvel individu
           DISPLAY "Entrez un ID : ".
           ACCEPT WS-ID-USER .
           DISPLAY "Entrez le nom de l'utilisateur : ".
           ACCEPT WS-NOM-USER.
           DISPLAY "Entrez le mot de passe : ".
           ACCEPT WS-PASSWORD-USER-TEMP.

           MOVE FUNCTION TRIM(WS-PASSWORD-USER-TEMP) 
                                       TO WS-PASSWORD-USER.
           DISPLAY FUNCTION LENGTH(WS-PASSWORD-USER).

      * Insertion dans la base de données 
           EXEC SQL
               INSERT INTO USERS (ID_USER, NOM, PASSWORD)
               VALUES (:WS-ID-USER , :WS-NOM-USER, :WS-PASSWORD-USER)
           END-EXEC.   

      * Verification du resultat et validation
           IF SQLCODE = 0
               DISPLAY "Insertion reussie."
               DISPLAY "------------------------------------"
               DISPLAY "Utilisateur que vous avez inséré    "
               DISPLAY "------------------------------------"
               DISPLAY "ID : " WS-ID-USER 
               DISPLAY "Nom : "WS-NOM-USER
               DISPLAY "Mot de Passe : "WS-PASSWORD-USER
               DISPLAY "------------------------------------"
           ELSE
               DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
           END-IF.

           EXEC SQL COMMIT END-EXEC.

           MOVE WS-ID-USER TO LK-ID-USER
           MOVE WS-NOM-USER TO LK-NOM-USER
           MOVE WS-PASSWORD-USER TO LK-PASSWORD-USER.

       END PROGRAM insert1.
