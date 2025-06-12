      ******************************************************************
      * PROGRAMME : MAIN                                               *
      * AUTEUR   : Vincent                                             *
      * OBJET    : Appel de 2 sous-programmes :                        *
      *            - 'greeting' pour générer une salutation            *
      *            - 'count-chars' pour compter les caractères         *
      * DATE     : 10/05/2025                                          *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Zone contenant le nom à traiter
       01  WS-NOM        PIC X(20) VALUE "OSS 117".

      * Zone pour récupérer la réponse du sous-programme 'greeting'
       01  WS-REPONSE    PIC X(30).

      * Zone pour stocker le nombre de caractères du nom
       01  WS-COUNT      PIC 9(02).

       PROCEDURE DIVISION.

      * Appel du programme 'greeting' avec le nom et 
      * récupération du message
           CALL 'greeting'    USING WS-NOM WS-REPONSE.
           DISPLAY WS-REPONSE.

      * Appel du programme 'count-chars' pour compter les caractères
           CALL 'count-chars' USING WS-NOM WS-COUNT.
           DISPLAY "Nombre de caractères dans le nom : "  WS-COUNT.

      * Fin normale du programme
           STOP RUN.


