       IDENTIFICATION DIVISION.    
      * Déclaration de l'identité du programme, ici nommé "HELLO".
       PROGRAM-ID. HELLO.
       AUTHOR. Vincent-cmd1.

      *> ---------------------------------------------------------------
      *> PROGRAMME : HELLO
      *> OBJET     : Demande à l'utilisateur de saisir son nom et prénom,
      *>             puis affiche un message de bienvenue personnalisé.
      *> ---------------------------------------------------------------

       DATA DIVISION.
      * Début de la section des variables temporaires.
       WORKING-STORAGE SECTION.         
      * Zone pour stocker le NOM (jusqu’à 12 caractères).
       01 WS-NAME            PIC X(12).
      * Zone pour stocker le PRÉNOM (aussi 12 caractères max).
       01 WS-SURNAME         PIC X(12).

       PROCEDURE DIVISION.              
      * Début de la logique du programme, le "cerveau" de l’affaire.
      * Affiche un message demandant le nom.
           DISPLAY "Veuillez saisir votre NOM".
      * Capture la saisie de l’utilisateur dans WS-NAME.
           ACCEPT WS-NAME.                         

      * Affiche un message demandant le prénom.
           DISPLAY "Veuillez saisir votre PRENOM". 
      * Capture la saisie dans WS-SURNAME.
           ACCEPT WS-SURNAME.                    

           DISPLAY "Bonjour " FUNCTION TRIM (WS-SURNAME) " " 
                              FUNCTION TRIM (WS-NAME) 
      * Affiche un message de salutation complet, en supprimant les blancs inutiles.
                   " Comment allez-vous ?".      

      * Fin du programme. Rideau.
           STOP RUN.                 

