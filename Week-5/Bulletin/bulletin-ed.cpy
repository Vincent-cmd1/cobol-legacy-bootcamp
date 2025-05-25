      ******************************************************************
      * BULLETIN-ED.CPY                                                *
      * Sous-programme contenant les variables d'édition               *
      * et de formatage pour le bulletin de notes                      *
      ******************************************************************

      *----------------------------------------------------------------*
      * VARIABLES D'ÉDITION ET DE FORMATAGE                            *
      * Définition des formats de sortie pour le bulletin              *
      *----------------------------------------------------------------*
       01 WS-ASTX-ED                    PIC X(100)   VALUE ALL "*".
       01 WS-TIRET-ED                   PIC X(100)   VALUE ALL "-".
       
      *----------------------------------------------------------------*
      * En-tête principal du bulletin                                  *
      *----------------------------------------------------------------*
       01 WS-LIGNE-ENTETE-1-ED.
           05 FILLER                    PIC X(42)    VALUE SPACES.
           05 WS-DESIGN-ED              PIC X(17)      
                                              VALUE "BULLETIN DE NOTES".   
           05 FILLER                    PIC X(41)    VALUE SPACES.      
      
      *----------------------------------------------------------------*
      * En-tête des colonnes du tableau de notes                       *
      *----------------------------------------------------------------*
       01 WS-LIGNE-ENTETE-2-ED.
           05 FILLER                    PIC X(15)    VALUE " NOM".
           05 FILLER                    PIC X(3)     VALUE " | ".
           05 FILLER                    PIC X(14)    VALUE "PRENOM".
           05 FILLER                    PIC X(3)     VALUE " | ".
           05 FILLER                    PIC X(7)     VALUE "MOYENNE".
           05 FILLER                    PIC X(3)     VALUE " | ".
           05 FILLER                    PIC X(06)    VALUE "Cour-1".
           05 FILLER                    PIC X(3)     VALUE " | ".
           05 FILLER                    PIC X(06)    VALUE "Cour-2".
           05 FILLER                    PIC X(3)     VALUE " | ".
           05 FILLER                    PIC X(06)    VALUE "Cour-3".
           05 FILLER                    PIC X(3)     VALUE " | ".
           05 FILLER                    PIC X(06)    VALUE "Cour-4".
           05 FILLER                    PIC X(3)     VALUE " | ".
           05 FILLER                    PIC X(06)    VALUE "Cour-5".
           05 FILLER                    PIC X(3)     VALUE " | ".
           05 FILLER                    PIC X(06)    VALUE "Cour-6".
           05 FILLER                    PIC X(3)     VALUE " | ".
       
      *----------------------------------------------------------------*
      * Zone de construction des lignes de données étudiants           *
      *----------------------------------------------------------------*
       01 WS-LIGNE-ELEVE-ED             PIC X(100).

      *----------------------------------------------------------------*
      * Descriptions des cours avec coefficients et matières           *
      *----------------------------------------------------------------*
       01 WS-LIGNE-C1                   PIC X(60)      
           VALUE "Cours-1 = Coef: 1.0 ; Matiere: Systeme d'information".
       01 WS-LIGNE-C2                   PIC X(60)      
           VALUE "Cours-2 = Coef: 1.0 ; Matiere: Programmation VBA".    
       01 WS-LIGNE-C3                   PIC X(60)      
           VALUE "Cours-3 = Coef: 2.0 ; Matiere: SQL".
       01 WS-LIGNE-C4                   PIC X(60)      
           VALUE "Cours-4 = Coef: 2.0 ; Matiere: Schema relationnel".
       01 WS-LIGNE-C5                   PIC X(60)      
           VALUE "Cours-5 = Coef: 1.0 ; Matiere: Reseaux informatiques".
       01 WS-LIGNE-C6                   PIC X(60)      
           VALUE "Cours-6 = Coef: 1.5 ; Matière: Modelisation".
  
      *----------------------------------------------------------------*
      * Libellés pour les statistiques finales                         *
      *----------------------------------------------------------------*
       01 WS-NB-ELEVES-ED               PIC X(20)    
                                           VALUE " Nombre d'eleves : ".
       01 WS-NB-COURS-ED                PIC X(20)    
                                           VALUE " Nombre de cours : ".
       01 WS-NB-NOTES-ED                PIC X(20)    
                                           VALUE " Nombre de notes : ".                                    
       01 WS-NB-ELEVES-TXT-ED           PIC X(100).
       01 WS-NB-COURS-TXT-ED            PIC X(100).
       01 WS-NB-NOTES-TXT-ED            PIC X(100).

      *----------------------------------------------------------------*
      * Pied de page du bulletin                                        *
      *----------------------------------------------------------------*
       01 WS-FIN-BULLETIN.
           05 FILLER                    PIC X(41)    VALUE SPACES.
           05 WS-FIN-ED                 PIC X(15)      
                                           VALUE "FIN DU BULLETIN".   
           05 FILLER                    PIC X(41)    VALUE SPACES.

      *----------------------------------------------------------------*
      * Variables de formatage numérique pour l'affichage              *
      *----------------------------------------------------------------*
       01 WS-S-NOM-ED                   PIC X(15).
       01 WS-S-PRENOM-ED                PIC X(14).    
       01 WS-S-MOYENNE-ED               PIC Z9,99.
       01 WS-C-NOTE-ED                  PIC Z9,99.
       01 WS-POINTER                    PIC 9(03)    VALUE 1.

      *----------------------------------------------------------------*
      * Variables additionnelles pour l'édition                        *
      *----------------------------------------------------------------*
       01 WS-VARS-EDITION.
           05 WS-LIGNE-VIDE             PIC X(100)   VALUE SPACES.
           05 WS-SEPARATEUR-SECTION     PIC X(100)   VALUE ALL "=".
           05 WS-LIGNE-TITRE-SECTION    PIC X(100).
           05 WS-COMPTEUR-LIGNE         PIC 9(03)    VALUE ZEROS.
           05 WS-NB-LIGNES-PAGE         PIC 9(03)    VALUE 50.

      ******************************************************************
      * FIN DU SOUS-PROGRAMME BULLETIN-ED                              *
      ******************************************************************
