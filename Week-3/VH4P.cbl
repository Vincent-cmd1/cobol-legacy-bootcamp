      *> ---------------------------------------------------------------
      *> PROGRAMME : VH4P
      *> OBJET     : Saisie de données pour deux produits (nom, prix, 
      *>             stock initial, quantité vendue), calcul du stock 
      *>             restant, et affichage de la valeur totale résiduelle 
      *>             du stock en euros.
      *> AUTEUR    : Vincent-cmd1
      *> REMARQUE  : 
      *>             - Les produits sont stockés dans un tableau 
      *>               (OCCURS 2 TIMES).
      *>             - Un accumulateur totalise la valeur des stocks 
      *>               restants après ventes.
      *>             - Affichage formaté avec édition numérique (Z(5).ZZ).
      *>             - Programme linéaire structuré avec PERFORM VARYING.
      *> ---------------------------------------------------------------
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VH4P.                    
       AUTHOR. Vincent-cmd1.                

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Déclaration d’un tableau de 2 produits avec leurs attributs
       01 WS-LISTE-PRODUIT.
           05 WS-PRODUIT OCCURS 2 TIMES.
               10 WS-NOM-PRODUIT        PIC X(12).     
               10 WS-PRIX-UNITAIRE      PIC 9(5)V9.   
               10 WS-STOCK-PRODUIT      PIC 9(4).      
               10 WS-VENTE-PRODUIT      PIC 9(4).     

      * Stock résiduel pour un produit après vente
       01 WS-STOCK-PROD-FINAL           PIC 9(4).

      * Accumulateur de la valeur totale du stock restant
       01 WS-STOCK-VALEUR               PIC 9(10)V9 VALUE ZERO.

      * Index utilisé pour la boucle de saisie et de traitement
       01 WS-INDEX-A                    PIC 9(1).

      * Zone d’édition pour affichage formaté du total
       01 WS-STOCK-VALEUR-ED            PIC Z(5).ZZ.

       PROCEDURE DIVISION.

      * Initialisation du total à zéro avant la boucle
           MOVE 0 TO WS-STOCK-VALEUR.

      * --------------------------------------------------------------
      * Boucle de saisie et de calcul, pour chaque produit
      * --------------------------------------------------------------
           PERFORM VARYING WS-INDEX-A FROM 1 BY 1 UNTIL WS-INDEX-A > 2

      *        Saisie des informations produit
               DISPLAY "Veuillez entrer le nom du produit :"
               ACCEPT WS-NOM-PRODUIT(WS-INDEX-A)

               DISPLAY "Veuillez entrer le prix du produit :"
               ACCEPT WS-PRIX-UNITAIRE(WS-INDEX-A)

               DISPLAY "Veuillez entrer le stock inital total :"
               ACCEPT WS-STOCK-PRODUIT(WS-INDEX-A)

               DISPLAY "Veuillez entrer la quantite de produits vendue:"
               ACCEPT WS-VENTE-PRODUIT(WS-INDEX-A)

      *        Calcul du stock restant après vente
               SUBTRACT WS-VENTE-PRODUIT(WS-INDEX-A) 
                    FROM WS-STOCK-PRODUIT(WS-INDEX-A) 
                    GIVING WS-STOCK-PROD-FINAL

      *        Calcul et accumulation de la valeur du stock restant
               COMPUTE WS-STOCK-VALEUR = WS-STOCK-VALEUR +
                    (WS-STOCK-PROD-FINAL * WS-PRIX-UNITAIRE(WS-INDEX-A))

           END-PERFORM.

      * --------------------------------------------------------------
      * Formatage et affichage du total du stock restant
      * --------------------------------------------------------------
           MOVE WS-STOCK-VALEUR TO WS-STOCK-VALEUR-ED.

           DISPLAY "Valeur total des produits en stock : " 
                   FUNCTION TRIM (WS-STOCK-VALEUR-ED, LEADING) " €".

           STOP RUN.
