# Objectifs journaliers MS COBOL P4

### Lundi 5 Mai

- [x] Présenter un de ses programmes aux autres apprenants
- [x] Faire un retour rapide sur les exercices de la veille
- [x] Discuter de l'intérêt des tableaux pour organiser les données répétitives

#### 📌 Introduction aux **tableaux** (OCCURS)

- [x] Comprendre la clause `OCCURS` pour définir un tableau dans la `WORKING-STORAGE SECTION`
  - [x] Syntaxe de base : `OCCURS n TIMES`
  - [x] Importance du niveau hiérarchique dans la déclaration (`01`, `05`, etc.)
- [x] Présentation des **tableaux multidimensionnels** 
  - [x] Syntaxe avec `OCCURS n TIMES` imbriqués

#### 📌 Nomenclature des variables dans les tableaux

- [x] Les noms de variables doivent rester **clairs et cohérents**
  - Exemples : `NOTE-ELEVE`, `SALAIRE-MENSUEL`, `VENTES-JOUR`
- [x] Bonne pratique : Nom + indice explicite dans le code pour la lisibilité
- [x] Convention : Les noms en COBOL sont souvent en majuscules et avec des tirets

---

### Exercices pratiques

- [x] Saisir 3 notes dans un tableau via ACCEPT, les stocker dans un tableau, puis les afficher grâce à une boucle `PERFORM`.
- [x] Saisir 5 notes dans un tableau, calculer et afficher la moyenne.
- [x] Saisir les notes de 3 élèves pour 4 matières, en utilisant un **tableau multidimensionnel** . Afficher toutes les notes.
- [x] Saisir les ventes hebdomadaires de 4 produits. Chaque produit a un nom et un prix unitaire. 
      Afficher, pour chaque produit, le nom, la quantité vendue, et le prix unitaire.
      Afficher la valeur total du stock.

- [ ] Créer un programme COBOL qui permet :
      De saisir un ensemble de notes (stockées dans un tableau).
      De naviguer dans un menu pour :
        Saisir les 10 notes      *> ---------------------------------------------------------------
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
        Afficher la moyenne des notes
        Afficher la note la plus basse
        Afficher la note la plus haute
        Quitter le programme
      Les notes sont comprise entre 0 et 20. Lagit commit -m "Ajout/modification des fichiers de Week-1" moyenne d'une note s'affiche sous format: `99V99`

- [ ] (bonus) Faire un programme gérant une ToDoList.





---

### Gestion de son apprentissage

- [x] Ajouter un exemple de tableau dans sa cheat sheet COBOL
- [x] Documenter la clause `OCCURS`, l'utilisation des indices, et les bonnes pratiques de nommage

