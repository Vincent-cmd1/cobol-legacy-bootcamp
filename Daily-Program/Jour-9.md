# Objectifs journaliers MS COBOL P4

### Lundi 5 Mai

- [ ] Présenter un de ses programmes aux autres apprenants
- [ ] Faire un retour rapide sur les exercices de la veille
- [ ] Discuter de l'intérêt des tableaux pour organiser les données répétitives

#### 📌 Introduction aux **tableaux** (OCCURS)

- [ ] Comprendre la clause `OCCURS` pour définir un tableau dans la `WORKING-STORAGE SECTION`
  - [ ] Syntaxe de base : `OCCURS n TIMES`
  - [ ] Importance du niveau hiérarchique dans la déclaration (`01`, `05`, etc.)
- [ ] Présentation des **tableaux multidimensionnels** 
  - [ ] Syntaxe avec `OCCURS n TIMES` imbriqués

#### 📌 Nomenclature des variables dans les tableaux

- [ ] Les noms de variables doivent rester **clairs et cohérents**
  - Exemples : `NOTE-ELEVE`, `SALAIRE-MENSUEL`, `VENTES-JOUR`
- [ ] Bonne pratique : Nom + indice explicite dans le code pour la lisibilité
- [ ] Convention : Les noms en COBOL sont souvent en majuscules et avec des tirets

---

### Exercices pratiques

- [ ] Saisir 3 notes dans un tableau via ACCEPT, les stocker dans un tableau, puis les afficher grâce à une boucle `PERFORM`.
- [ ] Saisir 5 notes dans un tableau, calculer et afficher la moyenne.
- [ ] Saisir les notes de 3 élèves pour 4 matières, en utilisant un **tableau multidimensionnel** . Afficher toutes les notes.
- [ ] Saisir les ventes hebdomadaires de 4 produits. Chaque produit a un nom et un prix unitaire. 
      Afficher, pour chaque produit, le nom, la quantité vendue, et le prix unitaire.
      Afficher la valeur total du stock.

- [ ] Créer un programme COBOL qui permet :
      De saisir un ensemble de notes (stockées dans un tableau).
      De naviguer dans un menu pour :
        Saisir les 10 notes
        Afficher la moyenne des notes
        Afficher la note la plus basse
        Afficher la note la plus haute
        Quitter le programme
      Les notes sont comprise entre 0 et 20. La moyenne d'une note s'affiche sous format: `99V99`

- [ ] (bonus) Faire un programme gérant une ToDoList.





---

### Gestion de son apprentissage

- [ ] Ajouter un exemple de tableau dans sa cheat sheet COBOL
- [ ] Documenter la clause `OCCURS`, l'utilisation des indices, et les bonnes pratiques de nommage

