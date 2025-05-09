# Objectifs journaliers MS COBOL P4

### Mardi 6 Mai

- [ ] Faire un retour rapide sur les exercices de la veille
- [ ] Discuter de l'intérêt des tableaux pour organiser les données répétitives
 

### Objectifs de la journée :
- [ ] renforcer vos compétences en COBOL à travers une série d'exercices pratiques. 
- [ ] Maîtriser les boucles et conditions
- [ ] Manipuler des tableaux avec la clause OCCURS
- [ ] Appliquer des algorithmes classiques en COBOL


#### **Exercice 1 : Trouver le plus grand nombre parmi 3 nombres**

**Estimation : 30 minutes**

- [ ] Écrire un programme qui demande à l'utilisateur de saisir trois nombres, puis qui affiche le plus grand des trois.

* Utilise une table avec la clause `OCCURS` pour stocker les nombres saisis par l'utilisateur.
* Compare chaque nombre pour déterminer lequel est le plus grand en utilisant un `IF` imbriqué.
* Utilise `PERFORM VARYING` pour itérer sur les valeurs dans la table.

---

#### **Exercice 2 : Inverser une chaîne de caractères**

**Estimation : 30 minutes**

- [ ] Demander à l'utilisateur de saisir un mot, puis inverser ce mot et afficher le résultat.

* Utilise la fonction `LENGTH()` pour obtenir la longueur de la chaîne.
* Parcours la chaîne de droite à gauche pour l'inverser.

---

#### **Exercice 3 : Vérification des années bissextiles**

**Estimation : 30 minutes**

- [ ] Demander à l'utilisateur de saisir une année et vérifier si elle est bissextile.

* Utilise `MOD()` pour tester la divisibilité par 4, 100 et 400.

---

#### **Exercice 4 : Conjecture de Syracuse**

**Estimation : 45 minutes**

- [ ] Écrire un programme qui calcule la conjecture de Syracuse à partir d’un nombre entier non nul saisi par l’utilisateur, jusqu’à ce que ce nombre atteigne 1.

* Utilise une boucle `PERFORM UNTIL` pour répéter les calculs jusqu'à ce que le nombre soit égal à 1.
* Vérifie si le nombre est pair ou impair avec l'instruction `IF` et utilise `MOD`.
* Affiche les résultats à chaque étape.

---

#### **Exercice 5 : Calcul du PGCD**

**Estimation : 30 minutes**

- [ ] Demander à l'utilisateur de saisir deux nombres entiers positifs, puis calculer et afficher leur PGCD en utilisant l'algorithme d'Euclide.

* Utilise une boucle pour diviser successivement les nombres jusqu'à ce que le reste soit nul.
* Vérifie que les nombres sont positifs avant de commencer.

---

#### **Exercice 6 : Vérifier si un mot est un isogramme**

**Estimation : 45 minutes**

- [ ] Demander à l'utilisateur de saisir un mot et vérifier s’il s'agit d'un isogramme, c’est-à-dire un mot où chaque lettre apparaît une seule fois.

* Utilise un tableau pour stocker les lettres du mot.
* Vérifie la présence de chaque lettre pour t’assurer qu'elle n'apparaît qu'une seule fois.

---

### 💡 **Conseils supplémentaires pour t'aider à avancer :**

1. **Manipulation de chaînes de caractères :**

   * Utilise `TRIM` pour enlever les espaces inutiles.
   * Utilise `LENGTH()` pour connaître la longueur d'une chaîne.

2. **Gestion des boucles :**

   * Utilise `PERFORM UNTIL` ou `PERFORM VARYING` pour parcourir des listes et itérer sur des données.

3. **Vérification des conditions :**

   * Utilise `IF`, `MOD()` et `REMAINDER` pour tester les conditions et effectuer des calculs sur les entiers.

---
