# 📚 Semaine 3 – Formation MS COBOL P4 chez Simplon

Bienvenue dans le dossier `WEEK-3`, regroupant les travaux de la troisième semaine de formation COBOL. Cette semaine, on a joué avec les **tableaux**, les **chaînes de caractères**, les **algorithmes classiques** (PGCD, Fibonacci, etc.), et même le **screen section** pour un peu d’interaction à l'ancienne.

---

## 🗓 Suivi journalier

### 🧠 Lundi 5 mai – Introduction aux tableaux

- Présentation de programmes
- Discussion sur l’utilité des tableaux (`OCCURS`)
- Déclaration de tableaux unidimensionnels et multidimensionnels
- Exercices de base avec `PERFORM` et `OCCURS`

### 🔁 Mardi 6 mai – Conditions + Algorithmes simples

- Plus grand parmi 3 nombres (avec tableau)
- Inversion d’une chaîne avec `FUNCTION REVERSE`
- Année bissextile (divisibilité)
- Conjecture de Syracuse
- Calcul du PGCD (Euclide)
- Vérification d’isogramme

### 🧪 Mercredi 7 mai – Suite des exercices

- Isogramme (approfondissement)
- Nombre de pizzas à commander (avec arrondi)
- Formatage de résultats numériques

### 🖥 Vendredi 9 mai – IBM Z Xplore + Exercices bonus

- 📁 Découverte de VS Code + Zowe Explorer
- 📄 Premiers pas avec le JCL (`//JOB`, `//DD`)
- 💻 USS (Unix sur z/OS)
- 🧱 Affichage de notes et dates formatées
- 🧠 Table de personnes (transfert et affichage ciblé)

---

## 📂 Programmes COBOL présents dans ce dossier

| Nom du programme       | Description courte                                                                 |
|------------------------|--------------------------------------------------------------------------------------|
| `TBL5.cob`             | Saisie de 5 notes à virgule fixe, calcul de la moyenne avec tableau 1D             |
| `TBL3E4M.cob`          | Saisie des notes de 3 élèves pour 4 matières via tableau 2D                        |
| `VH4P.cob`             | Gestion de stock pour 2 produits, calcul de valeur résiduelle                       |
| `HIGHEST.cob`          | Détermination du plus petit nombre parmi 5 saisis                                  |
| `INVERT.cob`           | Inversion d’un mot saisi via `FUNCTION REVERSE`                                    |
| `BISSEX.cob`           | Vérifie si une année est bissextile avec plusieurs `REMAINDER`                     |
| `SYRACUSE.cob`         | Conjecture de Syracuse avec boucle `PERFORM UNTIL`                                 |
| `PGCD.cob`             | Calcul du PGCD via soustraction (algorithme d’Euclide)                             |
| `LOST.cob`             | Affiche les N premiers termes de la suite de Fibonacci                             |
| `ISOGRAM.cob`          | Vérifie si un mot est un isogramme (aucune lettre répétée)                         |
| `PIZZA.cob`            | Calcule les pizzas à commander selon le nombre d’invités, avec interface SCREEN    |


> 🧑‍🤝‍🧑 Certains exercices ont été réalisés en binôme ou en groupe : `HIGHEST`, `PGCD`, entre autres.

---

## 🧠 Consolidation & Documentation

- Mise à jour de la cheat sheet :
  - Tableaux `OCCURS`, avec ou sans `INDEXED BY`
  - Boucles `PERFORM VARYING`
  - Utilisation de `FUNCTION`, `MOD`, `REMAINDER`
  - Règles de nommage (ex : `WS-NOTE`, `WS-TABLEAU`, `WS-INDEX`)

- Révision des programmes avec édition alignée (`FILLER`, `Z(5).99` etc.)
- Ajout d’exemples SCREEN SECTION
- Documentation complémentaire sur IBM Z Xplore (modules Fundamentals)

---

## 🔗 Ressources utiles

- [IBM Z Xplore](https://ibmzxplore.influitive.com/forum/)
- [Cheat Sheet COBOL](./docs/cheat-sheet.md)

---


