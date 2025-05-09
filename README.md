# 🧠 COBOL Legacy Bootcamp – Mon aventure dans le monde du mainframe

Bienvenue dans `cobol-legacy-bootcamp`, mon dépôt dédié à l’apprentissage du langage **COBOL**, ce bon vieux dinosaure toujours bien vivant dans les entrailles d’IBM Z et des systèmes critiques.  
Ce dépôt contient l’ensemble des **exercices, projets, expérimentations** réalisés dans le cadre de ma formation MS COBOL chez Simplon.

---

## 🗂️ Structure du dépôt

Ce dépôt est organisé par semaine de progression :

### 📁 `WEEK-1` : Bases du COBOL
- Prise en main de la structure des programmes (`IDENTIFICATION DIVISION`, etc.)
- Instructions de base : `MOVE`, `DISPLAY`, `ACCEPT`
- Premiers exercices simples en console

### 📁 `WEEK-2` : Conditions & Boucles
- Utilisation des `IF`, `EVALUATE`, `PERFORM TIMES`, `PERFORM UNTIL`, `PERFORM VARYING`
- Programmes avec logique conditionnelle + boucles imbriquées
- Exemples : table de multiplication, todo-list, calculatrice simple

### 📁 `WEEK-3` : Tableaux et Algorithmes classiques
- Déclaration de tableaux avec `OCCURS`
- Tableaux 2D, boucles imbriquées
- Algorithmes COBOLisés : PGCD, Fibonacci, isogrammes, moyenne de notes...

### 📁 `WEEK-4+` : À venir...
- Lecture / écriture de fichiers séquentiels (avec FILE SECTION)
- Déclarations conditionnelles (`88`)
- Gestion d’états, affichage formaté (`Z`, `CR`, `BLANK WHEN ZERO`)
- Premiers tests sur IBM Z via **Zowe Explorer**

---

## 🔍 Exemple de programme

### ✅ `CALCU.cbl` – *La Cobolatrice*
> Ma calculatrice interactive ASCII en COBOL.  
Elle gère les quatre opérations, les nombres négatifs, et quelques messages sarcastiques au passage.  
Utilise une boucle `PERFORM UNTIL` pour un menu persistant, `EVALUATE` pour choisir l'opération, et une structure modulaire.

---

## 🎯 Objectifs pédagogiques

- Apprendre à écrire un code COBOL propre et modulaire
- Maîtriser les structures fondamentales du langage
- S'initier aux normes de programmation HN Institut
- Préparer le terrain pour les environnements z/OS et IBM Z

---

## 🚧 À venir

- 💾 Manipulation de fichiers (OPEN, READ, WRITE, CLOSE)
- 🧠 Utilisation avancée des variables conditionnelles (`88`)
- 📅 Manipulation de dates, formats et éditions
- 🐍 Peut-être… un Snake ASCII en COBOL (ne me tentez pas)

---

> ✍️ Rédigé par Vincent-cmd1 —  
> Accompagné (ou hanté) par **L'Esprit du COBOL™**, mentor grincheux mais redoutablement savant.
