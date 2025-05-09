# Objectifs journaliers MS COBOL P4


### Vendredi 9 Mai

## IBM Z Xplore – Réalisé ses premiers modules  

## 🧱 Fundamentals Level

Les modules du niveau **Fundamentals** introduisent les **bases de l’environnement IBM Z** : outils, concepts de fichiers, langages de script, interfaces

[Site IBM Z Xplore](https://ibmzxplore.influitive.com/forum/)
---

### 🔧 VSC1 (VS Code 1)

* **Objectif** : Découverte de VS Code et de l’extension Zowe Explorer pour IBM Z.
  * Installation de Zowe Explorer
  * Connexion au mainframe z/OS
  * Navigation dans les datasets, soumission de jobs
* **Utilité** : Se familiariser avec les outils modernes d'interaction avec un mainframe.

---

### 📁 Files

* **Objectif** : Comprendre la gestion des données (datasets et PDS).
* **Utilité** : Base essentielle pour toute tâche sur IBM Z.

---

### 📄 JCL1 (Job Control Language 1)

* **Objectif** : Introduction au langage JCL.
  * Écriture et soumission de JCL simple
  * Instructions clés : `//JOB`, `//DD`, etc.
  * Interprétation des résultats (ex. : `CC 0000`)
* **Utilité** : Indispensable pour automatiser les processus sur mainframe.

---

### 💻 USS - Unix System Services

* **Objectif** : Découverte de l’environnement Unix sur z/OS.
* **Utilité** : Permet d’exécuter des apps modernes (Java, Python) sur z/OS.

---

## Renforcer vos compétences en COBOL à travers une série d'exercices pratiques. 

#### **Exercice 1 : Saisie et affichage formaté de notes avec FILLER**


* [ ] Demander à l’utilisateur de saisir une série de notes (de 0 à 99) et leurs dates associées (au format JJ/MM/AAAA), puis afficher les données avec une mise en forme à l’aide de FILLER.

- Stocke les notes et les dates dans une table avec un OCCURS.
- Utilise des champs FILLER pour formater l’affichage de chaque ligne.
- Limite la saisie à 100 enregistrements, avec une option d’arrêt anticipée (ex. : note = 99).
- Affiche chaque note avec sa date dans une ligne formatée de type : `Note : 87 le 06/05/2025`.

---


#### **(Bonus) Exercice 2 : Manipulation de données dans une table**


- [ ] Écrire un programme pour gérer une table de données contenant des informations sur des personnes. Le programme doit :

1. Transférer des données depuis une chaîne pré-définie.
2. Afficher toutes les informations pour chaque personne dans la table.
3. Afficher les informations pour trois personnes spécifiques.

* Utilise la clause `OCCURS` pour déclarer la table des personnes.
* Utilise `PERFORM VARYING` pour parcourir la table et afficher les données.
* Utilise l’indexation pour extraire des informations spécifiques.

---

