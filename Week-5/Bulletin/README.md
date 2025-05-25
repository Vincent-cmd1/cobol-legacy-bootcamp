# 📄 Documentation Technique - Programme bulletin-gen

## Vue d'ensemble

Le programme `bulletin-gen` est une application COBOL développée pour automatiser la génération de bulletins de notes étudiants. Il traite des données brutes provenant d'un fichier séquentiel pour produire un bulletin formaté avec calcul automatique des moyennes pondérées.

### 📌 Informations générales
- **Auteur** : Vincent-Cmd1
- **Date de création** : 21/05/2025
- **Dernière mise à jour** : 25/05/2025
- **Langage** : COBOL (Standard industriel)
- **Type** : Application batch de traitement de données

 ## 🧱 Architecture du programme

 ### 🧩 Structure modulaire

Le programme suit une architecture modulaire classique COBOL avec une séparation claire des responsabilités :

```
PROGRAMME PRINCIPAL
├── 1000 - Initialisation
├── 2000 - Lecture et stockage des données
├── 3000 - Calcul des moyennes
├── 6320 - Génération du bulletin
└── 5000 - Finalisation
```

### 🔖 Divisions COBOL

#### IDENTIFICATION DIVISION
Définit l'identité du programme et son auteur.

#### ENVIRONMENT DIVISION
- **Configuration** : Utilisation de la virgule comme séparateur décimal
- **Fichiers** :
  - `F-INPUT` : Fichier d'entrée (`input.txt`) - Accès séquentiel
  - `F-OUTPUT` : Fichier de sortie (`output.dat`) - Accès séquentiel

#### DATA DIVISION
Structure de données hiérarchique optimisée pour le traitement batch :

**File Section** :
- Enregistrements d'entrée à longueur variable (2-1000 caractères)
- Enregistrements de sortie fixes (100 caractères)

**Working-Storage Section** :
- Table multidimensionnelle pour 999 étudiants maximum
- 20 cours maximum par étudiant
- Variables de contrôle et de calcul
- Variables d'édition importées via `COPY bulletin-ed`

## 🔄 Logique de traitement

### 1️⃣ Phase d'initialisation (Module 1000)

```cobol
1000-INIT-DEB
├── Ouverture fichier d'entrée (6010)
├── Ouverture fichier de sortie (6020)
└── Initialisation des compteurs
```

**Choix techniques** :
- Ouverture sécurisée avec contrôle d'erreurs
- Initialisation explicite des compteurs pour éviter les valeurs résiduelles

### 2️⃣ Phase de lecture et stockage (Module 2000)

Le programme utilise un système de codes types pour structurer les données :

- **Code 01** : Nouvel étudiant (Nom, Prénom, Âge)
- **Code 02** : Nouveau cours (Libellé, Coefficient, Note)

```cobol
Structure d'enregistrement :
[Code Type (2)] [Données (998)]
```

**Logique de traitement** :
1. Lecture séquentielle jusqu'à EOF
2. Analyse du code type via `EVALUATE`
3. Extraction des données selon la position fixe
4. Stockage dans la structure multidimensionnelle

### 3️⃣ Phase de calcul (Module 3000)

Implémentation de l'algorithme de moyenne pondérée :

```
Moyenne = Σ(Note × Coefficient) / Σ(Coefficients)
```

**Algorithme** :
1. Pour chaque étudiant :
   - Initialisation des accumulateurs
   - Calcul de la somme pondérée : `(Note × Coef) + Somme précédente`
   - Accumulation des coefficients
   - Division finale avec arrondi automatique

### 4️⃣ Phase de génération (Module 6320)

Construction du bulletin formaté en trois sections :

#### 🧾 Section Entête
- Encadrement avec astérisques
- Titre principal
- En-têtes de colonnes

#### 📊 Section Données
- Boucle sur tous les étudiants
- Construction manuelle de lignes formatées
- Positionnement précis des colonnes
- Ajout dynamique des notes

🔚 #### Section Pied de page
- Statistiques calculées dynamiquement
- Commentaires fixes
- Encadrement final

## ⚙️ Choix techniques

### 1. Gestion des fichiers
- **Contrôle d'erreurs systématique** sur toutes les opérations I/O
- **File Status** pour diagnostic précis des problèmes
- **Procédures d'arrêt d'urgence** en cas d'erreur

### 2. Structure de données
- **Table multidimensionnelle** : Optimisation mémoire vs flexibilité
- **Longueur variable** pour les enregistrements d'entrée
- **Format fixe** pour la sortie (compatibilité impression)

### 3. Calculs numériques
- **Arrondi automatique** via `COMPUTE ROUNDED`
- **Virgule décimale** configurée selon standards européens
- **Précision** : 2 décimales pour les moyennes

### 4. Formatage de sortie
- **Positionnement manuel** des colonnes pour contrôle précis
- **Construction dynamique** des lignes avec pointeur
- **Séparateurs visuels** pour améliorer la lisibilité

## 🛑 Gestion des erreurs

- **Contrôles systématiques** après chaque opération I/O
- **Messages d'erreur informatifs** avec codes de statut
- **Procédures de nettoyage** en cas d'arrêt anormal
- **Fermeture sécurisée** des fichiers dans tous les cas

## ⚠️ Limitations identifiées

### Limitations fonctionnelles
1. **Pas de validation** des formats de données d'entrée
2. **Absence de calcul** de moyennes par matière
3. **Pas de moyenne globale** de la classe

### Limitations techniques
1. **Format fixe** des données d'entrée (positions codées en dur)
2. **Pas de gestion** des noms/prénoms de longueur variable
3. **Arrêt brutal** en cas d'erreur (pas de récupération)

## ✅ Conclusion

Le programme bulletin-gen répond efficacement à son objectif : transformer des données brutes en bulletins lisibles, structurés et calculés automatiquement 📑. Grâce à une logique modulaire, un formatage maîtrisé et une rigueur dans la gestion des erreurs, il constitue une base solide pour des traitements pédagogiques automatisés.

🔧 Des améliorations futures sont envisageables (validation de données, moyennes globales, robustesse accrue), mais l'architecture actuelle est déjà opérationnelle pour des traitements batch simples et fiables.
