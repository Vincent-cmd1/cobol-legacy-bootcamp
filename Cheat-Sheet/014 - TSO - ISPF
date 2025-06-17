# Cheat-Sheet TSO/ISPF

---

## 🏗️ Architecture Mainframe IBM

### Évolution historique
- **MVS** (1974) → **OS/390** → **z/OS** (actuel)
- MVS = Multiple Virtual Storage
- Développé par IBM, jamais piraté, extrêmement robuste
- Coût élevé réservé aux grandes entreprises
- Performance mesurée en MIPS (Millions d'Instructions Par Seconde)

### Composants clés
- **JES2/JES3** : Job Entry Subsystem
- **TSO** : Time Sharing Option
- **ISPF** : Interactive System Productivity Facility
- **Catalogue** : Référence les datasets (crucial pour JCL)

---

## 📁 Types de Datasets

| Type | Description |
|------|-------------|
| **PS** | Sequential (fichier séquentiel) |
| **PDS** | Partitioned Dataset (comme un dossier Windows) |
| **SYSOUT** | Sortie imprimante/écran (rouge = erreur) |
| **SYSIN** | Données d'entrée pour JCL |
| **LOADLIB** | Bibliothèque programmes compilés |
| **WORK** | Fichiers temporaires |
| **VSAM** | Virtual Storage Access Method (accès indexé) |

---

## 🖥️ TSO - Time Sharing Option

### Présentation
- Interface en ligne de commande pour z/OS
- Permet le travail multi-utilisateurs simultané
- Accès via émulateur 3270
- Base pour l'exécution d'ISPF

### Commandes TSO essentielles

| Commande | Fonction |
|----------|----------|
| `SUBMIT` | Soumettre un job JCL |
| `LISTCAT` | Lister un dataset catalogué |
| `DELETE` | Supprimer un dataset |
| `ALLOCATE` | Créer un nouveau dataset |
| `RENAME` | Renommer un fichier |
| `SEND` | Envoyer un message utilisateur |
| `HELP` | Aide sur les commandes |
| `PROFILE` | Paramètres utilisateur |

### Commandes de session
- `=X` ou `LOGOFF` : Quitter la session
- `F3` : Retour/Sortie

---

## 🎯 ISPF - Interactive System Productivity Facility

### Fonctionnalités principales
- Interface menu pour édition COBOL/JCL
- Gestion de fichiers (option 3.4)
- Soumission et suivi de jobs
- Utilisation de raccourcis (=3.4, =6, etc.)

### Navigation ISPF
- **F3** : Retour
- **F7/F8** : Haut/Bas
- **=3.4** : Gestion datasets
- **=6** : Commandes TSO
- **SDSF** : Suivi jobs batch

---

## ✏️ Commandes d'édition ISPF

### Commandes de ligne

| Commande | Fonction |
|----------|----------|
| `I` | Insertion d'une ligne |
| `I5` | Insertion de 5 lignes |
| `D` | Suppression d'une ligne |
| `D3` | Suppression de 3 lignes |
| `DD` | Suppression bloc (entre 2 DD) |
| `C` | Copie d'une ligne |
| `CC` | Copie bloc (entre 2 CC) |
| `M` | Déplacement d'une ligne |
| `MM` | Déplacement bloc (entre 2 MM) |
| `R` | Répétition d'une ligne |
| `RR` | Répétition bloc (entre 2 RR) |

### Positionnement après copie/déplacement
- `A` : After (après une ligne)
- `B` : Before (avant une ligne)
- `O` : Override (sur une ligne existante)

### Décalage horizontal
- `)` : Décalage 2 colonnes à droite
- `)5` : Décalage 5 colonnes à droite
- `))` : Décalage bloc 2 colonnes à droite
- `(` : Décalage à gauche
- `COLS` : Afficher règle colonnes
- `RESET` : Effacer règle colonnes

---

## ⚠️ Codes d'erreur courants

### ABEND (Abnormal End)
- **S0C7** : Données alphanumériques dans champ numérique
- **S0CB** : Division par zéro
- **0C4** : Protection de mémoire
- **806** : Programme non trouvé
- **813** : Dataset non trouvé

### Analyse des erreurs
- Toujours lire les messages retournés
- Vérifier SYSOUT pour diagnostics
- Codes retour JES importants

---

## 🔧 Utilitaires système

| Utilitaire | Usage |
|------------|-------|
| **SDSF** | System Display and Search Facility (suivi jobs) |
| **IDCAMS** | Gestion fichiers VSAM |
| **IEFBR14** | Programme "vide" pour tests JCL |
| **SORT** | Tri de données |
| **IEBGENER** | Copie de fichiers |

---

## 💡 Bonnes pratiques

### Organisation du travail
1. **MVS** → **TSO** → **ISPF** (ordre logique)
2. Utiliser ISPF pour l'édition quotidienne
3. Maîtriser TSO pour l'automatisation
4. Comprendre les datasets et catalogues

### Productivité ISPF
- Apprendre par cœur les raccourcis F3, F7, F8
- Utiliser les commandes de ligne pour l'édition rapide
- Exploiter SDSF pour le débogage
- Organiser ses datasets de façon logique

### Sécurité et fiabilité
- Toujours vérifier les codes retour
- Sauvegarder régulièrement son travail
- Utiliser des datasets WORK pour les tests
- Respecter les conventions de nommage

---

## 🎯 Points clés à retenir

> **MVS est la colonne vertébrale des environnements COBOL/JCL**

- **TSO** = Interface de base incontournable
- **ISPF** = Environnement de développement principal
- **Maîtriser les deux** = Autonomie totale sur mainframe
- **Robustesse** = Millions de transactions sans faille
- **Coût élevé** mais **fiabilité maximale**
