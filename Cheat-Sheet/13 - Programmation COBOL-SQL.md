# 📋 Cheat Sheet COBOL-SQL
*Guide de référence rapide pour la programmation COBOL avec insertion SQL*

---

## 🎯 Définitions Clés

| Terme | Description |
|-------|-------------|
| **CREATE TABLE** | Instruction SQL pour définir une nouvelle table dans la base de données |
| **INSERT INTO** | Instruction SQL pour insérer des lignes dans une table |
| **SQLSTATE / SQLCODE** | Codes de retour SQL pour la gestion d'erreurs (`SQLCODE = 0` = succès) |
| **OCESQL** | Extension GnuCOBOL permettant l'exécution de requêtes SQL via précompilation |
| **COPYBOOK** | Fichier COBOL contenant des structures de données réutilisables |
| **SQLCA** | Structure standard COBOL pour le retour des requêtes SQL (incluse via COPYBOOK) |

---

## 🏗️ Structure de la Table `clients`

```sql
CREATE TABLE clients (
    id SERIAL PRIMARY KEY,
    nom VARCHAR(100) NOT NULL,
    prenom VARCHAR(100) NOT NULL,
    email VARCHAR(100)
);
```

**Champs :**
- `id` : Clé primaire auto-incrémentée
- `nom` : Nom du client (obligatoire, max 100 caractères)
- `prenom` : Prénom du client (obligatoire, max 100 caractères)  
- `email` : Adresse email (optionnelle, max 100 caractères)

---

## 🐘 Commandes PostgreSQL Essentielles

### Configuration initiale
```bash
psql -U postgres
```

### Commandes de base
```sql
-- Créer une base de données
CREATE DATABASE testdb;

-- Se connecter à la base
\c testdb

-- Lister les tables
\dt

-- Voir le contenu d'une table
SELECT * FROM clients;

-- Quitter psql
\q
```

---

## 💻 Structure COBOL-SQL

### Template de base
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT-CLIENT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  CLIENT-NOM     PIC X(100).
       01  CLIENT-PRENOM  PIC X(100).
       01  CLIENT-EMAIL   PIC X(100).
       01  USERNAME       PIC X(30) VALUE "cobol".
       01  PASSWD         PIC X(30) VALUE "mdp".
       01  DBNAME         PIC X(10) VALUE "testdb".
       EXEC SQL END DECLARE SECTION END-EXEC.
       
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       PROCEDURE DIVISION.
```

### Connexion à la base
```cobol
       EXEC SQL
            CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
       END-EXEC.
       
       IF SQLCODE NOT = 0
           DISPLAY "Erreur de connexion SQLCODE: " SQLCODE
           STOP RUN
       END-IF.
```

### Insertion de données
```cobol
       EXEC SQL
            INSERT INTO clients (nom, prenom, email)
            VALUES (:CLIENT-NOM, :CLIENT-PRENOM, :CLIENT-EMAIL)
       END-EXEC.
       
       IF SQLCODE = 0
           DISPLAY "Insertion réussie."
       ELSE
           DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
       END-IF.
       
       EXEC SQL COMMIT END-EXEC.
```

---

## ⚙️ Compilation et Exécution

### Variables d'environnement
```bash
export COB_LDFLAGS="-Wl,--no-as-needed -lm"
```

### Étapes de compilation
```bash
# 1. Précompilation OCESQL
ocesql insert_client.cbl insert_client.cob

# 2. Compilation GnuCOBOL
cobc -x -locesql -o run insert_client.cob

# 3. Exécution
./run
```

---

## 🚨 Gestion des Erreurs

### Codes SQLCODE courants
| Code | Signification |
|------|---------------|
| `0` | Succès |
| `-1` | Erreur générale |
| `100` | Aucune donnée trouvée |

### Pattern de gestion d'erreur
```cobol
IF SQLCODE NOT = 0
    DISPLAY "Erreur SQL - Code: " SQLCODE
    DISPLAY "État SQL: " SQLSTATE
    STOP RUN
END-IF.
```

---

## 🔧 Dépannage Fréquent

### Erreur : `undefined reference to log10`
```bash
export COB_LDFLAGS="-Wl,--no-as-needed -lm"
```

### Erreur : `authentication failed`
- Vérifier `pg_hba.conf`
- Utiliser `md5` ou `scram-sha-256`
- Vérifier utilisateur/mot de passe PostgreSQL

### Erreur : `libocesql.so not found`
```bash
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
```

### Localisation du SQLCA.CPY
```
/usr/local/share/gnucobol/copy/SQLCA.CPY
```

---

## 📝 Exemple Complet d'Insertion

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSERT-CLIENT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  CLIENT-NOM     PIC X(100).
       01  CLIENT-PRENOM  PIC X(100).
       01  CLIENT-EMAIL   PIC X(100).
       01  USERNAME       PIC X(30) VALUE "cobol".
       01  PASSWD         PIC X(30) VALUE "mdp".
       01  DBNAME         PIC X(10) VALUE "testdb".
       EXEC SQL END DECLARE SECTION END-EXEC.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       
       PROCEDURE DIVISION.
       DISPLAY "Connexion à la base de données...".
       EXEC SQL
            CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
       END-EXEC.
       
       IF SQLCODE NOT = 0
           DISPLAY "Erreur de connexion SQLCODE: " SQLCODE
           STOP RUN
       END-IF.
       
       DISPLAY "Entrez le nom du client : ".
       ACCEPT CLIENT-NOM.
       DISPLAY "Entrez le prénom du client : ".
       ACCEPT CLIENT-PRENOM.
       DISPLAY "Entrez l'email du client : ".
       ACCEPT CLIENT-EMAIL.
       
       EXEC SQL
            INSERT INTO clients (nom, prenom, email)
            VALUES (:CLIENT-NOM, :CLIENT-PRENOM, :CLIENT-EMAIL)
       END-EXEC.
       
       IF SQLCODE = 0
           DISPLAY "Insertion réussie."
       ELSE
           DISPLAY "Erreur d'insertion SQLCODE: " SQLCODE
       END-IF.
       
       EXEC SQL COMMIT END-EXEC.
       STOP RUN.
```

---

## ✅ Checklist de Validation

- [ ] Base de données `testdb` créée
- [ ] Table `clients` créée avec la bonne structure
- [ ] Variables d'environnement configurées
- [ ] Programme COBOL compilé sans erreur
- [ ] Test d'insertion réussi
- [ ] Données visibles dans PostgreSQL
- [ ] Gestion d'erreurs implémentée

---

## 🎓 Objectifs Pédagogiques

- [x] Comprendre la structure d'une table PostgreSQL
- [x] Créer une table `clients` via `psql`
- [x] Écrire un programme COBOL-SQL pour l'insertion
- [x] Utiliser `ACCEPT`, `EXEC SQL` et gérer les erreurs
- [x] Maîtriser l'utilisation d'OCESQL et SQLCA
- [x] Créer une cheat sheet récapitulative

---

*💡 **Astuce** : Gardez toujours un terminal `psql` ouvert pour vérifier vos insertions en temps réel !*
