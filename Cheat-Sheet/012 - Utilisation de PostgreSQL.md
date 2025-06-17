# 🐘 Cheat-sheet PostgreSQL Complet

## 🚀 Installation et Configuration

### 1. Vérifier que PostgreSQL est installé

```bash
psql --version
```

Si non installé :

```bash
sudo apt update
sudo apt install postgresql postgresql-contrib
```

### 2. Démarrer PostgreSQL et se connecter

```bash
sudo service postgresql start
sudo -u postgres psql
```

**Autres commandes de service :**
```bash
sudo service postgresql stop     # Arrêter
sudo service postgresql restart  # Redémarrer
sudo service postgresql status   # Vérifier le statut
```

---

## 👤 Gestion des utilisateurs et bases de données

### Utilisateur PostgreSQL vs Base de données

* **Utilisateur PostgreSQL** : Compte autorisé à se connecter au serveur PostgreSQL. Par défaut, il existe un utilisateur nommé `postgres`.
* **Base de données** : Conteneur logique dans lequel sont stockées les tables, vues, fonctions, etc.

### Création d'utilisateurs

```bash
# Utilisateur simple
sudo -u postgres createuser nom_utilisateur

# Utilisateur avec droits superutilisateur
sudo -u postgres createuser --superuser nom_utilisateur

# Avec mot de passe
sudo -u postgres createuser --pwprompt nom_utilisateur
```

### Création de bases de données

```bash
# Via ligne de commande
sudo -u postgres createdb nom_base

# Assigner à un utilisateur
sudo -u postgres createdb -O nom_utilisateur nom_base
```

```sql
-- Via psql
CREATE DATABASE nom_base;
CREATE DATABASE nom_base WITH OWNER nom_utilisateur;
```
### Se connecter avec ton propre utilisateur PostgreSQL

```bash 
psql -U "ton_nom_utilisateur" -d "ta_base_de_donnees" -W
```
```
psql -U "vincent-cmd1" -d exercice_sql -W
```
* `-U` = ton nom d’utilisateur PostgreSQL
* `-d` = la base de données que tu veux utiliser
* `-W` = demande le mot de passe
---

## 🔧 Commandes psql essentielles

Ces commandes commencent par un antislash (`\`) et ne sont **pas** du SQL :

| Commande | Description |
|----------|-------------|
| `\l` | 📋 Liste toutes les bases de données |
| `\c nom_base` | 🔗 Se connecter à une base de données |
| `\dt` | 📊 Liste les tables de la base actuelle |
| `\du` | 👥 Liste les utilisateurs PostgreSQL |
| `\q` | 🚪 Quitter l'interface psql |
| `\d nom_table` | 📝 Décrit la structure d'une table |
| `\di` | 🔍 Liste les index |
| `\ds` | 📋 Liste les séquences |
| `\df` | ⚙️ Liste les fonctions |
| `\dn` | 📂 Liste les schémas |
| `\h` | ❓ Aide sur les commandes SQL (`\h SELECT`) |
| `\?` | ❓ Aide sur les commandes psql |
| `\timing` | ⏱️ Active/désactive l'affichage du temps d'exécution |

---

## 📊 Types de données courants

| Type | Description | Exemple |
|------|-------------|---------|
| `INTEGER` | Entier | `42` |
| `SERIAL` | Entier auto-incrémenté | `id SERIAL PRIMARY KEY` |
| `BIGINT` | Grand entier | `9223372036854775807` |
| `VARCHAR(n)` | Chaîne (longueur max) | `nom VARCHAR(50)` |
| `TEXT` | Chaîne illimitée | `description TEXT` |
| `CHAR(n)` | Chaîne fixe | `code CHAR(5)` |
| `BOOLEAN` | Vrai/Faux | `true`, `false` |
| `DATE` | Date | `'2024-05-26'` |
| `TIME` | Heure | `'14:30:00'` |
| `TIMESTAMP` | Date + heure | `'2024-05-26 14:00:00'` |
| `NUMERIC(x,y)` | Nombre avec décimales | `prix NUMERIC(10,2)` |
| `REAL` | Nombre flottant | `3.14159` |
| `JSON` | Données JSON | `'{"nom": "value"}'` |
| `ARRAY` | Tableau | `tags TEXT[]` |

---

## 🏗️ Création et manipulation de tables

### Exemple complet : table clients

```sql
-- 1. Créer une base de données
CREATE DATABASE exercice_sql;
\c exercice_sql  -- Se connecter à la base

-- 2. Créer une table clients
CREATE TABLE clients (
    id SERIAL PRIMARY KEY,
    nom VARCHAR(50) NOT NULL,
    prenom VARCHAR(50) NOT NULL,
    email VARCHAR(100) UNIQUE,
    telephone VARCHAR(20),
    date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    actif BOOLEAN DEFAULT true
);

-- 3. Insérer des données
INSERT INTO clients (nom, prenom, telephone, email)
VALUES 
    ('Durand', 'Sophie', '0612345678', 'sophie.durand@email.com'),
    ('Lemoine', 'Alex', '0623456789', 'alex.lemoine@email.com'),
    ('Martin', 'Lucie', '0634567890', 'lucie.martin@email.com');
```

### Modification de structure

```sql
-- Ajouter une colonne
ALTER TABLE clients ADD COLUMN age INTEGER;

-- Modifier une colonne
ALTER TABLE clients ALTER COLUMN telephone TYPE VARCHAR(25);

-- Supprimer une colonne
ALTER TABLE clients DROP COLUMN age;

-- Renommer une colonne
ALTER TABLE clients RENAME COLUMN telephone TO tel;

-- Ajouter une contrainte
ALTER TABLE clients ADD CONSTRAINT check_age CHECK (age >= 0);
```

---

## 🔍 Requêtes SELECT avancées

### Syntaxe de base

```sql
-- Sélection simple
SELECT * FROM clients;
SELECT nom, prenom FROM clients;

-- Avec condition
SELECT * FROM clients WHERE nom = 'Durand';
SELECT * FROM clients WHERE id IN (1, 3, 5);
SELECT * FROM clients WHERE nom LIKE 'Dur%';
SELECT * FROM clients WHERE nom ILIKE 'dur%';  -- Insensible à la casse

-- Tri
SELECT * FROM clients ORDER BY nom ASC, prenom DESC;

-- Limite
SELECT * FROM clients LIMIT 10 OFFSET 20;  -- Pagination
```

### Fonctions d'agrégation

```sql
-- Comptage
SELECT COUNT(*) FROM clients;
SELECT COUNT(DISTINCT nom) FROM clients;

-- Autres agrégations
SELECT AVG(age) FROM clients;
SELECT MAX(date_creation) FROM clients;
SELECT MIN(id) FROM clients;
SELECT SUM(montant) FROM commandes;

-- Groupement
SELECT nom, COUNT(*) FROM clients GROUP BY nom;
SELECT nom, COUNT(*) FROM clients GROUP BY nom HAVING COUNT(*) > 1;
```

---

## 🔄 Jointures

```sql
-- INNER JOIN (intersection)
SELECT c.nom, c.prenom, cmd.montant
FROM clients c
INNER JOIN commandes cmd ON c.id = cmd.client_id;

-- LEFT JOIN (tous les clients, même sans commandes)
SELECT c.nom, c.prenom, cmd.montant
FROM clients c
LEFT JOIN commandes cmd ON c.id = cmd.client_id;

-- RIGHT JOIN
SELECT c.nom, c.prenom, cmd.montant
FROM clients c
RIGHT JOIN commandes cmd ON c.id = cmd.client_id;

-- FULL OUTER JOIN
SELECT c.nom, c.prenom, cmd.montant
FROM clients c
FULL OUTER JOIN commandes cmd ON c.id = cmd.client_id;
```

---

## ✏️ Modification des données

### INSERT

```sql
-- Insertion simple
INSERT INTO clients (nom, prenom) VALUES ('Nouveau', 'Client');

-- Insertion multiple
INSERT INTO clients (nom, prenom) VALUES 
    ('Client1', 'Prenom1'),
    ('Client2', 'Prenom2');

-- Insertion avec retour
INSERT INTO clients (nom, prenom) VALUES ('Test', 'User') RETURNING id;
```

### UPDATE

```sql
-- Mise à jour simple
UPDATE clients SET telephone = '0611111111' WHERE id = 1;

-- Mise à jour multiple
UPDATE clients SET actif = false, date_modification = CURRENT_TIMESTAMP 
WHERE date_creation < '2023-01-01';

-- Avec jointure
UPDATE clients SET statut = 'VIP'
FROM commandes 
WHERE clients.id = commandes.client_id 
AND commandes.montant > 1000;
```

### DELETE

```sql
-- Suppression avec condition
DELETE FROM clients WHERE actif = false;

-- Suppression avec jointure
DELETE FROM clients 
USING commandes 
WHERE clients.id = commandes.client_id 
AND commandes.date < '2020-01-01';
```

---

## 🔐 Index et performances

### Création d'index

```sql
-- Index simple
CREATE INDEX idx_clients_nom ON clients(nom);

-- Index composé
CREATE INDEX idx_clients_nom_prenom ON clients(nom, prenom);

-- Index unique
CREATE UNIQUE INDEX idx_clients_email ON clients(email);

-- Index partiel
CREATE INDEX idx_clients_actifs ON clients(nom) WHERE actif = true;
```

### Analyse des performances

```sql
-- Analyser une requête
EXPLAIN SELECT * FROM clients WHERE nom = 'Durand';

-- Avec détails d'exécution
EXPLAIN ANALYZE SELECT * FROM clients WHERE nom = 'Durand';

-- Statistiques des tables
ANALYZE clients;
```

---

## 🛡️ Contraintes et sécurité

### Types de contraintes

```sql
CREATE TABLE produits (
    id SERIAL PRIMARY KEY,
    nom VARCHAR(100) NOT NULL,
    prix NUMERIC(10,2) CHECK (prix > 0),
    categorie_id INTEGER REFERENCES categories(id),
    code_produit VARCHAR(20) UNIQUE,
    date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### Gestion des droits

```sql
-- Créer un utilisateur
CREATE USER app_user WITH PASSWORD 'motdepasse';

-- Accorder des droits
GRANT SELECT, INSERT, UPDATE ON clients TO app_user;
GRANT ALL PRIVILEGES ON DATABASE ma_base TO app_user;

-- Révoquer des droits
REVOKE DELETE ON clients FROM app_user;
```

---

## 💾 Sauvegarde et restauration

### Sauvegarde

```bash
# Sauvegarder une base complète
pg_dump nom_base > sauvegarde.sql

# Sauvegarder avec compression
pg_dump -Fc nom_base > sauvegarde.dump

# Sauvegarder seulement la structure
pg_dump --schema-only nom_base > structure.sql

# Sauvegarder seulement les données
pg_dump --data-only nom_base > donnees.sql
```

### Restauration

```bash
# Restaurer depuis un fichier SQL
psql nom_base < sauvegarde.sql

# Restaurer depuis un dump compressé
pg_restore -d nom_base sauvegarde.dump

# Créer la base et restaurer
createdb nouvelle_base
pg_restore -d nouvelle_base sauvegarde.dump
```

---

## 🎯 Conseils et bonnes pratiques

### Nomenclature 📝

* **Tables** : minuscules, pluriels (`clients`, `commandes`)
* **Colonnes** : `snake_case` (`date_creation`, `code_postal`)
* **Index** : préfixe `idx_` (`idx_clients_email`)
* **Contraintes** : préfixes `pk_`, `fk_`, `uk_`, `ck_`

### Optimisation ⚡

* Utiliser `LIMIT` pour les grandes requêtes
* Créer des index sur les colonnes fréquemment utilisées dans `WHERE`
* Éviter `SELECT *` en production
* Utiliser `EXPLAIN ANALYZE` pour optimiser les requêtes lentes

### Sécurité 🔒

* Ne jamais utiliser l'utilisateur `postgres` en production
* Créer des utilisateurs spécifiques avec droits limités
* Utiliser des mots de passe forts
* Sauvegarder régulièrement

---

## 🚨 Commandes d'urgence

```sql
-- Voir les connexions actives
SELECT * FROM pg_stat_activity;

-- Tuer une connexion
SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE datname = 'ma_base';

-- Voir la taille des bases
SELECT datname, pg_size_pretty(pg_database_size(datname)) FROM pg_database;

-- Voir la taille des tables
SELECT schemaname, tablename, pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename))
FROM pg_tables ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;
```

---

*💡 Astuce : Utilisez `\timing` dans psql pour voir la durée d'exécution de vos requêtes !*
