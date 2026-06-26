-- :name create-labels-table
-- :doc Create the labels lookup table.
CREATE TABLE labels (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(100) NOT NULL
);

-- :name create-entries-table
-- :doc Create the entries table tracking cash flow.
-- Note: SQLite does not have a native ENUM, so a CHECK constraint enforces the -1 or 1 requirement.
CREATE TABLE entries (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR(100) NOT NULL,
    amount FLOAT NOT NULL,
    direction INTEGER NOT NULL CHECK (direction IN (-1, 1)), -- -1 for Red (Out), 1 for Black (In)
    label_id INTEGER,
    FOREIGN KEY (label_id) REFERENCES labels(id) ON DELETE SET NULL
);

-- :name insert-label :insert
-- :doc Insert a new category label.
INSERT INTO labels (name) 
VALUES (:name);

-- :name insert-entry :insert
-- :doc Insert a financial entry (Red or Black).
INSERT INTO entries (name, amount, direction, label_id) 
VALUES (:name, :amount, :direction, :label_id);

-- :name get-balance :one
-- :doc Calculate the current net balance (sum of amount * direction).
SELECT TOTAL(amount * direction) AS net_balance 
FROM entries;

-- :name get-all-entries :many
-- :doc Fetch all entries with their associated labels.
SELECT e.id, e.name, e.amount, e.direction, l.name AS label_name
FROM entries e
LEFT JOIN labels l ON e.label_id = l.id;