(ns ledger.core
  (:require [hugsql.core :as hugsql]
            [hugsql.adapter.next-jdbc :as hugsql-adapter]
            [next.jdbc :as jdbc]))

;; Example usage with a db-spec map:
;; (insert-entry db-spec {:name "Coffee" :amount 4.50 :direction -1 :label_id 1})

;; Set HugSQL to use next.jdbc adapter
(hugsql/set-adapter! (hugsql-adapter/hugsql-adapter-next-jdbc))

;; Automatically generate clojure functions from the SQL script
(hugsql/def-db-fns "ledger/hugsql/finance.sql")

(def db-spec {:dbtype "sqlite" :dbname "finance.db"})


(defn initialize-db! []
  (try
    (create-labels-table db-spec)
    (create-entries-table db-spec)
    (println "Database initialized successfully.")
    (catch Exception e
      ;; Captures the exception if tables already exist
      (println "Database already initialized or accessible."))))

