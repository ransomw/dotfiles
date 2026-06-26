(ns core
  (:require [ledger.core :as ledger]))

;; Ready for REPL interaction or entrypoint execution
(defn -main []
  (println "Checking net ledger balance...")
  (println (ledger/get-balance ledger/db-spec)))