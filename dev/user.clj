(ns user
  (:require [integrant.repl :refer [clear go halt prep init reset reset-all]]
            [hundredrps.core :refer [get-config]]))

(integrant.repl/set-prep! #(get-config))
