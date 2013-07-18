(ns stocksim.data
  (:require [clj-http.client :as client])
  (:use [clojure-csv.core]
        [clj-time.coerce]
        [clj-time.format]))


(defn get-table
  [sym]
  ; TODO: is there a way to query only the Date and Adj Close?
  (->> (client/get "http://ichart.finance.yahoo.com/table.csv"
                   {:query-params {:s sym
                                   :ignore ".csv"}})
       :body
       parse-csv))

(def custom-formatter (formatter "yyyy-MM-dd"))

(defn series
  [sym]
  (map #(vector (to-long (parse custom-formatter (first %))) (Double/parseDouble (last %)))
       (rest (get-table sym))))


(def t (get-table "^GSPC"))
(filter #(<= (to-long (parse custom-formatter %)) 0) (map first (rest t)))

(filter #(<= % 0) (map first (series "^GSPC")))
