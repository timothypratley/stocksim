(ns stocksim.data
  (:require [clj-http.client :as client]
            [clj-time.core :as t]
            [clj-time.format :as f])
  (:use [clojure-csv.core]))


(defn get-table
  [sym]
  ; TODO: is there a way to query only the Date and Adj Close?
  (->> (client/get "http://ichart.finance.yahoo.com/table.csv"
                   {:query-params {:s sym
                                   :ignore ".csv"}})
       :body
       parse-csv))

(def custom-formatter (f/formatter "yyyy-MM-dd"))

(defn to-milliseconds-from-epoch
  [x]
  (t/in-msecs (t/interval (t/epoch) (f/parse custom-formatter x))))

(defn series
  [sym]
  (map #(vector (to-milliseconds-from-epoch (first %)) (Double/parseDouble (last %)))
       (rest (get-table sym))))

(series "^GSPC")
