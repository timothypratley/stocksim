(ns stocksim.data
  (:require [clj-http.client :as client]
            [clojure-csv.core :refer :all]
            [clj-time.core :as t]
            [clj-time.coerce :refer :all]
            [clj-time.format :refer :all]))


(defn get-table
  "Pull a raw timeseries from Yahoo"
  [sym]
  ; TODO: is there a way to query only the Date and Adj Close?
  (->> (client/get "http://ichart.finance.yahoo.com/table.csv"
                   {:query-params {:s sym
                                   :ignore ".csv"}})
       :body
       parse-csv))

(def custom-formatter (formatter "yyyy-MM-dd"))

(defn- parse-row [[date & fields]]
  (vector
   (to-long (parse custom-formatter date))
   (Double/parseDouble (last fields))))

(defn series
  "[Date, adjusted close] pairs ordered oldest to newest"
  [sym]
  (->> (get-table sym)
       rest  ; drop the header
       (map parse-row)
       reverse))


(def day-msec (* 24 60 60 1000))
(defn- now-seconds []
  (to-long (t/now)))

(defn trading-dates
  "Generates pesudo trading days in the future in msec since epoc"
  [days]
  (let [n (double (now-seconds))
        trade-day-fudge (/ 365.25 252)
        trade-days (iterate #(+ (* day-msec trade-day-fudge) %) n)
        trade-days (take days trade-days)]
    (map #(java.lang.Math/round %) trade-days)))
