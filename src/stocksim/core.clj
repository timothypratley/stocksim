(ns stocksim.core
  (:use [stocksim.data]
        [incanter core charts stats]))


(defn rand-step
  "Add a random number between -0.5 and 0.5"
  [price]
  (+ price (rand) -0.5))

(defn sim
  "Shows a chart of a 5 year iteration of step"
  [step]
  (let [days (* 5 252)
        prices (take days (iterate step 1))
        chart (line-chart (range days) prices)]
    (view chart)))

(sim rand-step)


; We can simulate a "normal" distribution by adding random numbers together
(defn random-normal []
  (reduce + -50 (repeatedly 100 rand)))

(view (histogram
       (repeatedly 10000 random-normal)))

; Asume market tends to go up,
; tends to change relative to current,
; day fluctuation are normally distributed
(defn normal-step
  [price]
  (let [biased (+ 0.2 (random-normal))
        scale 0.005]
    (+ price (* price biased scale))))

(sim normal-step)

; Retrieve the S&P 500 ETF prices from Yahoo
(def Ford (series "F"))
(def date (reverse (map first Ford)))
(def price (reverse (map last Ford)))
(view (time-series-plot date price))

; Store change ratios in a vector so we can index any value instead of scanning
(def changes (into []
                   (map / (rest price) price)))

(view (time-series-plot date changes))


; Average change is 0.67% per day, but max rise is 29% and fall is -25%
(mean changes)
(reduce max changes)
(reduce min changes)
(view (histogram changes :nbins 50))

(defn bootstrap-step [price]
  (* price (rand-nth changes)))

(sim bootstrap-step)


;TODO: refactor these to the one function
(defn momentum-sim
  "Produce a momentum affected chart of a simulation"
  [step]
  (let [days (* 5 252)
        m-step (fn [history]
                 (let [short-average (mean (take-last 5 history))
                       long-average (mean (take-last 50 history))
                       momentum (if (< short-average long-average)
                                  0.995
                                  1.005)
                       next-price (* momentum (step (last history)))]
                   (conj history next-price)))
        prices (nth (iterate m-step [1]) (dec days))
        chart (line-chart (range days) prices)]
    (view chart)))

(momentum-sim bootstrap-step)


