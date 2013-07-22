(ns stocksim.core
  (:use [stocksim.data]
        [incanter core charts stats]))


(defn rand-step
  "Add a random number between -0.5 and 0.5"
  [price]
  (+ price (rand) -0.5))

(def year 252)

(defn sim
  "Shows a chart of iterations of step over 30 years"
  [title step]
  (let [days (* 30 year)
        price (take days (iterate step 1))
        date (trading-dates days)
        chart (time-series-plot date price :title title)]
    (view chart)))

(sim "Random Step" rand-step)


(defn random-normal
  "Simulate a normal distribution by adding random numbers together"
  []
  (reduce + -50 (repeatedly 100 rand)))

(let [value (repeatedly 1000 random-normal)]
  (view (histogram value :title "Simulated normal distribution")))

; Asume the price tends to go up,
; tends to change relative to current,
; day fluctuation are normally distributed
(defn biased-normal-step
  "Add a biased random normal number relative to current price"
  [price]
  (let [biased (+ 0.05 (random-normal))
        scale 0.004]
    (+ price (* price biased scale))))

(sim "Biased normal step" biased-normal-step)


(defn fixed-step
  [rate price]
  (+ price (* (/ price year) rate)))

(sim "Fixed interest step" #(fixed-step 0.08 %))


(defn momentum
  "When short term average is above long term average, continue rising"
  [history]
  (let [short-average (mean (take-last 5 history))
        long-average (mean (take-last 50 history))]
    (if (< short-average long-average)
      0.999
      1.001)))

(defn sim-with-history
  "Shows a chart of a 5 year iteration of step"
  [title step]
  (let [days (* 30 year)
        prices (last (take days (iterate step [1])))
        date (trading-dates days)
        chart (time-series-plot date prices :title title)]
    (view chart)))

(defn momentum-normal-step
  [history]
  (conj history
        (* (momentum history)
           (biased-normal-step (last history)))))

(sim-with-history "Biased normal step with momentum" momentum-normal-step)

; Retrieve the S&P 500 ETF prices from Yahoo
(def sp (series "^GSPC"))

; plot the daily changes
(let [date (map first sp)
      price (map last sp)
      change (map / (rest price) price)
      get-stats (juxt mean
                      #(reduce max %)
                      #(reduce min %))]
  (view (time-series-plot date price :title "S&P 500 index"))
  (view (time-series-plot date change :title "Daily change ratio S&P 500"))
  (view (histogram change :nbins 50 :title "S&P 500 index change distribution"))
  (get-stats change))
; Average change is 0.337% per day, max rise is 11.5% and fall is -20.5%

(defn create-sample-step
  "Chooses a random step from real data"
  [sym]
  (let [price (map last (series sym))
        changes (map / (rest price) price)
        ; use a vector so we can index any value instead of scanning
        lookup (into [] changes)]
    #(* % (rand-nth lookup))))

(def sp-sample-step (create-sample-step "^GSPC"))

(sim "Step randomly drawn from sample" sp-sample-step)

; We could create a custom momentum step, but is there a more general solution?
(defn history-step
  "Take a step by several multiplicative factors which are calculated from history"
  [factors history]
  (let [get-factors (apply juxt factors)
        ratios (get-factors history)
        new-price (apply * (last history) ratios)]
    (conj history new-price)))

(let [price (map last sp)
      changes (map / (rest price) price)
      lookup (into [] changes)
      sample (fn [history] (rand-nth lookup))
      momentum-sample-step #(history-step [momentum sample] %)]
  (sim-with-history "Sample step with momentum" momentum-sample-step))


(defn buy-and-hold
  "Gross return after 30 years of holding"
  [step]
  (nth (iterate step 1)
       (* 30 year)))

(buy-and-hold sp-sample-step)

(let [return (repeatedly 1000 #(buy-and-hold sp-sample-step))]
  (view (histogram return :nbins 50 :title "Simulated 30 year returns"))
  (mean return))

(nth (iterate #(fixed-step 0.085 %) 1)
     (* 30 year))

; Does this match an underpinning ecconomy? What is the real distribution?
(let [price (map last sp)
      period (partition (* 30 year) 1 price)
      profit-for #(- (last %) (first %))
      return-for #(/ (profit-for %) (first %))
      return (map return-for period)]
  (view (histogram return :nbins 50 :title "All 30 year period returns distribution"))
  (mean return))

; What are some of the relevant fixed income rates to achieve these returns?
(nth (iterate #(fixed-step 0.069 %) 1)
     (* 30 year))
(nth (iterate #(fixed-step 0.054 %) 1)
     (* 30 year))
(nth (iterate #(fixed-step 0.037 %) 1)
     (* 30 year))

