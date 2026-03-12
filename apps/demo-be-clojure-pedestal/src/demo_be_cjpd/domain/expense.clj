(ns demo-be-cjpd.domain.expense
  "Expense domain model, currency, and unit validation."
  (:require [clojure.string :as str]))

(def supported-currencies
  "Map of currency code to decimal places."
  {"USD" 2
   "IDR" 0})

(def supported-units
  "Set of valid unit strings."
  #{"liter" "ml" "kg" "g" "km" "meter"
    "gallon" "lb" "oz" "mile" "piece" "hour"})

(def entry-types #{"income" "expense"})

(defn valid-currency?
  "Return true if currency is a supported 3-letter code."
  [currency]
  (and (= 3 (count currency))
       (contains? supported-currencies (str/upper-case currency))))

(defn valid-unit?
  "Return true if unit is supported or nil/empty."
  [unit]
  (or (nil? unit) (= "" unit) (contains? supported-units unit)))

(defn valid-type?
  "Return true if the entry type is income or expense."
  [t]
  (contains? entry-types t))

(defn parse-amount
  "Parse amount string to BigDecimal. Returns nil on parse failure."
  [amount-str]
  (try
    (bigdec amount-str)
    (catch Exception _
      nil)))

(defn validate-amount
  "Validate the amount string for currency precision. Returns error map or nil."
  [currency amount-str]
  (let [bd (parse-amount amount-str)]
    (cond
      (nil? bd)
      {:field "amount" :message "invalid amount format"}

      (neg? bd)
      {:field "amount" :message "amount must not be negative"}

      :else
      (let [upper    (str/upper-case currency)
            decimals (get supported-currencies upper 2)
            scale    (.scale bd)]
        (when (> scale decimals)
          {:field "amount"
           :message (str upper " requires at most " decimals " decimal places")})))))

(defn format-amount
  "Format amount BigDecimal for JSON output preserving currency precision."
  [currency amount-str]
  (let [bd       (bigdec amount-str)
        upper    (str/upper-case currency)
        decimals (get supported-currencies upper 2)]
    (.toPlainString (.setScale bd decimals java.math.RoundingMode/HALF_UP))))
