(ns demo-be-cjpd.in-memory-repos
  "Atom-backed in-memory implementations of repository protocols.
   Used in unit tests to avoid any database dependency."
  (:require [clojure.string :as str]
            [demo-be-cjpd.db.protocols :as proto]
            [demo-be-cjpd.domain.expense :as expense-domain])
  (:import (java.util UUID)))

(defn- now-str []
  (.format java.time.format.DateTimeFormatter/ISO_INSTANT
           (java.time.Instant/now)))

;; ============================================================
;; InMemoryUserRepo
;; ============================================================

(defrecord InMemoryUserRepo [store]
  proto/UserRepo

  (count-users [_]
    (count @store))

  (find-user-by-id [_ id]
    (get @store id))

  (find-user-by-username [_ username]
    (first (filter #(= username (:username %)) (vals @store))))

  (find-user-by-email [_ email]
    (first (filter #(= email (:email %)) (vals @store))))

  (create-user! [_ {:keys [username email password-hash display-name role status]}]
    (let [id  (str (UUID/randomUUID))
          now (now-str)
          user {:id                    id
                :username              username
                :email                 email
                :password-hash         password-hash
                :display-name          (or display-name username)
                :role                  (or role "USER")
                :status                (or status "ACTIVE")
                :failed-login-attempts 0
                :created-at            now
                :updated-at            now}]
      (swap! store assoc id user)
      user))

  (update-display-name! [_ user-id display-name]
    (let [now (now-str)]
      (swap! store update user-id assoc :display-name display-name :updated-at now)
      (get @store user-id)))

  (update-password! [_ user-id password-hash]
    (let [now (now-str)]
      (swap! store update user-id assoc :password-hash password-hash :updated-at now)
      (get @store user-id)))

  (update-status! [_ user-id status]
    (let [now (now-str)]
      (swap! store update user-id assoc :status status :updated-at now)
      (get @store user-id)))

  (increment-failed-attempts! [_ user-id]
    (let [now       (now-str)
          current   (get @store user-id)
          new-count (inc (or (:failed-login-attempts current) 0))
          status    (if (>= new-count 5) "LOCKED" (:status current))]
      (swap! store update user-id assoc
             :failed-login-attempts new-count
             :status status
             :updated-at now)
      (get @store user-id)))

  (reset-failed-attempts! [_ user-id]
    (let [now (now-str)]
      (swap! store update user-id assoc :failed-login-attempts 0 :updated-at now)
      (get @store user-id)))

  (list-users [_ {:keys [search page size]
                  :or   {page 1 size 20}}]
    (let [page      (max 1 page)
          offset    (* (dec page) size)
          all-users (sort-by :created-at #(compare %2 %1) (vals @store))
          filtered  (if (and search (not (str/blank? search)))
                      (filter #(or (str/includes? (:username %) search)
                                   (str/includes? (:email %) search))
                              all-users)
                      all-users)]
      {:data  (vec (take size (drop offset filtered)))
       :total (count filtered)
       :page  page
       :size  size})))

(defn make-user-repo
  "Create a fresh in-memory user repository."
  []
  (->InMemoryUserRepo (atom {})))

;; ============================================================
;; InMemoryExpenseRepo
;; ============================================================

(defrecord InMemoryExpenseRepo [store]
  proto/ExpenseRepo

  (create-expense! [_ {:keys [user-id type amount currency description category unit quantity date]}]
    (let [id  (str (UUID/randomUUID))
          now (now-str)
          expense (cond-> {:id          id
                           :user-id     user-id
                           :type        type
                           :amount      (expense-domain/format-amount currency amount)
                           :currency    (str/upper-case currency)
                           :description (or description "")
                           :category    (or category "")
                           :date        date
                           :created-at  now
                           :updated-at  now}
                    unit     (assoc :unit unit)
                    quantity (assoc :quantity (double (bigdec (str quantity)))))]
      (swap! store assoc id expense)
      expense))

  (find-expense-by-id [_ id]
    (get @store id))

  (find-expense-by-id-and-user [_ id user-id]
    (let [expense (get @store id)]
      (when (and expense (= user-id (:user-id expense)))
        expense)))

  (list-expenses-by-user [_ user-id {:keys [page size] :or {page 1 size 20}}]
    (let [page       (max 1 page)
          offset     (* (dec page) size)
          user-exps  (filter #(= user-id (:user-id %)) (vals @store))
          sorted     (sort-by (juxt #(str/replace (or (:date %) "") "-" "")
                                    #(str/replace (or (:created-at %) "") "-" ""))
                              #(compare %2 %1)
                              user-exps)]
      {:data  (vec (take size (drop offset sorted)))
       :total (count sorted)
       :page  page
       :size  size}))

  (update-expense! [_ id {:keys [type amount currency description category unit quantity date]}]
    (let [now (now-str)]
      (swap! store update id
             (fn [e]
               (cond-> (assoc e
                              :type        type
                              :amount      (expense-domain/format-amount currency amount)
                              :currency    (str/upper-case currency)
                              :description (or description "")
                              :category    (or category "")
                              :date        date
                              :updated-at  now)
                 unit     (assoc :unit unit)
                 (not unit) (dissoc :unit)
                 quantity (assoc :quantity (double (bigdec (str quantity))))
                 (not quantity) (dissoc :quantity))))
      (get @store id)))

  (delete-expense! [_ id]
    (swap! store dissoc id))

  (summary-by-user [_ user-id]
    (let [expenses (filter #(= user-id (:user-id %)) (vals @store))]
      (reduce (fn [acc e]
                (let [currency (or (:currency e) "USD")
                      type-str (or (:type e) "expense")
                      amount   (bigdec (or (:amount e) "0"))]
                  (update-in acc [currency type-str]
                             (fn [old] (.add (or old 0M) amount)))))
              {}
              expenses)))

  (pl-report [_ user-id from-date to-date currency]
    (let [upper    (str/upper-case currency)
          expenses (filter #(and (= user-id (:user-id %))
                                 (= upper (:currency %))
                                 (>= (compare (:date %) from-date) 0)
                                 (<= (compare (:date %) to-date) 0))
                           (vals @store))
          result   (reduce (fn [acc e]
                             (let [t      (or (:type e) "expense")
                                   cat    (or (:category e) "")
                                   amount (bigdec (or (:amount e) "0"))]
                               (-> acc
                                   (update-in [t :total] (fn [old] (.add (or old 0M) amount)))
                                   (update-in [t :breakdown cat]
                                              (fn [old] (.add (or old 0M) amount))))))
                           {}
                           expenses)
          income   (get-in result ["income" :total] 0M)
          expense  (get-in result ["expense" :total] 0M)]
      {:income-total      (.toPlainString (.setScale income 2 java.math.RoundingMode/HALF_UP))
       :expense-total     (.toPlainString (.setScale expense 2 java.math.RoundingMode/HALF_UP))
       :net               (.toPlainString (.setScale (.subtract income expense) 2 java.math.RoundingMode/HALF_UP))
       :income-breakdown  (into {}
                                (map (fn [[k v]] [k (.toPlainString (.setScale v 2 java.math.RoundingMode/HALF_UP))])
                                     (get-in result ["income" :breakdown] {})))
       :expense-breakdown (into {}
                                (map (fn [[k v]] [k (.toPlainString (.setScale v 2 java.math.RoundingMode/HALF_UP))])
                                     (get-in result ["expense" :breakdown] {})))})))

(defn make-expense-repo
  "Create a fresh in-memory expense repository."
  []
  (->InMemoryExpenseRepo (atom {})))

;; ============================================================
;; InMemoryAttachmentRepo
;; ============================================================

(defrecord InMemoryAttachmentRepo [store data-store]
  proto/AttachmentRepo

  (create-attachment! [_ {:keys [expense-id user-id filename content-type size data]}]
    (let [id  (str (UUID/randomUUID))
          now (now-str)
          attachment {:id           id
                      :expense-id   expense-id
                      :user-id      user-id
                      :filename     filename
                      :content-type content-type
                      :size         size
                      :created-at   now}]
      (swap! store assoc id attachment)
      (when data
        (swap! data-store assoc id data))
      attachment))

  (find-attachment-by-id [_ id]
    (get @store id))

  (list-attachments-by-expense [_ expense-id]
    (let [attachments (filter #(= expense-id (:expense-id %)) (vals @store))]
      (vec (sort-by :created-at attachments))))

  (delete-attachment! [_ id]
    (swap! store dissoc id)
    (swap! data-store dissoc id))

  (get-attachment-data [_ id]
    (get @data-store id)))

(defn make-attachment-repo
  "Create a fresh in-memory attachment repository."
  []
  (->InMemoryAttachmentRepo (atom {}) (atom {})))

;; ============================================================
;; InMemoryTokenRepo
;; ============================================================

(defrecord InMemoryTokenRepo [store]
  proto/TokenRepo

  (revoke-token! [_ jti user-id]
    (let [now (now-str)
          id  (str (UUID/randomUUID))]
      (swap! store assoc jti {:id id :jti jti :user-id user-id :revoked-at now})))

  (token-revoked? [_ jti]
    (contains? @store jti))

  (revoke-all-for-user! [_ user-id]
    (let [now          (now-str)
          id           (str (UUID/randomUUID))
          sentinel-jti (str "ALL:" user-id ":" now)]
      (swap! store assoc sentinel-jti {:id id :jti sentinel-jti :user-id user-id :revoked-at now})))

  (all-revoked-for-user? [_ user-id iat]
    (boolean
     (some (fn [[jti entry]]
             (when (str/starts-with? jti "ALL:")
               (when (= user-id (:user-id entry))
                 (let [revoked-epoch (-> (:revoked-at entry)
                                         java.time.Instant/parse
                                         .getEpochSecond)]
                   (>= revoked-epoch iat)))))
           @store))))

(defn make-token-repo
  "Create a fresh in-memory token repository."
  []
  (->InMemoryTokenRepo (atom {})))
