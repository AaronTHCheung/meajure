(ns aaronthcheung.meajure
  (:require
   [unit.core :as u]
   [clojure.edn :as edn]
   [clojure.walk :as w]
   [clojure.set :refer [union]]))

(do
  ; Registry initialization
  (defrecord Quantity [full-name short-name unit value tags insertion-order])

  (def unit-quantity-registry (atom {}))

  (swap! unit-quantity-registry  ; A unitless quantity to represent a raw number
         assoc
         :ul
         (->Quantity "unitless"
                     "ul"
                     [u/const-dimensions 1 0 false]
                     1
                     [:all]
                     (+ 1 (count @unit-quantity-registry))))

  (defn load-unitdefrecord
    [unitdef-recordmap]
    (let [short-name-kw (keyword (:short-name unitdef-recordmap))
          unit-quantity (map->Quantity (merge unitdef-recordmap
                                              {:value 1
                                               :insertion-order (+ 1 (count @unit-quantity-registry))}))]
      (swap! unit-quantity-registry
             assoc
             short-name-kw
             unit-quantity)))

  (defn load-unitdef-edn
    [filepath]
    (let [unit-defs (edn/read-string (slurp filepath))]
      "Need to use dorun/doall/doseq to consume the lazy sequence created by for/map"
      (doall (map load-unitdefrecord unit-defs))))

  ;; (load-unitdef-edn "unitdef.edn")
  )

(defn number->unitless-number
  [x]
  (assoc (:ul @unit-quantity-registry)
         :value
         x))

(defn convert-to-quantity
  [x]
  (cond
    (number? x) (number->unitless-number x)
    (keyword? x) (if-let [unit-quantity (x @unit-quantity-registry)]
                   unit-quantity
                   x)
    :else x))

(def quantified-form-operation-sym->fn
  {'+ +
   '- -
   '* *
   '/ /
   '<= <=
   '>= >=
   '< <
   '> >})

(defn eval-quantified-form
  [form]
  (cond
    (list? form) (let [[operator & operands] form]
                   (if-let [operator (operator quantified-form-operation-sym->fn)]
                     (let [result-unit (condp = operator
                                         * (apply u/mult (map :unit operands))
                                         / (apply u/div (map :unit operands))
                                         (let [first-unit (:unit (first operands))]
                                           (if (every? #(u/dim-eq first-unit
                                                                  (:unit %))
                                                       operands)
                                             first-unit
                                             (throw (ex-info "Incompatible dimensions in form"
                                                             {:form form})))))
                           scaling-factors (map (fn [operand]
                                                  "Determine the scaling factor to be multiplied to the each operand"
                                                  (/ (u/slope result-unit)
                                                     (u/slope (:unit operand))))
                                                operands)
                           result-unit-measured-values (map #(* (:value %1) %2)
                                                            operands
                                                            scaling-factors)
                           result-value (apply operator result-unit-measured-values)]
                       (->Quantity "unresolved" "unresolved" result-unit result-value [] nil))
                     form))
    :else form))

(defn eval-form
  [form]
  (let [quantified-form (w/postwalk convert-to-quantity form)]
    (w/postwalk eval-quantified-form quantified-form)))

(comment
  "Testing eval-form"
  (eval-form '(/ 1 (* :mm 4) ))
  (eval-form '(/ 1 (* :mm 4)))
  (eval-form '(/ (* :kg 2) (* :mm 4)))
  (eval-form '(/ :mm 4))
  (eval-form '(/ 4 :mm))
  (eval-form '(/ (* :kg 2) (/ :mm 4)))
  (eval-form '(* (/ 2 3) 8))
  (eval-form '(/ (* (* :kg 1.5) (* 2 :kg)) (* :mm 2))))

(w/postwalk convert-to-quantity '(* :mm 5))

(defn unit-eq
  "
     Test if two units are equivalent. Two units are equivalent if they have identical:
  
     - dimension, and;
     - slope, and;
     - y-intercept
     "
  [u1 u2]
  (and (u/dim-eq u1 u2)
       (= (u/slope u1)
          (u/slope u2))
       (= (u/y-intercept u1)
          (u/y-intercept u2))))

(defn resolve-quantity
  "
     Resolve a unresolved quantity by finding a unit quantity in the registry that has matching dimension and slope.
     If multiple unit quantities are matched in the registry, the match highest in place in the unit-quantity-registry will be used to resolve the quantity.
     If no suitable unit quantity registry is found in the registry, return nil
     
     If tags are provided, additional filter and sorting will be performed before the final matching.
     - Additional filter: only the unit quantites with tag :all, or with one tag in the provided tags will be matched
     - Additional sorting: if multiple quantities are matched, the final quantity that will be used to resolve the
                           unresolved quantity will be the first matched quantity after hierarchical sorting:
       1) has one of the provided argument tags?
       2) has the :all tag?
       3) higher in place in the unit-quantity-registry?
     "
  ([quantity & tags]
   (let [n (count @unit-quantity-registry)
         candidate-registry (filter #(unit-eq (:unit quantity)
                                              (:unit %))
                                    (vals @unit-quantity-registry))
         tags-set (set tags)
         matching-tags-set (union tags-set #{:all})
         matched-registry (filter #(some matching-tags-set (:tags %))
                                  candidate-registry)
         matched-rank (map #(- (:insertion-order %)
                               (if (some tags-set (:tags %)) n 0))
                           matched-registry)
         matched-unit-quantity (->> matched-rank
                                    (map-indexed vector)
                                    (sort-by second)
                                    (map first)
                                    (map #(nth matched-registry %))
                                    first)]
     (if matched-unit-quantity
       (assoc matched-unit-quantity :value (:value quantity))
       nil))))

(comment
  "test resolve-quantity"
  (resolve-quantity {:value 20
                     :unit [[1 1 0 0 0 0 0] 1 0 false]
                     :tags [:mechanical]}
                    :mechanical))