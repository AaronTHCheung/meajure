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
    "Side-effect function of loading unit definitions in the EDN file at filepath to the unit registry"
    (let [unit-defs (edn/read-string (slurp filepath))]
      (mapv load-unitdefrecord unit-defs))))

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
   '/ /})

(def quantified-form-comparison-sym->fn
  {'<= <=
   '>= >=
   '< <
   '> >
   '= ==
   '!= not=})

(def aggregate-form-operation-sym->fn
  {'and (fn and*
          [& args]
          (reduce (fn [x y] (and x y))
                  args))
   'or (fn or
         [& args]
         (reduce (fn [x y] (or x y))
                 args))
   'not not})

(defn eval-quantified-form
  [form]
  (cond
    (list? form) (let [[operator & operands] form]
                   (cond
                     (get quantified-form-operation-sym->fn operator) (let [operator (get quantified-form-operation-sym->fn operator)
                                                                            result-unit (condp = operator
                                                                                          * (apply u/mult (map :unit operands))
                                                                                          / (apply u/div (map :unit operands))
                                                                                          (let [first-unit (:unit (first operands))]
                                                                                            (if (every? #(u/dim-eq first-unit
                                                                                                                   (:unit %))
                                                                                                        operands)
                                                                                              first-unit
                                                                                              (throw (ex-info "Incompatible dimensions in form"
                                                                                                              {:form form})))))
                                                                            operands (if (some #(= % operator) [+ -])
                                                                                       (map #(assoc % 
                                                                                                    :value 
                                                                                                    (u/convert (:value %)
                                                                                                               (:unit %)
                                                                                                               result-unit))
                                                                                            operands)
                                                                                       operands)
                                                                            result-value (->> operands
                                                                                              (map :value)
                                                                                              (apply operator))]
                                                                        (->Quantity "unresolved" "unresolved" result-unit result-value [] nil))
                     (get quantified-form-comparison-sym->fn operator) (let [operator (get quantified-form-comparison-sym->fn operator)]
                                                                         (if (every? #(u/dim-eq (-> operands
                                                                                                    first
                                                                                                    :unit)
                                                                                                (:unit %))
                                                                                     (rest operands))
                                                                           (apply operator (map #(* (-> %
                                                                                                        :unit
                                                                                                        u/slope)
                                                                                                    (-> %
                                                                                                        :value))
                                                                                                operands))
                                                                           (throw (ex-info "Incompatible dimensions in form"
                                                                                           {:form form}))))
                     (get aggregate-form-operation-sym->fn operator) form ; TODO
                     :else form))
    :else form))

(defn eval-form
  "Evaluate a mathematical expression in list form to a Quantity or boolean"
  [form]
  (let [quantified-form (w/postwalk convert-to-quantity form)]
    (w/postwalk eval-quantified-form quantified-form)))

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
     If the 'quantity' is a boolean, return the boolean
     
     If tags are provided, additional filter and sorting will be performed before the final matching.
     - Additional filter: only the unit quantites with tag :all, or with one tag in the provided tags will be matched
     - Additional sorting: if multiple quantities are matched, the final quantity that will be used to resolve the
                           unresolved quantity will be the first matched quantity after hierarchical sorting:
       1) has one of the provided argument tags?
       2) has the :all tag?
       3) higher in place in the unit-quantity-registry?
     "
  ([quantity & tags]
   (if (boolean? quantity)
     quantity
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
         nil)))))

(defn unit-conversion:Quantity
  "Perform unit conversion to change a Quantity's :value and :unit in order to match target-unit.
   If target-unit does not exist in the unit quantity registry or a non-Quantity is provided, return nil.
   If the target-unit is not compatible with the quantity, throw an exception.
   Return the converted as a Quantity"
  [^Quantity quantity ^clojure.lang.Keyword target-unit]
  (when-let [target-quantity (and (instance? Quantity quantity)
                                  (target-unit @unit-quantity-registry))]
    (let [u1 (:unit quantity)
          u2 (:unit target-quantity)]
      (assoc target-quantity
             :value
             (u/convert (:value quantity)
                        u1 u2)))))

(defn quantity->canonical-form
  "Convert a Quantity into the canonical prefix-expression format '(* <unit-keyword> <value>)
   If the input is not a Quantity, return nil.
   If the quantity unit is not registered (e.g., unresolved) return nil"
  [quantity]
  (when (and (instance? Quantity quantity)
             ((-> quantity
                  :short-name
                  keyword) @unit-quantity-registry))
    (list '*
          (-> quantity
              :short-name
              keyword)
          (-> quantity
              :value))))

(defn unit-conversion:form
  "Perform unit conversion to change a quantity in the canonical format '(* <unit-keyword> <number-value>) to match target-unit.
   If target-unit does not exist in the unit quantity registry or the input form is not in the expected format (or the unit in the form not in registry), return nil.
   If the target-unit is not compatible with the quantity, throw an exception.
   Return the converted as a canonical form"
  [^clojure.lang.PersistentList form ^clojure.lang.Keyword target-unit]
  (when (and (= 3 (count form))
             (= '* (first form))
             (@unit-quantity-registry (second form))
             (number? (second (rest form))))
    (let [[unit value] (rest form)
          quantity (assoc (@unit-quantity-registry unit)
                          :value
                          value)]
      (-> quantity
          (unit-conversion:Quantity target-unit)
          quantity->canonical-form))))

(defn unit-conversion
  "General unit conversion of a quantity in the canonical format or Quantity record format format to match the target-unit.
   If successful, return the converted in the original format. Otherwise, return nil"
  [source ^clojure.lang.Keyword target-unit]
  (some #(% source target-unit) [unit-conversion:Quantity unit-conversion:form]))