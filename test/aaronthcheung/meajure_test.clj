(ns aaronthcheung.meajure-test
  (:require [clojure.test :refer :all]
            [aaronthcheung.meajure :refer :all]))

; Load unit definitions into the registry for testing
; Side-effect action
(mapv load-unitdefrecord [{:full-name "kilogram"
                           :short-name "kg"
                           :unit [[1 0 0 0 0 0 0] 1 0 true]
                           :tags [:all]}
                          {:full-name "gram"
                           :short-name "g"
                           :unit [[1 0 0 0 0 0 0] 0.001 0 true]
                           :tags [:all]},
                          {:full-name "meter"
                           :short-name "m"
                           :unit [[0 1 0 0 0 0 0] 1 0 true]
                           :tags [:all]},
                          {:full-name "millimeter"
                           :short-name "mm"
                           :unit [[0 1 0 0 0 0 0] 0.001 0 true]
                           :tags [:all]}
                          {:full-name "torque"
                           :short-name "Nm"
                           :unit [[1 1 0 0 0 0 0] 1 0 false]
                           :tags [:mechanical]}])

; TODO: add context to the testing cases (e.g., with clojure.test/testing, or as msg form in is-form)
(deftest equivalent-dimension:add-or-subtract
  ; If dimensions are equivalent, the unit of the same operand will be used
  (is (= (-> '(+ (* :kg 1) (* :g 1))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[1 0 0 0 0 0 0] 1 0 false],
          :value 1.001}))
  (is (= (-> '(+ (* :g 0) (* :kg 25.2))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[1 0 0 0 0 0 0] 0.001 0 false],
          :value 25200.0}))
  (is (= (-> '(- (* :kg 1) (* :g 2))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[1 0 0 0 0 0 0] 1 0 false],
          :value 0.998}))
  (is (= (-> '(- (* :g 0) (* :kg 1))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[1 0 0 0 0 0 0] 0.001 0 false],
          :value -1000.0}))
  (is (= (-> '(+ (* :g 25) (* :kg 0))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[1 0 0 0 0 0 0] 0.001 0 false],
          :value 25.0})))

(deftest arithmetic-evaluations
  (is (= (-> '(/ 1 (* :mm 4))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[0 -1 0 0 0 0 0] 1000.0 0 false],
          :value 1/4}))
  (is (= (-> '(/ (* :kg 2) (* :mm 4))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[1 -1 0 0 0 0 0] 1000.0 0 false],
          :value 1/2}))
  (is (= (-> '(* (* :mm 8) (* :m 4))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[0 2 0 0 0 0 0] 0.001 0 false],
          :value 32}))
  (is (= (-> '(/ :mm 4)
             eval-form
             (select-keys [:unit :value]))
         {:unit [[0 1 0 0 0 0 0] 0.001 0 false],
          :value 1/4}))
  (is (= (-> '(/ 4 :mm)
             eval-form
             (select-keys [:unit :value]))
         {:unit [[0 -1 0 0 0 0 0] 1000.0 0 false],
          :value 4}))
  (is (= (-> '(/ (* :kg 2) (/ :mm 4))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[1 -1 0 0 0 0 0] 1000.0 0 false],
          :value 8}))
  (is (= (-> '(* (/ 2 3) 8)
             eval-form
             (select-keys [:unit :value]))
         {:unit [[0 0 0 0 0 0 0] 1 0 false],
          :value 16/3}))
  (is (= (-> '(/ (* :kg 1.5) (* 2 :kg))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[0 0 0 0 0 0 0] 1 0 false],
          :value 0.75}))
  (is (= (-> '(/ (* (* :kg 1.5) (* 2 :kg)) (* :mm 2))
             eval-form
             (select-keys [:unit :value]))
         {:unit [[2 -1 0 0 0 0 0] 1000.0 0 false],
          :value 1.5})))

(deftest comparison-evaluations
  (is (true? (eval-form '(= (* :mm 8) (* 2 (* :mm 4))))))
  (is (true? (eval-form '(= (* :mm 5000) (* :m 5)))))
  (is (true? (eval-form '(>= (* :mm 5000) (* :m 5)))))
  (is (false? (eval-form '(< (* :mm 5000) (* :m 5))))))

(deftest test-resolve-quantity
  (is (= (resolve-quantity {:value 20
                            :unit [[1 1 0 0 0 0 0] 1 0 false]
                            :tags [:mechanical]}
                           :mechanical)
         (assoc (:Nm @unit-quantity-registry)
                :value
                20)))
  (is (false? (resolve-quantity (eval-form '(> (* :mm 4999) (* :m 5))))))
  (is (true? (resolve-quantity (eval-form '(> (* :mm 5001) (* :m 5))))))
  (is (true? (resolve-quantity (eval-form '(= (* :mm 5000) (* :m 5))))))
  (is (true? (resolve-quantity (eval-form '(!= (* :mm 5050) (* :m 5)))))))

(deftest test-unit-conversion
  (is (= (unit-conversion '(* :kg 5) :g)
         '(* :g 5000.0)))
  (is (= (some #(unit-conversion '(* :kg 50) %) [:this-unit-does-not-exist :g :kg])
         '(* :g 50000.0)))
  (is (= (unit-conversion (assoc (:kg @unit-quantity-registry)
                                 :value
                                 50) :g)
         (assoc (:g @unit-quantity-registry)
                :value
                (* 50.0 1000)))))