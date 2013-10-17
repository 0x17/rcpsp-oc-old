(ns rcpsp (:use clojure.set clojure.test rcpsp.helpers rcpsp.main))

;=======================================================================================================================
; Example data
;=======================================================================================================================

(def example-qlt (tbl->pair2num-map [[      4  5  6  7  8 9 10]
                                     ['AAA 50 40 30 20 10 9  8]
                                     ['AA  40 30 20 10  9 8  7]
                                     ['A   30 20 10  9  8 7  6]]))
(def example-ps {:J (set-range 0 6)
                 :d {0 0, 1 1, 2 1, 3 2, 4 2, 5 1, 6 0}
                 :k {0 0, 1 1, 2 2, 3 1, 4 1, 5 2, 6 0}
                 :E #{[0 1] [1 2] [2 3] [0 4] [4 5] [5 6] [3 6]}
                 :K 2
                 :oc-jumps {1 0, 3 5}
                 :zmax 10
                 :qlevels ['A 'AA 'AAA]
                 :qlt example-qlt})
(def example-λ '(0 1 2 3 4 5 6))

(def example-schedule (ssgs example-ps example-λ))
(def example-schedule2 (psgs example-ps example-λ))

; Result of rcpsp.zpl
(def optimal-schedule {0 1, 1 1, 2 2, 3 3, 4 3, 5 5, 6 6})
(def optimal-zt {1 0, 2 0, 3 0, 4 0, 5 0, 6 0})

;=======================================================================================================================
; Tests
;=======================================================================================================================
(deftest test-preds (is (= '() (preds (example-ps :E) 0)))
                    (is (= '(0) (preds (example-ps :E) 1) (preds (example-ps :E) 4))))

(deftest test-active-in-period
  (is (= '(1) (active-in-period example-ps {0 1, 1 1} 1)))
  (is (= '(3) (active-in-period example-ps {0 1, 1 1, 2 1, 3 1} 2))))

(deftest test-residual-in-period
  (is (= 1 (residual-in-period example-ps {0 1, 1 1, 4 1} 2))))

(deftest test-enough-capacity
  (is (false? (enough-capacity? example-ps {0 1, 1 1, 2 2, 3 3} 4 1)))
  (is (true? (enough-capacity? example-ps {0 1, 1 1, 2 2, 3 3} 4 3)))
  (is (false? (enough-capacity? example-ps {0 1, 1 1, 4 1} 2 2))))

(deftest test-first-t-qualifying (is (= 2 (first-t-qualifying even?))))

(deftest test-schedule-next (is (= {0 1, 1 1, 2 2, 3 3, 4 3} (schedule-next example-ps {0 1, 1 1, 2 2, 3 3} 4))))

(deftest test-preds-finished? (is (true? (preds-finished? example-ps {0 1, 1 1} 2 2)))
                              (is (true? (preds-finished? example-ps {0 1, 1 1} 4 1)))
                              (is (false? (preds-finished? example-ps {0 1, 1 1} 3 2))))

(deftest test-eligible-set (is (= '(2 4) (eligible-set example-ps {0 1, 1 1} 2))))

(deftest test-eligible-and-feasible-set (is (= '(4) (eligible-and-feasible-set example-ps {0 1, 1 1} 1))))

(deftest test-next-dp (is (= 2 (next-dp example-ps {0 1, 1 1} 1))))

(deftest test-schedule-in-dp
  (is (= {0 1, 1 1, 2 2} (schedule-in-dp example-λ example-ps {0 1, 1 1} 2)))
  (is (= {0 1, 1 1, 4 1} (schedule-in-dp example-λ example-ps {0 1, 1 1, 4 1} 2))))

(deftest psgs-step-test (is (= [3 {0 1, 1 1, 2 2}] (psgs-step example-λ example-ps [2 {0 1, 1 1}]))))

(deftest test-tblkey->cell (is (= 15 (tblkey->cell [[     5 ]
                                                    ['A   10]
                                                    ['AA  15]
                                                    ['AAA 20]] ['AA 5]))))
(deftest test-tbl->pair2num-map (is (= {['A 5] 10,
                                        ['AA 5] 15,
                                        ['AAA 5] 20}
                                       (tbl->pair2num-map [[     5 ]
                                                           ['A   10]
                                                           ['AA  15]
                                                           ['AAA 20]]))))

(deftest test-makespan-of-schedule
  (is (= 6 (makespan-of-schedule example-ps {6 6}))))

(deftest test-periods-in-schedule
  (is (= #{1 2 3 4 5 6} (periods-in-schedule example-ps {6 6}))))

(deftest test-res-usage-feasible?
  (is (false? (res-usage-feasible? example-ps {0 1, 1 1, 4 1, 2 2, 3 3, 5 5, 6 6})))
  (is (true? (res-usage-feasible? example-ps example-schedule)))
  (is (true? (res-usage-feasible? example-ps example-schedule2))))

(deftest test-precedence-adhered?
  (is (false? (precedence-adhered? example-ps {0 1, 1 1, 3 2}))))

(deftest test-feasible?
  (is (false? (feasible? example-ps {0 1, 1 1, 4 1, 2 2, 3 3, 5 5, 6 6})))
  (is (false? (feasible? example-ps {0 1, 1 1, 3 2})))
  (is (true? (feasible? example-ps example-schedule)))
  (is (true? (feasible? example-ps example-schedule2))))

(deftest test-n-period-left-shift
  (is (= {6 2} (n-period-left-shift {6 2} 6 0)))
  (is (= {6 2} (n-period-left-shift {6 6} 6 4))))

(deftest test-local-ls-feasible?
  (is (true? (local-ls-feasible? example-ps {0 1, 1 1, 2 3} 2)))
  (is (false? (local-ls-feasible? example-ps example-schedule 2)))
  (is (false? (local-ls-feasible? example-ps example-schedule 1))))

(deftest test-semi-active?
  (is (true? (semi-active? example-ps example-schedule))))

(deftest test-active?
  (is (true? (active? example-ps example-schedule))))

(run-tests)