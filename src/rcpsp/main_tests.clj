(ns rcpsp.main-tests (:use (clojure set test) (rcpsp helpers main psplib-reader)))

;=======================================================================================================================
; Example data
;=======================================================================================================================

(def example-qlt (tbl->pair2num-map [[      4  5  6  7  8 9 10]
                                     ['AAA 50 40 30 20 10 9  8]
                                     ['AA  40 30 20 10  9 8  7]
                                     ['A   30 20 10  9  8 7  6]]))
(def example-ps (map->projrec
                  {:J (set-range 0 6)
                   :d {0 0, 1 1, 2 1, 3 2, 4 2, 5 1, 6 0}
                   :k {0 0, 1 1, 2 2, 3 1, 4 1, 5 2, 6 0}
                   :E #{[0 1] [1 2] [2 3] [0 4] [4 5] [5 6] [3 6]}
                   :K 2
                   :oc-jumps {1 0}
                   :zmax 10
                   :qlevels ['A 'AA 'AAA]
                   :qlt example-qlt}))

(def example-ps-woc (assoc example-ps :oc-jumps {1 0, 3 5}))

(def example-λ '(0 1 2 3 4 5 6))

(def example-schedule (ssgs example-ps example-λ))
(def example-schedule2 (psgs example-ps example-λ))

; Result of rcpsp.zpl
(def optimal-schedule {0 1, 1 1, 2 2, 3 3, 4 1, 5 3, 6 4})
(def optimal-zt {1 0, 2 2, 3 3, 4 3, 5 1, 6 0})

;=======================================================================================================================
; Tests
;=======================================================================================================================
(deftest test-preds (is (= '() (preds (:E example-ps) 0)))
                    (is (= '(0) (preds (:E example-ps) 1) (preds (:E example-ps) 4))))

(deftest test-remove-redundant-jump-in-t
  (is (= [4 {1 0, 3 4}] (remove-redundant-jump-in-t [4 {1 0, 3 4, 5 4}] 5)))
  (is (= [4 {1 0, 3 4, 6 4}] (remove-redundant-jump-in-t [4 {1 0, 3 4, 6 4}] 5))))

(deftest test-remove-redundant-jumps
  (is (= {3 0} (remove-redundant-jumps {3 0})))
  (is (= {1 0, 3 4} (remove-redundant-jumps {1 0, 2 0, 3 4, 7 4, 6 4}))))

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

(deftest test-makespan-of-schedule
  (is (= 6 (makespan-of-schedule example-ps {6 6}))))

(deftest test-periods-in-schedule
  (is (= #{1 2 3 4 5} (periods-in-schedule example-ps {6 6}))))

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

(def example-content (read-lines "j301_9.sm"))

(deftest test-horizon
  (is (= 160 (horizon example-content))))

(deftest test-precedence-lines
  (is (= 32 (count (precedence-lines example-content))))
  (is (not-any? #(.contains % "jobnr") (precedence-lines example-content))))

(deftest test-reqdur-lines
  (is (= 32 (count (reqdur-lines example-content)))))

(deftest test-succ-to-edges
  (is (= #{[1 2] [1 3] [1 4] [2 3] [2 4] [4 1]}
         (succ-to-edges {1 #{2 3 4}, 2 #{3 4}, 4 #{1}}))))

(deftest test-succ-from-line
  (is (= {10 #{11 15 24}}
         (succ-from-line {} "  10        1          3          11  15  24"))))

(deftest test-succ-from-content
  (let [succ (succ-from-content example-content)]
    (is (= #{11 15 24} (succ 10)))))

(deftest test-parse-capacity-line
  (is (= {:R1 12 :R2 11 :R3 11 :R4 13} (parse-capacity-line "   12   11   11   13"))))

(deftest test-ps-from-content
  (let [ps (ps-from-content example-content)]
    (is (= (set-range 1 32) (:J ps)))
    (is (= 12 (:K ps)))
    (is (subset? #{[1 4] [4 5]} (:E ps)))))

(def example-psplib-ps (ps-from-content example-content))
(def example-psplib-λ (take 32 nat-nums))

(deftest test-restrict-to-max-oc
  (is (= {1 0, 3 5, 4 10} (restrict-to-max-oc example-ps {1 0, 3 5, 4 20}))))

(deftest test-capacity-missing
  (is (= '(1) (capacity-missing example-ps {1 1, 4 1} 2 #{2}))))

(deftest test-period-to-missing
  (is (= {2 1} (period-to-missing example-ps {1 1, 4 1} 2 #{2})))
  (is (= {2 1, 3 0} (period-to-missing example-ps (dissoc (ssgs example-ps example-λ) 4) 4 #{2 3}))))

(deftest test-book-oc
  (is (= {1 0, 2 1, 3 5} (:oc-jumps (book-oc example-ps-woc {1 1, 4 1} 2 2))))
  (is (= {1 0, 2 1, 3 0} (:oc-jumps (book-oc (assoc example-ps-woc :oc-jumps {1 0}) {1 1, 4 1} 2 2))))
  (is (= {1 0, 2 1, 3 0} (:oc-jumps (book-oc example-ps (dissoc (ssgs example-ps example-λ) 4) 1 4)))))

(deftest test-actual-est
  (is (= 5 (actual-est example-ps {0 0, 1 1, 2 4} 3)))
  (is (= 1 (actual-est example-ps (ssgs example-ps example-λ) 4))))

(deftest test-sum-missing
  (is (= 1 (sum-missing example-ps {0 1, 1 1, 2 2, 3 3} 4 1)))
  (is (= 1 (sum-missing example-ps (dissoc (ssgs example-ps example-λ) 4) 4 1)))
  (is (= 1 (sum-missing example-ps (dissoc (ssgs example-ps example-λ) 4) 4 2))))

(deftest test-best-stj
  (is (= 1 (best-stj example-ps {0 1, 1 1, 2 2, 3 3, 4 2} 4)))
  (is (= 1 (best-stj example-ps (ssgs example-ps example-λ) 4))))

(deftest test-try-oc-for
  (is (= {1 0} (:oc-jumps (try-oc-for example-λ example-ps 4))))
  (is (= {1 0, 2 1, 3 0} (:oc-jumps (try-oc-for example-λ (assoc-in example-ps [:k 5] 1) 4)))))

(deftest test-revenue
  (is (= 30 (revenue example-ps {0 1, 1 1, 2 1, 3 3, 4 3, 5 5, 6 6}))))

(deftest test-fitness
  (let [fitness1 (fitness example-ps {0 1, 1 1, 2 1, 3 3, 4 3, 5 5, 6 6}) ; = (ssgs ex-ps ex-λ)
        fitness2 (fitness example-ps {0 1, 1 1, 4 1, 2 2, 5 3, 3 3, 6 5})]
    (is (> fitness2 fitness1))
    (is (= 30 fitness1))
    (is (= 40 fitness2))))

(deftest test-naive-oc-heuristic
  (is (=  (:oc-jumps (naive-oc-heuristic example-ps example-λ)))))

(deftest test-naive-oc-heuristic-schedule
  (is (=  (naive-oc-heuristic-schedule example-ps example-λ))))