(ns rcpsp.schedule-view-tests (:use (rcpsp schedule-view main-tests) clojure.test))

;(create-window example-ps example-schedule)

(deftest test-times-capacity
  (is (= '(1 1 1 2 2) (times-capacity {:k {1 3, 2 2}} #{1 2}))))

(deftest test-fill-to-capacity
  (is (= '(0 0 0 0 1 1 1 2 2) (fill-to-capacity {:K 5, :oc-jumps {1 4}} 1 '(1 1 1 2 2)))))

(deftest test-col-for-period
  (let [eps {:K 5, :oc-jumps {1 4}, :k {1 3, 2 2}, :J #{1 2}, :d {1 1, 2 1}}]
    (is (= '(0 0 0 0 1 1 1 2 2) (col-for-period eps {1 1, 2 1} 1)))))
