(ns rcpsp.helpers-tests (:use rcpsp.helpers clojure.test))

(deftest test-index-ofp
  (is (= 4 (index-ofp even? '(1 3 5 9 8 2 1)))))

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

(deftest test-transpose
  (is (= [[1 2 3]
          [4 5 6]
          [7 8 9]] (transpose [[1 4 7]
                               [2 5 8]
                               [3 6 9]]))))