(ns rcpsp.test-runner (:use clojure.test (rcpsp helpers-tests main-tests)))

(run-tests 'rcpsp.helpers-tests
           'rcpsp.main-tests)