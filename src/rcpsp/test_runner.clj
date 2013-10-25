(ns rcpsp.test-runner (:use clojure.test (rcpsp helpers-tests main-tests schedule-view-tests)))

(run-tests 'rcpsp.helpers-tests
           'rcpsp.main-tests
           'rcpsp.schedule-view-tests)