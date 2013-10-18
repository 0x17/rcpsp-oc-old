(ns rcpsp.test-runner (:use clojure.test rcpsp.helpers-tests rcpsp.main-tests))
(run-tests 'rcpsp.helpers-tests 'rcpsp.main-tests)