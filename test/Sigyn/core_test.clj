(ns Sigyn.core-test
  (:use clojure.test
        Sigyn.core))

; Run these tests with (run-tests 'Sigyn.core-test) or simply (run-tests). If the repl tells you run-tests is not 
; defined, try pasting the above ns call in the repl, including the :use part.

(deftest tuple-match-test-1
  (testing "Simple tuple matching."
           (is (match? (tuple [1 2 nil]) (tuple [1 2 nil])))
           (is (match? (tuple [1 2 nil]) (tuple [1 2 3])))
           (is (match? (tuple [1 2 3]) (tuple [1 2 3])))
           (is (not (match? (tuple [1 2 3]) (tuple [1 2 nil]))))
           (is (not (match? (tuple [1 2 3]) (tuple [1 2 4]))))
           ))

(deftest hash-memory-test
  (testing "Hash memory test."
           (let
             [mem1 (empty-hash-mem)
              mem2 (insert mem1 (tuple [1 2]) 3)]
             (is (query mem2 (tuple [1 2])) 3)
             )
           )
  )