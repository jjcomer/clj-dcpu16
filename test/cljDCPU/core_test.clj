(ns cljDCPU.core-test
  (:use clojure.test
        cljDCPU.core))

(deftest word-parsing
  (testing "Word Parsing"
    (testing "Word 0x7C01"
      (let [word 0x7C01]
        (is (= 1 (get-o word)))
        (is (= 0 (get-a word)))
        (is (= 0x1F (get-b word)))))
    (testing "Word 0x61C1"
      (let [word 0x61C1]
        (is (= 1 (get-o word)))
        (is (= 0x1C (get-a word)))
        (is (= 0x18 (get-b word)))))))
