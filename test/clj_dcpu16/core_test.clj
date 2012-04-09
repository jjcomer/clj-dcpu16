(ns clj-dcpu16.core-test
  (:use clojure.test
        clj-dcpu16.core))

(defn- clear-memory []
  (set-memory (fn [] {:pc 0x0000 :sp 0xFFFF})))

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

(deftest instruction-length
  (testing "Instruction Length"
    (testing "Word 0x7DE1"
      (is (= 3 (op-size 0x7DE1))))
    (testing "Word 0x7C10"
      (is (= 2 (op-size 0x7C10))))
    (testing "Word 0x9037"
      (is (= 1 (op-size 0x9037))))))