(ns clj-dcpu16.core-test
  (:use clojure.test
        clj-dcpu16.core))

(defn- clear-memory []
  (dosync (alter memory (fn [old] {:pc 0x0000 :sp 0xFFFF}))))

(defn- force-memory [address value]
  (dosync (alter memory #(assoc % address value))))

(defn- place-instruction
  [address & inst]
  (loop [a address i inst]
    (if (empty? i)
      nil
      (do (force-memory a (first i))
          (recur (inc a) (rest i))))))

(deftest memory-access
  (testing "Get memory: "
    (testing "Get a 0 for non initialized memory"
      (clear-memory)
      (is (= 0 (get-memory 0xABCD)))
      (is (= 0 (get-memory :b))))
    (testing "Get value of initialized memory"
      (clear-memory)
      (force-memory 0x1234 0xEEEE)
      (is (= 0xEEEE (get-memory 0x1234))))
    (testing "Peek"
      (clear-memory)
      (force-memory :sp 0x1234)
      (force-memory 0x1234 0xFFFF)
      (is (= 0xFFFF (get-memory :peek))))
    (testing "Pop"
      (clear-memory)
      (force-memory :sp 0xFFFE)
      (force-memory 0xFFFF 0x2)
      (force-memory 0xFFFE 0x1)
      (is (= 0x1 (get-memory :peek)))
      (is (= 0x1 (get-memory :pop)))
      (is (= 0x2 (get-memory :peek)))
      (is (= 0xFFFF (get-memory :sp)))))
  (testing "Change memory: "
    (testing "Change uninitialized memory location"
      (clear-memory)
      (change-memory 0xABCD 0xFFFF)
      (is (= 0xFFFF (get-memory 0xABCD)))
      (change-memory :x 0x1234)
      (is (= 0x1234 (get-memory :x))))
    (testing "Change initialized memory"
      (clear-memory)
      (force-memory 0xEAEA 0x1234)
      (change-memory 0xEAEA 0x4321)
      (is (= 0x4321 (get-memory 0xEAEA))))
    (testing "Push"
      (clear-memory)
      (change-memory :push 0x1234)
      (is (= 0xFFFE (get-memory :sp)))
      (is (= 0x1234 (get-memory :peek)))
      (change-memory :push 0x4321)
      (is (= 0xFFFD (get-memory :sp)))
      (is (= 0x4321 (get-memory :peek)))))
  (testing "Inc memory"
    (clear-memory)
    (inc-memory 0xAB13)
    (is (= 1 (@memory 0xAB13)))
    (inc-memory :pc)
    (is (= 1 (@memory :pc)))
    (inc-memory :a)
    (inc-memory :a)
    (is (= 2 (@memory :a))))
  (testing "Dec memory"))

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

(deftest processing-instructions
  (testing "Instruction Processing: "
    (testing "Register accessing"
      (clear-memory)
      (force-memory :a 0x3333)
      (is (= [0x3333 :a] (get-address-and-value 0x0)))
      (force-memory :j 0x1111)
      (is (= [0x1111 :j] (get-address-and-value 0x7))))
    (testing "Register De-reference"
      (clear-memory)
      (force-memory :c 0x1234)
      (force-memory 0x1234 0x1111)
      (is (= [0x1111 0x1234] (get-address-and-value 0xA))))
    (testing "Register + Next Word De-reference"
      (clear-memory)
      (force-memory :c 0x1)
      (force-memory 0x1 0x1)
      (force-memory 0x2 0x1234)
      (is (= [0x1234 0x2] (get-address-and-value 0x12))))
    (testing "Pop"
      (clear-memory)
      (force-memory 0xFFFE 0x1234)
      (force-memory :sp 0xFFFE)
      (is (= [0x1234 0xFFFF] (get-address-and-value 0x18))))
    (testing "Peek"
      (clear-memory)
      (force-memory 0xFFFF 0x1234)
      (is (= [0x1234 0xFFFF] (get-address-and-value 0x19))))
    (testing "Push"
      (clear-memory)
      (force-memory 0xFFFF 0x1234)
      (is (= [0x1234 :push] (get-address-and-value 0x1A))))
    (testing "De-reference next word"
      (clear-memory)
      (place-instruction 0 0x1111 0x2222 0x3333)
      (force-memory 0x2222 0x1234)
      (is (= [0x1234 0x2222] (get-address-and-value 0x1E))))
    (testing "Literal next word"
      (clear-memory)
      (place-instruction 0 0x1111 0x2222 0x3333)
      (is (= [0x2222 0x1] (get-address-and-value 0x1F))))
    (testing "Literal value"
      (clear-memory)
      (is (= [0x0 :nil] (get-address-and-value 0x20)))
      (is (= [0x4 :nil] (get-address-and-value 0x24))))))

(deftest instruction-length
  (testing "Instruction Length"
    (testing "Word 0x7DE1"
      (is (= 3 (op-size 0x7DE1))))
    (testing "Word 0x7C10"
      (is (= 2 (op-size 0x7C10))))
    (testing "Word 0x9037"
      (is (= 1 (op-size 0x9037))))))

;;This is the test program from Notch's README on the DCPU16
(def test-prog [0x7c01 0x0030 0x7de1 0x1000 0x0020 0x7803 0x1000 0xc00d
                0x7dc1 0x001a 0xa861 0x7c01 0x2000 0x2161 0x2000 0x8463
                0x806d 0x7dc1 0x000d 0x9031 0x7c10 0x0018 0x7dc1 0x001a
                0x9037 0x61c1 0x7dc1 0x001a 0x0000 0x0000 0x0000 0x0000])

(deftest run-a-program
  (testing "Running Notch's Example Program"
    (clear-memory)
    (apply place-instruction 0 test-prog)
    (while (not= 0x001a (get-memory :pc))
      (println (format "%x followed to %x" (get-memory :pc) (follow-memory :pc)))
      (execute (follow-memory :pc)))
    (is (= 0x40 (get-memory :x)))))