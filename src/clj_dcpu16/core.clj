(ns clj-dcpu16.core)

(def memory (ref {}))
(def register-conversion {0 :a, 1 :b, 2 :c, 3 :x, 4 :y, 5 :z, 6 :i, 7 :j, 0x1B :sp, 0x1C :pc, 0x1D :o
                          0x18 :pop, 0x19 :peek, 0x1a :push})

(declare follow-memory)
(declare inc-memory)
(declare dec-memory)

(defn get-memory
  "Fetch the provided memory address. Valid addresses are 0x0 to 0x10000.
   This function will also fetch registers using keynames
   No check is done to verify the provided address is within the range.
   Assume the default value of memory is 0x0"
  [address]
  (case address
    :pop (let [v (follow-memory :sp)]
           (inc-memory :sp)
           v)
    :peek (follow-memory :sp)
    (get @memory address 0)))

(defn set-memory
  "Set the memory address with value. Valid addresses are 0x0 to 0x10000.
   As each word is 16 bits, the max value is 0xFFFF. If the provided value
   is greater than 0xFFFF the value saved will be truncated"
  [address f]
  (dosync
   (alter memory f)))

(defn change-memory
  [address value]
  (let [address (if (not= address :push)
                  address
                  (do (dec-memory :sp)
                      (get-memory :sp)))]
    (set-memory address #(assoc % address (bit-and 0xFFFF value)))))

(defn inc-memory
  [address]
  (set-memory address inc))

(defn dec-memory
  [address]
  (set-memory address dec))

(def follow-memory
  "Fetch the value of the memory location which is stored in another memory location"
  (comp get-memory get-memory))

(defn- mask-and-shift
  "Generates a function which applies a bit mask to a word and then
   right shifts the result"
  [mask shift]
  (fn [word]
    (-> word
        (bit-and mask)
        (bit-shift-right shift))))

(def get-o
  "retrieves the opcode from the word. If 0 is returned then the opcode is
   non-basic and is stored in position b of the word"
  (mask-and-shift 0xF 0))

(def get-a
  "retrieves the a parameter from the word"
  (mask-and-shift 0x3F0 4))

(def get-b
  "retrieves the b parameter from the word"
  (mask-and-shift 0xFC00 10))

(defn process
  [word])

(defn op-size [pc])

(defmulti execute get-o)
(defmethod execute 0x0 [word]
  (if (= 1 (get-b word))
    (let [[a b out] (process word)]
      (change-memory :push (bit-and 0xFFFF (inc (get-memory :pc))))
      (change-memory :pc a))))
(defmethod execute 0x1 [word]
  ;; SET a to b
  (let [[a b out] (process word)]
    (change-memory out b)
    (inc-memory :pc)))
(defmethod execute 0x2 [word]
  ;; ADD a to b
  (let [[a b out] (process word)]
    (if (> 0xFFFF (+ a b))
      (change-memory :o 1)
      (change-memory :o 0))
    (change-memory out (bit-and 0xFFFF (+ a b)))
    (inc-memory :pc)))
(defmethod execute 0x3 [word]
  ;; SUB a from b
  (let [[a b out] (process word)]
    (if (< 0 (- a b))
      (change-memory :o 0xFFFF)
      (change-memory :o 0))
    (change-memory out (bit-and 0xFFFF (- a b)))
    (inc-memory :pc)))
(defmethod execute 0x4 [word]
  ;; MUL a = a * b
  (let [[a b out] (process word)]
    (change-memory :o (bit-and 0xFFFF (bit-shift-right (* a b) 16)))
    (change-memory out (bit-and 0xFFFF (* a b)))
    (inc-memory :pc)))
(defmethod execute 0x5 [word]
  ;; DIV a = a / b
  (let [[a b out] (process word)]
    (change-memory :o (bit-and 0xFFFF (/ (bit-shift-right a 16) b)))
    (change-memory out (bit-and 0xFFFF (/ a b)))
    (inc-memory :pc)))
(defmethod execute 0x6 [word]
  ;; MOD a = a % b
  (let [[a b out] (process word)]
    (if (= 0 b)
      (change-memory out 0)
      (change-memory out (bit-and 0xFFFF (mod a b))))
    (inc-memory :pc)))
(defmethod execute 0x7 [word]
  ;; SHL a = a << b
  (let [[a b out] (process word)]
    (change-memory :o (bit-and 0xFFFF (bit-shift-right (bit-shift-left a b) 16)))
    (change-memory out (bit-and 0xFFFF (bit-shift-left a b)))
    (inc-memory :pc)))
(defmethod execute 0x8 [word]
  ;; SHR a = a >> b
  (let [[a b out] (process word)]
    (change-memory :o (bit-and 0xFFFF (bit-shift-right (bit-shift-left a 16) b)))
    (change-memory out (bit-and 0xFFFF (bit-shift-right a b)))
    (inc-memory :pc)))
(defmethod execute 0x9 [word]
  ;; AND a = a & b
  (let [[a b out] (process word)]
    (change-memory out (bit-and a b))
    (inc-memory :pc)))
(defmethod execute 0xa [word]
  ;; BOR a = a | b
  (let [[a b out] (process word)]
    (change-memory out (bit-or a b))
    (inc-memory :pc)))
(defmethod execute 0xb [word]
  ;; XOR a = a ^ b
  (let [[a b out] (process word)]
    (change-memory out (bit-xor a b))
    (inc-memory :pc)))
(defmethod execute 0xc [word]
  ;; IFE execute next instruction iff a==b
  (let [[a b out] (process word)
        pc (get-memory :pc)]
    (if (= a b)
      (inc-memory :pc)
      (change-memory :pc (+ pc 1 (op-size (inc pc)))))))
(defmethod execute 0xd [word]
  ;; IFN execute next instruction iff a!=b
  (let [[a b out] (process word)
        pc (get-memory :pc)]
    (if (not= a b)
      (inc-memory :pc)
      (change-memory :pc (+ pc 1 (op-size (inc pc)))))))
(defmethod execute 0xe [word]
  ;; IFG execute next instruction iff a>b
  (let [[a b out] (process word)
        pc (get-memory :pc)]
    (if (> a b)
      (inc-memory :pc)
      (change-memory :pc (+ pc 1 (op-size (inc pc)))))))
(defmethod execute 0xf [word]
  ;; IFB execute next instruction iff (a&b)!= 0
  (let [[a b out] (process word)
        pc (get-memory :pc)]
    (if (not= 0 (bit-and a b))
      (inc-memory :pc)
      (change-memory :pc (+ pc 1 (op-size (inc pc)))))))

(defn run!
  "Start execution at 0x0000 unless specified"
  ([pc]
     (set-memory :pc pc)
     (run!))
  ([]
     (while true
       (execute (follow-memory :pc)))))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))