(ns clj-dcpu16.core)

(def memory (ref {}))
(def register-conversion {0 :a, 1 :b, 2 :c, 3 :x, 4 :y, 5 :z,
                          6 :i, 7 :j, 0x1B :sp, 0x1C :pc, 0x1D :o,
                          0x18 :pop, 0x19 :peek, 0x1a :push})

(defn set-memory
  "Set the memory address with value. Valid addresses are 0x0 to 0x10000.
   As each word is 16 bits, the max value is 0xFFFF. If the provided value
   is greater than 0xFFFF the value saved will be truncated"
  [f]
  (dosync
   (alter memory f)))

(declare change-memory get-memory follow-memory)
(defn inc-memory
  [address]
  (change-memory address (inc (get-memory address))))

(defn dec-memory
  [address]
  (change-memory address (dec (get-memory address))))

(defn get-memory
  "Fetch the provided memory address. Valid addresses are 0x0 to 0x10000.
   This function will also fetch registers using keywords
   No check is done to verify the provided address is within the range.
   Assume the default value of memory is 0x0"
  [address]
  (case address
    :pop (let [v (follow-memory :sp)]
           (inc-memory :sp)
           v)
    :peek (follow-memory :sp)
    (get @memory address 0)))

(def follow-memory
  "Fetch the value of the memory location which is stored in another memory location"
  (comp get-memory get-memory))

(defn change-memory
  [address value]
  (let [address (if-not (= address :push)
                  address
                  (do (dec-memory :sp)
                      (get-memory :sp)))]
    (set-memory #(assoc % address (bit-and 0xFFFF value)))))

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

(defn- between
  "Determine if n is x <= n <= y"
  [n x y]
  (and (<= x n) (>= y n)))

(defn get-address-and-value
  [param]
  (cond
   ;;register
   (between param 0x00 0x07) (let [r (register-conversion param)]
                               [(get-memory r) r])
   ;;[register]
   (between param 0x08 0x0f) (let [r (register-conversion (- param 0x08))
                                   a (get-memory r)]
                               [(get-memory a) a])
   ;;[register + next word]
   (between param 0x10 0x17) (let [r (register-conversion (- param 0x10))
                                   a (get-memory (inc (get-memory :pc)))
                                   a (+ (get-memory r) a)]
                               (inc-memory :pc)
                               [(get-memory a) a])
   ;;POP / [SP++]
   (= param 0x18) [(get-memory :pop) (get-memory :sp)]
   ;;PEEK / [SP]
   (= param 0x19) [(follow-memory :sp) (get-memory :sp)]
   ;;PUSH / [--SP]
   (= param 0x1A) [(follow-memory :sp) :push]
   ;;SP
   (= param 0x1B) [(get-memory :sp) :sp]
   ;;PC
   (= param 0x1C) [(get-memory :pc) :pc]
   ;;O
   (= param 0x1D) [(get-memory :o) :o]
   ;;[next word]
   (= param 0x1E) (let [a (get-memory (inc (get-memory :pc)))]
                    (inc-memory :pc)
                    [(get-memory a) a])
   ;;next word (literal)
   (= param 0x1F) (let [a (get-memory (inc (get-memory :pc)))]
                    (inc-memory :pc)
                    [a (get-memory :pc)])
   ;;literal value 0x00-0x1F
   (between param 0x20 0x3F) [(- param 0x20) :nil]))

(defn process
  "Given a word, fetch the values for a, b, and the location to save the result.
   Returned as [a b out]"
  [word]
  (let [[a out] (get-address-and-value (get-a word))
        [b _] (get-address-and-value (get-b word))]
    [a b out]))

(defn op-size
  "Given a word, calculate how many words the
   next instruction will consume and return the jump distance"
  [pc]
  (let [params [(get-a pc) (get-b pc)]]
    (apply + 1 (map #(if (or (between % 0x10 0x17)
                             (between % 0x1e 0x1f)) 1 0) params))))

(defmulti execute get-o)

;;Special OP Codes **currently only JMP**
(defmethod execute 0x0 [word]
  (if (= 1 (get-a word))
    (let [[a b out] (process word)]
      (change-memory :push (bit-and 0xFFFF (inc (get-memory :pc))))
      (change-memory :pc b))))

;; SET a to b
(defmethod execute 0x1 [word]
  (let [[a b out] (process word)]
    (change-memory out b)
    (if-not (= out :pc) (inc-memory :pc))))

;; ADD a to b
(defmethod execute 0x2 [word]
  (let [[a b out] (process word)]
    (if (> 0xFFFF (+ a b))
      (change-memory :o 1)
      (change-memory :o 0))
    (change-memory out (bit-and 0xFFFF (+ a b)))
    (inc-memory :pc)))

;; SUB a from b
(defmethod execute 0x3 [word]
  (let [[a b out] (process word)]
    (if (pos? (- a b))
      (change-memory :o 0xFFFF)
      (change-memory :o 0))
    (change-memory out (bit-and 0xFFFF (- a b)))
    (inc-memory :pc)))

;; MUL a = a * b
(defmethod execute 0x4 [word]
  (let [[a b out] (process word)]
    (change-memory :o (bit-and 0xFFFF (bit-shift-right (* a b) 16)))
    (change-memory out (bit-and 0xFFFF (* a b)))
    (inc-memory :pc)))

;; DIV a = a / b
(defmethod execute 0x5 [word]
  (let [[a b out] (process word)]
    (change-memory :o (bit-and 0xFFFF (/ (bit-shift-right a 16) b)))
    (change-memory out (bit-and 0xFFFF (/ a b)))
    (inc-memory :pc)))

;; MOD a = a % b
(defmethod execute 0x6 [word]
  (let [[a b out] (process word)]
    (if (zero? b)
      (change-memory out 0)
      (change-memory out (bit-and 0xFFFF (mod a b))))
    (inc-memory :pc)))

;; SHL a = a << b
(defmethod execute 0x7 [word]
  (let [[a b out] (process word)]
    (change-memory :o (bit-and 0xFFFF (bit-shift-right (bit-shift-left a b) 16)))
    (change-memory out (bit-and 0xFFFF (bit-shift-left a b)))
    (inc-memory :pc)))

;; SHR a = a >> b
(defmethod execute 0x8 [word]
  (let [[a b out] (process word)]
    (change-memory :o (bit-and 0xFFFF (bit-shift-right (bit-shift-left a 16) b)))
    (change-memory out (bit-and 0xFFFF (bit-shift-right a b)))
    (inc-memory :pc)))

;; AND a = a & b
(defmethod execute 0x9 [word]
  (let [[a b out] (process word)]
    (change-memory out (bit-and a b))
    (inc-memory :pc)))

;; BOR a = a | b
(defmethod execute 0xa [word]
  (let [[a b out] (process word)]
    (change-memory out (bit-or a b))
    (inc-memory :pc)))

;; XOR a = a ^ b
(defmethod execute 0xb [word]
  (let [[a b out] (process word)]
    (change-memory out (bit-xor a b))
    (inc-memory :pc)))

;; IFE execute next instruction iff a==b
(defmethod execute 0xc [word]
  (let [[a b out] (process word)]
    (if (= a b)
      (inc-memory :pc)
      (change-memory :pc (let [pc (get-memory :pc)]
                           (+ pc 1 (op-size (get-memory (inc pc)))))))))

;; IFN execute next instruction iff a!=b
(defmethod execute 0xd [word]
  (let [[a b out] (process word)]
    (if (not= a b)
      (inc-memory :pc)
      (change-memory :pc (let [pc (get-memory :pc)]
                           (+ pc 1 (op-size (get-memory (inc pc)))))))))

;; IFG execute next instruction iff a>b
(defmethod execute 0xe [word]
  (let [[a b out] (process word)]
    (if (> a b)
      (inc-memory :pc)
      (change-memory :pc (let [pc (get-memory :pc)]
                           (+ pc 1 (op-size (get-memory (inc pc)))))))))

;; IFB execute next instruction iff (a&b)!= 0
(defmethod execute 0xf [word]
  (let [[a b out] (process word)]
    (if (not= 0 (bit-and a b))
      (inc-memory :pc)
      (change-memory :pc (let [pc (get-memory :pc)]
                           (+ pc 1 (op-size (get-memory (inc pc)))))))))

(defn run!
  "Start execution at 0x0000 unless specified"
  ([pc]
     (change-memory :pc pc)
     (run!))
  ([]
     (change-memory :sp 0xFFFF)
     (while true
       (execute (follow-memory :pc)))))
