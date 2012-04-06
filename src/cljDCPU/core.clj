(ns cljDCPU.core)

(defn- mask-and-shift
  "Generates a function which applies a bit mask to a word and then
   right shifts the result"
  [mask shift]
  (fn [word]
    (-> word
        (bit-and mask)
        (bit-shift-right shift))))

(def get-o
  "retrieves the opcode from the word"
  (mask-and-shift 0xF 0))

(def get-a
  "retrieves the a parameter from the word"
  (mask-and-shift 0x3F0 4))

(def get-b
  "retrieves the b parameter from the word"
  (mask-and-shift 0xFC00 10))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))