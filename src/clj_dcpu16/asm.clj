(ns clj-dcpu16.asm
  (:use [clojure.java.io]
        [clojure.string :only [split upper-case trim]]))

(def opcode-map {"SET" 0x1 "ADD" 0x2 "SUB" 0x3 "MUL" 0x4 "DIV" 0x5
                 "MOD" 0x6 "SHL" 0x7 "SHR" 0x8 "AND" 0x9 "BOR" 0xA
                 "XOR" 0xB "IFE" 0xC "IFN" 0xD "IFG" 0xE "IFB" 0xF
                 "JSR" 0})

(def param-map {"A" 0x0 "B" 0x01 "C" 0x02 "X" 0x03 "Y" 0x04 "Z" 0x05
                "POP" 0x18 "PEEK" 0x019 "PUSH" 0x1A "SP" 0x1B "PC" 0x1C
                "O" 0x1D})

(def comment-regex #"^;.*$")
(def deref-regex #"\[(.*)\]")
(def line-regex #"^(:\w*)+?\s*(\w.*)\s*(;.*)+?")

(defn parse [line]
  (let [sline (split line #"\\s*")]))

(defn parse-lines [lines]
  (map parse lines))

(defn first-pass [lines])

(defn second-pass [fp lines])

(defn output [result w])

(defn parse [filename]
  (with-open [r (reader filename)
              w (writer (str filename ".out"))]
    (let [lines (line-seq r)]
      (-> lines
          parse-lines
          first-pass
          (second-pass lines)
          (output w)))))