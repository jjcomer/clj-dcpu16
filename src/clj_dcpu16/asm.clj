(ns clj-dcpu16.asm
  (:use [clojure.java.io]))

(def opcode-map {"SET" 0x1 "ADD" 0x2 "SUB" 0x3 "MUL" 0x4 "DIV" 0x5
                 "MOD" 0x6 "SHL" 0x7 "SHR" 0x8 "AND" 0x9 "BOR" 0xA
                 "XOR" 0xB "IFE" 0xC "IFN" 0xD "IFG" 0xE "IFB" 0xF
                 "JSR" 0})

(def comment-regex #"$;.*")

(defn parse [line]
  )

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