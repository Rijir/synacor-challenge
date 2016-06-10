(ns synacor-vm.core
  (:gen-class)
  (:import java.nio.file.Files
           java.nio.file.Paths))

(defn register?
  [idx]
  (> idx 32767))

(defn register-arg?
  [state idx]
  (register? ((:memory state) idx)))

(defn idx->reg
  [idx]
  (bit-and 0x7 idx))

(defn get-val
  [state idx]
  ;;(println "<DEBUG> get-val [state =" state "idx =" idx "]")
  (let [v ((:memory state) idx 0)]
    (if (register? v)
      ((:reg state) (idx->reg v))
      v)))

(defn store
  [state idx v]
  ;;(println "<DEBUG> store [state =" state "idx =" idx "val =" v "]")
  (if (register? idx)
    (assoc state :reg (assoc (:reg state) (idx->reg idx) v))
    (assoc state :memory (assoc (:memory state) idx v))))

(defn op-halt
  [state]
  ;;(println "<DEBUG> op-halt [state=" state "]")
  (assoc state :running false))

(defn op-set
  [state]
  (let [reg ((:memory state) (+ 1 (:pc state)))]
    (if (register? reg)
      (assoc (store state reg (get-val state (+ 2 (:pc state))))
             :pc (+ 3 (:pc state)))
      (do
        (println "Illegal argument:" reg "is not a register")
        (assoc state :running false)))))

(defn vm-push
  [state v]
  (assoc state :stack (cons v (:stack state))))

(defn op-push
  [state]
  ;;(println "push" (:stack state) (:reg state))
  (let [ret
        (assoc (vm-push state (get-val state (+ 1 (:pc state))))
               :pc (+ 2 (:pc state)))]
    ;;(println (:stack ret))
    ret))

(defn vm-pop
  [state]
  (let [v (first (:stack state))
        new-stack (rest (:stack state))]
    (list v (assoc state :stack new-stack))))

(defn op-pop
  [state]
  ;;(println "pop" (:stack state) (:reg state))
  (if (empty? (:stack state))
    (do
      (println "Error: stack is empty")
      (assoc state :running false))
    (let [[v new-state] (vm-pop state)
          dest (if (register-arg? state (+ 1 (:pc state)))
                 ((:memory state) (+ 1 (:pc state)))
                 (+ 1 (:pc state)))]
      ;;(println (:stack new-state) v)
      (assoc (store new-state
                    ((:memory state) (+ 1 (:pc state)))
                    v)
             :pc (+ 2 (:pc state))))))

(defn compare-op
  [state f]
  (let [pc (:pc state)
        a (if (register-arg? state (+ 1 pc))
            ((:memory state) (+ 1 pc))
            (+ 1 pc))
        b (get-val state (+ 2 pc))
        c (get-val state (+ 3 pc))
        v (if (f b c) 1 0)]
    ;;(println "a =" a "b =" b "c =" c "v =" v "f =" f)
    (assoc (store state a v) :pc (+ 4 pc))))

(defn op-eq
  [state]
  (compare-op state =))

(defn op-gt
  [state]
  (compare-op state >))

(defn op-jmp
  [state]
  (assoc state :pc (get-val state (+ 1 (:pc state)))))

(defn cond-jump-op
  [state f]
  (let [a (get-val state (+ 1 (:pc state)))
        dest (if (f a)
               (get-val state (+ 2 (:pc state)))
               (+ 3 (:pc state)))]
    (assoc state :pc dest)))

(defn op-jt
  [state]
  (cond-jump-op state (partial not= 0)))

(defn op-jf
  [state]
  (cond-jump-op state (partial = 0)))

(defn arithmetic-op
  [state f]
  ;;(println "<DEBUG> op-add [state=" state "]")
  (let [pc (:pc state)
        dest ((:memory state) (+ 1 pc))
        arg1 (get-val state (+ 2 pc))
        arg2 (get-val state (+ 3 pc))]
    ;;(println "dest =" dest "arg1 =" arg1 "arg2 =" arg2)
    (assoc (store state dest (mod (f arg1 arg2) 32768))
           :pc (+ 4 pc))))

(defn op-add
  [state]
  (arithmetic-op state +))

(defn op-mult
  [state]
  (arithmetic-op state *))

(defn op-mod
  [state]
  (arithmetic-op state mod))

(defn op-and
  [state]
  (arithmetic-op state bit-and))

(defn op-or
  [state]
  (arithmetic-op state bit-or))

(defn op-not
  [state]
  (let [pc (:pc state)
        dest ((:memory state) (+ 1 pc))
        arg (get-val state (+ 2 pc))]
    (assoc (store state dest (bit-xor 0x7fff arg))
           :pc (+ 3 pc))))

(defn op-rmem
  [state]
  (let [a ((:memory state) (+ 1 (:pc state)))
        arg2 (get-val state (+ 2 (:pc state)))
        b (if (register? arg2)
            (get-val state arg2)
            arg2)]
    ;;(println "rmem" "pc =" (:pc state) "arg2 =" arg2 "a =" a "b =" b)
    ;;(println (:reg state))
    (assoc (store state
                  a
                  (get-val state b))
           :pc (+ 3 (:pc state)))))

(defn op-wmem
  [state]
  (let [a (get-val state (+ 1 (:pc state)))
        b (get-val state (+ 2 (:pc state)))]
    ;;(println "wmem" "pc =" (:pc state) "a =" a "b =" b)
    ;;(println (:reg state))
    (let [ret (assoc (store state a b)
                     :pc (+ 3 (:pc state)))]
      ;;(println (:reg ret))
      ret)))

(defn op-call
  [state]
  (assoc (vm-push state (+ 2 (:pc state)))
         :pc (get-val state (+ 1 (:pc state)))))

(defn op-ret
  [state]
  (if (empty? (:stack state))
    (assoc state :running false)
    (let [[idx new-state] (vm-pop state)]
      (assoc new-state :pc idx))))

(defn op-out
  [state]
  ;(println "<DEBUG> op-out [pc=" (:pc state) "]")
  (let [c (char (get-val state (+ 1 (:pc state))))]
    (if (= c \newline)
      (println)
      (print c))
    (assoc state :pc (+ 2 (:pc state)))))

(defn op-in
  [state]
  (let [input (if (empty? (:input state))
                (concat (seq (read-line))
                        (list (char 10)))
                (:input state))
        c (int (first input))
        new-input (rest input)
        pc (:pc state)
        dest ((:memory state) (+ 1 pc))
        state (store state dest c)]
    ;(println "input" input)
    ;(println "new-input" new-input)
    ;(println "dest" dest)
    ;(println "c" c)
    ;(println "pc" pc)
    ;(println "registers" (:reg state))
    
    (assoc state
           :input new-input
           :pc (+ pc 2))))

(defn noop
  [state]
  ;;(println "<DEBUG> noop [state=" state "]")
  (assoc state :pc (+ 1 (:pc state))))

(def ops
  {0  op-halt
   1  op-set
   2  op-push
   3  op-pop
   4  op-eq
   5  op-gt
   6  op-jmp
   7  op-jt
   8  op-jf
   9  op-add
   10 op-mult
   11 op-mod
   12 op-and
   13 op-or
   14 op-not
   15 op-rmem
   16 op-wmem
   17 op-call
   18 op-ret
   19 op-out
   20 op-in
   21 noop})

(defn init-state
  []
  {:memory {}
   :reg [0 0 0 0 0 0 0 0]
   :stack '()
   :running true
   :pc 0
   :input '()})

(defn get-word
  [program idx]
  (let [low-byte (mod (+ 256 (nth program (* 2 idx))) 256)
        high-byte (mod (+ 256 (nth program (inc (* 2 idx)))) 256)]
    (+ (* high-byte 256) low-byte)))

(defn get-word-escaped
  [program idx]
  (let [word (get-word program idx)]
    (if (register? word)
      (str "$" (idx->reg word))
      word)))

(defn get-char-escaped
  [program idx]
  (let [word (get-word program idx)]
    (if (register? word)
      (str "$" (idx->reg word))
      (if (char-escape-string (char word))
        (char-escape-string (char word))
        (char word)))))

(defn load-program
  ([state program]
   (load-program state program 0))
  ([state program idx]
   (if (>= (* 2 idx) (alength program))
     state
     (recur (store state idx (get-word program idx))
            program
            (+ 1 idx)))))

(defn read-program-bin
  [uri]
  (Files/readAllBytes (Paths/get uri)))

(defn print-program
  ([program] (print-program program 0))
  ([program idx]
   (when (< (* 2 idx) (alength program))
     (print idx ": ")
     (case (get-word program idx)
       0 (do
           (println "halt")
           (recur program (+ idx 1)))
       1 (let [a (get-word-escaped program (+ idx 1))
               b (get-word-escaped program (+ idx 2))]
           (println "set" a b)
           (recur program (+ idx 3)))
       2 (let [a (get-word-escaped program (+ idx 1))]
           (println "push" a)
           (recur program (+ idx 2)))
       3 (let [a (get-word-escaped program (+ idx 1))]
           (println "pop" a)
           (recur program (+ idx 2)))
       4 (let [a (get-word-escaped program (+ idx 1))
               b (get-word-escaped program (+ idx 2))
               c (get-word-escaped program (+ idx 3))]
           (println "eq" a b c)
           (recur program (+ idx 4)))
       5 (let [a (get-word-escaped program (+ idx 1))
               b (get-word-escaped program (+ idx 2))
               c (get-word-escaped program (+ idx 3))]
           (println "gt" a b c)
           (recur program (+ idx 4)))
       6 (let [a (get-word-escaped program (+ idx 1))]
           (println "jmp" a)
           (recur program (+ idx 2)))
       7 (let [a (get-word-escaped program (+ idx 1))
               b (get-word-escaped program (+ idx 2))]
           (println "jt" a b)
           (recur program (+ idx 3)))
       8 (let [a (get-word-escaped program (+ idx 1))
               b (get-word-escaped program (+ idx 2))]
           (println "jf" a b)
           (recur program (+ idx 3)))
       9 (let [a (get-word-escaped program (+ idx 1))
               b (get-word-escaped program (+ idx 2))
               c (get-word-escaped program (+ idx 3))]
           (println "add" a b c)
           (recur program (+ idx 4)))
       10 (let [a (get-word-escaped program (+ idx 1))
                b (get-word-escaped program (+ idx 2))
                c (get-word-escaped program (+ idx 3))]
            (println "mult" a b c)
            (recur program (+ idx 4)))
       11 (let [a (get-word-escaped program (+ idx 1))
                b (get-word-escaped program (+ idx 2))
                c (get-word-escaped program (+ idx 3))]
            (println "mod" a b c)
            (recur program (+ idx 4)))
       12 (let [a (get-word-escaped program (+ idx 1))
                b (get-word-escaped program (+ idx 2))
                c (get-word-escaped program (+ idx 3))]
            (println "and" a b c)
            (recur program (+ idx 4)))
       13 (let [a (get-word-escaped program (+ idx 1))
                b (get-word-escaped program (+ idx 2))
                c (get-word-escaped program (+ idx 3))]
            (println "or" a b c)
            (recur program (+ idx 4)))
       14 (let [a (get-word-escaped program (+ idx 1))
                b (get-word-escaped program (+ idx 2))]
            (println "not" a b)
            (recur program (+ idx 3)))
       15 (let [a (get-word-escaped program (+ idx 1))
                b (get-word-escaped program (+ idx 2))]
            (println "rmem" a b)
            (recur program (+ idx 3)))
       16 (let [a (get-word-escaped program (+ idx 1))
                b (get-word-escaped program (+ idx 2))]
            (println "wmem" a b)
            (recur program (+ idx 3)))
       17 (let [a (get-word-escaped program (+ idx 1))]
            (println "call" a)
            (recur program (+ idx 2)))
       18 (do
            (println "ret")
            (recur program (+ idx 1)))
       19 (let [a (get-char-escaped program (+ idx 1))]
            (println "out" a)
            (recur program (+ idx 2)))
       20 (let [a (get-word-escaped program (+ idx 1))]
            (println "in" a)
            (recur program (+ idx 2)))
       21 (do
            (println "noop")
            (recur program (+ idx 1)))
       (do
         (println "unknown" (get-word program idx))
         (recur program (+ idx 1)))))))

(defn run
  [state]
  ;;(println "<DEBUG> run [pc=" (:pc state) "]")
  (if (:running state)
    (let [op (get-val state (:pc state))
          op-fn (ops op)]
      (if (nil? op-fn)
        (do
          (println "Unknown operation:" op))
        (recur (op-fn state))))
    (println "\nProgram terminated: pc =" (:pc state))))

(defn -main
  [& args]
  ;;(print-program (read-program-bin (.toURI (clojure.java.io/resource "challenge.bin"))))
  
  (run (load-program (init-state)
                     (read-program-bin (.toURI (clojure.java.io/resource "challenge.bin"))))))
