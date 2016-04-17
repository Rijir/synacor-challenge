(ns synacor-vm.core
  (:gen-class))

(defn register?
  [idx]
  (>= idx 32768))

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

(defn op-push
  [state]
  (assoc state
         :stack (cons get-val (+ 1 (:pc state))
                      (:stack state))
         :pc (+ 2 (:pc state))))

(defn op-pop
  [state]
  (if (empty? (:stack state))
    (do
      (println "Error: stack is empty")
      (assoc state :running false))
    (let [v (first (:stack state))
          new-stack (rest (:stack state))]
      (assoc (store state
                    ((:memory state) (+ 1 (:pc state)))
                    v)
             :stack new-stack
             :pc (+ 2 (:pc state))))))

(defn compare-op
  [state f]
  (let [pc (:pc state)
        a (+ 1 pc)
        b (get-val state (+ 2 pc))
        c (get-val state (+ 3 pc))
        v (if (f b c) 1 0)]
    (store state a v)))

(defn op-eq
  [state]
  (compare-op state =))

(defn op-gt
  [state]
  (compare-op state >))

(defn op-jmp
  [state]
  (assoc state :pc (get-val (+ 1 (:pc state)))))

(defn cond-jump-op
  [state f]
  (let [a (get-val (+ 1 (:pc state)))
        dest (if (f a)
               (get-val (+ 2 (:pc state)))
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
  (arithmetic-op state (partial bit-xor 0x7fff)))

(defn op-out
  [state]
  ;;(println "<DEBUG> op-out [state=" state "]")
  (print (char (get-val state (+ 1 (:pc state)))))
  (assoc state :pc (+ 2 (:pc state))))

(defn noop
  [state]
  ;;(println "<DEBUG> noop [state=" state "]")
  (assoc state :pc (+ 1 (:pc state))))

(def ops
  {0  op-halt
   1  op-set
   2  op-push
   9  op-add
   19 op-out
   21 noop})

(defn init-state
  []
  {:memory {}
   :reg [0 0 0 0 0 0 0 0]
   :stack '()
   :running true
   :pc 0})

(defn load-program
  ([state program]
   (load-program state program 0))
  ([state program idx]
   (if (empty? program)
     state
     (load-program (store state idx (first program))
                   (rest program)
                   (+ 1 idx)))))

(defn run
  [state]
  ;;(println "<DEBUG> run [state=" state "]")
  (if (:running state)
    (let [op (get-val state (:pc state))
          op-fn (ops op)]
      (if (nil? op-fn)
        (do
          (println "Unknown operation:" op))
        (recur (op-fn state))))
    (println "\nProgram terminated")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
