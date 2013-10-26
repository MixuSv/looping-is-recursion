(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [foo (fn[a-seq]
              (if (empty? (rest a-seq))
                (first a-seq)
                (recur (rest a-seq))
              ))]
    (foo a-seq)))

(defn seq= [seq1 seq2]
  (let [foo (fn[seq1 seq2]
              (cond
                (not (= (count seq1) (count seq2))) false
                (not (= (first seq1) (first seq2))) false
                (and (empty? (rest seq1)) (empty? (rest seq2))) true
                :else (recur (rest seq1) (rest seq2))
                ))]
    (foo seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0]
    (cond
     (>= i (count a-seq)) nil
     (pred (get a-seq i)) i
     :else (recur (inc i))
      )))

(defn avg [a-seq]
  (loop [i 0
         sum 0]
    (if (>= i (count a-seq))
      (/ sum (count a-seq))
      (recur (inc i) (+ sum (get a-seq i))))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
  (loop [i 0
         return #{}]
    (if (>= i (count a-seq))
      return
      (recur (inc i) (toggle return (get a-seq i)))))))

(defn fast-fibo [n]
  (loop [i 1
         foo 1
         bar 0]
    (cond
      (= 0 n) 0
      (= 1 n) 1
      (>= i n) foo
      :else (recur (inc i) (+ foo bar) foo)
     )))

(defn cut-at-repetition [a-seq]
  (loop [check-set #{}
         return-vector []
         iteration-seq a-seq]
    (if (or (empty? iteration-seq) (contains? check-set (first iteration-seq)))
      return-vector
      (recur (conj check-set (first iteration-seq))
             (conj return-vector (first iteration-seq))
             (rest iteration-seq)))))
