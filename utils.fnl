(global lume (require :lume))

(fn inc [x] (+ x 1))
(fn dec [x] (- x 1))
(fn empty? [arr] (= (length arr) 0))
(fn rest [arr] (if (= (length arr) 1) [] (lume.slice arr 2 (length arr))))
(fn butlast [arr] (lume.slice arr 1 (dec (length arr))))
(fn take [n arr] (lume.slice arr 1 n))
(fn drop [n arr] (lume.slice arr (inc n) (length arr)))
(fn apply [f arr] (f (table.unpack arr)))
(fn cons [x arr] [x (table.unpack arr)])
(fn conj [arr x]
    (let [t [(table.unpack arr)]]
      (table.insert t x)
      t))

(fn last [arr] (. arr (length arr)))
(fn partition [count step a]
    (fn iter [acc arr]
        (if
         (empty? arr)
         acc
         (iter (conj acc (take count arr))
               (drop step arr))))

    (iter [] a))

(fn second [arr]
    (. arr 2))

{
 :second second
 :inc inc
 :dec dec
 :empty? empty?
 :rest rest
 :butlast butlast
 :take take
 :apply apply
 :partition partition
 :last last
 :drop drop
 :cons cons
 :conj conj
 }
