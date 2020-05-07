(local fennel (require :fennel))
(table.insert (or package.loaders package.searchers) fennel.searcher)

(global test (require :u-test))
(global u (require :utils))

(fn test.add []
    (test.equal (+ 1 1) 2))

(fn test.inc []
    (test.equal 2 (u.inc 1)))

(fn test.dec []
    (test.equal 4 (u.dec 5)))

(fn test.empty? []
    (test.assert (u.empty? []))
    (test.assert (not (u.empty? [1]))))

(fn test.partition []
    (let [t [1 2 3 4 5 6 7 8]
            p (u.partition 2 2 t)]
      (test.equal (length p) 4)
      (test.equal (length p) 4)))

(test.summary)
