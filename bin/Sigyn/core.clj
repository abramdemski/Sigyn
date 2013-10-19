(ns Sigyn.core)

; Working in eclipse (with the counterclockwise plugin), hit ctrl-alt-s to open the repl (command-alt-s for mac),
; then load the desired namespace like (ns Sigyn.core).

; The currently favoured technique of defining your data structures in clojure is to use defrecord. These have
; the downside of not being imported with the rest of a namespace, because they are created as Java classes.
; The thing to remember is to require and then import.

; Also note that while normal functions will get an updated definition when you send the file to the repl
; again after altering something, the record protocol implementations persist with any objects you've
; already created!! This means you should re-create those objects to use any new definitions.

(defprotocol Pattern
  "The pattern protocol must be implemented by any type which will be stored in memories. For now, the instances 
being matched against are to be of the same type. So, if we think of patterns as representing sets, match? is 
implementing a subset relation."
  (match? [this instance])
  )

(defprotocol Memory
  "The memory protocol defines the behavior required to act as a table on which rules can be defined."
  (query [this pattern])
  (insert [this instance])
  )

; It seems defrecord does not allow for docstrings! So, I'll be putting comments above.
; Atomic discrete pattern type; nil matches anything, everything else matches when equal.
(defrecord term-pattern [const]
  Pattern
  (match? [this instance]
    (if (nil? const) true (= const (:const instance))))
  )

(defn term [n]
  "A quick function for creating terms."
  (->term-pattern n))

; This can serve as a tuple of any more basic pattern type, but is specifically intended as a tuple of
; term patterns, for simple discrete patterns.
(defrecord tuple-pattern [vec]
  Pattern
  (match? [this instance]
    (every? #(apply match? %) (map #(vector %1 %2) (:vec this) (:vec instance))))
  )

(defn tuple [v]
  "A quick function for creating tuples."
  (->tuple-pattern (map term v)))

(defrecord hash-mem [hash]
  

