(ns Sigyn.core)

; Working in eclipse and the counterclockwise plugin, hit ctrl-alt-s to open the repl (command-alt-s for mac),
; then load the desired namespace like (ns Sigyn.core). Hitting ctrl-alt-s should send new code to the repl
; after you make any edits. I find that this sometimes fails. When this happens, I hit the green run button
; instead. This starts a new repl rather than evaluating the code in the currnet repl.

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
  (match? [this item])
  )

(defprotocol Memory
  "The memory protocol defines the behavior required to act as a table on which rules can be defined."
  (query [this pattern]) ; Returns a seq of item/value pairs.
  (insert [this item value]) ; Puts item/value pairs into the memory.
  )

; It seems defrecord does not allow for docstrings! So, I'll be putting comments above.
; Atomic discrete pattern type; nil matches anything, everything else matches when equal.
(defrecord term-pattern [const]
  Pattern
  (match? [this item]
    (if (nil? const) true (= const (:const item))))
  )
; Hm, needing to access the record contents using :const is slightly annoying, and this
; trend continues with other single-item records here... these may be dominated later by
; more complex records, but this kind of access also seems inefficient if such things
; end up being computation workhorses. I wonder if there is a better way? I think it's
; possible to do something useful with the "extend" function, whichh allows existing
; types to implement protocols.

(defn term [n]
  "A quick function for creating terms."
  (if (= (type n) term-pattern)
    n
    (->term-pattern n)))

; This can serve as a tuple of any more basic pattern type, but is specifically intended as a tuple of
; term patterns, for simple discrete patterns.
(defrecord tuple-pattern [vec]
  Pattern
  (match? [this item]
    (every? #(apply match? %) (map #(vector %1 %2) (:vec this) (:vec item))))
  )

(defn tuple 
  "A quick function for creating tuples."
  [v]
  (->tuple-pattern (map term v)))

(defn all-patterns
  "Generates all patterns that would match the given tuple, by inserting nil in all 
combinations of locations. This is not intended to be feasible for long tuples. It 
is assumed that the items in the tuple are non-nil; otherwise, redundant patterns may 
be produced."
  [instance]
  (if (= (count (:vec instance)) 1)
    (list (tuple [nil]) instance)
    (concat
      (map
        #(tuple (cons nil (:vec %)))
        (all-patterns (tuple (rest (:vec instance)))))
      (map
        #(tuple (cons (first (:vec instance))
                      (:vec %)))
        (all-patterns (tuple (rest (:vec instance)))))))
  )

; A hash memory will index tuples by every possible query. This isn't intended as an
; optimal solution, it's just to get things going. The structure is a hashmap from
; patterns to seqs of pattern-value pairs. To improve this for longer-term it would
; be good to add control over which queries things get explicitly indexed for. The
; immediate purpose of this structure is to allow me to understand how things go
; with a simple memory type before implementing spatial trees. However, it will be useful
; to have a discrete memory type in the long term as well; spatial trees are probably
; not efficient for everything.

; Hash here holds a ref to a hashmap. A ref is updated with alter. Alter needs to occur
; within a dosync, which defines a transaction boundary; it is not necessary that the
; dosync is in the same lexical context (we can leave it out, but then we need to use
; dosync somewhere in any function which calls that code). For now, I've placed it
; directly wrapping the call to alter.

; The hashmap is nested; it's a map from patterns to item-value pairs matching the
; pattern, and the item-value pairs are themselves stored in a map.

(defrecord hash-mem [hash] ; note: this name shadows the clojure function 'hash', preventing us from using it
  Memory
  (query [this pattern]
    ((:hash this) pattern))
  (insert [this item value]
    ; Generate all patterns consistent with the instance, and update the hash table
    ; to add the item/value pair to those entries, adding them if they don't exist.
    (->hash-mem
      (reduce ; Alter takes a function to update the value with, not a new value!
       conj 
       (cons
         hash
         (map
           (fn [p] (assoc (hash p) item value)) ; adds item-value pair if absent, overwrites if present
           (all-patterns item)))))))

(defn empty-hash-mem []
  (->hash-mem (hash-map)))
  
