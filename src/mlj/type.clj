(ns mlj.type
  "ML (Hindley Milner) type system."
  (:gen-class))

(def ^:dynamic *type-map* {:int {:type java.lang.Long
                                 :fn integer?}
                           :real {:type java.lang.Double
                                  :fn float?}
                           :bool {:type java.lang.Boolean
                                  :fn #{true false}}
                           :string {:type java.lang.String
                                    :fn string?}
                           :char {:type java.lang.Character
                                  :fn char?}})

(defn type? 
  "Returns true if t is a type form."
  [t]
  (contains? *type-map* t))

(defn type-of 
  "Gets the type of a expression"
  [x]
  (-> (filter #((:fn (% 1)) x) *type-map*)
      first
      key))

(defn check-type 
  "Check if v is of type t"
  [v t]
  ((-> *type-map* t :fn) v))

(defn $
  "Type operator. Assigns type t to v"
  [v t]
  [v t])
