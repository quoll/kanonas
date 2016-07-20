(ns kanonas.rules
  "Defines rule structures and constructors to keep them consistent"
  (:require [schema.core :as s]
            [kanonas.structs :as st]
            [kanonas.util :as u])
  (:import [clojure.lang Symbol]
           [kanonas.structs Rule]))

(defn gen-rule-name [] (gensym "rule-"))

(s/defn rule :- Rule
  "Creates a new rule"
  ([head body] (rule head body (gen-rule-name)))
  ([head body name]
   (assert (and (sequential? body) (or (empty? body) (every? sequential? body)))
           "Body must be a sequence of constraints")
   (assert (sequential? head) "Head must be a constraint")
   (st/new-rule head body name)))

(s/defn named-rule :- Rule
  "Creates a rule the same as an existing rule, with a different name."
  [name :- Rule
   {:keys [head body downstream]} :- s/Str]
  (st/new-rule head body name downstream))

(defn- de-ns
  "Remove namespaces from symbols in a pattern"
  [pattern]
  (letfn [(clean [e] (if (symbol? e) (symbol (name e)) e))]
    (apply vector (map clean pattern))))

(defmacro r
  "Create a rule, with an optional name.
   Var symbols need not be quoted."
  [& [f :as rs]]
  (let [[nm# rs#] (if (string? f) [f (rest rs)] [(gen-rule-name) rs])
        not-sep# (partial not= :-)
        head# (de-ns (first (take-while not-sep# rs#)))
        body# (map de-ns (rest (drop-while not-sep# rs#)))]
    `(rule (quote ~head#) (quote ~body#) ~nm#)))

(defn check-symbol
  "Asserts that symbols are unbound variables for a query. Return true if it passes."
  [sym]
  (let [n (name sym)]
    (assert (= \? (first n)) (str "Unknown symbol type in rule: " n)) )
  true)

(defprotocol Matching
  (compatible [x y] "Returns true if both elements are compatible"))

(extend-protocol Matching
  Symbol
  (compatible [x _]
    (check-symbol x))
  Object
  (compatible [x y]
    (or (= x y) (and (symbol? y) (check-symbol y)))))

(defn match?
  "Does pattern a match pattern b?"
  [a b]
  (every? identity (map compatible a b)))

(defn find-matches
  "returns a sequence of name/pattern pairs where a matches a pattern in a named rule"
  [a [nm sb]]
  (letfn [(matches? [b]
            "Return a name/pattern if a matches the pattern in b"
            (if (match? a b) [nm b]))]
    (keep matches? sb)))

(defn create-program
  "Converts a sequence of rules into a program.
   A program consists of a map of rule names to rules, where the rules have dependencies."
  [rules]
  (let [name-bodies (u/mapmap :name :body rules)
        triggers (fn [head]
                   (->> name-bodies
                        (mapcat (partial find-matches head))
                        (map first)))
        deps (fn [{:keys [head body name]}]
               (st/new-rule head body name (triggers head)))]
    (u/mapmap :name (map deps rules))))


