
(ns weblarda3.helpers
  (:require
   [goog.object]))

(defn clj->js
  "Recursively transforms ClojureScript values to JavaScript.
  sets/vectors/lists become Arrays, Keywords and Symbol become Strings,
  Maps become Objects. Arbitrary keys are encoded to by `key->js`.
  Options is a key-value pair, where the only valid key is
  :keyword-fn, which should point to a single-argument function to be
  called on keyword keys. Default to `name`."
  [x & {:keys [keyword-fn]
        :or   {keyword-fn name}
        :as options}]
  (letfn [(keyfn [k] (key->js k thisfn))
          (thisfn [x] (cond
                        (nil? x) nil
                        (satisfies? IEncodeJS x) (-clj->js x)
                        (keyword? x) (keyword-fn x)
                        (symbol? x) (str x)
                        (map? x) (let [m (js-obj)]
                                   (doseq [[k v] x]
                                     (goog.object/set m (keyfn k) (thisfn v)))
                                   m)
                        (coll? x) (let [arr (array)]
                                    (doseq [x (map thisfn x)]
                                      (.push arr x))
                                    arr)
                        :else x))]
    (thisfn x)))
