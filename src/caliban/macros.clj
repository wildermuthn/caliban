(ns caliban.macros
  (:require [clojure.java.io]))

;;; Logging

(defn map->css [coll]
  (let [mapper (fn [[k v]]
                 (str (name k) ":" (name v) ";"))]
    (reduce str "" (map mapper coll))))

(defn style-msg [msg style]
  (let [style (map->css style)]
    `((str "%c" (with-out-str (cljs.pprint/pprint ~msg))) ~style)))

(defn log*
  ([msg] (log* msg :black))
  ([msg color]
   (let [css {:color (name color)}
         style (style-msg msg css)]
     `(do
        (.info js/console ~@style)
        true))))

(defmacro log [& args]
  `(js/console.log ~@args))

(defmacro kmap [& args]
  (let [ks (mapv keyword args)
        vs (mapv identity args)]
    `(zipmap ~ks ~vs)))

(defmacro klog [& args]
  (let [[args1 color] (if (< 1 (count args))
                        [(first args) (last args)]
                        [(first args) :black])
        args2 (if (vector? args1)
                `(kmap ~@args1)
                `(kmap ~args1))]
    `(log ~args2 ~color)))

;;; D3


(defmacro x-i-fn [x]
  `["x" (~'fn [d# i#] (~x i#))])

(defn d3->cljs [[type data]]
  (let [type-sym (-> type name symbol)]
    (cond

      (and
        (re-find #"call-" (name type))
        (symbol? data)) (let [calling-fn (symbol (str ".-" (last (clojure.string/split (name type) #"call-"))))]
                          `(~'call (fn [sel#]
                                     (.apply (~calling-fn sel#) sel# (~'clj->js ~data)))))
      (vector? data) `(~type-sym ~@data)
      (not (nil? data)) `(~type-sym ~data)
      :else `~type-sym)))

(defmacro d3 [& args]
  (let [d3-args (map d3->cljs (partition 2 args))]
    `(.. js/d3
         ~@d3-args)))

(defmacro d3r [r & args]
  (let [d3-args (map d3->cljs (partition 2 args))]
    `(.. ~r ~@d3-args)))

(defmacro d3rv [r & args]
  (let [d3-args (map d3->cljs (partition 2 args))]
    `(.. ~r ~@d3-args)))

