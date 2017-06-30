(ns caliban.d3
  (:require
    [caliban.id3 :refer [ID3 ID3-Events initialize-selection select data render render-enter render-exit
                         initialize-handlers events]]
    [ganymede.core :refer [make-memoize]]
    [ganymede.box]
    [cljs.pprint :refer [pprint]]
    [cljs.spec.alpha :as s]
    [cljs.spec.test.alpha]
    [clojure.string :as str]
    cljsjs.d3)
  (:require-macros [caliban.macros :refer [d3 d3r log klog]]
                   [ganymede.macros :refer [div]]))

;;; Handlers

(defn el-ref [k state]
  (let [elements (->> (:elements state)
                      (reduce merge))
        el (get elements k)]
    (when el
      (-> el :instance (aget "ref")))))

(defn bar-handler [event this state unknown node-index nodes]
  (let [kclass (keyword (.-el-class this))
        ref (el-ref kclass state)
        node (get nodes node-index)
        parent-node (.-parentNode node)]
    (condp = [event (keyword (.-el-class this))]
      [:mouseover :tooltips-el] (let [ref (el-ref kclass state)
                                      line (el-ref :line-el state)]
                                  (d3r line
                                       :attr ["stroke" "red"])
                                  (d3r ref
                                       :style ["fill" "blue"]))

      [:mouseout :tooltips-el] (let [ref (el-ref kclass state)
                                     line (el-ref :line-el state)]
                                 (d3r line
                                      :attr ["stroke" "green"])
                                 (d3r ref
                                      :style ["fill" "white"]))

      [:mouseover :bar3] (let [node (get nodes node-index)
                               g-node (.-parentNode node)
                               color "red"]
                           (d3 :select g-node
                               :select "rect.bar"
                               :attr ["fill" color])
                           (d3 :select g-node
                               :select "text"
                               :style ["display" "block"]))

      [:mouseout :bar3] (let [node (get nodes node-index)
                              g-node (.-parentNode node)
                              color (:fill (.-config this))]
                          (d3 :select g-node
                              :select "rect.bar"
                              :attr ["fill" color])
                          (d3 :select g-node
                              :select "text"
                              :style ["display" "none"]))




      nil)))

;;; Style Utils

(defn attr-op [attribute f]
  (fn [sel]
    (.. sel
        (attr attribute f))
    sel))

(defn scale-int [scale]
  (fn [d i]
    (scale d i)))

(defn scale-translate [scale-x scale-y]
  (fn [d i]
    (str "translate(" (scale-x d i) "," (scale-y d i) ")")))

(defn scale-translate-y [scale]
  (fn [d i]
    (str "translate(0" "," (scale d i) ")")))

(defn scale-translate-x [scale]
  (fn [d i]
    (str "translate(" (scale d i) ",0)")))

(defn call-attr-scaled [attr scale]
  (attr-op attr (scale-int scale)))

(defn call-translate-y [scale]
  (attr-op "transform" (scale-translate-y scale)))

(defn call-translate-x [scale]
  (attr-op "transform" (scale-translate-x scale)))

(defn call-translate [scale-x scale-y]
  (attr-op "transform" (scale-translate scale-x scale-y)))

(defn call-attr [attr f]
  (attr-op attr f))

;;; Draw Utils

(defn reduce-utils [element state config & {:keys [root?]}]
  (let [coll (if root? state element)
        getter (:getter coll)
        utils (:utils coll)]
    (reduce-kv (fn [acc k v]
                 (assoc acc k (v getter state config)))
               {}
               utils)))

(defn merge-utils [state element config]
  (let [element-utils (reduce-utils element state config)
        root-utils (reduce-utils element state config :root? true)]
    (assoc state :utils (merge root-utils element-utils))))

;;; Main Draw Loop

(defn draw [state & {:keys [initialize?]}]
  (let [fns (doall (for [coll (:elements state)]
                     (let [element (-> coll vals first)
                           select-fn (if initialize? initialize-selection select)
                           {:keys [instance getter config]} element
                           ;; TODO: Fix scale/translate utils story
                           ; state (merge-utils state element config)
                           js-data (getter (:data state))
                           this (-> instance
                                    (select-fn state js-data)
                                    (data state js-data)
                                    render-exit
                                    (render state js-data)
                                    (render-enter state js-data))]
                       (when (satisfies? ID3-Events instance)
                         (events this state js-data)
                         (fn [s]
                           (initialize-handlers this s js-data))))))]
    (when initialize?
      (doseq [f fns]
        (when f
          (f state))))))

