(ns caliban.render
  (:require
    [caliban.d3 :refer [draw]]
    [caliban.id3 :refer [remove-handlers ID3-Events]]
    [ganymede.core :refer [make-memoize select]]
    [ganymede.box]
    [reagent.core :as r]
    [cljs.pprint :refer [pprint]]
    [goog.events :as events]
    [goog.dom :as dom]
    [cljs.spec.alpha :as s]
    [cljs.spec.test.alpha :refer [check]]
    cljsjs.d3)
  (:require-macros [ganymede.macros :refer [div]]
                   [client.macros :refer [log]]))

;;; Figwheel Utility

(defonce unmounting-state (atom nil))

;;; Goog

(def win (dom/getWindow))

;;; Utils

(defn build-element
  "Build a map of data for d3 elements"
  [chart-id form]
  (let [el (first form)
        attrs (second form)
        id (:id attrs)
        el-class (if (keyword? id) (name id) id)
        svg-class (str "." (name chart-id))]
    {id {:instance (el. svg-class el-class (:config attrs))
         :utils (:utils attrs)
         :getter (:getter attrs)
         :config (:config attrs)}}))

(defn update-element-config! [els new-el-hiccup]
  (let [[el-type opts] new-el-hiccup
        {:keys [id utils getter config]} opts
        el-map (reduce merge {} els)
        el (get el-map id)
        instance (get el :instance)]
    (set! (.-config instance) config)))

(defn svg-size [node]
  {:width (.-clientWidth node)
   :height (.-clientHeight node)})

;;; State

(defn make-state
  "Transform raw data with top-level 'getter', pluck chart elements as 'children'"
  ([comp]
   (let [hiccup (r/argv comp)]
     (make-state comp hiccup)))
  ([comp hiccup]
   (let [node (r/dom-node comp)
         config (second hiccup)
         chart-id (:id config)]
     (-> config
         (assoc :svg-size (svg-size node))
         (assoc :elements (->> (rest (rest hiccup))
                               (map (partial build-element chart-id))))))))


(defn update-state
  [comp hiccup]
  (let [old-state (r/state comp)
        node (r/dom-node comp)
        config (second hiccup)
        new-els (-> hiccup rest rest)
        old-state-els (-> old-state :state :elements)
        state (-> config
                  (assoc :svg-size (svg-size node))
                  (assoc :elements old-state-els))]
    (doseq [el new-els]
      (update-element-config! old-state-els el))
    state))

;;; React Lifecycle Methods

(defn comp-render-update
  ([comp]
    ;; On resize listener event
   (let [hiccup (r/argv comp)]
     (comp-render-update comp hiccup)))
  ([comp hiccup]
   (let [state (update-state comp hiccup)]
     (reset! unmounting-state state)
     (log :comp-render-update)
     (draw state))))

(defn d3-svg-mount [comp]
  (let [state (make-state comp)
        #_listener #_(events/listen win (.-RESIZE events/EventType)
                                    #(comp-render-update comp))]
    (log :dv3-svg-mount)
    (reset! unmounting-state state)
    (draw state :initialize? true)
    (r/set-state comp {:state state
                       ;:resize-listener listener
                       })))

(defn d3-svg-unmount [comp & rest]
  (log :d3-svg-unmount)
  (let [state @unmounting-state
        els (:elements state)]
    (doseq [el els]
      (let [instance (-> el vals first :instance)]
        (when (satisfies? ID3-Events instance)
          (remove-handlers instance)))))
  #_(let [state (r/state comp)
          listener (:resize-listener state)]
      (events/unlistenByKey listener)))


(defn svg-element
  [attr]
  (->> attr :id name (str "svg.cljsd3.") keyword vector))


(def d3-svg
  (r/create-class
    {:component-did-mount d3-svg-mount
     :component-will-receive-props comp-render-update
     :component-will-unmount d3-svg-unmount
     :should-component-update (constantly false)
     :reagent-render svg-element}))


