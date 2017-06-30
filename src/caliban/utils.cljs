(ns caliban.utils
  (:require-macros [caliban.macros :refer [log klog]]))

;;; Data Getters

(defn get-data [idx coll]
  (let [diffs (map #(-> % :metrics vals idx) coll)
        data (reduce (fn [acc v]
                       (conj acc (+ (last acc) v)))
                     [(first diffs)]
                     (rest diffs))]
    (clj->js data)))

(def scale-data (partial get-data first))
(def views (partial get-data second))
(def comments (partial get-data last))

;;; Scaling

(defn scale-time [domain range]
  (.. js/d3
      (scaleTime)
      (domain (clj->js domain))
      (range (clj->js range))))

(defn scale-linear [domain range]
  (.. js/d3
      (scaleLinear)
      (domain (clj->js domain))
      (range (clj->js range))))

(defn scale-equal [total n]
  (constantly (/ total n)))

(defn scale-by-index [total spacing n]
  (let [container-with-padding (+ total (* spacing n))
        total-item-height (/ container-with-padding n)]
    (fn [datum index]
      (* index total-item-height))))

(defn scale-line [getter state config]
  (let [{:keys [data svg-size]} state
        {:keys [width height]} svg-size
        data (getter data)
        domain [0 (apply max data)]
        scale-x (scale-linear [0 (count data)] [0 width])
        scale-y (scale-linear domain [height 0])]
    [scale-x scale-y]))

(defn scale-line-perc [percentage getter state config]
  (let [{:keys [data svg-size]} state
        {:keys [width height]} svg-size
        [xp yp] percentage
        data (getter data)
        domain [0 (apply max data)]
        scale-x (scale-linear [0 (count data)] [0 (* xp width)])
        scale-y (scale-linear domain [(* yp height) 0])]
    [scale-x scale-y]))


(defn scale-line-perc-zoom [percentage getter state config]
  (fn [k]
    (let [{:keys [data svg-size]} state
          {:keys [width height]} svg-size
          [xp yp] percentage
          data (getter data)
          domain [0 (apply max data)]
          x-start 0
          x-end (count data)
          x-end-k (* k x-end)
          x-end-max (if (<= x-end-k x-end)
                      x-end-k
                      x-end)
          x-range [x-start x-end-max]
          scale-x (scale-linear x-range [0 (* xp width)])
          scale-y (scale-linear domain [(* yp height) 0])]
      [scale-x scale-y])))

(defn scale-vertical-full-fn [getter state config]
  (let [{:keys [data svg-size]} state
        {:keys [width height]} svg-size
        data (getter data)
        domain [0 (apply max data)]
        scale-w (scale-linear domain [0 width])
        scale-h (scale-equal height (count data))
        scale-y (scale-by-index height (:spacing config) (count data))]
    [scale-w scale-h scale-y]))

(defn scale-vertical-perc-fn [percentage getter state config]
  (let [{:keys [data svg-size]} state
        {:keys [width height]} svg-size
        {:keys [spacing]} config
        [xp yp] percentage
        data (getter data)
        domain [0 (apply max data)]
        n (count data)
        mod-height (- (* height yp)
                      (* n spacing))
        scale-w (scale-linear domain [0 (* width xp)])
        scale-h (scale-equal mod-height (count data))
        scale-y (scale-by-index mod-height
                                spacing
                                (count data))]
    [scale-w scale-h scale-y]))


(defn scale-horizontal-full-fn [getter state config]
  (let [{:keys [data svg-size]} state
        {:keys [width height]} svg-size
        data (getter data)
        domain [0 (apply max data)]
        scale-h (scale-linear domain [0 height])
        scale-w (scale-equal width (count data))
        scale-x (scale-by-index width (:spacing config) (count data))]
    [scale-w scale-h scale-x]))

(defn scale-horizontal-perc-fn [percentage getter state config]
  (let [{:keys [data svg-size]} state
        {:keys [width height]} svg-size
        {:keys [spacing]} config
        [xp yp] percentage
        data (getter data)
        domain [0 (apply max data)]
        n (count data)
        mod-width (- (* width xp)
                     (* n spacing))
        scale-h (scale-linear domain [0 (* height yp)])
        scale-w (scale-equal mod-width (count data))
        scale-x (scale-by-index mod-width
                                spacing
                                (count data))]
    [scale-w scale-h scale-x]))

;;; Translation

(defn trans-rect-perc-fn [percentage _ state _]
  (let [{:keys [data svg-size]} state
        {:keys [width height]} svg-size
        svg-size2 (map * [width height] percentage)]
    (constantly svg-size2)))

