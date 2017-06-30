(ns caliban.id3)

(defprotocol ID3
  (-initialize-selection [this state data])
  (-select [this state data])
  (-data [this state data])
  (-render [this state data])
  (-render-enter [this state data])
  (-render-exit [this]))

(defprotocol ID3-Events
  (-events [this state data])
  (-initialize-handlers [this state data])
  (-remove-handlers [this]))

;;; Protocol Fn Wrappers

(defn initialize-selection [this state data]
  (-initialize-selection this state data))

(defn select [this state data]
  (-select this state data))

(defn data [this state data*]
  (-data this state data*))

(defn render [this state data]
  (-render this state data))

(defn render-enter [this state data]
  (-render-enter this state data))

(defn render-exit [this]
  (-render-exit this))

(defn events [this state data]
  (-events this state data))

(defn initialize-handlers [this state data]
  (-initialize-handlers this state data))

(defn remove-handlers [this]
  (-remove-handlers this))
