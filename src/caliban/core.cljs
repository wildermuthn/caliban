(ns caliban.core
  (:require [reagent.core :as r]
            [caliban.id3]
            [caliban.spec]
            [caliban.d3 :refer [bar-handler el-ref]]
            [caliban.elements :refer [VerticalBar Line LinePoints GridAxis LineTooltips LineHover Brush
                                      Zoomer ClipPath VerticalHoverGridLine]]
            [caliban.render :refer [d3-svg]]
            [caliban.utils :as u]
            [ganymede.core :refer [make-memoize select]]
            [ganymede.box]
            [cljs.spec.alpha :as s]
            [cljs.test :refer [deftest]]
            [clojure.test.check.generators :as cg]
            [cljs.core.async :refer [chan pub sub]])
  (:require-macros [ganymede.macros :refer [div]]
                   [cljs.core.async.macros :refer [go go-loop]]
                   [caliban.macros :refer [d3r log]]))

;;; Error Handling

(defn uncaught-error-handler [msg url line col error]
  (if-let [explain-data (aget error "data")]
    (do
      (.log js/console explain-data)
      true)
    false))

(set! (.-onerror js/window)
      (fn [msg url line col error]
        (uncaught-error-handler msg url line col error)))

;;; API Data

;; General

(s/def ::abs-int (s/and integer? pos? #(< % 1000)))

;; Facebook Posts

(s/def ::facebook_posts (s/coll-of ::facebook_post
                                   :min-count 20))

(s/def ::facebook_post (s/keys :req-un [::post_id
                                        ::video_url
                                        ::length
                                        ::message
                                        ::page
                                        ::metrics]))

(s/def ::post_id string?)
(s/def ::video_url string?)
(s/def ::length ::abs-int)
(s/def ::message string?)
(s/def ::page string?)
(s/def ::timestamp string?)
(s/def ::metrics (s/keys :req-un [::shares ::likes ::comments]))

(s/def ::shares ::abs-int)
(s/def ::likes ::abs-int)
(s/def ::comments ::abs-int)

;;; Generators

(defn rand-facebook-data []
  (cg/generate (s/gen ::facebook_posts)))

(def facebook-data (r/atom (rand-facebook-data)))

;;; Vars

(def graph-blue "#3696DD")

;;; Elements

(div rows
     :display :flex
     :flex-direction :column
     :justify-content :flex-end
     :align-items :stretch
     :position :absolute
     :top 0 :left 0 :right 0 :bottom 0)

(div svg-container
     :flex "1 1 auto"
     :display "flex"
     :padding "50px"
     :align-items :stretch
     :justify-content :center
     :cursor :move
     :position :relative)

(div tooltip-p
     :padding "10px"
     :font-size "16px"
     :background-color :white
     :border "1px solid lightgray"
     :color "gray")

(div tooltip-header
     :text-transform :uppercase
     :font-size "12px"
     :margin-bottom "10px"
     :color "lightgray")

(div tooltip-metrics
     :font-family "Arial"
     :letter-spacing "1.5px"
     :margin-bottom "5px"
     :font-weight "lighter"
     :color graph-blue)

(div tooltip-date
     :color "gray")

(select ["svg.chart"]
        {:flex "1 1 auto"
         :background-color :white})

(select ["svg.chart-2"]
        {:flex "1 1 auto"
         :background-color :white})

(select ["svg .axis-x"]
        {:stroke :black})

(select ["svg .axis-y"]
        {:stroke :black})

(select ["svg .axis-x line"]
        {:stroke :black
         :stroke-width ".5px"
         :opacity .2})

(select ["svg .axis-x path"]
        {:display :none})

(select ["svg .axis-y path"]
        {:display :none})

(select ["svg .axis-y text"]
        {:transform "translate(-50px,0px)"
         :stroke "lightgray"
         :font-size "14px"})

(select ["svg .axis-x text"]
        {:transform "translate(0px,50px)"
         :stroke "lightgray"
         :font-size "14px"})

(select ["svg .axis-y line"]
        {:stroke :black
         :stroke-width ".5px"
         :opacity .2})

(select ["div.tooltips"]
        {:background-color :black
         :width 100
         :height 100})

(defonce counter (r/atom 0))

;;; Components

(defonce publisher (chan))
(defonce publication (pub publisher #(:el-class %)))
(def margin {:top 100 :bottom 100 :left 100 :right 100})
(def start-date (new js/Date 2016 6 1))
(def resolution :days)


(defn simple-chart [attrs id data]
  [svg-container attrs
   [d3-svg
    {:data @data
     :publisher publisher
     :publication publication
     :id id}


    [GridAxis
     {:id :axis-y
      :getter u/views
      :utils {}
      :config {:margin margin
               :ticks 5
               :refs {:zoom-el :Zoomer}
               :type :left}}]

    [GridAxis
     {:id :axis-x
      :getter u/views
      :utils {}
      :config {:date {:start start-date
                      :resolution resolution}
               :margin margin
               :refs {:zoom-el :Zoomer}
               :type :bottom}}]

    [Line
     {:id :line-el
      :getter u/views
      :utils {}
      :config {:stroke graph-blue
               :stroke-width 2
               :curve (.. js/d3 -curveLinear)
               :refs {:zoom-el :Zoomer}
               :margin margin}}]

    [ClipPath
     {:id :clip-path-el
      :getter (constantly false)
      :utils {}
      :config {:margin margin}}]


    [VerticalHoverGridLine
     {:id :hovergridline-el
      :getter u/views
      :utils {}
      :config {:visible? false
               :margin margin
               :stroke-width .25
               :stroke "black"
               :refs {:linehover-el :LineHover
                      :zoom-el :Zoomer}}}]

    [LinePoints
     {:id :points-el
      :getter u/views
      :utils {}
      :config {:visible? false
               :margin margin
               :fill "#3696DD"
               :stroke-width 2
               :stroke "white"
               :r "5"
               :refs {:linehover-el :LineHover
                      :zoom-el :Zoomer}}}]

    [LineTooltips
     {:id :tooltips-el
      :getter u/views
      :utils {}
      :config {:margin {:top 160 :bottom 0 :left 160 :right 0}
               :html (fn [coll]
                       (let [{:keys [i d date]} coll]
                         (r/render-to-string [tooltip-p
                                              [tooltip-header (str "Views:")]
                                              [tooltip-metrics d]
                                              [tooltip-date date]])))
               :date {:start start-date
                      :resolution resolution}
               :visible? false
               :refs {:linehover-el :LineHover
                      :points-el :LinePoints
                      :axis-x :GridAxis
                      :zoom-el :Zoomer}}}]

    [LineHover
     {:id :linehover-el
      :getter u/views
      :utils {}
      :config {:margin margin
               :refs {:zoom-el :Zoomer}}}]

    [Zoomer
     {:id :zoom-el
      :getter (constantly false)
      :utils {}
      :config {:margin margin}}]

    ]])

(def sample-target-id "main-app-area")

(defn render-sample-chart [target-id]
  (r/render [rows
             [simple-chart
              {:on-click #(do
                           (reset! facebook-data (rand-facebook-data)))}
              :chart
              facebook-data]]
            (.getElementById js/document target-id)))
