(ns caliban.elements
  (:require
    [caliban.d3 :refer [call-attr-scaled call-attr call-translate-y call-translate-x call-translate]]
    [caliban.id3 :refer [ID3 -initialize-selection -select -data -render -render-enter -render-exit
                         ID3-Events -events -initialize-handlers -remove-handlers]]
    [caliban.utils :refer [scale-linear scale-time]]
    [clojure.set :refer [map-invert]]
    [cljs.core.async :refer [put! sub chan alts! close!]]
    [cljs.spec.alpha :as s])
  (:require-macros
    [caliban.macros :refer [d3r d3 klog d3rv x-i-fn kmap]]
    [client.macros :refer [log]]
    [cljs.core.async.macros :refer [go-loop go]]))

;;; Utils

(defn resolution->seconds [resolution]
  (condp = resolution
    :seconds (* 1000)
    :minutes (* 1000 60)
    :half-hours (* 1000 60 30)
    :hours (* 1000 60 60)
    :days (* 1000 60 60 24)
    :weeks (* 1000 60 60 24 7)
    :months (/ (* 1000 60 60 24 365)
               12)
    (* 1000 60 60 24 365)))

(defn element-refs
  [state target]
  (let [refs (reduce
               (fn [acc item]
                 (let [k (-> item keys first)
                       v (-> item vals first)
                       ref (-> v :instance (aget "ref"))]
                   (if-let [alias (get target k)]
                     (assoc acc alias ref)
                     acc)))
               {}
               (:elements state))]
    refs))

(defn type->class [config type-k]
  (some-> config :refs clojure.set/map-invert type-k name))

(defn ref-by-i [ref i]
  (d3r ref :filter #(= %2 i)))

(defn data-by-ref [ref]
  (aget (.data (.merge ref (.enter ref))) 0))

(defn ref-by-data [ref]
  (.data (.merge ref (.enter ref))))

(defn element-listen [publication el-class event-type-fns]
  (let [kill-ch (chan)
        sub-ch (sub publication el-class (chan))]
    (go-loop []
      (let [[msg ch-k] (alts! [sub-ch kill-ch])]
        (condp = ch-k
          sub-ch (when-let [f (get event-type-fns (:event msg))]
                   (apply f (:data msg))
                   (recur))
          kill-ch (close! sub-ch)
          nil)))
    kill-ch))

(defn get-transform [ref]
  (if-let [node (.. ref (node))]
    (.zoomTransform js/d3 node)
    (.-zoomIdentity js/d3)))

(defn size-less-margin [state config]
  (let [{:keys [top left bottom right]
         :or {top 0 left 0 bottom 0 right 0}} (:margin config)
        {:keys [svg-size]} state
        {:keys [width height]} svg-size
        width (- width left right)
        height (- height top bottom)]
    [width height]))

(defn margin-translation [config]
  (let [{:keys [top left bottom right]
         :or {top 0 left 0 bottom 0 right 0}} (:margin config)
        translation (str "translate(" left "," top ")")]
    translation))

(defn translation-str [state config]
  (let [translate (-> state :utils :translate)
        [tx ty] (if translate
                  (translate state config)
                  [0 0])
        translation (str "translate(" tx "," ty ")")]
    translation))

(defn remove-all [ref]
  (d3r ref
       :exit nil
       :remove nil))

;;; Bar Element

(deftype VerticalBar [svg-class el-class ^:mutable config ^:mutable ref]

  ID3

  (-initialize-selection [this state data]
    (let [translation (translation-str state config)]
      (set! ref (d3 :select svg-class
                    :append "g"
                    :attr ["class" el-class]
                    :attr ["transform" translation]
                    :selectAll "g"

                    ))
      this))

  (-select [this _ data]
    (set! ref (d3 :select svg-class
                  :select (str "." el-class)
                  :selectAll "g"))
    this)

  (-data [this state data]
    (set! ref (d3r ref :data data))
    this)

  (-render [this state data]
    (let [{:keys [utils svg-size]} state
          {:keys [height]} svg-size
          [scale-w scale-h scale-x] (if-let [scale (:scale config)]
                                      (scale state config)
                                      (:scale utils))
          text-fn (or (:text config)
                      (fn [d] d))
          bar-width (scale-w 0)
          scale-y #(- height (scale-h %))
          width-f (call-attr-scaled "width" scale-w)
          height-f (call-attr-scaled "height" scale-h)
          y-f (call-attr-scaled "y" scale-y)
          x-f (call-attr-scaled "x" scale-x)
          text-y-f (call-attr-scaled "y" (comp #(- % 10) scale-y))
          text-x-f (call-attr-scaled "x" (comp #(+ % (/ bar-width 2)) scale-x))]

      ;; hover area

      (d3r ref
           :select "rect.hit"
           :attr ["height" height]
           :style ["opacity" 0]
           :call width-f
           :attr ["y" 0]
           :attr ["height" height]
           :transition nil
           :duration 500
           :call x-f)

      (d3r ref
           :select "rect.bar"
           :call width-f
           :transition nil
           :duration 500
           :call y-f
           :call x-f
           :call height-f
           )
      (d3r ref
           :select "text"
           :transition nil
           :duration 500
           :call text-y-f
           :call text-x-f
           :text text-fn))
    this)

  (-render-enter [this state data]
    (let [{:keys [utils svg-size]} state
          {:keys [height]} svg-size
          [scale-w scale-h scale-x] (if-let [scale (:scale config)]
                                      (scale state config)
                                      (:scale utils))
          text-fn (or (:text config)
                      (fn [d] d))
          bar-width (scale-w 0)
          scale-y #(- height (scale-h %))
          width-f (call-attr-scaled "width" scale-w)
          height-f (call-attr-scaled "height" scale-h)
          y-f (call-attr-scaled "y" scale-y)
          x-f (call-attr-scaled "x" scale-x)
          text-y-f (call-attr-scaled "y" (comp #(- % 10) scale-y))
          text-x-f (call-attr-scaled "x" (comp #(+ % (/ bar-width 2)) scale-x))
          color-f (call-attr "fill" (:fill config))
          bar (d3r ref
                   :enter nil
                   :append "g")

          ]
      ;; hover area
      (d3r bar
           :append "rect"
           :attr ["class" "hit"]
           :attr ["height" height]
           :style ["opacity" 0]
           :on ["mouseover" (partial (:handler config) :mouseover this state)]
           :on ["mouseout" (partial (:handler config) :mouseout this state)]
           :call width-f
           :attr ["y" 0]
           :attr ["height" height]
           :transition nil
           :duration 500
           :call x-f)

      ;; chart
      (d3r bar
           :append "rect"
           :on ["mouseover" (partial (:handler config) :mouseover this state)]
           :on ["mouseout" (partial (:handler config) :mouseout this state)]
           :attr ["class" "bar"]
           :call width-f
           :call color-f
           :attr ["height" "0"]
           :attr ["x" "0"]
           :attr ["y" height]
           :transition nil
           :duration 500
           :call height-f
           :call x-f
           :call y-f
           )
      (d3r bar
           :append "text"
           :attr ["text-anchor" "middle"]
           :attr ["display" "none"]
           :attr ["fill" "black"]
           :attr ["font-size" "16px"]
           :attr ["font-weight" "bold"]
           :call text-y-f
           :call text-x-f
           :text text-fn))
    this)

  (-render-exit [this]
    (remove-all ref)
    this)

  )

;;; Line Utils

(defn line-point-scales [ref state config]
  (let [data (if-let [f (:domain state)]
               (f (:data state))
               (data-by-ref ref))
        [width height] (size-less-margin state config)
        transform (get-transform ref)
        n (dec (count data))
        x-domain [0 n]
        y-domain (.. js/d3
                     (extent data))
        x (scale-linear x-domain [0 width])
        y (scale-linear [0 (last y-domain)] [height 0])
        xr (.rescaleX transform x)]
    [xr y]))

(defn line-zoom-scales [ref state config]
  (let [data (if-let [f (:domain state)]
               (f (:data state))
               (data-by-ref ref))
        [width height] (size-less-margin state config)
        transform (get-transform ref)
        x-domain [0 (dec (count data))]
        y-domain (.. js/d3
                     (extent data))
        x (scale-linear x-domain [0 width])
        y (scale-linear [0 (last y-domain)] [height 0])
        xr (.rescaleX transform x)]
    [xr y]))

(defn line-hover-scale [ref state config]
  (let [data (ref-by-data ref)
        [width _] (size-less-margin state config)
        n (dec (count data))
        transform (get-transform ref)
        x-domain [0 n]
        x (scale-linear x-domain [0 width])
        xr (.rescaleX transform x)]
    xr))

(defn make-curve [[x y]]
  (let [curve-fn (.. js/d3 -curveLinear)
        line (.. js/d3 (line)
                 (x (fn [d i] (x i)))
                 (y (fn [d i] (y d))))]
    (.curve line curve-fn)))

;;; Line Elements

(defn draw-line [ref state config & opts]
  (let [{:keys [tduration]} opts
        stroke-width (or (-> config :stroke-width) 1)
        curve (-> (line-zoom-scales ref state config)
                  make-curve)]
    (.. ref
        (attr "stroke" (-> config :stroke))
        (attr "stroke-width" stroke-width)
        (attr "fill" "none")
        (transition)
        (duration (or tduration 500))
        (attr "d" curve))))

(deftype Line [svg-class el-class ^:mutable config ^:mutable ref ^:mutable kill-ch]
  ID3
  (-initialize-selection [this _ _]
    (let [translation (margin-translation config)]
      (set! ref (d3 :select svg-class
                    :append "g"
                    :attr ["transform" translation]
                    :attr ["class" el-class]
                    :attr ["clip-path" (str "url(#clip)")]
                    :append "path")))
    this)

  (-select [this _ _]
    (set! ref (d3 :select svg-class
                  :select (str "." el-class)
                  :select "path"))
    this)

  (-data [this _ data]
    (set! ref (d3r ref :datum data))
    this)

  (-render [this _ _] this)

  (-render-enter [this state _]
    (draw-line ref state config)
    this)

  (-render-exit [this]
    (remove-all ref)
    this)

  ID3-Events

  (-events [this _ _] this)

  (-initialize-handlers [this state _]
    (let [ch (element-listen
               (:publication state)
               (type->class config :Zoomer)
               {:zoom (fn [t]
                        (.. ref
                            (call (.-transform (js/d3.zoom)) t))
                        (draw-line ref state config :tduration 0))})]
      (set! kill-ch ch)
      this))

  (-remove-handlers [this]
    (put! kill-ch true)
    this))

(deftype LinePoints [svg-class el-class config ^:mutable ref ^:mutable kill-ch-a
                     ^:mutable kill-ch-b]

  ID3

  (-initialize-selection [this state data]
    (let [translation (margin-translation config)]
      (set! ref (d3 :select svg-class
                    :append "g"
                    :attr ["transform" translation]
                    :attr ["class" el-class]
                    :selectAll "circle"))
      this))

  (-select [this _ data]
    (set! ref (d3 :select svg-class
                  :select (str "g." el-class)
                  :selectAll "circle"))
    this)

  (-data [this state data]
    (set! ref (d3r ref :data data))
    this)

  (-render [this state data]
    (let [{:keys [utils svg-size]} state
          [x y] (line-point-scales ref state config)]
      (when (= false (:visible? config))
        (d3r ref
             :style ["display" "none"]))
      (d3r ref
           :transition nil
           :duration 500
           :attr ["cx" (fn [d i] (x i))]
           :attr ["cy" (fn [d i] (y d))])
      this))

  (-render-enter [this state data]
    (let [{:keys [utils svg-size]} state
          [x y] (line-point-scales ref state config)
          {:keys [stroke fill r stroke-width]} config
          enter-ref (d3r ref :enter nil :append "circle")]
      (d3r enter-ref
           :attr ["r" r]
           :attr ["fill" fill]
           :attr ["stroke" stroke]
           :attr ["stroke-width" stroke-width]
           :attr ["cx" (fn [d i] (x i))]
           :attr ["cy" (fn [d i] (y d))])
      (when (= false (:visible? config))
        (d3r enter-ref
             :style ["display" "none"]))
      (set! ref (d3r enter-ref :merge ref))
      this))

  (-render-exit [this]
    (remove-all ref)
    this)

  ID3-Events

  (-events [this _ _] this)

  (-initialize-handlers [_ state _]
    (set! kill-ch-a (element-listen
                      (:publication state)
                      (type->class config :Zoomer)
                      {:zoom
                       (fn [t]
                         (d3r ref
                              :style ["display" "none"])
                         (let [[x y] (line-point-scales ref state config)]
                           (.. ref
                               (call (.-transform (js/d3.zoom)) t))
                           (d3r ref
                                :attr ["cx" (fn [d i] (x i))]
                                :attr ["cy" (fn [d i] (y d))])))}))
    (set! kill-ch-b (element-listen
                      (:publication state)
                      (type->class config :LineHover)
                      {:mouseover (fn [d i _]
                                    (let [point (ref-by-i ref i)]
                                      (d3r point
                                           :style ["display" "block"])))
                       :mouseout (fn [d i _]
                                   (let [point (d3r ref :filter #(= %2 i))]
                                     (d3r point
                                          :style ["display" "none"])))})))

  (-remove-handlers [this]
    (put! kill-ch-a :kill)
    (put! kill-ch-b :kill)))

(deftype LineTooltips [svg-class el-class ^:mutable config ^:mutable ref
                       ^:mutable kill-ch-a ^:mutable kill-ch-b]

  ID3

  (-initialize-selection [this state data]
    (set! ref (d3 :select "div.svg-container"
                  :append "div"
                  :attr ["class" el-class]
                  :style ["position" "absolute"]
                  :style ["pointer-events" "none"]
                  :html "<div></div>"))
    this)

  (-select [this _ data]
    (set! ref (d3 :select (str "div." el-class)))
    this)

  (-data [this state data]
    (set! ref (d3r ref :data data))
    this)

  (-render [this state data]
    (when (= false (:visible? config))
      (d3r ref
           :style ["display" "none"]))
    this)

  (-render-enter [this state data]
    this)

  (-render-exit [this]
    (remove-all ref)
    this)

  ID3-Events

  (-events [this _ _] this)

  (-initialize-handlers [this state _]
    (let [{:keys [top left bottom right]
           :or {top 0 left 0 bottom 0 right 0}} (:margin config)]
      (set! kill-ch-a (element-listen
                        (:publication state)
                        (type->class config :Zoomer)
                        {:zoom
                         (fn [t]
                           (let []
                             (.. ref
                                 (call (.-transform (js/d3.zoom)) t))
                             (.. ref
                                 (style "display" "none"))))}))
      (set! kill-ch-b (element-listen
                        (:publication state)
                        (type->class config :LineHover)
                        {:mouseover (fn [d i _]
                                      (let [data (.. ref
                                                     (enter)
                                                     (merge ref)
                                                     (data))
                                            ;;; TODO: abstract away into util functions
                                            {:keys [start resolution]} (:date config)
                                            start-date-ts (.getTime start)
                                            resolution (resolution->seconds resolution)
                                            point-date (new js/Date (+ start-date-ts (* i resolution)))
                                            refs (element-refs state (:refs config))
                                            line-points (:LinePoints refs)
                                            point (d3r line-points :filter #(= %2 i))
                                            node (d3r point :node nil)
                                            [cx cy] [(+ left (int (.getAttribute node "cx")))
                                                     (+ top (int (.getAttribute node "cy")))]
                                            date-format (js/d3.timeFormat "%b %e - %I:%M%p ")]
                                        (d3r ref
                                             :style ["left" (str cx "px")]
                                             :style ["top" (str cy "px")]
                                             :html ((:html config) {:d (aget data i)
                                                                    :i i
                                                                    :date (date-format point-date)})
                                             :style ["display" "block"])))
                         :mouseout #(d3r ref
                                         :style ["display" "none"])}))))

  (-remove-handlers [this]
    (put! kill-ch-a :kill)
    (put! kill-ch-b :kill)))

(defn draw-line-hover [ref state config]
  (let [[width height] (size-less-margin state config)
        x (line-hover-scale ref state config)
        width (- (x 1) (x 0))]
    (d3r ref
         :attr ["x" (fn [d i] (- (x i)
                                 (/ width 2)))]
         :attr ["y" (fn [d i] 0)]
         :attr ["width" width]
         :attr ["height" height])))

(deftype LineHover [svg-class el-class ^:mutable config ^:mutable ref ^:mutable kill-ch]
  ID3

  (-initialize-selection [this state data]
    (let [translation (margin-translation config)]
      (set! ref (d3 :select svg-class
                    :append "g"
                    :attr ["transform" translation]
                    :attr ["class" el-class]
                    :selectAll "rect"))
      this))

  (-select [this _ _]
    (set! ref (d3 :select svg-class
                  :select (str "g." el-class)
                  :selectAll "rect"))
    this)

  (-data [this _ data]
    (set! ref (d3r ref :data data))
    this)

  (-render-exit [this]
    (remove-all ref)
    this)

  (-render [this state data]
    this)

  (-render-enter [this state data]
    (let [enter-ref (d3r ref
                         :enter nil
                         :append "rect"
                         :attr ["fill" "red"]
                         :attr ["opacity" "0"])]
      (set! ref (d3r enter-ref
                     :merge ref))
      (draw-line-hover ref state config)
      this))

  ID3-Events

  (-events [this state data]
    (d3r ref
         :on ["mouseover" (fn [d i nodes]
                            (let [pub-ch (:publisher state)]
                              (put! pub-ch {:el-class el-class
                                            :event :mouseover
                                            :data [d i nodes]})))] ;; publishes :mouseover event
         :on ["mouseout" (fn [d i nodes]
                           (let [pub-ch (:publisher state)]
                             (put! pub-ch {:el-class el-class
                                           :event :mouseout
                                           :data [d i nodes]})))]) ;; publishes :mouseout event
    this)

  (-initialize-handlers [this state data]
    (set! kill-ch (element-listen
                    (:publication state)
                    (type->class config :Zoomer)
                    {:zoom
                     (fn [t]
                       (let []
                         (.. ref
                             (call (.-transform (js/d3.zoom)) t))
                         (draw-line-hover ref state config)))}))
    this)

  (-remove-handlers [this]
    (d3r ref
         :on ["mouseover" nil]
         :on ["mouseout" nil])
    (put! kill-ch :kill)))

;;; Axis

(defn axis-scale-time [ref state config data]
  (let [[width height] (size-less-margin state config)
        {:keys [start resolution]} (:date config)
        transform (get-transform ref)
        n (dec (count data))

        ;;; TODO: Abstract away into util functions, as this is resused by tooltip
        start-date-ts (.getTime (get-in config [:date :start]))
        resolution (resolution->seconds resolution)
        end-date-ts (+ start-date-ts (* n resolution))
        x (scale-time [(new js/Date start-date-ts)
                       (new js/Date end-date-ts)]
                      [0 width])
        xr (.rescaleX transform x)]
    xr))

(defn axis-scale-linear [ref state config data]
  (let [[width height] (size-less-margin state config)
        transform (get-transform ref)
        n (dec (count data))
        x (scale-linear [0 n] [0 width])
        xr (.rescaleX transform x)]
    xr))

(defn axis-scales [ref state config data]
  (let [[width height] (size-less-margin state config)
        min-y 0
        max-y (last data)
        y (scale-linear [min-y max-y] [height 0])
        x (if (:date config)
            (axis-scale-time ref state config data)
            (axis-scale-linear ref state config data))]
    [x y]))

(defn create-axis
  ([ref state config]
   (create-axis ref state config 1))
  ([ref state config k]
   (let [data (data-by-ref ref)
         [x y] (axis-scales ref state config data)
         [width height] (size-less-margin state config)
         etype (:type config)
         inner-tick-size (condp = etype
                           :top 0
                           :bottom (- height)
                           :left (- width)
                           :right 0)
         axis (condp = etype
                :top (.axisTop js/d3 x)
                :bottom (.axisBottom js/d3 x)
                :left (.axisLeft js/d3 y)
                :right (.axisRight js/d3 y))]

     (.. axis
         (tickSizeOuter 0)
         (tickSizeInner inner-tick-size)
         (ticks 7))
     axis)))

(defn draw-axis
  ([ref axis svg-class]
   (d3r ref
        :call axis))
  ([ref axis svg-class transition]
   (d3r ref
        :transition nil
        :duration transition
        :call axis)))

(deftype GridAxis [svg-class el-class ^:mutable config ^:mutable ref ^:mutable state-ref
                   ^:mutable kill-ch]

  ID3

  (-initialize-selection [this state data]
    (let [etype (:type config)
          [width height] (size-less-margin state config)
          {:keys [top left bottom right]
           :or {top 0 left 0 bottom 0 right 0}} (:margin config)
          top (if (= etype :bottom) (+ top height) top)
          translation (str "translate(" left "," top ")")]
      (set! ref (d3 :select svg-class
                    :append "g"
                    :attr ["transform" translation]
                    :attr ["class" el-class]))
      this))

  (-select [this _ data]
    this)

  (-data [this state data]
    (set! ref (d3r ref :datum data))
    (set! state-ref state)
    this)

  (-render [this state data]
    (let [axis (create-axis ref state config)]
      (draw-axis ref axis svg-class 500)
      this))

  (-render-enter [this state data]
    this)

  (-render-exit [this]
    this)

  ID3-Events

  (-events [this _ _] this)

  (-initialize-handlers [this state data]
    (set! kill-ch (element-listen
                    (:publication state)
                    (type->class config :Zoomer)
                    {:zoom
                     (fn [t]
                       (let [axis (create-axis ref state-ref config)]
                         (.. ref
                             (call (.-transform (js/d3.zoom)) t))
                         (draw-axis ref axis svg-class)))})))

  (-remove-handlers [this]
    (put! kill-ch :kill)))

(deftype VerticalHoverGridLine [svg-class el-class ^:mutable config ^:mutable ref
                                ^:mutable kill-ch-a ^:mutable kill-ch-b]

  ID3

  (-initialize-selection [this state data]
    (let [translation (margin-translation config)]
      (set! ref (d3 :select svg-class
                    :append "g"
                    :attr ["transform" translation]
                    :attr ["class" el-class]
                    :selectAll "rect"))
      this))

  (-select [this _ data]
    (set! ref (d3 :select svg-class
                  :select (str "g." el-class)
                  :selectAll "rect"))
    this)

  (-data [this state data]
    (set! ref (d3r ref :data data))
    this)

  (-render [this state data]
    (let [{:keys [utils svg-size]} state
          [x y] (line-point-scales ref state config)]
      (when (= false (:visible? config))
        (d3r ref
             :style ["display" "none"]))
      (d3r ref
           :transition nil
           :duration 500
           :attr ["x" (fn [d i] (x i))]
           :attr ["y" 0])
      this))

  (-render-enter [this state data]
    (let [[_ height] (size-less-margin state config)
          [x _] (line-point-scales ref state config)
          {:keys [stroke fill stroke-width]} config
          enter-ref (d3r ref :enter nil :append "rect")]
      (d3r enter-ref
           :attr ["width" stroke-width]
           :attr ["height" height]
           :attr ["fill" stroke]
           :attr ["x" (fn [d i] (x i))]
           :attr ["y" 0])
      (when (= false (:visible? config))
        (d3r enter-ref
             :style ["display" "none"]))
      (set! ref (d3r enter-ref :merge ref))
      this))

  (-render-exit [this]
    (remove-all ref)
    this)

  ID3-Events

  (-events [this _ _] this)

  (-initialize-handlers [_ state _]
    (set! kill-ch-a (element-listen
                      (:publication state)
                      (type->class config :Zoomer)
                      {:zoom
                       (fn [t]
                         (d3r ref
                              :style ["display" "none"])
                         (let [[x _] (line-point-scales ref state config)]
                           (.. ref
                               (call (.-transform (js/d3.zoom)) t))
                           (d3r ref
                                :attr ["x" (fn [d i] (x i))]
                                :attr ["y" 0])))}))
    (set! kill-ch-b (element-listen
                     (:publication state)
                     (type->class config :LineHover)
                     {:mouseover (fn [d i _]
                                   (let [point (ref-by-i ref i)]
                                     (d3r point
                                          :style ["display" "block"])))
                      :mouseout (fn [d i _]
                                  (let [point (d3r ref :filter #(= %2 i))]
                                    (d3r point
                                         :style ["display" "none"])))})))

  (-remove-handlers [this]
    (put! kill-ch-a :kill)
    (put! kill-ch-b :kill)))

;;; UI Elements

(deftype Brush [svg-class el-class ^:mutable config ^:mutable ref ^:mutable brush-ref]

  ID3

  (-initialize-selection [this state data]
    (let [translate (-> state :utils :translate)
          [tx ty] (if translate
                    (translate state config)
                    [0 0])
          translation (str "translate(" tx "," ty ")")
          {:keys [utils svg-size]} state
          {:keys [width height]} svg-size
          [x y] (if-let [scale (:scale config)]
                  (scale state config)
                  (:scale utils))
          brush (.. js/d3
                    (brushX)
                    (extent #js [#js [0 0] #js [width height]]))
          widthd4 (/ width 4)]
      (set! brush-ref brush)
      (set! ref (d3 :select svg-class
                    :append "g"
                    :attr ["transform" translation]
                    :attr ["class" el-class]
                    :attr ["width" (x width)]
                    :attr ["height" (y height)]))
      (d3r ref
           :append "rect"
           :attr ["fill" "gray"]
           :style ["opacity" ".5"]
           :attr ["width" "100%"]
           :attr ["height" "100%"])
      (d3r ref
           :call brush)

      (.move brush ref #js [0 width])

      this))

  (-select [this _ data]
    this)

  (-data [this state data]
    this)

  (-render [this state data]
    this
    this)

  (-render-enter [this state data]
    this)

  (-render-exit [this]
    this)

  ID3-Events

  (-events [this state data]
    (let [pub-ch (:publisher state)]
      (d3r ref
           :call (.. js/d3
                     (zoom)
                     (on "zoom" (fn []
                                  (let [t (.. js/d3 -event -transform)
                                        k (.-k t)]
                                    (put! pub-ch {:el-class el-class
                                                  :event :zoom
                                                  :data [k]})))))))
    this)

  (-initialize-handlers [this state data]
    (element-listen
      (:publication state)
      el-class
      {:zoom
       (fn [k]
         (let [width (get-in state [:svg-size :width])
               widthdk (* width k)
               padding (- width widthdk)]
           (.move brush-ref ref #js [padding widthdk])))})
    this)

  (-remove-handlers [this] this))

(deftype Zoomer [svg-class el-class ^:mutable config ^:mutable ref]
  ID3
  (-initialize-selection [this state _] this)
  (-select [this _ _] this)
  (-data [this _ _] this)
  (-render [this _ _] this)
  (-render-enter [this _ _] this)
  (-render-exit [this] this)

  ID3-Events

  (-events [this state _]
    (let [pub-ch (:publisher state)
          {:keys [svg-size]} state
          {:keys [width height]} svg-size
          {:keys [top left bottom right]
           :or {top 0 left 0 bottom 0 right 0}} (:margin config)
          width (- width left right)
          height (- height top bottom)]
      (.. js/d3
          (select svg-class)
          (call (.. js/d3
                    (zoom)
                    (scaleExtent #js [1 js/Infinity])
                    (translateExtent #js [#js [0 0] #js [width height]])
                    (extent #js [#js [0 0] #js [width height]])
                    (on "zoom" (fn []
                                 (let [t (.. js/d3 -event -transform)]
                                   (put! pub-ch {:el-class el-class
                                                 :event :zoom
                                                 :data [t]}))))))))
    this)

  (-initialize-handlers [this _ _] this)
  (-remove-handlers [this] this))

(deftype ClipPath [svg-class el-class config]
  ID3
  (-initialize-selection [this state _]
    (let [[width height] (size-less-margin state config)]
      (d3 :select svg-class
          :append "defs"
          :append "clipPath"
          :attr ["id" "clip"]
          :append "rect"
          :attr ["width" width]
          :attr ["height" height]))
    this)
  (-select [this _ _] this)
  (-data [this _ data] this)
  (-render [this _ _] this)
  (-render-enter [this state _] this)
  (-render-exit [this] this))

;;; Specs

;; VerticalBar

(s/def :element.VerticalBar/fill string?)
(s/def :element.VerticalBar/spacing int?)
(s/def :element.VerticalBar/text fn?)

(s/def :element.VerticalBar/config (s/keys :req-un [::handler
                                                    :element.VerticalBar/fill
                                                    :element.VerticalBar/spacing
                                                    :element.VerticalBar/text]))

;; Line

(s/def :element.Line/stroke string?)
(s/def :element.Line/curve fn?)

(s/def :element.Line/config (s/keys :req-un [:element.Line/stroke
                                             :element.Line/curve]))

;; LinePoints

(s/def :element.LinePoints/visible? boolean?)
(s/def :element.LinePoints/config (s/keys :req-un [:element.LinePoints/visible?]))


;; LineTooltips

(s/def :element.LineTooltips/visible? boolean?)
(s/def :element.LineTooltips/config (s/keys :req-un [:element.LineTooltips/visible?]))

;; LineHover

(s/def :element.LineHover/config any?)

;; Axis

(s/def :element.Axis/type #{:top :bottom :left :right})
(s/def :element.Axis/config (s/keys :req-un [:element.Axis/type]))
(s/def :element.GridAxis/type #{:top :bottom :left :right})
(s/def :element.GridAxis/config (s/keys :req-un [:element.GridAxis/type]))
