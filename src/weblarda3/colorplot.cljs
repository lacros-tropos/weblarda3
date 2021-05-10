(ns weblarda3.colorplot
  (:require
   [cljsjs.d3]
   [cljs.core.async :as async]
   [colormaps]
   [goog.object]
   [goog.string.format]
   [antizer.reagent :as ant]
   [weblarda3.vis :as vis]
   [weblarda3.helpers :as h]
   [weblarda3.colormap :as cmap]
   [custom :as c]))


(def avail-var-converters ["" "log" "dB"])

(defn colorplot-render [colorplotProps-cursor id]
  [:div {:id (str "cp-"id) :class "colorplot" 
         :width (get-in @colorplotProps-cursor [:outerSize :w])
         :height (get-in @colorplotProps-cursor [:outerSize :h])}       
   [:canvas {:width (get-in @colorplotProps-cursor [:outerSize :w])
             :height (get-in @colorplotProps-cursor [:outerSize :h])}]
   [:svg {:width (get-in @colorplotProps-cursor [:outerSize :w])
          :height (get-in @colorplotProps-cursor [:outerSize :h])
          :id (str "cp-svg-" id) :class "colorplot-svg"}]])
   ;[:svg {:width (get-in @colorplotProps-cursor [:sizeHoverTree :w])
   ;       :height (get-in @colorplotProps-cursor [:sizeHoverTree :h])
   ;       :id "tooltip-colorplot"}]
   

(defn gen-plain-selector 
  "generate a selector map {03.05 [0]}"
  [keys node]
  (into {} (map (fn [k] [k [node]]) keys)))


(defn setup-rel-coords [plotProps]
  (fn [[mouseX mouseY]]
    (let [relx (/ (- mouseX 50) (get-in plotProps [:imageSize :w]))
          rely (/ (- mouseY 5) (get-in plotProps [:imageSize :h]))]
      (if (and (<= 0 relx 1) (<= 0 rely 1))
          [relx (- 1 rely)]
          [nil nil]))))

(defn rel-to-loc [rel-coords x-mapping y]
  ;(println "rel to loc" x-mapping (count x-mapping))
  ;(println "rel coords" rel-coords (* (get rel-coords 0) (- (count x-mapping) 1)) (* (get rel-coords 1) (- (count y) 1)))
  [(nth x-mapping (js/Math.round (* (get rel-coords 0) (- (count x-mapping) 1)))) (js/Math.round (* (get rel-coords 1) (- (count y) 1)))])  


(defn add-click-maker [parent xycoords]
  (println "add marker " xycoords)
  (-> parent
    (.append "path")
    (.attr "d" (-> (js/d3.symbol) (.type js/d3.symbolCross) (.size 18)))
    (.attr "transform" (str " translate("(get xycoords 0) "," (get xycoords 1) ")"))
    (.attr "fill" "dimgrey")))


(defn data-container-as-js [data-container]
  (let [new (h/clj->js (dissoc data-container "var" "mask" "ts" "rg"))]
    (goog.object/set new "var" (get data-container "var"))
    (goog.object/set new "mask" (get data-container "mask"))
    (goog.object/set new "ts" (get data-container "ts"))
    (goog.object/set new "rg" (get data-container "rg"))
   new))

; map pixel to time if time is non-linear
; {pixel-no ind-time}
(defn abs[x] (max x (- x)))

(defn nearest 
  "input sorted-map from list [1 4 6 8 10] => {1 0, 4 1, 6 2, 8 3, 10 4}
  (apply sorted-map (flatten (map-indexed (fn [i v] [v i]) [1 4 6 8 10])))"
  [sm x]           
  (let [greater (first (subseq sm >= x))
        smaller (first (rsubseq sm <= x))]
    (reverse (apply min-key #(abs (- (first %) x)) (remove nil? [greater smaller])))))

(defn pixel-time-map [plotwidth times]
  (let [t-beg (first times)
        t-end (last times)
        t-at-pixel (map (fn [p] (+ t-beg (* p (/ (- t-end t-beg) plotwidth)))) (range plotwidth))
        sorted-times (apply sorted-map (flatten (map-indexed (fn [i v] [v i]) times)))]
      (map (fn [t-px] (first (nearest sorted-times t-px))) t-at-pixel)))
; ----------------------------------------

(defn plain-node-selector [no]
  (fn [] no))
  
(defn corr-node-selector [correspNodes] 
 (fn [x y] (get correspNodes [x y])))

(defn get-data-container-value [data-container loc-coords]
  (let [value (-> (get data-container "var") (goog.object/get (first loc-coords)) (goog.object/get (second loc-coords)))
        mask (-> (get data-container "mask") (goog.object/get (first loc-coords)) (goog.object/get (second loc-coords)))]
   (if-not mask value -999)))

(defn get-data-container-value-timeseries [data-container loc-coords]
  (let [value (-> (get data-container "var") (goog.object/get (first loc-coords)))
        mask (-> (get data-container "mask") (goog.object/get (first loc-coords)))]
    (if-not mask value -999)))


(defn tooltip-text [data-container loc-coords rel-coords]
  ;todo add the var conversion
  (let [ts (-> (get data-container "ts") (goog.object/get (first loc-coords)))
        rg (-> (get data-container "rg") (goog.object/get (second loc-coords)))
        unit (get data-container "var_unit")
        val-convert (case (get data-container "plot_varconverter")
                     "dB" #(* (js/Math.log10 %) 10)
                     (fn [a] a))
        value (val-convert (get-data-container-value data-container loc-coords))
        fmt (if (and (< value 0.001) (> value 0)) ".3e" ".3f")
        value-fmted ((js/d3.format fmt) value)]
   (str (apply goog.string.format (flatten ["[%.2f %.2f]" rel-coords])) "   " loc-coords "<br />" 
    (.format (js/moment.utc ts "X") "YYYYMMDD_HHmmss") "<br />" 
    (goog.string.format "%.1f m" rg) "<br /> Value " value-fmted " " unit)))


(defn tooltip-text-timeseries [data-container loc-coords rel-coords]
  ;todo add the var conversion
  (let [ts (-> (get data-container "ts") (goog.object/get (first loc-coords)))
        ;rg (-> (get data-container "rg") (goog.object/get (second loc-coords)))
        unit (get data-container "var_unit")
        val-convert (case (get data-container "plot_varconverter")
                      "dB" #(* (js/Math.log10 %) 10)
                      (fn [a] a))
        value (val-convert (get-data-container-value-timeseries data-container loc-coords))
        fmt (if (and (< value 0.001) (> value 0)) ".3e" ".3f")
        value-fmted ((js/d3.format fmt) value)]
    (str (apply goog.string.format (flatten ["[%.2f %.2f]" rel-coords])) "   " loc-coords "<br />"
         (.format (js/moment.utc ts "X") "YYYYMMDD_HHmmss") 
     "<br /> Value " value-fmted " " unit)))

(defn update-tooltip [data-container loc-coords rel-coords mouse]
  (let [dimlabel (get data-container "dimlabel")
        text (case dimlabel
              ["time" "range"] (tooltip-text data-container loc-coords rel-coords)
              ["time"] (tooltip-text-timeseries data-container loc-coords rel-coords)
              (ant/notification-error {:message "Error"  :duration 20
                                                :description (str dimlabel "tooltip not supported yet.")}))]
   (-> (js/d3.select "#tree-tooltip") (.style "opacity" 1)
       (.html text)
       (.style "left" (str (+ (first mouse) 20) "px"))
       (.style "top" (str (+ 15 (last mouse)) "px")))))


(defn plot-timeheight [colorplotProps-cursor id data-containers-cursor which click-channel] 
 (console.log "plot timeheight" id which)
 (if-not (nil? (get-in @data-containers-cursor [which]))
  (let [;plotProps (get-in @app-state [:colorplotProps])
        plotProps @colorplotProps-cursor
        data-container (get-in @data-containers-cursor [which])
        colormap (get data-container "colormap")
        thres (js/thresholds.get colormap)
        color (cmap/make-colormap (js-invoke js/colormaps (if (.includes (js/colormaps.available) colormap) colormap "rainbow"))
                                  :scale (get data-container "plot_varconverter")
                                  :rg (h/clj->js (get data-container "var_lims"))
                                  :thres thres)
        context (-> (js/d3.select (str "#cp-" id " canvas")) (.node) (.getContext "2d"))
        px-ts-mapping (pixel-time-map (get-in plotProps [:imageSize :w]) (get data-container "ts"))
        calc-rel-coords (setup-rel-coords plotProps)]
    
    (println "Update colorplot " (keys data-container) (get data-container "system") (get data-container "name"))
    (-> (js/d3.select (str "#cp-" id " svg")) (.selectAll "*") (.remove))
    (-> context (.clearRect 0 0 (get-in plotProps [:outerSize :w]) (get-in plotProps [:outerSize :h])))
    ; paint the canvas             
    (js/custom.paint_color context (data-container-as-js data-container) (h/clj->js plotProps) color (h/clj->js px-ts-mapping) js/d3.rgb)

    (let [times (map (fn [t] (js/Date. (* t 1000))) (get data-container "ts"))
          timedomain [(first times) (last times)]
          timerange [0 (get-in plotProps [:imageSize :w])]
          scaletime (-> (js/d3.scaleTime) (.domain (h/clj->js timedomain)) (.range (h/clj->js timerange)))]
      (-> (js/d3.select (str "#cp-" id " svg"))
          (.append "g")
          (.attr "transform" (str "translate(50," (+ 5 (get-in plotProps [:imageSize :h])) ")"))
          (.call (-> (js/d3.axisBottom scaletime)
                     (.tickFormat (js/d3.utcFormat "%H:%M:%S"))))))

    (let [ranges (get data-container "rg")
          rgdomain #js[(first ranges) (last ranges)]
          rgrange #js[(get-in plotProps [:imageSize :h]) 0]
          scalerg (-> (js/d3.scaleLinear) (.domain rgdomain) (.range rgrange))]
      (-> (js/d3.select (str "#cp-" id " svg"))
          (.append "g")
          (.attr "transform" (str "translate(50," "5)"))
          (.call (-> (js/d3.axisLeft scalerg)))))

    (-> (js/d3.select (str "#cp-" id " svg"))
        (.append "text")
        ;(.attr "class" "yearlabel")
        (.attr "transform" (str "translate(" 
                                (/ (get-in plotProps [:imageSize :w]) 2) "," 
                                (+ (get-in plotProps [:imageSize :h]) 40) ")"))
        (.attr "font-weight" "bold")
        (.text "Time [UTC]"))

    (-> (js/d3.select (str "#cp-" id " svg"))
        (.append "text")
        ;(.attr "class" "yearlabel")
        (.attr "transform" (str "translate(" 
                                11 "," 
                                (+ (/ (get-in plotProps [:imageSize :h]) 2) 25) ")" 
                                "rotate(-90)"))
        (.attr "font-weight" "bold")
        (.text "Range [m]"))
    
    (-> (js/d3.select (str "#cp-" id " svg"))
        (.append "text")
        ;(.attr "class" "yearlabel")
        (.attr "transform" (str "translate(" 
                                (+ (get-in plotProps [:imageSize :w]) 68) "," 
                                (- (get-in plotProps [:imageSize :h]) 7) ")" 
                                "rotate(-90)"))
        ;(.attr "font-weight" "bold")
        (.text (str which)))
    
    (-> (js/d3.select (str "#cp-" id " svg"))
        (.append "text")
        (.attr "transform" (str "translate(" 
                                (+ (get-in plotProps [:imageSize :w]) 85) "," 
                                (- (get-in plotProps [:imageSize :h]) 7) ")" 
                                "rotate(-90)"))

        (.text (str " [" (get data-container "var_unit") "]")))

    (-> (js/d3.select (str "#cp-" id " svg"))
        (.append "line")
        (.attr "class" "line-vert")
        (.attr "transform" "translate(50,5)")
        (.attr "stroke" "black") (.attr "stroke-width" "1px"))
      ;(.attr "x1" x) (.attr "x2" x))
      ;(.attr "y1" 0) (.attr "y2" (get imageSize :h)))
    (-> (js/d3.select (str "#cp-" id " svg"))
        (.append "line")
        (.attr "class" "line-hor")
        (.attr "transform" "translate(50,5)")
        (.attr "stroke" "black") (.attr "stroke-width" "1px"))
      ;(.attr "x1" x) (.attr "x2" x)))
      ;(.attr "y1" 0) (.attr "y2" (get imageSize :h))))
    
    (-> (js/d3.select (str "#cp-" id " svg"))
        (.append "g") (.attr "class" "colormap")
        (.attr "transform" (str "translate(" (- (get-in plotProps [:outerSize :w]) 60) ", 10)")))
    (cmap/add-colormap-vert (js/d3.select (str "#cp-" id " svg .colormap")) color (str "colormap2dplot-" id)  (get data-container "plot_varconverter"))

    (defn update-crosshair [[relx rely]]
      (let [imageSize (get-in plotProps [:imageSize])
            x (* relx (get imageSize :w))
            y (* (- 1 rely) (get imageSize :h))]
        (-> (js/d3.selectAll ".colorplot svg .line-vert")
            (.attr "x1" x) (.attr "x2" x)
            (.attr "y1" 0) (.attr "y2" (get imageSize :h)))
        (-> (js/d3.selectAll ".colorplot svg .line-hor")
            (.attr "x1" 0) (.attr "x2" (get imageSize :w))
            (.attr "y1" y) (.attr "y2" y))))

    (-> (js/d3.select (str "#cp-" id " svg"))
        (.on "mousemove" (fn [] (this-as this ;(swap! app-state assoc-in [:mouse] (js->clj (js/d3.mouse this))) 
                                         (let [rel-coords (calc-rel-coords (js->clj (js/d3.mouse this)))
                                               loc-coords (rel-to-loc rel-coords px-ts-mapping (get data-container "rg"))
                                               mouse      [(aget js/d3.event "pageX") (aget js/d3.event "pageY")]]
                                               ;loc        (vis/restructure-loc loc-coords)]
                                           (update-crosshair rel-coords)
                                           (if-not (nil? (first rel-coords))
                                             (update-tooltip data-container loc-coords rel-coords mouse)
                                             (-> (js/d3.select "#tree-tooltip") (.style "opacity" 0)))))))

        (.on "click" (fn [] (this-as this
                                     (let [rel-coords (calc-rel-coords (js->clj (js/d3.mouse this)))
                                           loc-coords (rel-to-loc rel-coords px-ts-mapping (get data-container "rg"))]
                                          ;loc        (vis/restructure-loc loc-coords)]
                                       (println ((js/d3.utcFormat "%Y-%m-%d %H:%M:%S") (js/Date. (* (goog.object/get (get data-container "ts") (first loc-coords)) 1000))) loc-coords)
                                       (add-click-maker (js/d3.select (str "#cp-" id " svg")) (js/d3.mouse this))
                                       (async/put! click-channel loc-coords)))))


        (.on "mouseleave" (fn [] (update-crosshair [nil nil]) (-> (js/d3.select "#tree-tooltip") (.style "opacity" 0))))))))



(defn plot-timeseries [colorplotProps-cursor id data-containers-cursor which click-channel]
  (console.log "plot timeseries" id which)
  (if-not (nil? (get-in @data-containers-cursor [which]))
    (let [;plotProps (get-in @app-state [:colorplotProps])
          plotProps @colorplotProps-cursor
          data-container (get-in @data-containers-cursor [which])
          context (-> (js/d3.select (str "#cp-" id " canvas")) (.node) (.getContext "2d"))
          px-ts-mapping (pixel-time-map (get-in plotProps [:imageSize :w]) (get data-container "ts"))
          calc-rel-coords (setup-rel-coords plotProps)
          scale-fn (case (get data-container "plot_varconverter")
                         "log" (js/d3.scaleLog)
                            (js/d3.scaleLinear))]
          
      (println "Update colorplot " (keys data-container) (get data-container "system") (get data-container "name"))
      (-> (js/d3.select (str "#cp-" id " svg")) (.selectAll "*") (.remove))
      (-> context (.clearRect 0 0 (get-in plotProps [:outerSize :w]) (get-in plotProps [:outerSize :h])))

     (let [times (map (fn [t] (js/Date. (* t 1000))) (get data-container "ts"))
           timedomain [(first times) (last times)]
           timerange [0 (get-in plotProps [:imageSize :w])]
           scaletime (-> (js/d3.scaleTime) (.domain (clj->js timedomain)) (.range (clj->js timerange)))
           ;
           var_lims (get data-container "var_lims")
           vardomain (case (get data-container "plot_varconverter")
                      "log" [(max 1e-8 (first var_lims)) (last var_lims)]
                      [(first var_lims) (last var_lims)])
           varrange [(get-in plotProps [:imageSize :h]) 0]
           scalevar (-> scale-fn (.clamp true) (.domain (clj->js vardomain)) (.range (clj->js varrange)))
           data (js/custom.merge_mask_1d (clj->js times) (get data-container "var") (get data-container "mask"))]

       (-> (js/d3.select (str "#cp-" id " svg"))
           (.append "g")
           (.attr "transform" (str "translate(50," (+ 5 (get-in plotProps [:imageSize :h])) ")"))
           (.call (-> (js/d3.axisBottom scaletime)
                      (.tickFormat (js/d3.utcFormat "%H:%M:%S")))))
       (-> (js/d3.select (str "#cp-" id " svg"))
           (.append "g")
           (.attr "transform" (str "translate(50," "5)"))
           (.call (-> (js/d3.axisLeft scalevar))))    
       
       (-> (js/d3.select (str "#cp-" id " svg"))
           (.append "path")
           (.datum data)
           (.attr "fill" "none")
           (.attr "stroke" "steelblue")
           (.attr "transform" (str "translate(50," "5)"))
           (.attr "stroke-width", 1.5)
           (.attr "d", (-> (js/d3.line)
                           (.x (fn [d] (scaletime (goog.object/get d "t"))))
                           (.y (fn [d] (scalevar (goog.object/get d "v"))))))))
      

     (-> (js/d3.select (str "#cp-" id " svg"))
         (.append "text")
         (.attr "transform" (str "translate("
                                 (/ (get-in plotProps [:imageSize :w]) 2) ","
                                 (+ (get-in plotProps [:imageSize :h]) 40) ")"))
         (.attr "font-weight" "bold")
         (.text "Time [UTC]"))

     (-> (js/d3.select (str "#cp-" id " svg"))
         (.append "text")
         (.attr "transform" (str "translate("
                                 11 ","
                                 (+ (/ (get-in plotProps [:imageSize :h]) 2) 70) ")"
                                 "rotate(-90)"))
         (.attr "font-weight" "bold")
         (.text (str which " [" (get data-container "var_unit") "]")))

     (-> (js/d3.select (str "#cp-" id " svg"))
         (.append "text")
         (.attr "transform" (str "translate("
                                 (+ (get-in plotProps [:imageSize :w]) 68) ","
                                 (- (get-in plotProps [:imageSize :h]) 7) ")"
                                 "rotate(-90)"))
         (.text (str which)))

     (-> (js/d3.select (str "#cp-" id " svg"))
         (.append "text")
         (.attr "transform" (str "translate("
                                 (+ (get-in plotProps [:imageSize :w]) 85) ","
                                 (- (get-in plotProps [:imageSize :h]) 7) ")"
                                 "rotate(-90)"))

         (.text (str " [" (get data-container "var_unit") "]")))

     (-> (js/d3.select (str "#cp-" id " svg"))
         (.append "line")
         (.attr "class" "line-vert")
         (.attr "transform" "translate(50,5)")
         (.attr "stroke" "black") (.attr "stroke-width" "1px"))
      ;(.attr "x1" x) (.attr "x2" x))
      ;(.attr "y1" 0) (.attr "y2" (get imageSize :h)))
     (-> (js/d3.select (str "#cp-" id " svg"))
         (.append "line")
         (.attr "class" "line-hor")
         (.attr "transform" "translate(50,5)")
         (.attr "stroke" "black") (.attr "stroke-width" "1px"))
      ;(.attr "x1" x) (.attr "x2" x)))
      ;(.attr "y1" 0) (.attr "y2" (get imageSize :h))))

     (defn update-crosshair [[relx rely]]
       (let [imageSize (get-in plotProps [:imageSize])
             x (* relx (get imageSize :w))
             y (* (- 1 rely) (get imageSize :h))]
         (-> (js/d3.selectAll ".colorplot svg .line-vert")
             (.attr "x1" x) (.attr "x2" x)
             (.attr "y1" 0) (.attr "y2" (get imageSize :h)))
         (-> (js/d3.selectAll ".colorplot svg .line-hor")
             (.attr "x1" 0) (.attr "x2" (get imageSize :w))
             (.attr "y1" y) (.attr "y2" y))))

     (-> (js/d3.select (str "#cp-" id " svg"))
         (.on "mousemove" (fn [] (this-as this ;(swap! app-state assoc-in [:mouse] (js->clj (js/d3.mouse this))) 
                                          (let [rel-coords (calc-rel-coords (js->clj (js/d3.mouse this)))
                                                loc-coords (rel-to-loc rel-coords px-ts-mapping (h/clj->js (range (get-in plotProps [:imageSize :h]))))
                                                mouse      [(aget js/d3.event "pageX") (aget js/d3.event "pageY")]]
                                               ;loc        (vis/restructure-loc loc-coords)]
                                            (update-crosshair rel-coords)
                                            (if-not (nil? (first rel-coords))
                                              (update-tooltip data-container loc-coords rel-coords mouse)
                                              (-> (js/d3.select "#tree-tooltip") (.style "opacity" 0)))))))

         (.on "click" (fn [] (this-as this
                                      (let [rel-coords (calc-rel-coords (js->clj (js/d3.mouse this)))
                                            loc-coords (rel-to-loc rel-coords px-ts-mapping (get data-container "rg"))]
                                          ;loc        (vis/restructure-loc loc-coords)]
                                        (println ((js/d3.utcFormat "%Y-%m-%d %H:%M:%S") (js/Date. (* (goog.object/get (get data-container "ts") (first loc-coords)) 1000))) loc-coords)
                                        (add-click-maker (js/d3.select (str "#cp-" id " svg")) (js/d3.mouse this))
                                        (async/put! click-channel loc-coords)))))


         (.on "mouseleave" (fn [] (update-crosshair [nil nil]) (-> (js/d3.select "#tree-tooltip") (.style "opacity" 0))))))))



(defn plot-dispatch [colorplotProps-cursor id data-containers-cursor which click-channel]
  (if-not (nil? (get-in @data-containers-cursor [which]))
   (let [data-container (get-in @data-containers-cursor [which])
         dimlabel (get data-container "dimlabel")]
     (println "plot dispatch" dimlabel)
     (case dimlabel
       ["time" "range"] (plot-timeheight colorplotProps-cursor id data-containers-cursor which click-channel)
       ["time"] (plot-timeseries colorplotProps-cursor id data-containers-cursor which click-channel)
       (ant/notification-error {:message "Error"  :duration 20
                                :description (str "dimlabel " dimlabel " of " which " not supported yet.")})))))
