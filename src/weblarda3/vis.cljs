(ns weblarda3.vis
  (:require
   [reagent.core :as reagent]
   [cljs.core.async :as async]
   [clojure.string :as string]
   [goog.iter]
   [cljsjs.d3]
   [weblarda3.colormap :as cmap]))



(defn split-to-keyword [kwstr]
  ;(console.log (str kwstr))
  (mapv keyword (string/split kwstr #"\|")))

(defn keys-to-str [list]
 ;(console.log (str list))  
 (goog.iter/join (clj->js (map name list)) "|"))


(def shape-def {1 ["M 0.5 0.5 L 13 0.5 L 13 13 L 0.5 13 Z"]
                2 ["M 0.5 0.5 L 13 0.5 L 0.5 13 Z"
                   "M 0.5 13 L 13 13 L 13 0.5 Z"]
                3 ["M  0.5 0.5 L 6.25 6.25 L 13 0.5 Z"
                   "M  13 0.5 L 6.25 6.25 L 13 13 Z"
                   "M  13 13 L 6.25 6.25 L 0.5 13 Z"
                   "M  0.5 13 L 6.25 6.25 L 0.5 0.5 Z"]
                4 ["M  0.5 0.5 L 6.25 6.25 L 13 0.5 Z"
                   "M  13 0.5 L 6.25 6.25 L 13 13 Z"
                   "M  13 13 L 6.25 6.25 L 0.5 13 Z"
                   "M  0.5 13 L 6.25 6.25 L 0.5 0.5 Z"]})



(defn map-params-to-path [p-strings connectors]
  (let [keys (mapv split-to-keyword p-strings)
        paths (mapv (fn [[system param]] 
                     (keys-to-str [system (get-in connectors [system :params param])])) 
               keys)]
   (zipmap p-strings paths)))


(defn extract-daylist [connectors]
 (flatten (mapv (fn [[s con]] (mapv (fn [[path dates]] (keys dates)) (get con :avail))) connectors)))

(defn get-avail-day [connectors day]
 (let [lst (mapv (fn [[s con]] (mapv (fn [[path dates]] [(keys-to-str [s path]) (get dates day)]) (get con :avail))) connectors)]
  ;(println "avail-day" (into {} (mapv vec (partition 2 (map #(if (nil? %) 0 %) (flatten lst))))))
  (into {} (mapv vec (partition 2 (map #(if (nil? %) 0 %) (flatten lst)))))))

(defn remap-connectors [connectors]
 (let [alldays (vec (sort (set (remove nil? (extract-daylist connectors)))))]
  ;(println "alldays" alldays)
  ;(println "avail for single day" (get-avail-day connectors :20190110))
  (apply array (map (fn [d] (clj->js {"key" (name d) "data" (get-avail-day connectors d)})) alldays))))


(defn make-year-group [data]
  ;(console.log "starting make year group")
  ;(console.log "looped once" data)
  (-> (js/d3.select "svg#calendar") (.selectAll "*") (.remove))
  (-> (js/d3.select "svg#calendar")
      ;(.style "background-color" "grey")
      (.selectAll)
      (.data (-> (d3.nest) (.key #(.getFullYear ((js/d3.utcParse "%Y%m%d") (aget % "key")))) (.entries data)))
      (.enter)
      (.append "g")
      (.attr "id" (fn [d i] (aget d "key")))
      (.attr "transform" (fn [d i] (str "translate(0, " (+ 5 (* i 160)) ")")))))
 


(defn get-color [max-files]
 (fn [no]
   (if (> no 0)
     ((-> (js/d3.scaleSequential js/d3.interpolateRdYlGn) (.domain (clj->js [0 max-files]))) no)
     "red")))


(defn get-daterange [year]
 (js/d3.timeDays (js/Date. (js/parseInt year) 0 1) (js/Date. (+ (js/parseInt year) 1) 0 1)))

(defn week-pos [d] (js/parseInt ((js/d3.timeFormat "%W") d)))
(defn day-pos [d] (mod (+ (.getDay d) 6) 7))


(defn monthpath [t0] 
 (let [t1 (js/Date. (.getFullYear t0) (+ (.getMonth t0) 1) 0)
       d0 (day-pos t0)
       w0 (week-pos t0)
       d1 (day-pos t1)
       w1 (week-pos t1)]
   (str "M" (* (+ w0 1) 14) "," (* d0 14)
        "H" (* w0 14) "V" (* 7 14)
        "H" (* w1 14) "V" (* (+ d1 1) 14)
        "H" (* (+ w1 1) 14) "V" 0
        "H" (* (+ w0 1) 14) "Z")))  


;; rendering, plotting updating
(defn tooltip-content [d]
  ;(console.log (aget d "data"))
  (str "<strong>"(aget d "key") "</strong><table>" 
       (goog.iter/join (clj->js (mapv 
                                 (fn [[d n]] (str "<tr><td class='left'>" d ":</td><td  class='right'>" n "</td></tr>")) 
                                 (sort (js->clj (aget d "data"))))) "")
   "</table>"))

(defn calendar-render [app-state]
  ; for some reason we have to deref the state once to trigger the updates
  [:div {:id "cal"} (str (get-in @app-state [:c-info :config-file :duration]))
   [:div {:id "tree-tooltip"}]
   [:svg {:width 780
          :height 1000
          :id "calendar"}]])


(defn get-link [host campaign ts-interval rg-interval params]
; ?interval=1549385800.0-1549396800.0%2C0-8000&params=CLOUDNET|WIDTH
  (let [ts-interval (string/join "-" ts-interval)
        rg-interval (string/join "-" rg-interval)
        interval (str ts-interval "," rg-interval)
        params-str (string/join "," params)]
    (str host "explorer/" campaign "?interval=" interval "&params=" params-str)))


(defn make-explorer-link [host date campaign sel-param]
  (let [begin (js/moment.utc date "YYYYMMDD")
        begin-ts (.format begin "X")
        end (.add (js/moment.utc date "YYYYMMDD") 1 "days")
        end-ts (.format end "X")
        link (get-link host campaign [begin-ts end-ts] [0 12000] sel-param)]
    (println link)
    (.open js/window link "_blank")))

(defn calendar-did-mount [app-state]
  (println "@calendar-did-mount update detected" (get @app-state :param-sel))
  (let [param-sel (get-in @app-state [:param-sel])
        connectors (get-in @app-state [:c-info :connectors])
        sel-param-to-path (map-params-to-path param-sel connectors)
        sel-paths (set (vals sel-param-to-path))
        data  (remap-connectors connectors)
        [offsetCalX offsetCalY] [25 30]
        colors (get-color (get @app-state :max-files))
        ;grouped-data (group-by #(.getFullYear (get % "date")) data)
        cals (make-year-group data)]    
   (-> cals
       (.append "text")
       (.attr "class" "yearlabel")
       (.attr "offset" 50)
       (.attr "y" 14)
       (.attr "font-weight" "bold")
       (.text #(aget % "key")))
   (-> cals
       (.append "g")
       (.attr "id" "alldays")
       (.selectAll ".day")
       (.data #(get-daterange (aget % "key")))
       (.enter) (.append "rect")
       (.attr "id" #((js/d3.timeFormat "%Y-%m-%d") %))
       (.attr "class" "day")
       (.attr "width" 14) (.attr "height" 14)
       (.attr "x" #(+ offsetCalX (* 14 (week-pos %))))
       (.attr "y" #(+ offsetCalY (* 14 (day-pos %))))
       (.attr "fill" "#e4e4e4")
        ;(.attr "stroke" "#ccc")
       (.attr "stroke" "white")
       (.datum (js/d3.timeFormat "%Y-%m-%d")))

   (-> cals
       (.append "g")
       (.attr "id" "dayLabels")
       (.selectAll ".daylabel")
       (.data (clj->js ["Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"]))
       (.enter)
       (.append "text")
       (.attr "class" "daylabel") (.attr "x" 0)
       (.attr "y" (fn [d i] (+ offsetCalY 5 (* i 14))))
       (.attr "dy" "0.6em")
       (.text (fn [d i] d)))
   (-> cals
       (.append "g")
       (.attr "id" "monthLabels")
       (.selectAll ".monthlabel")
       (.data (clj->js ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]))
       (.enter)
       (.append "text")
       (.attr "class" "daylabel") (.attr "y" 17)
       (.attr "x" (fn [d i] (+ offsetCalX 27 (* i 60))))
       (.attr "dy" "0.6em")
       (.text (fn [d i] d)))

   (-> cals (.append "g")
       (.attr "id" "dataDays")
       (.selectAll ".dataday-group")
       (.data #(aget % "values"))
       (.enter) (.append "g")
       (.attr "class" "dataday-group")
       (.attr "id" #(aget % "key"))
       (.attr "width" 14) (.attr "height" 14)
       (.attr "stroke" "black")
       (.attr "x" #(+ offsetCalX (* 14 (week-pos ((js/d3.utcParse "%Y%m%d") (aget % "key"))))))
       (.attr "y" #(+ offsetCalY (* 14 (day-pos ((js/d3.utcParse "%Y%m%d") (aget % "key"))))))
       (.attr "transform" #(str "translate("
                                (+ offsetCalX (* 14 (week-pos ((js/d3.utcParse "%Y%m%d") (aget % "key"))))) ", "
                                (+ offsetCalY (* 14 (day-pos ((js/d3.utcParse "%Y%m%d") (aget % "key"))))) ")"))
        ;(.attr "fill" (fn [d] (colors (goog.object/getValueByKeys d "data" (first sel-paths)))))
       (.on "click" (fn [d] (make-explorer-link (get @app-state :host) (.. d -key) (get-in @app-state [:c-selected]) (get-in @app-state [:param-sel]))))
       (.on "mouseover" (fn [d] (let [mouse [(aget js/d3.event "pageX") (aget js/d3.event "pageY")]]
                                   ;(println mouse)                         
                                  (-> (js/d3.select "#tree-tooltip") (.style "opacity" 1)
                                      (.html (tooltip-content d))
                                      (.style "left" (str (- (first mouse) 70) "px"))
                                      (.style "top" (str (+ 40 (last mouse)) "px"))))))
       (.on "mouseout" (fn [d] (-> (js/d3.select "#tree-tooltip") (.style "opacity" 0)))))

;(-> (js/d3.selectAll ".dataday-group")
    

    ; (-> (js/d3.selectAll ".dataday-group")
    ;     ;(.selectAll ".data-elem")
    ;     ;(.data (fn [d] (console.log "data-elem" d) d))
    ;     ;(.enter)
    ;     (.append "path")
    ;     (.attr "fill" (fn [d] (colors (goog.object/getValueByKeys d "data" (first sel-paths)))))
    ;     (.attr "stroke-width" 0.0)
    ;     (.attr "d" "M 0.5 0.5 L 13 0.5 L 0.5 13 Z")
    ;     (.attr "class" "data-elem"))
    
    ; (-> (js/d3.selectAll ".dataday-group")
    ;     ;(.selectAll ".data-elem")
    ;     ;(.data (fn [d] (console.log "data-elem" d) d))
    ;     ;(.enter)
    ;     (.append "path")
    ;     (.attr "fill" (fn [d] (colors (goog.object/getValueByKeys d "data" (second sel-paths)))))
    ;     (.attr "stroke-width" 0.0)
    ;     (.attr "d" "M 0.5 13 L 13 13 L 13 0.5 Z")
    ;     (.attr "class" "data-elem"))
    
    ;add the rectangles for days with data
   (doseq [[i p] (map-indexed vector sel-paths)]
     ;(println i p (count sel-paths))
     (-> (js/d3.selectAll ".dataday-group")
         (.append "path")
         (.attr "fill" (fn [d] (colors (goog.object/getValueByKeys d "data" p))))
         (.attr "stroke-width" 0.0)
         (.attr "d" (nth (get shape-def (count sel-paths)) i))
         (.attr "class" "data-elem")))
    
    ;add the colormap
   (-> (js/d3.select "svg#calendar") 
       (.append "text") (.text "No of files")
       (.style "font-weight" "bold")
       (.style "font-size" 12)
       (.attr "transform" (str "translate(" 540 ", 154)")))
   (-> cals
       (.append "g") (.attr "id" "colormap")
       (.attr "transform" (str "translate(" 615 ", 135)")))
   (cmap/add-colormap (js/d3.select "#colormap")
                      (-> (js/d3.scaleSequential js/d3.interpolateRdYlGn) (.domain (clj->js [0 (get @app-state :max-files)])))
                      "colormap2dplot")

   (-> cals (.append "g")
       (.attr "id" "monthoutlines")
       (.selectAll ".month-line")
       (.data #(js/d3.timeMonths
                (js/Date. (js/parseInt (aget % "key")) 0 1)
                (js/Date. (+ (js/parseInt (aget % "key")) 1) 0 1)))
       (.enter) (.append "path")
       (.attr "class" "month-line")
       (.attr "transform" (str "translate(" offsetCalX ", " offsetCalY ")"))
       (.attr "d" monthpath)))

   ;(println grouped-data)
  
 "empty")

;(defn calendar-did-update [app-state]
;(println (map-params-to-path (get-in @app-state [:param-sel]) (get-in @app-state [:c-info :connectors])))
;"empty")

