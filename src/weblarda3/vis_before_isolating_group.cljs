(ns weblarda3.vis_old
  (:require
   [reagent.core :as reagent]
   [cljs.core.async :as async]
   [clojure.string :as string]
   [goog.iter]
   [cljsjs.d3]))



(defn split-to-keyword [kwstr]
  (mapv keyword (string/split kwstr #"\|")))

(defn keys-to-str [list]
  (goog.iter/join (clj->js (map name list)) "|"))
 

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
   ;(println "avail-day" (into {} (mapv vec (partition 2 (flatten lst)))))
  (into {} (mapv vec (partition 2 (flatten lst))))))

(defn remap-connectors [connectors]
 (let [alldays (vec (sort (set (remove nil? (extract-daylist connectors)))))]
  ;(println "alldays" alldays)
  ;(println "avail for single day" (get-avail-day connectors :20190110))
  (mapv (fn [d] {"date" ((js/d3.utcParse "%Y%m%d") (name d)) "key" d "data" (get-avail-day connectors d)}) alldays)))


(defn make-year-group [data]
  ;(console.log "make-year-group" (clj->js data))
  (-> (js/d3.select "svg#calendar") (.selectAll "*") (.remove))
  (-> (js/d3.select "svg#calendar")
      ;(.style "background-color" "grey")
      (.selectAll)
      (.data (-> (d3.nest) (.key #(.getFullYear (aget % "date"))) (.entries (clj->js data))))
      (.enter)
      (.append "g")
      (.attr "id" (fn [d i] (aget d "key")))
      (.attr "transform" (fn [d i] (str "translate(0, " (+ 5 (* i 160)) ")")))))
 


(defn get-color [no] 
 (if (> no 0) 
  ((-> (js/d3.scaleSequential js/d3.interpolateCool) (.domain (clj->js [30 0]))) no)
  "red"))


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
   [:svg {:width 800
          :height 1000
          :id "calendar"}]])

(defn calendar-did-mount [app-state]
  (println "update detected" (get @app-state :param-sel))
  (let [param-sel (get-in @app-state [:param-sel])
        connectors (get-in @app-state [:c-info :connectors])
        sel-param-to-path (map-params-to-path param-sel connectors)
        sel-paths (set (vals sel-param-to-path))
        data  (remap-connectors connectors)
        [offsetCalX offsetCalY] [25 30]
        colors get-color
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
        (.attr "fill" "white") (.attr "stroke" "#ccc")
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
        (.selectAll ".dataday")
        (.data #(aget % "values"))
        (.enter) (.append "rect")
        (.attr "class" ".dataday")
        (.attr "id" #(aget % "date"))
        (.attr "width" 14) (.attr "height" 14)
        (.attr "stroke" "lightgrey")
        (.attr "x" #(+ offsetCalX (* 14 (week-pos (aget % "date")))))
        (.attr "y" #(+ offsetCalY (* 14 (day-pos (aget % "date")))))
        (.attr "fill" (fn [d] (colors (goog.object/getValueByKeys d "data" (first sel-paths)))))
        (.on "mouseover" (fn [d] (let [mouse [(aget js/d3.event "pageX") (aget js/d3.event "pageY")]]   
                                   ;(println mouse)                         
                                   (-> (js/d3.select "#tree-tooltip") (.style "opacity" 1) 
                                       (.html (tooltip-content d))
                                       (.style "left" (str (- (first mouse) 70) "px")) 
                                       (.style "top" (str (+ 40 (last mouse)) "px"))))))
        (.on "mouseout" (fn [d] (-> (js/d3.select "#tree-tooltip") (.style "opacity" 0)))))
    
    

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
