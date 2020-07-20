(ns weblarda3.colormap
  (:require
   [cljsjs.d3]))



(defn colorscale2gradient [color]
  (let [values (js->clj (.domain color))
        color-domain [(first values) (last values)]
        new-scale (-> (js/d3.scaleLinear) (.domain (clj->js [0 100])) (.range (clj->js color-domain)))]
    (apply array (map (fn [r] (clj->js {"offset" (str r "%") "color" (color (new-scale r))})) (range 101)))))

(defn add-colormap [g color idcolormap]
  ;(println (colorscale2gradient color))
  ;(println (clj->js (colorscale2gradient color)))
  (-> g
      (.append "defs")
      (.append "linearGradient")
      (.attr "id" idcolormap)
      (.attr "x1" "0%")
      (.attr "y1" "0%")
      (.attr "x2" "100%")
      (.attr "y2" "0%"))
  (-> (js/d3.select (str "#" idcolormap))
      (.selectAll "stop")
      (.data (colorscale2gradient color))
      (.enter)
      (.append "stop")
      (.attr "offset" (fn [d] (.-offset d)))
      (.attr "stop-color" (fn [d] (.-color d))))

  (-> g
      (.append "rect")
      (.attr "width" 150)
      (.attr "height" 12)
      (.style "fill" (str "url(#" idcolormap ")")))
  (let [values (js->clj (.domain color))
        zscale (-> (js/d3.scaleLinear) (.domain (clj->js [(first values) (last values)]))
                   (.range (clj->js [0 150])))]
    (-> g
        (.append "g")
        (.attr "transform" "translate(-0.5,9)")
        (.call (-> (js/d3.axisBottom zscale) (.ticks 5))))))



(defn add-colormap-vert [g color idcolormap scale]
  ;(println (colorscale2gradient color))
  ;(println (clj->js (colorscale2gradient color)))
  (-> g
      (.append "defs")
      (.append "linearGradient")
      (.attr "id" idcolormap)
      (.attr "x1" "0%")
      (.attr "y1" "100%")
      (.attr "x2" "0%")
      (.attr "y2" "0%"))
  (-> (js/d3.select (str "#" idcolormap))
      (.selectAll "stop")
      (.data (clj->js (colorscale2gradient color)))
      (.enter)
      (.append "stop")
      (.attr "offset" (fn [d] (.-offset d)))
      (.attr "stop-color" (fn [d] (.-color d))))

  (-> g
      (.append "rect")
      (.attr "width" 20)
      (.attr "height" 200)
      (.style "fill" (str "url(#" idcolormap ")")))
  (let [values (js->clj (.domain color))
        min-max (case scale
                  "log" (mapv #(js/Math.pow 10 %) [(first values) (last values)])
                  [(first values) (last values)])
        scale-fn (case scale
                   "log" (js/d3.scaleLog)
                   (js/d3.scaleLinear))
        zscale (-> scale-fn (.domain (clj->js min-max))
                   (.range (clj->js [200 0])))]
    (-> g
        (.append "g")
        (.attr "transform" "translate(15,0)")
        (.call (-> (js/d3.axisRight zscale))))))
        ;(.call (-> (js/d3.axisRight zscale) (.tickFormat (js/d3.format ".1e")))))))

(defn evenly-spaced-indices ;get a number of evenly spaced indices from a range of length
  [no length]
  (let [dx (/ (dec length) (dec no))]
    (mapv #(js/Math.round (* dx %)) (vec (range no)))))

(defn make-colormap [colors_cmap & & {:keys [:rg :thres :scale] :or {:rg [-40 5] :thres [] :scale ""}}]
  ;(println "scale at make-colormap" scale rg (mapv js/Math.log10 rg))
  (let [rg-scale (case scale
                   "log" (mapv js/Math.log10 rg)
                   rg)
        indices-cmap (js/custom.range 0 (alength colors_cmap))
        ;cmap-steps (map (-> (js/d3.scaleLinear)
        ;             (.domain (array 0 (alength colors_cmap)))
        ;             (.range (clj->js rg-scale)))
        ;             (range (alength colors_cmap)))
        cmap-steps (.map indices-cmap (-> (js/d3.scaleLinear)
                                       (.domain (array 0 (alength colors_cmap)))
                                       (.range (clj->js rg-scale))))
        colors-thres (if (empty? thres)
                       colors_cmap
                       (.map cmap-steps (-> (js/d3.scaleThreshold)
                                         (.domain (clj->js thres))
                                         (.range (clj->js (vals (select-keys colors_cmap (evenly-spaced-indices (inc (count thres)) (alength colors_cmap)))))))))]   
                            
    ;(println rg-scale cmap-steps)
    (-> (js/d3.scaleLinear)
        (.domain cmap-steps)
        (.range colors-thres))))
