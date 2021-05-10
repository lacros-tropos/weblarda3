(ns weblarda3.explorer
  (:require
   [reagent.core :as reagent]
   [cljs.core.async :as async]
   [cljs-http.client :as http]
   [ajax.core :as ajax] ; library making error handling possible  
   [ajax.protocols :as protocol]    
   [antizer.reagent :as ant]
   [clojure.string :as string]
   [clojure.set :as set]
   [goog.iter]
   [weblarda3.vis :as vis]
   [cljsjs.msgpack-lite :as msgpack]
   [weblarda3.vis :as vis]
   [weblarda3.colorplot :as cplot]))
   

;
;
; link for the data api
; http://larda.tropos.de/larda3/api/lacros_dacapo/POLLY/attbsc1064?rformat=json&interval=1549238460.0-1549396800.0%2C0-8000
; rformat = {bin|msgpack|json}
;
; for the explorer
; http://larda.tropos.de/larda3/explorer/lacros_dacapo/

; http://localhost:3449/explorer.html?interval=1549385800.0-1549396800.0%2C0-8000&params=CLOUDNET|WIDTH
; http://localhost:3449/explorer.html?interval=1549385800.0-1549396800.0%2C0-8000&params=POLLYNET|attbsc1064
; http://localhost:3449/explorer.html?interval=1549385800.0-1549396800.0%2C0-8000&params=CLOUDNET|Z
; http://larda.tropos.de/larda3/explorer/lacros_dacapo?interval=1549385800.0-1549396800.0,0-8000&params=CLOUDNET|Z,CLOUDNET|LDR,CLOUDNET_LIMRAD|Z,POLLYNET|attbsc1064
; http://larda.tropos.de/larda3/explorer/lacros_dacapo?interval=1549385800.0-1549396800.0,0-8000&params=CLOUDNET|Z,CLOUDNET|LDR,CLOUDNET_LIMRAD|Z,POLLYNET|attbsc1064,SHAUN|VEL

; TODO
; - update link on changes
; - info panel
; - categorial colormaps
; - more agressive compiler options
 
(println "search" (.. js/window -location -search))
(println "href" (.. js/window -location -href))

; dev or hard coded
;(defonce host "http://larda.tropos.de/larda3/")
;(defonce host "http://larda3.tropos.de/")
;get from index.html <script> tag
;(defonce host js/hostaddr)
(defonce host (second (re-find #"(.+)explorer" js/hostaddr)))
(println "set host " host)

(defn keys-to-str [list]
  (goog.iter/join (clj->js (map name list)) "|"))

(defn paramstr-to-vec [param_string]
 (string/split param_string #"\|"))

(defonce app-state
  (reagent/atom {:coll1-vis true
                 :c-list []
                 :c-selected "mosaic"
                 :c-info {}
                 :sel-time-range {"ts" [(-> (js/moment) (.utc) (.subtract 4 "hours") (.format "X"))
                                        (-> (js/moment) (.utc) (.format "X"))]
                                  "rg" [0 12000]}
                 :data-containers {}
                 :mapping-container-plot {}                 
                 :data-loading-progress false
                 ;:colorplotProps {:outerSize {:w 800 :h 450} :imageSize {:w 685 :h 400}}
                 :colorplotProps {:outerSize {:w 750 :h 450} :imageSize {:w 635 :h 400}}}))


(defonce modal-container (reagent/atom {:show false
                                        :param-str ""}))

(defonce modal-info (reagent/atom {:show false
                                   :title ""
                                   :content "loading.."}))

(defn fetch-description [param_string]
  (let [c-name (get-in @app-state [:c-selected])
        system (first (string/split param_string #"\|"))
        param (second (string/split param_string #"\|"))]
    (println "request string" (str host "description/" c-name "/" system "/" param))
    (swap! modal-info assoc-in [:title] (str system ": " param))
    (async/go (let [response (async/<! (http/get (str host "description/" c-name "/" system "/" param) {:as :raw :with-credentials? false}))]
                ;(println "retrieved" (:status response) (:body response))
                (println "fetched new description string " c-name system param (:status response))
                (swap! modal-info assoc-in [:content] (get-in response [:body]))))))

(def colorplotProps-cursor (reagent/cursor app-state [:colorplotProps]))
(def data-containers-cursor (reagent/cursor app-state [:data-containers]))
(def cplot-click (async/chan))

(defn get-permalink []
; ?interval=1549385800.0-1549396800.0%2C0-8000&params=CLOUDNET|WIDTH
 (let [ts-interval (string/join "-" (get-in @app-state [:sel-time-range "ts"]))
       rg-interval (string/join "-" (get-in @app-state [:sel-time-range "rg"]))
       interval (str ts-interval "," rg-interval)
       params (string/join "," (keys (get-in @app-state [:data-containers])))] 
  (str host "explorer/" (get @app-state :c-selected) "?interval=" interval "&params=" params)))

(defn parse-number-nan [str]
  (if (and (> (count str) 0) (not (js/isNaN (js/parseFloat str))))
    (js/parseFloat str)
    nil))

(defn data-container-extract-meta [data-container]
 (let [meta data-container
       var (goog.object/get data-container "var")
       mask (goog.object/get data-container "mask")
       ts (goog.object/get data-container "ts")
       rg (goog.object/get data-container "rg")
       varconverter (case (goog.object/get data-container"plot_varconverter")
                     "lin2z" "dB"
                     (goog.object/get data-container"plot_varconverter"))]
  (println "extract-meta:" varconverter)      
  (js-delete meta "var")
  (js-delete meta "mask")
  (js-delete meta "ts")
  (js-delete meta "rg")
  (assoc (js->clj meta)
         "plot_varconverter" varconverter
         "var" var "mask" mask
         "ts" ts "rg" rg)))

(defn hex-comma-and-split [str]
  (if str 
   (string/split (string/replace str #"%2C" ",") #",") []))

(defn infer-time-format [str]
  (if (string/includes? str "_") 
    (case (count str) 
      11 (.format (js/moment.utc str "YYYYMMDD_HH") "X")
      13 (.format (js/moment.utc str "YYYYMMDD_HHmm") "X")
      15 (.format (js/moment.utc str "YYYYMMDD_HHmmss") "X")) 
    str))

(defn interval-to-dict [interval]
  (case (count interval) 
   0 {"ts" [(-> (js/moment) (.utc) (.subtract 4 "hours") (.format "X"))
            (-> (js/moment) (.utc) (.format "X"))]
      "rg" [0 12000]} 
   1 {"ts" (mapv infer-time-format (string/split (first interval) #"-")) 
      "rg" [0 12000]} 
   2 {"ts" (mapv infer-time-format (string/split (first interval) #"-")) 
      "rg" (mapv js/parseFloat (string/split (second interval) #"-"))}))
 
(defn empty-places [dict places]
 (vec (set/difference (set places) (set (keys dict)))))

(defn update-plots []
  (println "update plots" (get-in @app-state [:mapping-container-plot]))
  ;(println (keys @data-containers-cursor))
  (doall (for [k [0 1 2 3]] 
          (do (-> (js/d3.select (str "#cp-" k " svg")) (.selectAll "*") (.remove))
              (-> (js/d3.select (str "#cp-" k " canvas")) (.node) (.getContext "2d")
                  (.clearRect 0 0 (get-in @colorplotProps-cursor [:outerSize :w]) (get-in @colorplotProps-cursor [:outerSize :h]))))))
  (doall (for [[k v] (get-in @app-state [:mapping-container-plot])]
           (do ;(println "update " k v)
            ;(cplot/colorplot-did-mount colorplotProps-cursor k data-containers-cursor v cplot-click)
            (cplot/plot-dispatch colorplotProps-cursor k data-containers-cursor v cplot-click)))))

(defn http-error-handler [uri]
 (fn [{:keys [status status-text response]}]
  (ant/notification-error {:message (str "Error " status) :duration 0 :description (str "while fetching data " status-text)})
  (.log js/console (str "Error occured: " status) 
        ; new TextDecoder ("utf-8") .decode (uint8array)
        status-text uri (.decode (js/TextDecoder. "utf-8") response))))         

(defn custom-http-get [uri]
  (let [channel (async/chan)]
    (async/go 
     (ajax/GET uri
       {:with-credentials false
        :response-format {:content-type "application/msgpack" :description "data conainer msgpack" :read protocol/-body :type :arraybuffer}     
        :handler #(async/put! channel %)
        :error-handler (http-error-handler uri)}))
   channel))


(defn fetch-data [c-name param_string time-interval range-interval]
  (let [sys_param (paramstr-to-vec param_string)
        url (str host "api/" c-name "/" (first sys_param) "/" (second sys_param) "?rformat=msgpack&interval="
                 (first time-interval) "-" (last time-interval) "%2C" (first range-interval) "-" (last range-interval))]
    (js/console.log "fetch data larda url: " url)
    (swap! app-state assoc-in [:data-loading-progress] true)
    (ant/notification-open {:message (str "retrieving data from larda") :duration 0 :key "retData"})
    ; (async/go
    ;   (let [response (async/<! (custom-http-get url))]
    ;     (println response)
    ;     ;(println (msgpack.decode response))
    ;     (println (msgpack.decode (new js/Uint8Array response)))))
    ;(async/go (let [response (async/<! (http/get url {:response-type :array-buffer :with-credentials? false}))]
    (async/go (let [response (async/<! (custom-http-get url))]
                (println "fetched data " c-name param_string time-interval range-interval)
                (let [decoded (msgpack.decode (new js/Uint8Array response))
                      mapping-container-plot (get @app-state :mapping-container-plot)
                      empty-plot (first (empty-places mapping-container-plot [0 1 2 3]))]
              ;(goog.object/set decoded "var" (c/pull_loc_in decoded))
              ;(println (type (:body response)) (:body response))
              ;(js/console.log (type (:body response)) (:body response))
              ;(js/console.log response (new  js/Uint8Array (:body response)))
              ;(js/console.log "decoded data" decoded)
              ;(js/console.log "decoded data" (msgpack.decode (new js/Uint8Array (:body response))))
              ;(swap! app-state assoc-in [:alltrees] decoded)
              ;(reset! alltrees-cursor decoded)
                  (swap! app-state assoc-in [:data-containers param_string] (data-container-extract-meta decoded))
                  (swap! app-state assoc-in [:backend :data-loading-progress] false)
              ; TODO more sophisticated version required here
                  (if-not (or (nil? empty-plot) (some #(= param_string %) (vals mapping-container-plot)))
                    (swap! app-state assoc-in [:mapping-container-plot empty-plot] param_string))
                  (ant/notification-close "retData")
                  (update-plots)
                  (println "data loading done; mapping " mapping-container-plot (get @app-state :mapping-container-plot)))))))


; extract from query string
(defn extract-from-address [address]
 (let [href (.. address -href)
       ;href "http://larda.tropos.de/larda3/explorer/lacros_dacapo/"
       ;href "http://larda3.tropos.de/explorer/lacros_cycare/"
       c-name (last (string/split (first (string/split href #"\?")) #"/"))
       query-string (.. address -search)
       ;query-string "?rformat=json&interval=1549238460.0-1549396800.0%2C0-8000&params=CLOUDNET_LIMRAD|VEL"
       ;query-string "?rformat=json&interval=1549385800.0-1549396800.0%2C0-8000&params=CLOUDNET_LIMRAD|VEL,CLOUDNET|CLASS"
       ;                                    19951231_235959
       ;query-string "?rformat=json&interval=1549238460.0-1549396800.0"
       ;query-string "?rformat=json&interval=19951231_23-19951231_235959"
       interval (hex-comma-and-split (second (re-find #"interval=([^&]*)" query-string)))
       sel-time-range (interval-to-dict interval)
       params (hex-comma-and-split (second (re-find #"params=([^&]*)" query-string)))]
   ;define default values for interval
   (println "extract from address" href query-string)
   (println (string/split (first (string/split href #"\?")) #"/") "=> " c-name)
   (println "interval" interval (count interval) sel-time-range) (println "params" params)

   (swap! app-state assoc-in [:c-selected] c-name)
   (if (> (count interval) 0)
     (swap! app-state assoc-in [:sel-time-range] sel-time-range))
   (doall (for [param params]
            (fetch-data (get @app-state :c-selected) param (get-in @app-state [:sel-time-range "ts"]) (get-in @app-state [:sel-time-range "rg"]))))))


;query-string (second (re-find #"camp=([^&]*)" (.. js/window -location -search)))

(extract-from-address (.. js/window -location))

; get the paramaeter list...
(defn fetch-c-info [c-name & [additional-atom]]
  (ant/notification-open {:message "loading data availability " :description c-name :duration 0 :key "retData"})
  (swap! app-state assoc-in [:param-sel] [])
  (async/go (let [response (async/<! (http/get (str host "api/" c-name "/") {:as :json :with-credentials? false}))]
             ;(println (:status response) (:body response))
              (println "fetched new c-info for " c-name (:status response))
              (ant/notification-close "retData")
              (if-not (= (:status response) 200)
               (do (println "response " response)
                (ant/notification-error {:message "Error" :duration 0 :description "while fetching campaign info"})))
              ;(ant/notification-info {:message "Hint" :description "right-click on parameter to display description text" :duration 20})
              ;(set-initial-selection (get-in response [:body]))
              (let [duration (get-in response [:body :config_file :duration])
                    last (last (last duration))
                    timestart (.format (js/moment.utc last "YYYYMMDD") "X")
                    timeend (str (+ (js/parseFloat timestart) (* 6 3600)))]
                (if-not (nil? additional-atom) 
                  (do (swap! additional-atom assoc-in ["ts"] [timestart timeend])
                      (swap! app-state assoc-in [:sel-time-range "ts"] [timestart timeend]))))
              (swap! app-state assoc-in [:c-info] (get-in response [:body])))))

; replace camp by /camp/
(async/go (let [response (async/<! (http/get (str host "api/") {:as :json :with-credentials? false}))
                query-string (second (re-find #"camp=([^&]*)" (.. js/window -location -search)))]
            ;(println "found string " query-string (get-in response [:body :campaign_list]))
            (if-not (= (:status response) 200)
              (do (println "response " response)
                (ant/notification-error {:message "Error" :duration 0 :description (str "while fetching basic info " host)})))
            (if-not (some #{(get-in @app-state [:c-selected])} (get-in response [:body :campaign_list]))
              (swap! app-state assoc-in [:c-selected] (first (get-in response [:body :campaign_list]))))
            (if (and (not (nil? query-string)) (some #{query-string} (get-in response [:body :campaign_list])))
              (swap! app-state assoc-in [:c-selected] query-string))
            ; at first check if Query_String provided campaign
            (println "fetch campaign list" (:status response) (:body response))
            (fetch-c-info (get @app-state :c-selected))
            (swap! app-state assoc-in [:c-list] (get-in response [:body :campaign_list]))))


(defn change-campaign [& [additional-atom]]
  (fn [c-name]
  ;this is to update the link
  ;(let [query-string (.. js/window -location -search)]
  ;  (if (count query-string)
  ;    (js/window.history.replaceState (clj->js nil) (clj->js nil) (string/replace query-string #"camp=([^&]*)" (str "camp=" c-name)))))
    (swap! app-state assoc-in [:c-selected] (js->clj c-name))
    (swap! app-state assoc-in [:data-containers] {})
    (swap! app-state assoc-in [:mapping-container-plot] {})
    (update-plots)
    (fetch-c-info c-name additional-atom)))
  


(defn update-time-range [args]
  ; update app-state
  ; download for all the present parameters
  (swap! app-state assoc-in [:sel-time-range] args)
  ;when time is updated we need to upddate the data as well
 (let [params (vec (keys (get-in @app-state [:data-containers])))] 
   (println "update params " params)
   (swap! app-state assoc-in [:data-containers] {})
   (doall (map #(fetch-data (get @app-state :c-selected) % (get-in @app-state [:sel-time-range "ts"]) (get-in @app-state [:sel-time-range "rg"])) params)))
  ;  (doall (for [param params]
  ;           (do (println "update time range" param)
  ;               (fetch-data (get @app-state :c-selected) param 
  ;                           (get-in @app-state [:sel-time-range "ts"]) (get-in @app-state [:sel-time-range "rg"]))))))
 (println "update time range " args))

(defn add-param [param]
  ;#(reset! panel1-param (js->clj %))
  ; download
  (fetch-data (get @app-state :c-selected) param (get-in @app-state [:sel-time-range "ts"]) (get-in @app-state [:sel-time-range "rg"]))
  (console.log "add param " param))


(defn delete-container [param_string]
 (fn [] (println param_string (keys (get @app-state :data-containers)))
  (swap! app-state assoc-in [:data-containers] (dissoc (get @app-state :data-containers) param_string))
  (let [map-cont-plot (get-in @app-state [:mapping-container-plot])
        filtered (into {} (filter (fn [[k v]] (not= param_string v)) map-cont-plot))]
    (println "filtered " filtered)
    (swap! app-state assoc-in [:mapping-container-plot] filtered))
  (println "new mapping " (get-in @app-state [:mapping-container-plot]))
  (update-plots)))
 
(defn entry-in-list []
  (fn [data-containers k v]
    ;(js/console.log entry)
    ;(js/console.log "operator " (get entry "operator") (type (get entry "operator")))
    ;(js/console.log "ndfilters " (get-in entry ["ndfilters"]))
    ; (println "id?" (get entry "time") (get entry "id"))
    [:tr
     [:td k] 
     [:td [:select {:on-change (fn [e] ;(println (.. e -target -value) (get-in @data-containers [k "colormap"]))
                                 (swap! data-containers assoc-in [k "colormap"]  (.. e -target -value)))
                    :value (get-in @data-containers [k "colormap"])}          
           (for [cmap (js->clj (js/colormaps.available))]
             ^{:key cmap} [:option {:value cmap} cmap])]]
     [:td [:select {:on-change (fn [e] ;(println (.. e -target -value) (get-in @data-containers [k "colormap"]))
                                 (swap! data-containers assoc-in [k "plot_varconverter"]  (.. e -target -value)))
                    :value (get-in @data-containers [k "plot_varconverter"])}          
           (for [cmap cplot/avail-var-converters]
             ^{:key cmap} [:option {:value cmap} cmap])]]
     [:td [ant/input {:size "small" :style {:margin-left "0px" :margin-top "0px" :width "65px" :text-align "right"}
                      :default-value (get-in @data-containers [k "var_lims" 0])
                      :onChange #(swap! data-containers assoc-in [k "var_lims" 0] (js->clj (.. % -target -value)))}]
          " - "
          [ant/input {:size "small" :style {:margin-left "0px" :margin-top "0px" :width "65px" :text-align "right"}
                      :default-value (get-in @data-containers [k "var_lims" 1])
                      :onChange #(swap! data-containers assoc-in [k "var_lims" 1] (js->clj (.. % -target -value)))}]]
     [:td [ant/button {:on-click #((swap! modal-container assoc-in [:param-str] k) (swap! modal-container update-in [:show] not)) :size "small" :icon "bars"}]]
     [:td [ant/button {:on-click #((swap! modal-info update-in [:show] not) (fetch-description k)) :size "small" :icon "info-circle"}]]
     [:td [ant/button {:on-click (delete-container k) :size "small" :type "danger"} "x"]]]))
   

(defn editor-container-list [app-state]
  (let [data-containers (reagent/atom (get @app-state :data-containers))]
   (fn []
     (if-not (= (set (keys @data-containers)) (set (keys (get @app-state :data-containers)))) 
      (do (reset! data-containers (get @app-state :data-containers)) (println "made editor data containers consistent")))
     [:div
      [:table#editorleft 
       [:thead [:tr [:th "Parameter"] [:th "Colormap"] [:th "Scale"] [:th "Limits"] [:th ""] [:th "Descr."]]]
       [:tbody (for [[k v] (get @app-state :data-containers)] ^{:key k} [entry-in-list data-containers k v])]]
      [ant/button {:on-click #((swap! app-state assoc-in [:data-containers] @data-containers) (update-plots))
                   :size "small"} "update plots"] 
      [:br][:br]
      "container - plot - mapping"
      [:table#editorleft 
       [:tbody (doall (for [v [0 1 2 3]] ^{:key v} [:tr 
                                                    [:td (str "Plot " v)] 
                                                    [:td [:select {:on-change (fn [e] ;(println (.. e -target -value) (get-in @data-containers [k "colormap"]))
                                                                               (swap! app-state assoc-in [:mapping-container-plot v]  (.. e -target -value))
                                                                               (update-plots))
                                                                   :value (get-in @app-state [:mapping-container-plot v])}          
                                                          (doall (for [params (conj (keys (get @app-state :data-containers)) "")]
                                                                  ^{:key params} [:option {:value params} params]))]]]))]]])))
    

(defn update-time-by [atm idx-old idx-new amount] 
 (let [value (js/parseFloat (get-in @atm ["ts" idx-old]))
       new (+ value amount)]
   ;(println "update time " value new)
   (swap! atm assoc-in ["ts" idx-new] (str new))))
   
(defn disabled-date? [current]
  (let [duration (get-in @app-state [:c-info :config_file :duration])
        testfn (fn [pair current] (and (> current (js/moment.utc (first pair) "YYYYMMDD"))
                                       (< current (.add (js/moment.utc (second pair) "YYYYMMDD") 1 "days"))))
        in-one? (mapv #(testfn % current) duration)]
    (not (some true? in-one?))))

(defn editor []
  (let [panel1-time-range (reagent/atom (get @app-state :sel-time-range))
        panel1-param (reagent/atom "select")
        check-valid (fn [] (and (< (int (get-in @panel1-time-range ["ts" 0])) (int (get-in @panel1-time-range ["ts" 1])))
                                (< (- (int (get-in @panel1-time-range ["ts" 1])) (int (get-in @panel1-time-range ["ts" 0]))) 86400)
                                (< (int (get-in @panel1-time-range ["rg" 0])) (int (get-in @panel1-time-range ["rg" 1])))))]
   (fn []
    [:div#editor {:style {:min-width "600px" :width "700px" :margin-left "30px"}}
     [ant/collapse
      [ant/collapse-panel  {:header "Time, Range, Parameter" :key "1"}
       [ant/row
        [ant/col {:span 12}       
         [:table#editorleft 
          [:thead]
          [:tbody
           [:tr [:td "Begin"]
            [:td [ant/date-picker {:format "YYYYMMDD_HHmm" :value (.utc (js/moment.unix (get-in @panel1-time-range ["ts" 0])))
                                   :disabled-date disabled-date?
                                   :on-change (fn [_ d] (swap! panel1-time-range assoc-in ["ts" 0] (.format (js/moment.utc d "YYYYMMDD_HHmm") "X")))
                                   :style {:width "100%"} :show-today false :size "small"
                                   :showTime {:use12Hours false :format "HH:mm"}}] 
             [:br] 
             [:span.clickable {:on-click #(update-time-by panel1-time-range 0 0 -3600)} "-1h"] 
             [:span.clickable {:on-click #(update-time-by panel1-time-range 0 0 -600)} "-10min"]
             [:span.clickable {:on-click #(update-time-by panel1-time-range 0 0 600)} "+10min"]
             [:span.clickable {:on-click #(update-time-by panel1-time-range 0 0 3600)} "+1h"]]]
           [:tr [:td "End"]
            [:td [ant/date-picker {:format "YYYYMMDD_HHmm" :value (.utc (js/moment.unix (get-in @panel1-time-range ["ts" 1])))
                                   :disabled-date disabled-date?
                                   :on-change (fn [_ d] (swap! panel1-time-range assoc-in ["ts" 1] (.format (js/moment.utc d "YYYYMMDD_HHmm") "X")))
                                   :style {:width "100%"} :show-today false :size "small"
                                   :showTime {:use12Hours false :format "HH:mm"}}]             
             [:br]
             [:span.clickable {:on-click #(update-time-by panel1-time-range 1 1 -3600)} "-1h"]
             [:span.clickable {:on-click #(update-time-by panel1-time-range 1 1 -600)} "-10min"]
             [:span.clickable {:on-click #(update-time-by panel1-time-range 1 1 600)} "+10min"]
             [:span.clickable {:on-click #(update-time-by panel1-time-range 1 1 3600)} "+1h"]
             [:span.clickable {:on-click #(update-time-by panel1-time-range 0 1 14400)} "Begin +4h"]]]
           [:tr [:td "Range"] [:td [ant/input {:size "small" :style {:margin-left "6px" :margin-top "5px" :width "70px" :text-align "right"}
                                               :default-value (get-in @panel1-time-range ["rg" 0])
                                               :onChange #(swap! panel1-time-range assoc-in ["rg" 0] (js->clj  (.. % -target -value)))}]
                               " - "
                               [ant/input {:size "small" :style {:margin-left "6px" :margin-top "5px" :width "70px" :text-align "right"}
                                           :default-value (get-in @panel1-time-range ["rg" 1])
                                           :onChange #(swap! panel1-time-range assoc-in ["rg" 1] (js->clj (.. % -target -value)))}]]]
           [:tr]]] [ant/button {:on-click #(update-time-range @panel1-time-range) 
                                :disabled (not (check-valid))
                                :size "small"} "update"]]
         
        [ant/col {:span 12}
         [:table#editorleft 
          [:thead]
          [:tbody
           [:tr [:td "Campaign"] [:td [ant/select {:showSearch true
                                                   :placeholder "select campaign"
                                                   :value (get-in @app-state [:c-selected])
                                                   :onChange (change-campaign panel1-time-range)
                                                   :style {:width "150px"} :size "small"}
                                       (for [c-name (get-in @app-state [:c-list])] ^{:key c-name} [ant/select-option {:value c-name} c-name])]]]
           [:tr [:td "Add Parameter"]
            [:td
             [ant/tree-select {:value @panel1-param :size "small"
                               :placeholder "Please select" :showSearch true
                               :onChange (fn [param] (reset! panel1-param (js->clj param)) (add-param (js->clj param)))}
              (doall (for [system (sort (keys (get-in @app-state [:c-info :connectors])))]
                       ^{:key system} [ant/tree-tree-node {:title system :selectable false :style {:font-size 11}}
                                       (for [param (sort (keys (get-in @app-state [:c-info :connectors (keyword system) :params])))]
                                         ^{:key (keys-to-str [system param])} [ant/tree-tree-node 
                                                                               {:value (keys-to-str [system param]) 
                                                                                :title (keys-to-str [system param])}])]))]]]]]]
        (if (> (int (get-in @panel1-time-range ["ts" 0])) (int (get-in @panel1-time-range ["ts" 1]))) [:p.er "Time: begin > end"])         
        (if (> (- (int (get-in @panel1-time-range ["ts" 1])) (int (get-in @panel1-time-range ["ts" 0]))) 86400) [:p.er "Time: too long"])
        ;(if (< (count (get @panel1-data :sel-camp)) 1) [:p.er "Select campaign"])
        (if (> (int (get-in @panel1-time-range ["rg" 0])) (int (get-in @panel1-time-range ["rg" 1]))) [:p.er "Range: top > bottom"])

        [ant/col {:span 24} [:br] [:div [ant/button {:on-click #(js/custom.copy_string (get-permalink)) :size "small"} "copy permalink"] 
                                   [:span.permalink (get-permalink)]]]]]]
     [ant/collapse
           [ant/collapse-panel  {:header "Plot properties" :key "1"}
            [editor-container-list app-state]]]])))


; (defn list-data-contianers [] 
;  [:div#rightfloat  (doall (for [data-cont (keys (get-in @app-state [:data-containers]))] ^{:key data-cont} [:div data-cont [:br]]))])


(defn colorplot-component [id]
  (reagent/create-class
   {:reagent-render #(cplot/colorplot-render colorplotProps-cursor id)
    :component-did-mount #(cplot/plot-dispatch colorplotProps-cursor id data-containers-cursor (get-in @app-state [:mapping-container-plot id]) cplot-click)
    :component-did-update #(cplot/plot-dispatch colorplotProps-cursor id data-containers-cursor (get-in @app-state [:mapping-container-plot id]) cplot-click)}))


(defn display-modal-info []
  (fn []
    [ant/modal {:visible (get-in @modal-info [:show]) :title (str "Description " (get-in @modal-info [:title]))
                :on-ok #(reset! modal-info {:show false :title "" :content "loading..."})
                :on-cancel #(reset! modal-info {:show false :title "" :content "loading..."})}
     (reagent/as-element [:div {:dangerouslySetInnerHTML {:__html (string/replace (get-in @modal-info [:content]) #"\n" "<br />")}}])]))


(defn data-container-meta-only [data-container]
  (let [new (dissoc data-container "var" "mask" "ts" "rg")
        ts (get data-container "ts")
        rg (get data-container "rg")]
   (assoc new
          "ts" [(first ts) (last ts)] "rg" [(first rg) (last rg)])))

(defn pretty-container [data-container]
 (with-out-str (cljs.pprint/pprint data-container)))

(defn display-modal-container []
  (fn []
    [ant/modal {:visible (get-in @modal-container [:show]) :title (str "data_container " (get-in @modal-container [:param-str]))
                :on-ok #(reset! modal-container {:show false :param-str ""})
                :on-cancel #(reset! modal-container {:show false :param-str ""})}
     (let [container-meta (data-container-meta-only (get-in @app-state [:data-containers (get-in @modal-container [:param-str])]))
           pretty-str (js/JSON.stringify (clj->js container-meta) js/undefined 2)]
       ;(console.log (clj->js pretty-str))
       (reagent/as-element [:div {:dangerouslySetInnerHTML {:__html (str "<pre>" (string/replace pretty-str #"\n" "<br />") "</pre>")}}]))]))

(defn page []
  [:div
   [:h1 "Data explorer"]
   [editor]
   ;[list-data-contianers]
   [:div.colorplot-container
    [colorplot-component 0]
    [colorplot-component 1]]
   [:div.colorplot-container
    [colorplot-component 2]
    [colorplot-component 3]]
   [:div {:id "tree-tooltip"}]
   [display-modal-info]
   [display-modal-container]
   [:br]])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize App

; (defn dev-setup []
;   (when ^boolean js/goog.DEBUG
;     (enable-console-print!)
;     (println "dev mode")))

(enable-console-print!)
    

(defn reload []
  (reagent/render [page]
                  (.getElementById js/document "app")))

(defn ^:export main []
  ;(dev-setup)
  (reload))


